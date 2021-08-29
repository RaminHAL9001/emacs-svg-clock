;;; svg.el --- SVG image creation functions -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Ramin Honary <ramin.honary@gmail.org>
;; Keywords: svg clock time gui
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Defines `svg-analog-clock-display', which creates a buffer "*svg-analog-clock*"
;; and uses the Emacs SVG functionality to draw an analog clock with
;; the current time, which is update every second by a timer
;; object. The timer is deleted when the display buffer is
;; deleted. You can also delete the update timer with the
;; `svg-analog-clock-cancel-update-timer' function.
;;
;; 1. Open this file and do "M-x eval-buffer"
;;
;; 2. do "M-x svg-analog-clock-display" every time you want to see the clock
;;
;; 3. When you have seen enough of the clock, press "q" in the clock
;;    buffer window to delete the buffer.
;;
;; The timer object is set in the global variable
;; `svg-analog-clock-global-timer', so do not write to this variable ever, or
;; you may loose the handle on the timer and have to use the
;; `list-timers' function and take a guess at which timer is the one
;; updating the clock and delete it by hand.
;;
;; TODO:
;;
;; I would like to start breaking-out a lot of the drawing functions
;; defined in this program into their own custom variables and invite
;; people to have fun creating their own themes for this clock. I
;; envision people creating all sorts of different clock faces, such
;; as a clock with Tux the Penguin's hands pointing at the numbers, or
;; clocks showing the clockwork gears and sprockets, or a grandfather
;; clock face. I'd also like the option of showing year, month, and
;; date displays, something you'd see on a luxury-brand pocket watch.

(require 'cl)
(require 'seq)
(require 'svg)

;; -------------------------------------------------------------------------------------------------

(defvar svg-analog-clock-global-update-time-step 1.0
  "This sets the amount of time (in seconds) to wait between
updates of the \"*svg-analog-clock*\" buffer, this value is the
reciporical of the FPS or frame rate of the clock animation. The
default value is 1 second, the minimum value is 0.05 which will
set a timer at a rate of about 20 frames per second. After
changing this value the changes will not take effect until the
`svg-analog-clock-install-update-timer' function is callled.")

(defvar svg-analog-clock-show-seconds t
  "Set to `t' if you want to see the second hand, set to `nil' if
  you want to see only the hour and minute hand.")

(defun svg-analog-clock-change-update-time (dt)
  "Prompt for a new update time step value to be set to
`svg-analog-clock-global-update-time-step', and also reset the
timer so changes take effect immediately. The minimum valus is
0.04 seconds, the maximum value is 60.0 seconds."
  (interactive "nSVG analog clock update time: ")
  (svg-analog-clock-cancel-update-timer)
  (setq svg-analog-clock-global-update-time-step (max 0.04 (min 60.0 dt)))
  (svg-analog-clock-install-update-timer))

;; -------------------------------------------------------------------------------------------------
;; Implementing my own matrix arithmetic for rotating, scaling, and translating points on the
;; canvas. If there is a way to do this using only SVG directives, someone please show me how.

(defun point-rotate2d (angle cell)
  "Rotate a 2D point expressed in a cons cell."
  (let ((ca (cos angle))
        (sa (sin angle))
        (x (car cell))
        (y (cdr cell)))
    (cons (- (* x ca) (* y sa)) (+ (* x sa) (* y ca)))))

(defun point-scale2d (width height cell)
  "Rotate a 2D point expressed in a cons cell."
  (cons (* width (car cell)) (* height (cdr cell))))

(defun point-trans2d (x-off y-off cell)
  "Translating a 2D point expressed in a cons cell."
  (cons (+ x-off (car cell)) (+ y-off (cdr cell))))

(progn
 (byte-compile 'point-rotate2d)
 (byte-compile 'point-scale2d)
 (byte-compile 'point-trans2d)
 t)

;; -------------------------------------------------------------------------------------------------
;; Drawing the clock

(defun svg-analog-clock-draw (size hours minutes &optional seconds)
  "Produce an SVG image of a clock, with a square size of SIZE,
with the hands pointing at HOURS and MINUTES, and optionally
drawing a seconds hand pointing at SECONDS."
  (setq hours   (mod hours   12))
  (setq minutes (mod minutes 60))
  (let*((r (/ size 2))
        (s (numberp seconds))
        (orig r)
        (img (svg-create size size))
        hrs-a mins-a secs-a
        to-screen
        clockwise
        radial
        draw-tick)
    (setq seconds (if s (mod seconds 60) nil))
    (fset 'minsec-angle (lambda (mins) (/ (* mins pi) 30))) ; Works on minutes or seconds
    (fset 'hours-angle (lambda (hrs) (/ (* hrs pi) 6)))
    (setq secs-a (if s (minsec-angle seconds) nil))
    (setq mins-a (+ (minsec-angle minutes) (if s (/ secs-a 60.0) 0)))
    (setq hrs-a  (+ (hours-angle  hours)   (/ mins-a 12.0)))
    (fset 'to-screen (lambda (p) (point-scale2d r (- r) (point-trans2d 1.0 -1.0 p))))
    (fset 'clockwise (lambda (angle) (- (/ pi 2) angle)))
    (fset 'radial (lambda (angle p) (to-screen (point-rotate2d (clockwise angle) p))))
    (fset
     'draw-tick
     (lambda (img width length angle)
       (let*((outer (radial angle (cons 0.96 0.00)))
             (inner (radial angle (cons (- 0.96 length) 0.00))))
         (svg-line
          img (car outer) (cdr outer) (car inner) (cdr inner)
         :stroke-width width
         :stroke-color "black"))))
    ;; Border circle
    (svg-circle
     img orig orig (* 0.98 orig)
     :stroke-width 3
     :stroke-color "black"
     :fill-color "white")
    ;; Ticks
    (cl-loop ; Major ticks
     for hrs from 0 to 11 do
     (let*((hrs-a (hours-angle hrs))
           (fontsz (/ r 5.0))
           (fontshift (/ fontsz 3.0))
           (txpt
            (point-trans2d
             (- fontshift) fontshift
             (radial (- hrs-a (/ pi 90.0)) '(0.72 . 0.00)))))
       (draw-tick img 8 (if (= 0 (mod hrs 3)) 0.12 0.08) hrs-a)
       (svg-text img
        (prin1-to-string (if (= 0 hrs) 12 hrs))
        :x (car txpt) :y (cdr txpt)
        :font-size fontsz)
       (cl-loop ; Minor ticks
        for mins from 1 to 4 do
        (draw-tick img 1 0.06 (+ hrs-a (minsec-angle (* 6.0 mins)))))))
    ;; Hours Hand
    (svg-polyline img
     (mapcar
      (lambda (p) (radial hrs-a p))
      '(( 0.55 .  0.00)
        ( 0.50 .  0.04)
        (-0.08 .  0.06)
        (-0.08 . -0.06)
        ( 0.50 . -0.04)
        ( 0.55 .  0.00)))
     :line-width 0.5
     :stroke-color "white"
     :fill-color "black")
    ;; Minutes Hand
    (svg-polyline img
     (mapcar
      (lambda (p) (radial mins-a p))
      '(( 0.90 .  0.00)
        ( 0.84 .  0.02)
        (-0.12 .  0.03)
        (-0.12 . -0.03)
        ( 0.84 . -0.02)
        ( 0.90 .  0.00)))
     :line-width 0.5
     :stroke-color "white"
     :fill-color "black")
    ;; Seconds hand
    (when s
      (let ((tip  (radial secs-a '( 0.96 . 0.00)))
            (tail (radial secs-a '(-0.15 . 0.00))))
        (svg-line img orig orig (car tip ) (cdr tip ) :stroke-width 3 :stroke-color "red")
        (svg-line img orig orig (car tail) (cdr tail) :stroke-width 10 :stroke-color "red")))
    ;; Center point
    (svg-circle
     img orig orig (* r 0.04)
     :stroke-width (if s 0 1)
     :stroke-color (if s "red" "white")
     :fill-color (if s "red" "black"))
    ;; Return image
    (svg-image img)))

(progn
  (byte-compile 'svg-analog-clock-draw)
  t)

;; -------------------------------------------------------------------------------------------------
;; Define the major mode function

(define-derived-mode svg-analog-clock-major-mode special-mode "svg-analog-clock"
  (list
   :syntax-table nil
   :abbrev-table nil
   :after-hook
   (setq-local kill-buffer-hook #'svg-analog-clock-cleanup-hook)
   (setq-local cursor-type nil)
   (local-set-key (kbd "q") #'svg-analog-clock-kill-buffer)
   (local-set-key (kbd "C-x k") #'svg-analog-clock-kill-buffer)
   (local-set-key (kbd "<Del>") #'svg-analog-clock-cancel-update-timer)
   (local-set-key (kbd "<Backspace>") #'svg-analog-clock-cancel-update-timer)
   (local-set-key (kbd "<Ret>") #'svg-analog-clock-install-update-timer)))

(defun svg-analog-clock-update-time-to (hr min &optional sec)
  "This is the function that actuall calls the `svg-analog-clock-draw'
function which renders the SVG image and places it into a buffer
called \"*svg-analog-clock*\". The SEC (seconds) value is optional, and
may be a non-integer. If SEC is nil, no seconds hand will be
drawn on the clock."
  (let ((svg-analog-clock-result nil))
    (unwind-protect
        (setq
         svg-analog-clock-result
         (with-selected-window svg-analog-clock-global-window-handle
           (with-current-buffer svg-analog-clock-global-buffer-handle
             (remove-images (point-min) (point-max))
             (erase-buffer)
             (put-image
              (svg-analog-clock-draw
               (min (window-pixel-width)
                    (- (window-pixel-height) (window-mode-line-height)))
               hr min
               (if svg-analog-clock-show-seconds sec nil))
              (point-min))
             t)))
      (unless svg-analog-clock-result
        (message "*svg-analog-clock* cancelling update timer due to error...")
        (svg-analog-clock-cancel-update-timer)))))

(defun svg-analog-clock-default-get-time-hook ()
  "This function is used as the default value for the 'svg-analog-clock-get-time-hook"
  (let ((now (current-time)))
    (seq-let (sec min hr) (decode-time now)
      (vector hr min (+ sec (/ (caddr now) 1000000.0))))))

(defvar svg-analog-clock-get-time-hook #'svg-analog-clock-default-get-time-hook
  "This variable must be set to a function that that takes no
arguments and returns a vector of 3 integers containing the hour,
minute, and second value of the time that you want to have be
displayed in the \"*svg-analog-clock*\" buffer: [HOUR MINUTE SECOND].")

(defun svg-analog-clock-get-current-time ()
  "Calls the `svg-analog-clock-get-time-hook'"
  (if (functionp svg-analog-clock-get-time-hook)
      (funcall svg-analog-clock-get-time-hook)
    (svg-analog-clock-default-get-time-hook)))

(defun svg-analog-clock-update-time-now ()
  "Updates the current time display to the time returned by
`svg-analog-clock-get-current-time'."
  (interactive)
  (let ((svg-analog-clock-result nil))
    (unwind-protect
        (setq
         svg-analog-clock-result
         (seq-let [hr min sec] (svg-analog-clock-get-current-time)
           (svg-analog-clock-update-time-to hr min sec)
           t))
      (unless svg-analog-clock-result
        (message "*svg-analog-clock* cancelling timer due to error...")
        (svg-analog-clock-cancel-update-timer)))))

(progn
  (byte-compile 'svg-analog-clock-default-get-time-hook)
  (byte-compile 'svg-analog-clock-get-current-time)
  (byte-compile 'svg-analog-clock-update-time-now)
  (byte-compile 'svg-analog-clock-update-time-to)
  t)

(setq svg-analog-clock-global-update-timer nil)

(defun svg-analog-clock-install-update-timer ()
  "This function creates a new timer function that repeats at a
rate given by the `svg-analog-clock-global-update-time-step'
variable. If the variable is not set to a valid number value,
this function still succeeds but uses a default value of 1.0
seconds per update."
  (interactive)
  (let ((fps svg-analog-clock-global-update-time-step))
    (if (numberp fps)
        (setq fps (max fps 0.04))
      (setq fps 1.0))
    (when (timerp svg-analog-clock-global-update-timer)
      (svg-analog-clock-cancel-update-timer))
    (message (format "*svg-analog-clock* install timer, cycle every %0.4f seconds" fps))
    (setq
     svg-analog-clock-global-update-timer
     (run-at-time 0 fps #'svg-analog-clock-update-time-now))))

(defun svg-analog-clock-cancel-update-timer ()
  "This function cancels the global clock update timer for the \"*svg-analog-clock*\"."
  (interactive)
  (when (timerp svg-analog-clock-global-update-timer)
    (cancel-timer svg-analog-clock-global-update-timer)
    (message "*svg-analog-clock* timer canceled")
    (setq svg-analog-clock-global-update-timer nil)))

(defun svg-analog-clock-cleanup-hook ()
  (svg-analog-clock-cancel-update-timer)
  (unintern 'svg-analog-clock-global-buffer-handle obarray)
  (unintern 'svg-analog-clock-global-window-handle obarray))

(defun svg-analog-clock-kill-buffer ()
  "Delete the \"*svg-analog-clock*\" buffer, ensuring the update timer
  is canceled."
  (interactive)
  (kill-buffer "*svg-analog-clock*")
  (svg-analog-clock-cleanup-hook))

(defun svg-analog-clock ()
  "This function displays an analog clock rendered as an SVG
graphic. Obviously, you must be using the GUI version of
Emacs (as opposed to the CLI version) in order to use this
function. This function calls `svg-analog-clock-install-update-timer'
and lets the timer begin updating the \"*svg-analog-clock*\" buffer by
repeatedly calling the `svg-analog-clock-update-time-now' function. You
can cancel the timer by calling `svg-analog-clock-cancel-update-timer',
and then you can call `svg-analog-clock-update-time-to' function to
display whatever arbitrary time you choose. Killing the
\"*svg-analog-clock*\" buffer will delete the timer automatically."
  (interactive)
  (setq svg-analog-clock-global-buffer-handle (get-buffer-create "*svg-analog-clock*"))
  (setq svg-analog-clock-global-window-handle (get-buffer-window svg-analog-clock-global-buffer-handle))
  (unless svg-analog-clock-global-window-handle
    (setq svg-analog-clock-global-window-handle (selected-window)))
  (switch-to-buffer svg-analog-clock-global-buffer-handle)
  (with-selected-window svg-analog-clock-global-window-handle
    (with-current-buffer svg-analog-clock-global-buffer-handle
      (svg-analog-clock-major-mode)
      (svg-analog-clock-install-update-timer))))

(provide 'svg-analog-clock)
