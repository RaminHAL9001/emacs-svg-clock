#!/bin/sh
# Run "./svg-analog-clock.el" as a stand-alone application.

exec emacs -Q -mm \
     -l './svg-analog-clock.el' \
     --eval='(tool-bar-mode 0)' \
     --eval='(menu-bar-mode 0)' \
     --eval='(scroll-bar-mode 0)' \
     --eval='(scroll-bar-mode 0)' \
     --funcall='svg-clock-mode' ;
