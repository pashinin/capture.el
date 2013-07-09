#capture.el
[![Build Status](https://api.travis-ci.org/pashinin/capture.el.png)](https://travis-ci.org/pashinin/capture.el)

It is an Emacs extension to record video from dekstop.

## Configuration

    (require 'capture)

    (defun my-capture-presets ()
      "Make my presets for capturing."
      (interactive)
      (capture-presets-clear)
      (capture-add-preset 524 333 854 480 15 "webm" "854px (webcam mic)"
                          (list "Webcam C270 Analog Mono")
                          (concat capture-background-path my-854-wallpaper)))
    (my-capture-presets)

Where:

    524, 333              - x and y offset
    854, 480              - width and height of the video
    15                    - frames per second
    "webm"                - extension for a filename

Optional parameters:

    "854px (webcam mic)"  - title of our preset
    (list "Webcam C270 Analog Mono") - list of audio devices to record
    (concat capture-background-path my-854-wallpaper) - wallpaper to set
