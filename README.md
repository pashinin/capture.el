#capture.el
[![Build Status](https://api.travis-ci.org/pashinin/capture.el.png)](https://travis-ci.org/pashinin/capture.el)

It is an Emacs extension to record video from dekstop.

## Configuration

    (require 'capture)

    (setq capture-video-dest-dir "~/screencasts/SORT/")
    (global-set-key (kbd "<s-f12>") 'capture-run-mode)

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

You can also write some triggers:

    (defun capture-before-capture ()
      "Run this function before starting capturing."
      (interactive)
      ;;(suspend-frame)
      )

    (defun capture-after-capture ()
      "Run this function after we stopped capturing video."
      (interactive)
      (set-desktop-background
       (concat capture-background-path my-default-wallpaper)))

## Run

Run it with:

    M-x capture-mode

or (for example):

    super+f12
    (global-set-key (kbd "<s-f12>") 'capture-run-mode)

## Useful keys

`g` - update buffer

When on audio string:

`w` - copy device title (`Built-in Audio Analog Stereo`)

`W` - copy device name (`alsa_input.pci-0000_00_1b.0.analog-stereo`)
