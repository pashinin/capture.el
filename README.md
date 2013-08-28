#capture.el
[![Build Status](https://api.travis-ci.org/pashinin/capture.el.png)](https://travis-ci.org/pashinin/capture.el)

It is an Emacs extension to record video from dekstop.

## Install

    # some programs:
    sudo apt-get install libav-tools

## Configuration

    (require 'capture)

    (setq capture-video-dest-dir "~/screencasts/SORT/")
    (global-set-key (kbd "<s-f12>") 'capture-run-mode)

    (defun my-capture-presets ()
      "Make my presets for capturing."
      (interactive)
      (capture-presets-clear)
      (capture-add-preset 454 74 1280 720 15 "webm"
                          ""
                          "1280px (no audio)"))
    (my-capture-presets)

Where:

    454, 74               - x and y offset
    1280, 720             - width and height of the video
    15                    - frames per second
    "webm"                - extension for a filename

Optional parameters:

    ""                    - additional arguments for ffmpeg (avconv)
    "1280px (no audio)"   - preset title

You can also define some triggers:

    (defun capture-before-capture ()
      "Run this function before starting capturing."
      (interactive)
      ;;(suspend-frame)
      )

    (defun capture-after-capture ()
      "Run this function after we stopped capturing video."
      (interactive)
      ;;(message "finished")
      )

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
