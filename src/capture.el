;;; capture.el --- screencasting with "avconv" or "ffmpeg"
;;
;; Copyright (C) 2013 Sergey Pashinin
;; Author: Sergey Pashinin <sergey at pashinin dot com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;;
;;; Commentary:
;;
;; Capture is an Emacs mode to make screencasts on Linux easier.
;; (Well, maybe for some of you who use Emacs)
;;
;; Configuration
;; ------------------------
;;
;; (require 'capture)
;;
;; (setq capture-video-dest-dir "~/screencasts/SORT/")
;; (global-set-key (kbd "<s-f12>") 'capture-run-mode)
;;
;; (defun my-capture-presets ()
;;   "Make my presets for capturing."
;;   (interactive)
;;   (capture-presets-clear)
;;   (capture-add-preset 524 333 854 480 15 "webm" "854px (webcam mic)"
;;                       (list "Webcam C270 Analog Mono")
;;                       (concat capture-background-path my-854-wallpaper)))
;; (my-capture-presets)
;;
;; See more at https://github.com/pashinin/capture.el
;;
;;; Code:

(require 'capture-vars)
(require 'capture-helpers)
(require 'capture-functions)
(require 'capture-keys)
(require 'capture-ui)

(defun capture-mode ()
  "Create or open capture buffer and update it."
  (interactive)
  (switch-to-buffer (get-buffer-create capture-buffer-name))
  (kill-all-local-variables)
  (setq mode-name "capture")
  (use-local-map capture-mode-map)
  (if (not capture-presets)
      (setq capture-presets capture-presets-standard))
  (capture-update-buffer))

(defun capture-run-mode ()
  "Just call capture-mode."
  (interactive)
  (capture-mode))

(provide 'capture)
;;; capture.el ends here
