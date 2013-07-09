;;; capture --- easy screencasting
;;; Commentary:
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
  (setq mode-name "Screencast")
  (use-local-map capture-mode-map)
  (capture-update-buffer))

(defun capture-run-mode ()
  "Just call capture-mode."
  (interactive)
  (capture-mode))

(provide 'capture)
;;; capture.el ends here
