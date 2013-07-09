;;; capture-keys --- keys
;;; Commentary:
;;; Code:

(require 'capture-ui)

(defvar capture-mode-map nil "Keymap for capture-mode.")
;;(defvar capture-mode-map
(setq capture-mode-map
      (let ((capture-mode-map (make-keymap)))
        ;;(define-key capture-mode-map "\C-j" 'newline-and-indent)
        (define-key capture-mode-map (kbd "<up>")
          (lambda ()(interactive)(capture-jump-to-prev-button)))
        (define-key capture-mode-map (kbd "<down>")
          (lambda ()(interactive)(capture-jump-to-next-button)))
        (define-key capture-mode-map (kbd "<left>")
          (lambda ()(interactive)))
        (define-key capture-mode-map (kbd "<right>")
          (lambda ()(interactive)))
        (define-key capture-mode-map (kbd "t") 'capture-get-marked-audio)
        (define-key capture-mode-map (kbd "v")
          (lambda ()
            (interactive)
            (goto-char (point-min))
            (search-forward "Video:")
            (goto-char (+ (point) 5))
            ))
        (define-key capture-mode-map (kbd "a")
          (lambda ()
            (interactive)
            (goto-char (point-min))
            (search-forward "Audio:")
            (goto-char (+ (point) 5))
            ))
        (define-key capture-mode-map (kbd "w")
          (lambda ()
            (interactive)
            (kill-ring-save (+ (line-beginning-position) 4) (line-end-position))))
        (define-key capture-mode-map (kbd "C-c C-c") 'capture-current-preset-cmd)
        (define-key capture-mode-map (kbd "W")
          (lambda ()
            (interactive)
            (let ((title
                   (buffer-substring
                    (+ (line-beginning-position) 4) (line-end-position))))
              (with-temp-buffer
                (insert (capture-get-audio-name-by-title title))
                (kill-ring-save
                 (+ (line-beginning-position) 0) (line-end-position))))))
        (define-key capture-mode-map (kbd "SPC")
          (lambda ()
            (interactive)

            ))
        (define-key capture-mode-map (kbd "?")
          (lambda ()
            (interactive)
            (message "help")
            ))
        (define-key capture-mode-map (kbd "g") 'capture-update-buffer)
        (define-key capture-mode-map (kbd "G") 'capture-force-update-buffer)
        capture-mode-map))

;;"Keymap for capture-mode.")

(provide 'capture-keys)
;;; capture-keys.el ends here
