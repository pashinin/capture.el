;;; capture-ui --- some UI functions
;;; Commentary:
;;; Code:

(require 'capture-vars)
(require 'capture-functions)

(defun capture-select-preset-btn-click (button)
  "Function for selecting current preset."
  (interactive)
  (let ((num
         (buffer-substring (line-beginning-position) (+ (line-beginning-position) 1))))
    ;;(message num)
    (setq capture-selected-number (- (string-to-number num) 1))
    ;;(setq capture-preset-current (nth  capture-presets))
    (capture-update-buffer)))

(defun capture-start-btn-click (button)
  "Function for \"Start button\" for capture-mode."
  (interactive)
  (capture-start)
  (capture-update-buffer))

(defun capture-stop-btn-click (button)
  "Function for \"Stop button\" for capture-mode."
  (interactive)
  (capture-stop-all)
  (sleep-for 1)
  (capture-update-buffer)
  (message "All processes stopped"))

(defun capture-get-marked-audio ()
  "Return a list of marked audio devices to record."
  (interactive)
  (save-excursion
    (let ((res "asd") mk)
      (goto-char (search-forward "Audio:"))
      (forward-line 1)
      (setq mk (buffer-substring (line-beginning-position)
                                 (+ (line-beginning-position) 3)))
      (while (or (string= mk "[x]")
                 (string= mk "[ ]"))
        ;;(message "aa")
        (forward-line 1)
        (setq mk (buffer-substring (line-beginning-position)
                                   (+ (line-beginning-position) 3)))
        (if (string= mk "[x]")
            (message (buffer-substring (+ (line-beginning-position) 4)
                                       (line-end-position)))
          )))))

(defun capture-change-destdir-btn-click (button)
  "Choose temp dir for saving video."
  (interactive)
  (call-interactively 'capture-change-tmpdir))

(defun capture-change-tmpdir-btn-click (button)
  "Choose temp dir for saving video."
  (interactive)
  (call-interactively 'capture-change-tmpdir))

(defun capture-audio-check-btn-click (button)
  "Choose temp dir for saving video."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\[ \\]")
      (message "asd")
    (if (looking-at "\\[x\\]")
        (message "asd2"))))

(define-button-type 'capture-select-preset-btn
  'action 'capture-select-preset-btn-click
  'follow-link t
  'help-echo "Click button")
(define-button-type 'capture-start-btn
  'action 'capture-start-btn-click
  'follow-link t
  'help-echo "Click button")
(define-button-type 'capture-stop-btn
  'action 'capture-stop-btn-click
  'follow-link t
  'help-echo "Click button")
(define-button-type 'capture-change-destdir-btn
  'action 'capture-change-destdir-btn-click
  'follow-link t
  'help-echo "Click button")
(define-button-type 'capture-change-tmpdir-btn
  'action 'capture-change-tmpdir-btn-click
  'follow-link t
  'help-echo "Click button")
(define-button-type 'capture-audio-check-btn
  'action 'capture-audio-check-btn-click
  'follow-link t
  'help-echo "Click button")

(defun capture-jump-to-next-button ()
  "Jump to the next text button from current position."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'button)
                 (point-max)))
  (goto-char (or (next-single-property-change (point) 'button)
                 (point-max))))

(defun capture-jump-to-prev-button ()
  "Jump to the next text button from current position."
  (interactive)
  (let ((minpoint 14))
    (goto-char (or (previous-single-property-change (point) 'button)
                   minpoint))
    (goto-char (or (previous-single-property-change (point) 'button)
                   minpoint))))

(defun capture-preset-caption (preset)
  "Generate a caption for PRESET."
  (interactive)
  (let ((x (car preset))
        (y (nth 1 preset))
        (w (nth 2 preset))
        (h (nth 3 preset))
        (fps (nth 4 preset))
        (title (nth 7 preset))
        (audio (nth 8 preset)))
    (concat title " " (number-to-string w) "x" (number-to-string h) ", "
            (number-to-string fps) "fps, "
            (nth 5 preset))
    ))

(defun capture-update-buffer (&optional force)
  "Update the content of the capture buffer.
FORCE - use t to update the list of devices."
  (interactive)
  (when (and (string= capture-buffer-name (buffer-name (current-buffer)))
           (eq major-mode 'fundamental-mode))
    (if capture-selected-number
        (setq capture-preset-current (nth capture-selected-number capture-presets))
      (setq capture-preset-current (car capture-presets)))
    (read-only-mode -1)
    (erase-buffer)
    (capture-get-audio-devices force)
    (if (capture-warning-no-program)
        (progn
          (local-unset-key (kbd "<up>"))
          (local-unset-key (kbd "<down>"))
          (local-unset-key (kbd "<left>"))
          (local-unset-key (kbd "<right>"))
          (if (eq system-type 'windows-nt)
              (progn
                (insert "Can not detect FFMPEG!\n")
                (insert " * Please install win32 static ffmpeg from here:\n")
                (insert "     http://ffmpeg.zeranoe.com/builds/\n")
                (insert " \n * And add ffmpeg's \"bin\" folder to PATH variable.\n\n"))
            (progn
              (insert "Can not detect \"avconv\" or \"ffmpeg\"!\n")
              (insert " Please install avconv or ffmpeg.\n\n")))
          (goto-char (point-min))
          (forward-line 2))

      (progn
        (define-key capture-mode-map (kbd "<up>")
          (lambda ()(interactive)(capture-jump-to-prev-button)))
        (define-key capture-mode-map (kbd "<down>")
          (lambda ()(interactive)(capture-jump-to-next-button)))
        (define-key capture-mode-map (kbd "<left>")
          (lambda ()(interactive)))
        (define-key capture-mode-map (kbd "<right>")
          (lambda ()(interactive)))

      (insert "Presets:\n")
      (let (preset
            (ind 1))
        (dolist (element capture-presets)
          (setq preset element)
          (insert (number-to-string ind) ". ")
          (insert-text-button (capture-preset-caption preset)
                              :type 'capture-select-preset-btn)
          (insert "\n")
          (setq ind (+ ind 1))))
      (when (capture-get-processes)
        (insert "\nProcesses:    ")
        (if (eq system-type 'windows-nt)
            (insert "\n Terminate by Ctrl+C in a ffmpeg window\n")
          (progn
            (insert-text-button "Stop all" :type 'capture-stop-btn)
            (insert "\n")
            (let ((index 0))
              (dolist (element (capture-get-processes))
                (insert "   " element "\n"))
              (setq index (+ index 1))))
          ))
      (insert "\n")

      ;; Current settings + START
      (insert "Selected:\n")
      (insert (capture-preset-caption capture-preset-current)
              "     ")
      (insert-text-button "Start" :type 'capture-start-btn)
      (insert "\nAudio: ")
      (let ((audio (nth 8 capture-preset-current)))
        (insert (if (not audio) "No audio" (car audio)) "\n"))

      (insert "\nAudio:\n")
      (let ((index 0) devname title
            (audio-rec-list (nth 8 capture-preset-current)))
        (dolist (element (append capture-audio-speakers
                                 capture-audio-microphones))
          (if (= (% index 2) 1)
              (progn
                (setq title element)
                (if (member title audio-rec-list)
                    (insert-text-button "[x]" :type 'capture-audio-check-btn)
                  (insert-text-button "[ ]" :type 'capture-audio-check-btn))
                (insert " " element "\n"))
            (setq devname element))
          (setq index (+ index 1))))
      (insert "\n")
      (read-only-mode)
      (goto-char 13)))))

(defun capture-force-update-buffer ()
  "Update capture-buffer and devices list."
  (capture-update-buffer t))

(provide 'capture-ui)
;;; capture-ui.el ends here
