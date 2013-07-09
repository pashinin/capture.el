;;; capture-functions --- Main functions
;;; Commentary:
;;; Code:

(require 'capture-helpers)

(defun capture-gen-avconv-audio-part (audio)
  "Return avconv cmd part based on AUDIO list."
  (interactive)
  (if audio
      (let (res)
        (setq res "")
        (dolist (element audio res)
          ;;(concat "-i " (capture-get-audio-name-by-title (car audio)) " ")
          (setq res
                (concat res " -f pulse -i " (capture-get-audio-name-by-title element) " ")))
        (if (> (length audio) 1)
            (setq res (concat res " -filter_complex amix=inputs="
                              (number-to-string (length audio)) ":duration=first:dropout_transition=3"))
          res))
      ""))

(defun capture-gen-cmd (x y w h fps filename &optional audio)
  "Generate \"avconv\" command with it's parameters.
X, Y - starting point,
W, H - size of the capturing frame,
FPS  - frames per second,
FILENAME - temp filename to save video,
AUDIO - a list of audio devices."
  (interactive)
  (if t
      (concat "avconv "
              (capture-gen-avconv-audio-part audio)
              " -show_region 1"
              " -f x11grab "
              " -r " (number-to-string fps)
              " -s " (number-to-string w) "x" (number-to-string h)
              " -i :0.0+" (number-to-string x) "," (number-to-string y) " "
              " -q 1 "
              " -b 8500000 "
              " -bt 8000000"
              " -preset ultrafast -threads 4"
              " -y " filename)))
;; (capture-gen-cmd 0 0 100 100 15 "asd" (list "SB X-Fi Analog Mono"))

(defun capture-filename (preset)
  "Generate an output filename for a PRESET."
  (interactive)
  (let (fname
        (ext (nth 5 preset)))
    (setq fname (concat capture-video-temp-dir "capture_"
                        (format-time-string "%d_%b_%Y_%H_%M_%S" (current-time)) "."
                        ext))))
;; (capture-filename capture-preset-current)

(defun capture-gen-cmd-for-preset (preset filename)
  "Generate a command to start capturing.
Based on PRESET.  Write to FILENAME."
  (interactive)
  (let (cmd
        (x (car preset))
        (y (nth 1 preset))
        (w (nth 2 preset))
        (h (nth 3 preset))
        (fps (nth 4 preset))
        (ext (nth 5 preset))
        (title (nth 6 preset))
        (audio (nth 7 preset))  ; list of audio devices to record
        (wallpaper (nth 8 preset)))
    (setq cmd (capture-gen-cmd x y w h fps filename audio))
    ))
;; (capture-filename capture-preset-current)

(defun capture-current-preset-cmd ()
  "Generate a command to start capturing.
Based on a preset under the cursor."
  (interactive)
  (let ((cmd (capture-gen-cmd-for-preset
              (capture-preset-under-cursor)
              (capture-filename (capture-preset-under-cursor)))))
    (with-temp-buffer
      (insert cmd)
      (kill-ring-save (line-beginning-position) (line-end-position))
      ;;(clipboard-kill-ring-save (+ (line-beginning-position) 4) (line-end-position))
      )
    (message cmd)))

(defun capture-preset-under-cursor ()
  "Return a preset under the cursor."
  (interactive)
  (with-current-buffer capture-buffer-name
  (let ((num
         (buffer-substring (line-beginning-position) (+ (line-beginning-position) 1)))
        preset)
    (setq num (- (string-to-number num) 1))
    (setq preset (nth num capture-presets)))))

;;(defun capture-before-capture ()
;;  "Run this function before starting capturing."
;;  (interactive)
;;  ;;(suspend-frame)
;;  )
;;
;;(defun capture-after-capture ()
;;  "Run this function after stopping capturing video."
;;  (interactive))

(defun capture-start ()
  "Run capture process with settings in `capture-preset-current'."
  (interactive)
  (if (not (file-directory-p capture-video-dest-dir))
      (error (concat "Destination dir doesn't exist: " capture-video-dest-dir)))
  (if (fboundp 'capture-before-capture)
      (capture-before-capture))
  (let ((preset capture-preset-current))
    (let ((filename (capture-filename preset))
          (wallpaper (nth 8 preset)))
      (if (and wallpaper
               (file-exists-p  wallpaper))
          (set-desktop-background wallpaper))
      (capture-run-daemonized-command-no-buf
       (capture-gen-cmd-for-preset preset filename)))))

(defun capture-presets-clear ()
  "Remove all presets from `capture-presets'."
  (interactive)
  (setq capture-presets (list)))

(defun capture-add-preset (x y w h fps ext &optional title audio wallpaper)
  "Add a preset to `capture-presets'.
x, Y - top left point of capturing frame
W, H - width and height
EXT  - filename extension (\"webm\")"
  (interactive)
  (add-to-list 'capture-presets (list x y w h fps ext title audio wallpaper)))

(defun capture-get-processes ()
  "Get list of commands of running \"avconv\" processes."
  (interactive)
  (let ((cmds
         (shell-command-to-string "ps -o command -C avconv")))
    (if (> (length cmds) 2)
        (butlast (cdr (split-string cmds "\n")) 1)
      (list))))
;; (capture-get-processes)

;;(defun capture-get-frames-processes ()
;;  "Get list of commands of border frames."
;;  (interactive)
;;  (let ((cmds
;;         (shell-command-to-string "ps -o pid -o command -C python3")))
;;    (if (> (length cmds) 2)
;;        (butlast (cdr (split-string cmds "\n")) 1)
;;      (list))))
;; (capture-get-frames-processes)

(defun capture-get-processes-files ()
  "Get list of files that avconv is writing to."
  (interactive)
  (let ((processes (capture-get-processes))
  ;;(let ((processes (list "avconv  -f pulse -i alsa_input.usb-046d_0825_F4CCEA20-02-U0x46d0x825.analog-mono  -show_region 1 -f x11grab  -r 15 -s 854x480 -i :0.0+524,333  -q 1  -b 8500000  -bt 8000000 -preset ultrafast -threads 4 -y /tmp/capture_10_Jul_2013_00_25_26.webm"))
        files
        res)
    (setq res (list))
    (dolist (element processes res)
      ;;(concat "-i " (capture-get-audio-name-by-title (car audio)) " ")
      (setq res (append res (last (split-string element " "))))
  )))
;; (capture-get-processes-files)

(defun capture-make-dst-filename (src)
  "Return dst-filename from SRC-filename."
  (interactive)
  (let (f)
    (setq f (concat capture-video-dest-dir
                    (file-name-base src) "."
                    (file-name-extension src)))))
;; (capture-make-dst-filename "/tmp/test.webm")

(defun capture-move-files (files)
  "Move captured files to `capture-video-dest-dir'."
  (interactive)
  (if (not (file-directory-p capture-video-dest-dir))
      (error (concat "Destination dir doesn't exist: " capture-video-dest-dir)))
  (let (dstfile)
    (dolist (element files)
      ;;(concat "-i " (capture-get-audio-name-by-title (car audio)) " ")
      ;;(setq res (append res (last (split-string element " "))))
      (setq dstfile (capture-make-dst-filename element))
      (if (file-exists-p dstfile)
          (message (concat "Not copying! File exists: " dstfile))
        (rename-file element dstfile))
      )))
;; (capture-move-files)

(defun capture-stop-all ()
  "Just stops all avconv processes."
  (interactive)
  (let ((processes (capture-get-processes))
        files)
    (setq files (capture-get-processes-files))
    (capture-run-daemonized-command-no-buf
     "killall -q -INT -w avconv")
    (if (fboundp 'capture-after-capture)
        (capture-after-capture))
    (capture-move-files files)
    ))
;; (capture-stop-all)

(provide 'capture-functions)
;;; capture-functions.el ends here
