;;; capture-vars --- Variables for capture-mode
;;; Commentary:
;;; Code:
(defcustom capture-buffer-name
  "*capture*" "Buffer name for capture-mode."
  :group 'capture-vars)

(defcustom capture-audio-speakers
  (list) "A list of your \"What you hear\" devices."
  :group 'capture-vars)

(defcustom capture-audio-microphones
  (list) "A list of your microphones."
  :group 'capture-vars)

(defcustom capture-video-temp-dir
  "/tmp/" "Temp dir for storing files."
  :group 'capture-vars)

(defcustom capture-video-dest-dir
  "~/Videos/" "Where to move your files when you're done.."
  :group 'capture-vars)

(defcustom capture-background-path
  "/maybe/your/path/to/wallpapers/" "Just a path
  that can be used to generate wallpaper filenames."
  :group 'capture-vars)

(defcustom capture-presets-standard
  (list (list 0 0 854  480 15 "webm" "" "My video"  nil nil)
        (list 0 0 1280 720 15 "webm" "" "Bigger video" nil nil))
  "Sample presets for capture-mode."
  :group 'capture-vars)

(defcustom capture-presets
  (list ) "Standard presets for capture-mode."
  :group 'capture-vars)

(defcustom capture-preset-current
  (list 0 0 854 480 15 "webm" "Youtube")
  "Current preset that will be used for capturing."
  :group 'capture-vars)

(defcustom capture-selected-number
  nil
  ""
  :group 'capture-vars)
;; (makunbound 'capture-preset-current)

(provide 'capture-vars)
;;; capture-vars.el ends here
