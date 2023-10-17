;;; my-utils.el -*- lexical-binding: t; -*-
;;; General-purpose elisp helpers
;;;
(defun ab--open-in-external-app (file-path)
  "Open `file-path' in external application.

NOTE: Spacemacs' `spacemacs/open-in-external-app' was more robust wrt different systems.
"
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))

(defun ab--dired-xdg-open-file ()
  "Opens the file-at-point in Dired using the default system app."
  (interactive)
  (ab--open-in-external-app (dired-get-filename)))

(map! :map dired-mode-map
      :leader
      :desc "Open in the default app"
      "f o" #'ab--dired-xdg-open-file)
