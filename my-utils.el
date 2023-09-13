;;; my-utils.el -*- lexical-binding: t; -*-
;;; Common elisp helpers
;;;
(defun ab--open-in-external-app (file-path)
  "Open `file-path' in external application.

NOTE: Spacemacs' `spacemacs/open-in-external-app' was more robust wrt different systems.
"
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))
