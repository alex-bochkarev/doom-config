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

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(require 'key-chord)
(key-chord-mode t)
(setq key-chord-two-keys-delay 0.05)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "kj" 'evil-normal-state)
