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

;; Convenience function: highlighting arbitrary strings
;; in a buffer

 (defvar ab--highlight-patterns
  '(("@mentions" . ("\\(@[A-Za-z][A-Za-z0-9]*\\)" . font-lock-comment-face))
    ("parens" . ("\\(([^)]*)\\)" . hi-red-b)))
  "Alist of (NAME . (REGEX . FACE)) for custom highlights.")

(defvar-local ab--highlight-states nil
  "Alist of (NAME . ENABLED) tracking which highlights are enabled in the buffer.")

(defun ab--toggle-highlight (name)
  "Toggle highlighting for the pattern named NAME in `ab--highlight-patterns`."
  (interactive)
  (require 'hi-lock)
  (let* ((pattern-entry (assoc name ab--highlight-patterns))
         (regex (car (cdr pattern-entry)))
         (face (cdr (cdr pattern-entry)))
         (enabled (assoc name ab--highlight-states)))
    (if (and pattern-entry enabled)
        (progn
          ;; Disable an existing highlight
          (hi-lock-unface-buffer regex)
          (setq ab--highlight-states (assoc-delete-all name ab--highlight-states))
          (message "Highlight %s disabled" name))
      (when pattern-entry)
        ;; Enable a highlight
        (hi-lock-face-buffer regex face)
        (push (cons name t) ab--highlight-states)
        (message "Highlight %s enabled" name)))
    ;; Ensure hi-lock patterns are applied
      (hi-lock-mode 1))
;; end of custom highlighting
