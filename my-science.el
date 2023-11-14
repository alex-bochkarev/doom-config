;;; my-science.el -*- lexical-binding: t; -*-

(setq! citar-bibliography '("~/PKB/sources/references.bib"))
(setq! citar-notes-paths '("~/PKB/notes/refs/"))

(setq reftex-default-bibliography "~/PKB/sources/references.bib")

(setq org-ref-pdf-directory '("~/PKB/sources/zotero_lib"))
(setq bibtex-completion-library-path org-ref-pdf-directory)

(setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (call-process "open" nil 0 nil fpath)))

(setq bibtex-completion-bibliography '("~/PKB/sources/references.bib"))
(setq bibtex-completion-notes-path "~/PKB/notes/refs/")

;; latex-specific
;;
;; bury the compilation buffer if everything is OK
;; see https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close

;; compilation: hiding window on OK
(defvar my-compilation-exit-code nil)
(defun my-compilation-exit-message-function (status_ code message)
  (setq my-compilation-exit-code code)
  (cons message code))

(setq compilation-exit-message-function 'my-compilation-exit-message-function)

(add-hook 'compilation-start-hook 'compilation-started)
(add-hook 'compilation-finish-functions 'hide-compile-buffer-if-successful)

(defcustom auto-hide-compile-buffer-delay 0
  "Time in seconds before auto hiding compile buffer."
  :group 'compilation
  :type 'number
  )

(defun hide-compile-buffer-if-successful (buffer string)
  (setq compilation-total-time (time-subtract nil compilation-start-time))
  (setq time-str (concat " (Time: " (format-time-string "%s.%3N" compilation-total-time) "s)"))

  (if
      (with-current-buffer buffer
        (setq warnings (eval compilation-num-warnings-found))
        (setq warnings-str (concat " (Warnings: " (number-to-string warnings) ")"))
        (setq errors (eval compilation-num-errors-found))

        (if (and
             (eq errors 0)
             (eq my-compilation-exit-code 0)) nil t))

      ;;If Errors then
      (message (concat "Compiled with Errors" warnings-str time-str))

    ;;If Compiled Successfully or with Warnings then
    (progn
      (bury-buffer buffer)
      (run-with-timer auto-hide-compile-buffer-delay nil 'delete-window (get-buffer-window buffer 'visible))
      (message (concat "Compiled Successfully" warnings-str time-str))
      )
    )
  )

(setq org-latex-pdf-process '("latexmk -interaction=nonstopmode -shell-escape -pdf -outdir=%o %f"))

;; (add-to-list 'org-beamer-environments-extra
;; '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

(make-variable-buffer-local 'compilation-start-time)

(defun compilation-started (proc)
  (setq compilation-start-time (current-time))
  )


;; Quoting the region
(defun ab/latex-quote-selection (beg end)
  "Wraps the region (selection) in ` and '."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (save-excursion
    (let ((repeats (if (eq (prefix-numeric-value current-prefix-arg) 4) 1 2)))
      (if (and beg end)
          (progn
            (goto-char beg)
            (insert (make-string repeats ?`))
            (goto-char (+ end repeats))
            (insert (make-string repeats ?')))
        (insert (concat (make-string repeats ?`) (make-string repeats ?')))))))

(global-set-key (kbd "H-'") 'ab/latex-quote-selection)
