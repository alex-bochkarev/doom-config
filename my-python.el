;;; my-python.el -*- lexical-binding: t; -*-

(use-package! python-black
  :demand t
  :after python
  :config
  (map! :map python-mode-map
        :localleader
        :prefix ("b" . "Blacken")
        :desc "Blacken Buffer" :gnv "b" #'python-black-buffer
        :desc "Blacken Region" :gnv "r" #'python-black-region
        :desc "Blacken Statement" "s" #'python-black-statement)
  (map! :map python-mode-map
        :localleader
        :prefix ("j" . "Jump around")
        :desc "jump to symbol" :gnv "j" #'lsp-ui-find-workspace-symbol))

;; Make reflow work okay in docstrings.
(defun my-python-docstring-fill-setup ()
  (setq-local paragraph-start "^\\s-*[A-Z][A-Za-z ]*:[[:space:]]*$\\|\\s-*$")
  (setq-local paragraph-separate "^\\s-*[A-Z][A-Za-z ]*:[[:space:]]*$\\|\\s-*$"))

(add-hook 'python-mode-hook #'my-python-docstring-fill-setup)
