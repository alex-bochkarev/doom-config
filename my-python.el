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
        :desc "Blacken Statement" "s" #'python-black-statement))

