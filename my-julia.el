;;; my-julia.el -*- lexical-binding: t; -*-
;; The module describes julia-specific customizations.
;;

(defun ab--julia-close-this ()
  "Adds `end' on the next line, with the same indent."
  (interactive)
  (save-excursion (let (cur-indent (current-indentation))
                    (move-end-of-line nil)
                    (newline)
                    (insert "end")
                    (julia-indent-line))))

(evil-define-key '(normal insert) julia-mode-map (kbd "H-<return>") 'ab|julia-close-this)

(map! :map julia-mode-map
      :desc "close-this" :gnv "H-]" #'ab--julia-close-this)

(map! :map julia-mode-map (:prefix-map  ("H-<backspace>" . "insert-special-symbol")
       :desc "α" :gnv "a" #'(lambda () (interactive) (insert "α"))
       :desc "β" :gnv "b" #'(lambda () (interactive) (insert "β"))
       :desc "π" :gnv "p" #'(lambda () (interactive) (insert "π"))
       :desc "Δ" :gnv "D" #'(lambda () (interactive) (insert "Δ"))
       :desc "δ" :gnv "d" #'(lambda () (interactive) (insert "δ"))
       :desc "∈" :gnv "i" #'(lambda () (interactive) (insert "∈"))
       :desc "∉" :gnv "n" #'(lambda () (interactive) (insert "∉"))
       :desc "ε" :gnv "e" #'(lambda () (interactive) (insert "ε"))
       :desc "τ" :gnv "t" #'(lambda () (interactive) (insert "τ"))))
