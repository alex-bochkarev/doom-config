;;; my-projects-magic.el -*- lexical-binding: t; -*-
;;; Projects-specific sorcery.
;;; This includes dwim-ish "view file"


(global-set-key (kbd "H-v") (lambda () (interactive)
                              (if (bound-and-true-p ab/default-file-to-view)
                                  (if (file-exists-p ab/default-file-to-view)
                                      (ab--open-in-external-app (expand-file-name ab/default-file-to-view))
                                    (message (concat ab/default-file-to-view ": file does not exist")))
                                (message "ab/default-file-to-view not defined (bind it in .dir-locals!)"))))

;; agenda set up
(setq org-agenda-files
      '("~/org"
        "~/PKB/notes/website.org"
        "~/PKB/notes/res-pipeline.org"
        "~/PKB/notes/proj-notes/QuanTUK"
        "~/PKB/notes/proj-notes/QuantumAktiv"
        "~/PKB/notes/proj-notes/align-BDD"
        "~/PKB/notes/proj-notes/qopt-overview"
        "~/PKB/notes/conferences.org"
        "~/PKB/notes/projects.org"
        "~/projects/qmath-course"
        "~/.dotfiles"))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "H-r") 'org-agenda-schedule)
            (local-set-key (kbd "H-d") 'org-agenda-deadline)))

;; project-management related filenames
(setq pkb-project-note-file "project.org")
(setq pkb-project-log-file "log.org")
(setq pkb-project-notation-file "notation.org")

;; key folders / directories (with a trailing slash)
(setq pkb-project-notes-root "~/PKB/notes/proj-notes")

(defvar pkb-project-notes-dir nil
  "PKB private project notes directory (directory-local variable).")

(defun ab/get-project-notes-dir ()
  "Returns project notes directory if it is defined as a dir-local,
     or uses the project name from projectile otherwise."
  (if (bound-and-true-p pkb-project-notes-dir)
      (concat pkb-project-notes-root "/" pkb-project-notes-dir "/")
    (concat pkb-project-notes-root "/" (projectile-project-name) "/")))

(defun pmagic--find-default-makefile ()
  "Returns default makefile for the project."
  (if (bound-and-true-p pmagic--default-makefile)
      pmagic--default-makefile
    (concat (projectile-project-root) "Makefile")))

(defun pmagic--get-project-file ()
  "Returns project PKB notes file."
  (concat
   (ab/get-project-notes-dir)
   pkb-project-note-file))
;; check out the projects directory for switching with ~SPC p p~
(projectile-discover-projects-in-directory "~/projects/" 1)
