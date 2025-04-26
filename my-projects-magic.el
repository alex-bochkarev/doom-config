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
        "~/PKB/notes/proj-notes/QuantumAI"
        "~/PKB/notes/proj-notes/DSPI"
        "~/PKB/notes/proj-notes/snn-tsp"
        "~/PKB/notes/proj-notes/stdp"
        "~/PKB/notes/conferences.org"
        "~/PKB/notes/projects.org"
        "~/PKB/notes/20231205101723-bo_vqe_milena_roers_bsc.org"
        "~/projects/qmath-course"
        "~/projects/stdp"
        "~/projects/coal-opt"
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

;; clocking to private notes file
;;
(defun pmagic--get-project-file ()
  "Returns project PKB notes file."
  (concat
   (ab/get-project-notes-dir)
   pkb-project-note-file))

(defun pmagic--attempt-clock-in (id file)
  "Attempts to clock into a header specified by ID in FILE."
    (if-let (marker (org-id-find-id-in-file id file t))
        (save-current-buffer
          (save-excursion
            (set-buffer (marker-buffer marker))
            (goto-char (marker-position marker))
            (org-clock-in)))
      (warn "Clock not started (Could not find ID '%s' in file '%s')" id file)))

(defun pmagic--clock-in-PKB-node ()
  "Clocks into a private node with the same heading under `Clocked time'."
  (interactive)
  (let ((current-heading (org-entry-get nil "ITEM")))
    (with-current-buffer
        (find-file-noselect (pmagic--get-project-file))
      (org-goto-location)
      (org-clock-in))))

;; check out the projects directory for switching with ~SPC p p~
(projectile-discover-projects-in-directory "~/projects/" 1)


(defvar pmagic--default-readme nil
  "README file for the current project (if defined).")

(defun pmagic--get-readme-file ()
  "Returns a path to the README file in the repo.

   This is given by `pmagic--default-readme' variable if defined,
   or `README.org' in the project root otherwise."
  (interactive)
  (concat (projectile-project-root)
          (if (bound-and-true-p pmagic--default-readme)
              pmagic--default-readme
            "README.org")))
