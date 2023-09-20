;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Alexey Bochkarev"
      user-mail-address "a@bochkarev.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira" :size 14 :weight 'semi-light))
(setq doom-font "Iosevka-12")
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(load! "my-utils.el")
(setq doom-unicode-font (font-spec :family "FiraCode"))


;; setting up org-roam
(setq org-roam-directory "~/PKB/notes")

(map! "H-SPC" #'org-roam-node-find)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; spacemacs-ey coveniences
(map! :desc "dired here" :leader "fj" #'dired-jump)

;; minor org-mode tweaks
(load! "my-org-captures.el")

(setq org-readme-file "~/org/readme.org")
(setq org-research-file "~/PKB/notes/projects.org")
(setq org-email-file "~/org/email.org")
(setq org-quotes-file "~/org/quotes.org")
(setq org-distractions-file "~/org/fun.org")
(setq org-current-file "~/org/current.org")
(setq org-someday-file "~/org/someday.org")
(setq org-blog-file "~/PKB/notes/blog.org")
(setq org-daily-summary-file "~/org/summaries.org.gpg")
(setq org-mobile-file "~/shares/mobile/inbox.org")
(setq org-mobile-directory "~/shares/mobile/")

(setq projectile-tags-command "ctags -Re --tag-relative=yes --exclude=@.ctagsignore -f \"%s\" %s .")
(setq projectile-project-search-path '(("~/projects/" . 1)))

(map! "H-/" 'projectile-find-tag)

(after! org
  (setq org-list-allow-alphabetical t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-use-fast-todo-selection t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "WIP(w)" "KTL(k@)" "LATER(l)" "|" "DONE(d!)" "CANCELED(q@!)"))))


  (setq org-tags-exclude-from-inheritance '("keydate"))) ;; that was the logic of keydate tag (needed for simple calendar generation)

(defun ab/get-date-if-not-nil(datearg)
  "Returns date if not nil and nil otherwise"
  (interactive)
  (if datearg
      (format-time-string "%d-%m-%Y" datearg)
    "          "))
(setq org-agenda-custom-commands
      '(
        ("d" "Daily agenda"
         (
          (agenda "" ((org-agenda-overriding-header "== Calendar: ========================================================================================================="))
                  (org-agenda-sorting-strategy '(priority-down))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'done)))
          (todo "WIP"
                ((org-agenda-overriding-header "== Started / WIP: ====================================================================================================")))
          (todo "KTL"
                ((org-agenda-overriding-header "== Control / watch / awaiting: ======================================================================================="))
                (org-agenda-sorting-strategy '(priority-down)))
          (todo "LATER"
                ((org-agenda-overriding-header "== LATER pool: =======================================================================================================")
                 ))
          (todo "TODO"
                ((org-agenda-overriding-header "== Not scheduled: ====================================================================================================")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))))))

        ("c" "The calendar plan: key dates."
         ((tags "keydate"
                ((org-agenda-prefix-format "%c: %i %?-12(concat \"\" (ab/get-date-if-not-nil (org-get-deadline-time nil)) \" -- \" )")
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-sorting-strategy '(deadline-up))
                 (org-agenda-overriding-header "== KEY MILESTONES - calendar plan =========================================\n")
                 (org-agenda-remove-tags t)))))))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(global-set-key (kbd "H-a") 'org-agenda)
;; clocking in-out fine-tuning
(global-set-key (kbd "H-i") 'org-clock-in)
(global-set-key (kbd "H-I") 'org-clock-out)
(global-set-key (kbd "C-H-i") 'org-clock-goto)


(load! "my-email.el")
;; general keybindings


(map! "H-h" 'evil-window-left)
(map! "H-l" 'evil-window-right)
(map! "H-j" 'evil-window-down)
(map! "H-k" 'evil-window-up)

(map! "C-e" 'end-of-visible-line) ;; this is convenient in latex (insert mode)!

(load! "my-projects-magic.el")

(map! (:prefix-map  ("H-o" . "hyper-open")
       :desc "general orgfile" "g" (lambda () (interactive) (find-file org-current-file))
       :desc "reading list" "r" (lambda () (interactive) (find-file org-readme-file))
       :desc "distractions" "D" (lambda () (interactive) (find-file org-distractions-file))
       :desc "mobile inbox" "m" (lambda () (interactive) (find-file org-mobile-file))
       :desc "website notes" "w" (lambda () (interactive) (find-file "~/PKB/notes/website.org"))
       :desc "shopping list" "S" (lambda () (interactive) (find-file (concat org-directory "shopping.org")))
       :desc "network and career" "j" (lambda () (interactive) (find-file (concat org-directory "network.org")))
       :desc "startpage.html" "s" (lambda () (interactive) (find-file "~/projects/startpage/start.html"))
       :desc "ledger" "l" (lambda () (interactive) (find-file "~/finance/ledger.beancount"))
       :desc "email backlog" "e" (lambda () (interactive) (find-file org-email-file))
       :desc "projects pipeline" "P" (lambda () (interactive) (find-file org-research-file))
       (:prefix-map ("d" . "dotfiles")
        :desc  "spacemacs" "E" '(lambda () (interactive) (find-file "~/.spacemacs.d/init.el"))
        :desc "doom" "e" '(lambda () (interactive) (find-file "~/.config/doom/config.el"))
        :desc ".zshrc" "z" '(lambda () (interactive) (find-file "~/.zshrc"))
        :desc "zsh_aliases" "a" '(lambda () (interactive) (find-file "~/.config/zsh_aliases"))
        :desc "sway" "w" '(lambda () (interactive) (find-file "~/.config/sway/config"))
        :desc ".mailrc" "m" '(lambda () (interactive) (find-file "~/.mailrc"))
        :desc "statusbar (waybar)" "s" '(lambda () (interactive) (find-file "~/.config/waybar/"))
        :desc "config folder" "c" '(lambda () (interactive) (find-file "~/.config/"))
        :desc "dotfiles folder" "d" '(lambda () (interactive) (find-file "~/.dotfiles/")))
       (:prefix-map ("f" . "folder")
        :desc "projects folder" "p" (lambda () (interactive) (find-file "~/projects/"))
        :desc "project notes" "n" (lambda () (interactive) (find-file pkb-project-notes-root))
        :desc "org folder" "o" (lambda () (interactive) (find-file org-directory)))
        :desc "mobile folder" "m" (lambda () (interactive) (find-file org-mobile-directory))
       (:prefix-map ("p" . "project place")
        :desc "PKB dir" "k" (lambda () (interactive) (find-file (ab/get-project-notes-dir)))
        :desc "PKB Org file" "o" (lambda () (interactive) (find-file (concat (ab/get-project-notes-dir) pkb-project-note-file)))
        :desc "TODOs" "t" (lambda () (interactive) (find-file (concat (projectile-project-root) "TODOs.org")))
        :desc "Changelog" "c" (lambda () (interactive) (find-file (concat (projectile-project-root) "CHANGELOG.org")))
        :desc "README.md" "R" (lambda () (interactive) (find-file (concat (projectile-project-root) "README.md")))
        :desc "README.org" "r" (lambda () (interactive) (find-file (concat (projectile-project-root) "README.org")))
        :desc ".gitignore" "i" (lambda () (interactive) (find-file (concat (projectile-project-root) ".gitignore")))
        :desc ".ctagsignore" "T" (lambda () (interactive) (find-file (concat (projectile-project-root) ".ctagsignore")))
        :desc "Makefile" "m" (lambda () (interactive) (find-file (concat (projectile-project-root) "Makefile")))
        :desc "setup.el" "s" (lambda () (interactive) (find-file (concat (projectile-project-root) "setup.el"))))))

(map! (:prefix-map  ("H-t" . "hyper-toggle")
       :desc "show @mentions" "m" #'ab-highlight-names))

(load! "my-science.el")

(use-package! ox-extra
              :after org
              :config
              (ox-extras-activate '(ignore-headlines)))

