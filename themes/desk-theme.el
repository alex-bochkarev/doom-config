;;; desk-theme.el --- a yellow-ish light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Alexey Bochkarev <https://www.bochkarev.io>
;; Maintainer:
;; Source: original
;;
;;; Commentary:
;;
;; The theme is based on doom-homage
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup desk-theme nil
  "Options for the `desk' theme."
  :group 'doom-themes)

(defcustom desk-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'desk-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme desk
  "A light, professional theme"

  ;; name        default   256       16
  ((bg         '("#FFFFF0" nil       nil            ))
   (bg-alt     '("#FFFDD0" nil       nil            ))
   (base0      '("#f0f0f0" "#f0f0f0" "ivory"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base5      '("#383a42" "#424242" "brightblack"  ))
   (base6      '("#202328" "#2e2e2e" "brightblack"  ))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))
   (fg         '("#383a42" "#424242" "black"        ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"  ))
   (bg-accent  '("#FFBF00" "#FFBF00" "orange"       ))

   (grey       base5)
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#FF5733" "#FFAA33" "brightred"    ))
   (green      '("#006400" "#006400" "darkgreen"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#FAFA33" "#FAFA33" "yellow"       ))
   (yellow-alt '("#fafadd" "#fafadd" "yellow"       ))
   (blue       '("#000080" "#000080" "brightblue"   ))
   (dark-blue  '("#030f64" "#030f64" "blue"         ))
   (light-blue  '("#e8f4f8" "#e8f4f8" "lightblue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#b751b6" "#b751b6" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      base3)
   (builtin        fg)
   (comments       green)
   (doc-comments   (doom-darken comments 0.15))
   (constants      fg)
   (functions      blue)
   (keywords       dark-blue)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        orange)
   (variables      fg)
   (numbers        red)
   (region         bg-accent)
   (error          red)
   (warning        (doom-darken bg-accent 0.2))
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright t)
   (-modeline-pad
    (when desk-padded-modeline
      (if (integerp desk-padded-modeline) desk-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base4)

   (modeline-bg
    (if -modeline-bright
        (doom-darken bg-alt 0.1)
      bg-alt))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken bg-alt 0.1)
      (doom-darken bg-alt 0.5)))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ;;;; Base theme face overrides
  ((font-lock-builtin-face       :inherit 'bold)
   ((font-lock-doc-face &override) :slant 'italic)
   (font-lock-function-name-face :inherit 'bold)
   (font-lock-keyword-face       :foreground blue :inherit 'bold)
   (font-lock-constant-face       :foreground cyan :inherit 'bold)
   (font-lock-type-face          :inherit 'bold)
   (line-number :foreground (doom-lighten base4 0.15))
   (line-number-current-line :foreground base8 :background bg-accent :inherit bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (tooltip :background base1 :foreground fg)
   (hl-line :background bg-alt)

   ;; Override secondary selection
   ((secondary-selection &override) :background base0)

   ;;;; org-mode
   (org-document-info-keyword :background yellow-alt :inherit 'bold
                              :box `(:line-width -1 :color ,blue :style 'released-button))
   (org-document-title :inherit 'bold :foreground blue)
   ((org-meta-line &override) :foreground blue)
   (org-level-1 :background light-blue :inherit 'underline)
   (org-level-3 :foreground magenta :inherit 'italic)
   (org-list-dt :foreground blue)
   (org-code :background base8 :foreground bg-accent :inherit 'bold :box `(:line-width -1 :color ,base5 :style 'button))
   (org-ref-cite-face :foreground (doom-darken bg-accent 0.2))

   ;;;; vertico
   (vertico-current :background bg-accent)
   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; magit
   ((magit-diff-hunk-heading           &override) :foreground base4 :background bg :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground fg    :background bg :bold bold)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; highlight-indent-guides
   (highlight-indent-guides-character-face :foreground base2)
   ;;;; ivy
   ((ivy-minibuffer-match-face-1 &override) :foreground (doom-darken grey 0.70))
   ;;;; ivy-posframe
   (ivy-posframe               :background base0)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)

   ;;;; email: mu4e and gnus
   (gnus-header-name :inherit 'underline)
   (gnus-header-content :foreground green)
   (gnus-header-from :foreground red)
   (gnus-signature :foreground green)
   (gnus-cite-2 :foreground orange)

   (mu4e-highlight-face         :background bg :inherit 'bold)
   (mu4e-flagged-face           :background bg :foreground magenta :inherit 'bold)
   (mu4e-related-face           :foreground base4 :slant 'italic)
   (mu4e-header-highlight-face :foreground dark-blue :background bg-accent :inherit 'bold)
   (mu4e-unread-face :foreground blue :inherit 'underline)

   ;;;; web mode (HTML)
   (web-mode-html-tag-face :inherit 'bold :foreground magenta)
   (web-mode-html-attr-name-face :foreground blue)

   ;;;; dired
   (diredfl-flag-mark-line :inherit 'underline :bold 'bold)
   (diredfl-flag-mark :background bg-accent)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground fg)
   ((outline-2 &override) :foreground fg)
   ((outline-3 &override) :foreground fg)
   ((outline-4 &override) :foreground fg)
   ((outline-5 &override) :foreground fg)
   ((outline-6 &override) :foreground fg)
   ((outline-7 &override) :foreground fg)
   ((outline-8 &override) :foreground fg)
   ;;;; org <built-in>
   ;; Make unfinished cookie & todo keywords bright to grab attention
   ((org-todo &override) :foreground red :background yellow-alt)
   ;; Make tags and dates to have pretty box around them
   ((org-tag &override)   :foreground fg :background yellow-alt
    :box `(:line-width -1 :color ,base5 :style 'released-button))
   ((org-date &override)  :foreground fg :background yellow-alt
    :box `(:line-width -1 :color ,base5  :style 'released-button))
   ;; Make drawers and special keywords (like scheduled) to be very bleak
   ((org-special-keyword &override)  :foreground base4)
   ((org-drawer          &override)  :foreground base4 :background base0)
   ;; Make ellipsis as bleak as possible and reset underline/boxing
   (org-ellipsis :underline nil :box nil :foreground fg :background bg)
   ;; Make blocks have a slightly different background
   ((org-block &override) :background yellow-alt)
   ((org-block-begin-line &override) :foreground fg :slant 'italic :background yellow)
   ((org-block-end-line &override) :foreground fg :slant 'italic :background yellow)
   ((org-quote &override) :background yellow-alt)
   ((org-table &override) :foreground fg)
   ;; Make "unimportant" things like distant deadlines and things scheduled for
   ;; today to be bleak.
   (org-upcoming-deadline         :foreground base8)
   (org-upcoming-distant-deadline :foreground fg)
   (org-scheduled                 :foreground fg)
   (org-scheduled-today           :foreground fg)
   (org-scheduled-previously      :foreground base8)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; swiper
   ((swiper-match-face-1 &override) :foreground bg :background fg)
   ((swiper-line-face    &override) :background (doom-lighten blue 0.70) :foreground fg)
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1))

  ;;;; Base theme variable overrides-
  ())

;;; desk-theme.el ends here
