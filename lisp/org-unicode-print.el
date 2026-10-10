;;; org-unicode-print.el --- Org -> Unicode text, simple math to Unicode  -*- lexical-binding: t; -*-

(require 'ox-ascii)
(require 'org-entities)
(require 'cl-lib)

(defvar oup-data-file
  (expand-file-name "org-unicode-print.eld"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Data file with the symbol and sub/superscript tables.")

(defvar oup-extra-symbols nil)
(defvar oup-sub nil)
(defvar oup-sup nil)

(defun oup-load-data ()
  "(Re)read the tables from `oup-data-file'."
  (interactive)
  (let ((data (with-temp-buffer
                (insert-file-contents oup-data-file)
                (read (current-buffer)))))
    (setq oup-extra-symbols
          ;; literal \{ \} are protected internally, restored after the checks
          (append (alist-get 'symbols data) '(("{" . "\x1") ("}" . "\x2")))
          oup-sub (alist-get 'sub data)
          oup-sup (alist-get 'sup data))))

(oup-load-data)

(defun oup--script (s table)
  "Map every char of S through TABLE, or nil if any is missing."
  (catch 'fail
    (mapconcat (lambda (c) (or (cdr (assoc (string c) table)) (throw 'fail nil)))
               (string-to-list s) "")))

(defun oup-convert (tex)
  "Unicode rendering of inline TeX, or nil if it is not simple."
  (let ((s tex))
    ;; 1. commands: extras first (they contain braces), then org-entities
    (dolist (p oup-extra-symbols)
      (setq s (replace-regexp-in-string
               (concat "\\\\" (regexp-quote (car p))
                       (if (string-match-p "[a-zA-Z]\\'" (car p)) "\\>" ""))
               (cdr p) s t t)))
    (setq s (replace-regexp-in-string
             "\\\\\\([a-zA-Z]+\\)"
             (lambda (m)
               (let* ((name (match-string 1 m))
                      (e (assoc name (append org-entities-user org-entities))))
                 (if e (nth 6 e) m)))   ; 6 = utf-8 representation
             s t t))
    ;; 2. sub/superscripts: _x, _{..}, ^x, ^{..}
    (let ((fail nil))
      (setq s (replace-regexp-in-string
               "\\([_^]\\)\\(?:{\\([^{}]*\\)}\\|\\([^{}\\ ]\\)\\)"
               (lambda (m)
                 (let* ((tab (if (string= (match-string 1 m) "_") oup-sub oup-sup))
                        (arg (or (match-string 2 m) (match-string 3 m)))
                        (r (oup--script (replace-regexp-in-string " " "" arg) tab)))
                   (cond (r r)
                         ;; no Unicode glyphs: keep it readable, e.g. ^|A|, _(a,b)
                         ((string-match-p "[\\\\{}]" arg) (setq fail t) m)
                         ((= (length arg) 1)
                          (concat (if (string= (match-string 1 m) "_") "\x3" "\x4") arg))
                         (t (concat (if (string= (match-string 1 m) "_") "\x3" "\x4")
                                    "(" arg ")")))))
               s t t))
      ;; 3. anything TeX-ish left over -> not simple
      (unless (or fail (string-match-p "[\\\\{}_^]" s))
        (setq s (replace-regexp-in-string "\x1" "{" s t t))
        (setq s (replace-regexp-in-string "\x2" "}" s t t))
        (setq s (replace-regexp-in-string "\x3" "_" s t t))
        (setq s (replace-regexp-in-string "\x4" "^" s t t))
        (setq s (replace-regexp-in-string " - " " − " s t t))
        (replace-regexp-in-string "[ \t\n]+" "\u00a0" (string-trim s))))))

(defun oup-latex-fragment (fragment _contents info)
  (let* ((raw (org-element-property :value fragment))
         (inner (cond ((string-match "\\`\\$\\(\\(?:.\\|\n\\)*\\)\\$\\'" raw) (match-string 1 raw))
                      ((string-match "\\`\\\\(\\(\\(?:.\\|\n\\)*\\)\\\\)\\'" raw) (match-string 1 raw)))))
    (or (and inner (oup-convert inner))
        (replace-regexp-in-string "[ \t]*\n[ \t]*" " " raw))))

;;; Page geometry, shared with oup-render.py (passed on its command line)

(defvar oup-text-width 72 "Columns of text, header and footer.")
(defvar oup-lines-per-page 60 "Lines per printed page, header and footer included.")
(defvar oup-font-size 10.0 "Font size in points.")
(defvar oup-leading 12.0 "Line height in points.")
(defvar oup-char-width 0.602 "Advance of DejaVu Sans Mono, in em.")
(defvar oup-margin-left 60.0 "Left margin in points.")
(defvar oup-margin-top 60.0 "Top margin in points.")
(defvar oup-default-dpi 96.0 "Assumed resolution of PNGs that don't record one.")
(defvar oup-image-max-lines 45 "Tallest image, in text lines.")

(defvar oup-renderer
  (expand-file-name "oup-render.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Script that turns the paginated text into a PDF.")

;;; Images: [[file:fig.png]] -> a marker line the renderer replaces

(defconst oup--image-re "\\[image:\\(.+\\)|\\([0-9.]+\\)x\\([0-9.]+\\)\\]")

(defun oup--png-size-pt (file)
  "Printed size (W . H) in points of PNG FILE: real size, capped to fit."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let* ((u32 (lambda (pos)            ; big-endian, 1-based buffer pos
                  (let ((v 0))
                    (dotimes (i 4 v) (setq v (+ (* v 256) (char-after (+ pos i))))))))
           (w (funcall u32 17)) (h (funcall u32 21))
           (dpi oup-default-dpi) (pos 9))
      ;; walk the chunks up to the image data, looking for pHYs (resolution)
      (catch 'done
        (while (< (+ pos 8) (point-max))
          (let ((len (funcall u32 pos))
                (type (buffer-substring (+ pos 4) (+ pos 8))))
            (cond ((equal type "pHYs")
                   (when (= (char-after (+ pos 16)) 1) ; unit: metre
                     (setq dpi (* (funcall u32 (+ pos 8)) 0.0254)))
                   (throw 'done nil))
                  ((equal type "IDAT") (throw 'done nil)))
            (setq pos (+ pos 12 len)))))
      (let* ((wpt (* w (/ 72.0 dpi)))
             (hpt (* h (/ 72.0 dpi)))
             (s (min 1.0
                     (/ (* oup-text-width oup-char-width oup-font-size) wpt)
                     (/ (* oup-image-max-lines oup-leading) hpt))))
        (cons (* wpt s) (* hpt s))))))

(defun oup-link (link desc info)
  (if (and (not desc)
           (org-export-inline-image-p link '(("file" . "\\.png\\'"))))
      (let* ((path (org-element-property :path link))
             (size (oup--png-size-pt (expand-file-name path))))
        (format "[image:%s|%.1fx%.1f]" path (car size) (cdr size)))
    (org-export-with-backend 'ascii link desc info)))

(defun oup-paragraph (paragraph contents info)
  "Standard paragraph; a captioned image gets \"Figure N: caption\" below.
N is Org's own ordinal, so it matches what [[fig:...]] links print."
  (let ((out (org-export-with-backend 'ascii paragraph contents info))
        (caption (org-export-get-caption paragraph)))
    (if (and caption (string-match-p oup--image-re out))
        (concat (string-trim-right out) "\n"   ; no blank line: same block
                (org-ascii--fill-string
                 (format "%s %s"
                         (format (org-export-translate "Figure %d:" :utf-8 info)
                                 (org-export-get-ordinal
                                  paragraph info nil #'org-ascii--has-caption-p))
                         (org-export-data caption info))
                 (org-ascii--current-text-width paragraph info) info))
      out)))

(org-export-define-derived-backend 'unicode-print 'ascii
  :translate-alist '((latex-fragment . oup-latex-fragment)
                     (link . oup-link)
                     (paragraph . oup-paragraph)))

;;; RFC-style pagination


(defun oup--lr (left right)
  "LEFT and RIGHT flushed to the edges of `oup-text-width'."
  (let ((gap (max 1 (- oup-text-width (string-width left) (string-width right)))))
    (concat left (make-string gap ?\s) right)))

(defun oup--heading-p (lines i)
  "Non-nil if line I is a heading, i.e. the next line is an underline."
  (let ((next (nth (1+ i) lines)))
    (and next (string-match-p "\\`[ \t]*[═─━=-]+[ \t]*\\'" next))))

(defun oup--expand-images (lines)
  "Reserve lines below each image marker.  Return (LINES . BLOCKS),
BLOCKS being (INDEX . LENGTH) of each image block.  A block covers the
image, one line of space and any non-blank lines right after (a caption)."
  (let ((out '()) (blocks '()) (i 0) (cur nil))
    (dolist (l lines)
      (push l out)
      (cond
       ((string-match oup--image-re l)
        (let ((k (1+ (max 1 (ceiling (/ (string-to-number (match-string 3 l))
                                        oup-leading))))))
          (setq cur (cons i k))
          (push cur blocks)
          (dotimes (_ (1- k)) (push "" out))
          (setq i (+ i k))))
       ((and cur (not (string-blank-p l)))   ; caption line: join the block
        (setcdr cur (1+ (cdr cur)))
        (setq i (1+ i)))
       (t (setq cur nil i (1+ i)))))
    (cons (nreverse out) blocks)))

(defun oup--keep-together (lines)
  "Blocks (INDEX . LENGTH) of LINES that should not be split:
LaTeX environments and tables (between two ━ rules)."
  (let ((v (vconcat lines)) (blocks '()) (i 0))
    (while (< i (length v))
      (let* ((l (aref v i))
             (end-re (cond ((string-match "\\\\begin{\\([^}]+\\)}" l)
                            (concat "\\\\end{" (regexp-quote (match-string 1 l)) "}"))
                           ((string-match-p "\\`[ \t]*━+[ \t]*\\'" l)
                            "\\`[ \t]*━+[ \t]*\\'")))
             (j (and end-re
                     (cl-loop for j from (1+ i) below (length v)
                              when (string-match-p end-re (aref v j)) return j))))
        (if j
            (progn (push (cons i (- (1+ j) i)) blocks) (setq i (1+ j)))
          (setq i (1+ i)))))
    blocks))

(defun oup-paginate (text title author date)
  "Split TEXT into RFC-style pages separated by form feeds."
  (let* ((expanded (oup--expand-images
                    (split-string (string-trim-right text) "\n")))
         (lines (car expanded))
         (blocks (append (cdr expanded) (oup--keep-together lines)))
         (n (length lines))
         (start 0) (page 1) (pages '()))
    (while (< start n)
      ;; page 1: title block instead of header -> 2 more body lines
      (let* ((body (- oup-lines-per-page (if (= page 1) 2 4)))
             (cut (min n (+ start body))))
        ;; never split a block (image, environment, table): move it to the next page
        (dolist (b blocks)
          (when (and (< start (car b) cut) (< cut (+ (car b) (cdr b))))
            (setq cut (car b))))
        ;; don't strand a heading in the last 3 lines of a page
        (when (< cut n)
          (cl-loop for k from (1- cut) downto (max start (- cut 3))
                   when (oup--heading-p lines k) do (setq cut k) and return nil))
        (let ((chunk (cl-subseq lines start cut)))
          (push (string-join
                 (append (unless (= page 1)
                           (list (oup--lr title date) ""))
                         chunk
                         (make-list (- body (length chunk)) "")
                         (list "" (oup--lr author (format "[Page %d]" page))))
                 "\n")
                pages))
        ;; next page starts at the next non-blank line
        (setq start cut page (1+ page))
        (while (and (< start n) (string-blank-p (nth start lines)))
          (setq start (1+ start)))))
    (string-join (nreverse pages) "\n\f")))

(defun oup-export-to-pdf (file)
  "Export Org FILE to FILE.pdf (via paginated Unicode text)."
  (with-current-buffer (find-file-noselect file)
    (let* ((org-ascii-charset 'utf-8)
           (org-ascii-text-width oup-text-width)
           (org-ascii-global-margin 0)
           (org-ascii-inner-margin 2)
           (org-export-with-sub-superscripts '{})
           (kw (org-collect-keywords '("TITLE" "AUTHOR" "DATE")))
           (get (lambda (k fallback) (or (cadr (assoc k kw)) fallback)))
           (pdf (expand-file-name (concat (file-name-base file) ".pdf")
                                  (file-name-directory (expand-file-name file))))
           (txt (make-temp-file "oup-" nil ".txt"))
           (log (get-buffer-create "*oup-render*")))
      (unwind-protect
          (let ((text (oup-paginate         ; in the Org buffer, not the temp one
                       (org-export-as 'unicode-print)
                       (funcall get "TITLE" (file-name-base file))
                       (funcall get "AUTHOR" user-full-name)
                       (funcall get "DATE" (format-time-string "%Y-%m-%d")))))
            (with-temp-file txt (insert text))
            (with-current-buffer log (erase-buffer))
            (unless (eq 0 (call-process
                           "python3" nil log nil oup-renderer txt pdf
                           "--base" default-directory
                           "--font-size" (number-to-string oup-font-size)
                           "--leading" (number-to-string oup-leading)
                           "--left" (number-to-string oup-margin-left)
                           "--top" (number-to-string oup-margin-top)
                           "--columns" (number-to-string oup-text-width)))
              (error "oup: rendering failed, see buffer *oup-render*")))
        (delete-file txt))
      pdf)))

(provide 'org-unicode-print)
