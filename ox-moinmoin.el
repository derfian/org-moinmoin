;; ox-moinmoin.el --- MoinMoin Back-End for Org Export Engine

;; Copyright (C) 2011-2013  Free Software Foundation, Inc.
;; Copyright (C) 2013-      Karl Mikaelsson <derfian@hamsterkollektivet.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Install ox-moinmoin by placing `ox-moinmoin.el' in `/path/to/elisp', a
;;; directory of your choice, and adding to your .emacs file:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp")
;;;   (eval-after-load "org" '(require 'ox-moinmoin nil t))

(require 'ox)

(org-export-define-derived-backend 'moinmoin 'ascii
  :translate-alist '(
                     (bold . org-moinmoin-bold)
                     ;; (center-block . org-moinmoin-center-block)
                     ;; (clock . org-moinmoin-clock)
                     (code . org-moinmoin-code)
                     (drawer . org-moinmoin-drawer)
                     ;; (dynamic-block . org-moinmoin-dynamic-block)
                     ;; (entity . org-moinmoin-entity)
                     (example-block . org-moinmoin-example-block)
                     ;; (export-block . org-moinmoin-export-block)
                     ;; (export-snippet . org-moinmoin-export-snippet)
                     (fixed-width . org-moinmoin-fixed-width)
                     (footnote-definition . org-moinmoin-footnote-definition)
                     (footnote-reference . org-moinmoin-footnote-reference)
                     (headline . org-moinmoin-headline)
                     (horizontal-rule . org-moinmoin-horizontal-rule)
                     (inline-src-block . org-moinmoin-inline-src-block)
                     ;; (inlinetask . org-moinmoin-inlinetask)
                     (inner-template . org-moinmoin-inner-template)
                     (italic . org-moinmoin-italic)
                     (item . org-moinmoin-item)
                     ;; (keyword . org-moinmoin-keyword)
                     ;; (latex-environment . org-moinmoin-latex-environment)
                     ;; (latex-fragment . org-moinmoin-latex-fragment)
                     (line-break . org-moinmoin-line-break)
                     (link . org-moinmoin-link)
                     (paragraph . org-moinmoin-paragraph)
                     (plain-list . org-moinmoin-plain-list)
                     (plain-text . org-moinmoin-plain-text)
                     ;; (planning . org-moinmoin-planning)
                     (property-drawer . org-moinmoin-property-drawer)
                     (quote-block . org-moinmoin-quote-block)
                     (quote-section . org-moinmoin-quote-section)
                     ;; (radio-target . org-moinmoin-radio-target)
                     (section . org-moinmoin-section)
                     ;; (special-block . org-moinmoin-special-block)
                     (src-block . org-moinmoin-src-block)
                     ;; (statistics-cookie . org-moinmoin-statistics-cookie)
                     (strike-through . org-moinmoin-strike-through)
                     (subscript . org-moinmoin-subscript)
                     (superscript . org-moinmoin-superscript)
                     (table . org-moinmoin-table)
                     (table-cell . org-moinmoin-table-cell)
                     (table-row . org-moinmoin-table-row)
                     ;; (target . org-moinmoin-target)
                     (template . org-moinmoin-template)
                     ;; (timestamp . org-moinmoin-timestamp)
                     (underline . org-moinmoin-underline)
                     ;; (verbatim . org-moinmoin-verbatim)
                     ;; (verse-block . org-moinmoin-verse-block)
                     )
  :export-block "MoinMoin"
  :menu-entry
  '(?m "Export to MoinMoin"
       ((?M "As MoinMoin buffer" org-moinmoin-export-as-moinmoin)
        (?m "As MoinMoin file" org-moinmoin-export-to-moinmoin)
        (?o "As MoinMoin file and open"
            (lambda (a s v b)
              (if a (org-moinmoin-export-to-moinmoin t s v b)
                (org-open-file (org-moinmoin-export-to-moinmoin nil s v b))))))))
;; Transcoding functions

(defun org-moinmoin-bold (bold contents info)
  (format "'''%s'''" contents))

;; (defun org-moinmoin-center-block (center-block contents info) contents)
;; (defun org-moinmoin-clock (clock contents info) contents)

(defun org-moinmoin-code (code contents info)
  (concat "{{{"
          (org-element-property :value code)
          "}}}"))

(defun org-moinmoin-drawer (drawer contents info)
  "")
;; (defun org-moinmoin-dynamic-block (dynamic-block contents info) contents)
;; (defun org-moinmoin-entity (entity info))

(defun org-moinmoin-example-block (example-block contents info)
  (concat "{{{\n"
          (org-export-format-code-default example-block info)
          "}}}\n"))

;; (defun org-moinmoin-export-block (export-block contents info)
;;   (when (string= (org-element-property :type export-block) "MOINMOIN")
;;     (org-remove-indentation (org-element-property :value export-block))))

;; (defun org-moinmoin-export-snippet (export-snippet contents info)
;;   (when (eq (org-element-snippet-backend :type export-snippet) 'moinmoin)
;;     (org-remove-indentation (org-element-property :value export-block))))

(defun org-moinmoin-fixed-width (fixed-width contents info)
  (concat "{{{" (org-remove-indentation (org-element-property :value fixed-width)) "}}}"))

(defun org-moinmoin-footnote-definition (footnote-definition contents info))

(defun org-moinmoin-footnote-reference (footnote-reference contents info)
  (format "<<FootNote(%s)>>"
          (org-trim (org-export-data
           (org-export-get-footnote-definition footnote-reference info) info))))

(defun org-moinmoin-headline (headline contents info)
  ;; Case 1: Ignore footnote sections.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level-rank (org-export-low-level-p headline info))
           (text (org-export-data (org-element-property :title headline) info))
           (level (org-export-get-relative-level headline info))
           (markup (concat (make-string level ?=))))
      ;; Case 2: Ignore deep subtrees (for now).
      ;; Case 3: Regular headline. Export as section.
      (format "%s %s %s\n\n%s" markup text markup contents))))

(defun org-moinmoin-horizontal-rule (horizontal-rule contents info)
  "\n----\n")

(defun org-moinmoin-inline-src-block (inline-src-block contents info)
  (format "`%s`" contents))

;; (defun org-moinmoin-inlinetask (inlinetask contents info))

(defun org-moinmoin-italic (italic contents info)
  (format "''%s''" contents))

(defun org-moinmoin--indent-string (s width)
  "Indent string S by WIDTH white spaces.
Empty lines are not indented."
  (when (stringp s)
    (replace-regexp-in-string
     "\\(^\\)[ \t]*\\S-" (make-string width ?\s) s nil nil 1)))

(defun org-moinmoin--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
    (case (org-element-property :checkbox item)
      (on "[X] ")
      (off "[ ] ")
      (trans "[-] ")))

(defun org-moinmoin-item (item contents info)
  "Transcode an ITEM element from Org to MoinMoin.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((checkbox (org-moinmoin--checkbox item info))
         (list-type (org-element-property :type (org-export-get-parent item)))
         (bullet
          ;; First parent of ITEM is always the plain-list.  Get
          ;; `:type' property from it.
          (org-list-bullet-string
           (case list-type
             (descriptive
              (concat checkbox
                      (org-export-data (org-element-property :tag item) info)
                      ": "))
             (ordered
              ;; Return correct number for ITEM, paying attention to
              ;; counters.
              (let* ((struct (org-element-property :structure item))
                     (bul (org-element-property :bullet item))
                     (num (number-to-string
                           (car (last (org-list-get-item-number
                                       (org-element-property :begin item)
                                       struct
                                       (org-list-prevs-alist struct)
                                       (org-list-parents-alist struct)))))))
                (replace-regexp-in-string "[0-9]+" num bul)))
             (t (let ((bul (org-element-property :bullet item)))
                  (replace-regexp-in-string
                   "-" "*"
                   (replace-regexp-in-string
                    "+" "*" bul))))))))
    (concat
     " "
     bullet
     (unless (eq list-type 'descriptive) checkbox)
     ;; Contents: Pay attention to indentation.  Note: check-boxes are
     ;; already taken care of at the paragraph level so they don't
     ;; interfere with indentation.
     (let ((contents (org-moinmoin--indent-string contents (string-width bullet))))
       (if (eq (org-element-type (car (org-element-contents item))) 'paragraph)
           (org-trim contents)
         (concat "\n" contents))))))

;; (defun org-moinmoin-keyword (keyword contents info) (format "%s" keyword))
;; (defun org-moinmoin-latex-environment (latex-environment contents info))
;; (defun org-moinmoin-latex-fragment (latex-fragment contents info))

(defun org-moinmoin-line-break (line-break contents info) "<<BR>>")

(defun org-moinmoin-link (link desc info)
  (concat "[["
          (org-element-property :raw-link link)
          (cond ((org-string-nw-p desc)
                 (concat "|" desc))
                (t nil))
          "]]"))

(defun org-moinmoin-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to MoinMoin.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let ((parent-type (car parent))
            (fixed-paragraph ""))
        (cond ((and (eq parent-type 'item)
                    (plist-get (nth 1 parent) :bullet ))
               (setq fixed-paragraph contents))
              ((eq parent-type 'section)
               (setq fixed-paragraph contents))
              ((eq parent-type 'footnote-definition)
               (setq fixed-paragraph contents))
              (t (setq fixed-paragraph contents)))
        ;; FIXME: This messes up lists somehow.
        (org-remove-indentation fixed-paragraph)))))

(defun org-moinmoin-plain-list (plain-list contents info)
  contents)

(defun org-moinmoin-plain-text (text info)
  text)

;; (defun org-moinmoin-planning (planning contents info)
;;   (format "%s" planning))

(defun org-moinmoin-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to MoinMoin.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")

(defun org-moinmoin-quote-block (quote-block contents info)
  (format "\n  %s" contents))

(defun org-moinmoin-quote-section (quote-section contents info)
  (format "\"%s\"" contents))

;; (defun org-moinmoin-radio-target (radio-target contents info)
;;   (format "%s" radio-target))

(defun org-moinmoin-section (section contents info)
  contents)

;; (defun org-moinmoin-special-block (special-block contents info)
;;   (format "%s" special-block))

(defun org-moinmoin-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info)))
    (if (equal code "")
        code
      (format "{{{#!highlight %s\n%s\n}}}\n"
              (if (not lang) "" lang)
              code))))

;; (defun org-moinmoin-statistics-cookie (statistics-cookie contents info)
;;   (format "%s" statistics-cookie))

(defun org-moinmoin-strike-through (strike-through contents info)
  (format "--(%s)--" contents))

(defun org-moinmoin-subscript (subscript contents info)
  (format ",,%s,," contents))

(defun org-moinmoin-superscript (superscript contents info)
  (format "^^%s^^" contents))

(defun org-moinmoin-table (table contents info)
  contents)

(defun org-moinmoin-table-cell (table-cell contents info)
  (format "| %s |" (or contents "")))

(defun org-moinmoin-table-row (table-row contents info)
  (if contents
      (format "|%s|" contents)))

;; (defun org-moinmoin-target (target contents info))
(defun org-moinmoin-inner-template (contents info)
  "Return body of document string after MoinMoin
conversion. CONTENTS is the transcoded contents string. INFO is a
plist holding export options."
  (concat
   (if (plist-get info :with-toc)
       (format "<<TableOfContents(%d)>>\n\n"
               (plist-get info :headline-levels))
     nil)
   contents))

(defun org-moinmoin-template (contents info)
  "Return complete document string after MoinMoin
conversion. CONTENTS is the transcoded contents string. INFO is a
plist holding export options."
  contents)
;; (defun org-moinmoin-timestamp (timestamp contents info))

(defun org-moinmoin-underline (underline contents info)
  (format "__%s__" contents))

;; (defun org-moinmoin-verbatim (verbatim contents info) (format "{{{%s}}}" contents))
;; (defun org-moinmoin-verse-block (verse-block contents info))

;;;###autoload
(defun org-moinmoin-export-as-moinmoin
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'moinmoin "*Org MoinMoin Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(provide 'ox-moinmoin)

;; (defun reload-org-moinmoin () (interactive) (load-file "~/src/org-moinmoin/ox-moinmoin.el"))
