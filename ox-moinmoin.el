;; MoinMoin exporter for org-mode

(require 'ox)
(require 'ox-publish)

(org-export-define-backend 'moinmoin
                           '((bold . org-moinmoin-bold)
                             ;;(center-block . org-moinmoin-center-block)
                             ;;(clock . org-moinmoin-clock)
                             (code . org-moinmoin-code)
                             ;;(drawer . org-moinmoin-drawer)
                             ;;(dynamic-block . org-moinmoin-dynamic-block)
                             ;;(entity . org-moinmoin-entity)
                             (example-block . org-moinmoin-example-block)
                             ;;(export-block . org-moinmoin-export-block)
                             ;;(export-snippet . org-moinmoin-export-snippet)
                             (fixed-width . org-moinmoin-fixed-width)
                             (footnote-definition . org-moinmoin-footnote-definition)
                             ;;(footnote-reference . org-moinmoin-footnote-reference)
                             (headline . org-moinmoin-headline)
                             (horizontal-rule . org-moinmoin-horizontal-rule)
                             (inline-src-block . org-moinmoin-inline-src-block)
                             ;;(inlinetask . org-moinmoin-inlinetask)
                             ;;(inner-template . org-moinmoin-inner-template)
                             (italic . org-moinmoin-italic)
                             (item . org-moinmoin-item)
                             ;;(keyword . org-moinmoin-keyword)
                             ;;(latex-environment . org-moinmoin-latex-environment)
                             ;;(latex-fragment . org-moinmoin-latex-fragment)
                             (line-break . org-moinmoin-line-break)
                             (link . org-moinmoin-link)
                             (paragraph . org-moinmoin-paragraph)
                             (plain-list . org-moinmoin-plain-list)
                             (plain-text . org-moinmoin-plain-text)
                             ;;(planning . org-moinmoin-planning)
                             ;;(property-drawer . org-moinmoin-property-drawer)
                             (quote-block . org-moinmoin-quote-block)
                             (quote-section . org-moinmoin-quote-section)
                             ;;(radio-target . org-moinmoin-radio-target)
                             (section . org-moinmoin-section)
                             (special-block . org-moinmoin-special-block)
                             (src-block . org-moinmoin-src-block)
                             ;;(statistics-cookie . org-moinmoin-statistics-cookie)
                             (strike-through . org-moinmoin-strike-through)
                             (subscript . org-moinmoin-subscript)
                             (superscript . org-moinmoin-superscript)
                             (table . org-moinmoin-table)
                             (table-cell . org-moinmoin-table-cell)
                             (table-row . org-moinmoin-table-row)
                             ;;(target . org-moinmoin-target)
                             ;;(template . org-moinmoin-template)
                             ;;(timestamp . org-moinmoin-timestamp)
                             (underline . org-moinmoin-underline)
                             (verbatim . org-moinmoin-verbatim))
                             ;;(verse-block . org-moinmoin-verse-block))
                            
                           :export-block "MoinMoin"
                           :filters-alist '(;;(:filter-options . org-moinmoin-infojs-install-script)
                                            ;;(:filter-final-output . org-moinmoin-final-function)
                                            )
                           :menu-entry
                           '(?m "Export to MoinMoin"
                                ((?M "As MoinMoin buffer" org-moinmoin-export-as-moinmoin)
                                 (?m "As MoinMoin file" org-moinmoin-export-to-moinmoin)
                                 (?o "As MoinMoin file and open"
                                     (lambda (a s v b)
                                       (if a (org-moinmoin-export-to-moinmoin t s v b)
                                         (org-open-file (org-moinmoin-export-to-moinmoin nil s v b)))))))
                           :options-alist
  '(;;(:moinmoin-extension nil nil org-moinmoin-extension)
    ;;(:moinmoin-postamble nil "moinmoin-postamble" org-moinmoin-postamble)
    ;;(:moinmoin-preamble nil "moinmoin-preamble" org-moinmoin-preamble)
    ))

(defun org-moinmoin-bold (bold contents info)
  (format "'''%s'''" contents))

;; (defun org-moinmoin-center-block (center-block contents info)
;;   (format "%s" contents))

;; (defun org-moinmoin-clock (clock contents info)
;;   (format "%s" contents))

(defun org-moinmoin-code (code contents info)
  (format "{{{%s}}}" contents))

;; (defun org-moinmoin-drawer (drawer contents info)
;;   (format "%s" contents))

;; (defun org-moinmoin-dynamic-block (dynamic-block contents info)
;;   (format "%s" dynamic-block))

;; (defun org-moinmoin-entity (entity contents info)
;;   (format "%s" entity))

(defun org-moinmoin-example-block (example-block contents info)
  (format "{{{%s}}}" contents))

;; (defun org-moinmoin-export-block (export-block contents info)
;;   (format "%s" export-block))

;; (defun org-moinmoin-export-snippet (export-snippet contents info)
;;   (format "%s" export-snippet))

(defun org-moinmoin-fixed-width (fixed-width contents info)
  (format "`%s`" contents))

(defun org-moinmoin-footnote-definition (footnote-definition contents info)
  (format "<<FootNote(%s)>>" contents))

;; (defun org-moinmoin-footnote-reference (footnote-reference contents info)
;;   (format "%s" footnote-reference))

(defun org-moinmoin-headline (headline contents info)
  (format "= %s =\n" headline))

(defun org-moinmoin-horizontal-rule (horizontal-rule contents info)
  "----\n")

(defun org-moinmoin-inline-src-block (inline-src-block contents info)
  (format "`%s`" contents))

;; (defun org-moinmoin-inlinetask (inlinetask contents info)
;;   (format "%s" inlinetask))

;; (defun org-moinmoin-inner-template (inner-template contents info)
;;   (format "%s" inner-template))

(defun org-moinmoin-italic (italic contents info)
  (format "''%s''" contents))

(defun org-moinmoin-item (item contents info)
  (format "%s" contents))

;; (defun org-moinmoin-keyword (keyword contents info)
;;   (format "%s" keyword))

;; (defun org-moinmoin-latex-environment (latex-environment contents info)
;;   (format "%s" latex-environment))

;; (defun org-moinmoin-latex-fragment (latex-fragment contents info)
;;   (format "%s" latex-fragment))

(defun org-moinmoin-line-break (line-break contents info)
  "<<BR>>")

(defun org-moinmoin-link (link contents info)
  (format "[%s]" contents))

(defun org-moinmoin-paragraph (paragraph contents info)
  (format "(%s) %s\n\n" paragraph contents))

(defun org-moinmoin-plain-list (plain-list contents info)
  (format "%s" contents))

(defun org-moinmoin-plain-text (text info)
  "Docstring"
  (let ((output text))
    ;; Escape any odd characters or do whatever.
    output))

;; (defun org-moinmoin-planning (planning contents info)
;;   (format "%s" planning))

;; (defun org-moinmoin-property-drawer (property-drawer contents info)
;;   (format "%s" property-drawer))

(defun org-moinmoin-quote-block (quote-block contents info)
  (format "\n  %s" contents))

(defun org-moinmoin-quote-section (quote-section contents info)
  (format "\"%s\"" contents))

;; (defun org-moinmoin-radio-target (radio-target contents info)
;;   (format "%s" radio-target))

(defun org-moinmoin-section (section contents info)
  (format "%s" contents))

;; (defun org-moinmoin-special-block (special-block contents info)
;;   (format "%s" special-block))

(defun org-moinmoin-src-block (src-block contents info)
  (format "{{{%s}}}" contents))

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
  (format "|%s|" contents))

(defun org-moinmoin-table-row (table-row contents info)
  (format "|%s|" contents))

;; (defun org-moinmoin-target (target contents info)
;;   (format "%s" target))

;; (defun org-moinmoin-template (template contents info)
;;   (format "%s" template))

;; (defun org-moinmoin-timestamp (timestamp contents info)
;;   (format "%s" timestamp))

(defun org-moinmoin-underline (underline contents info)
  (format "__%s__" contents))

(defun org-moinmoin-verbatim (verbatim contents info)
  (format "{{{%s}}}" contents))

;; (defun org-moinmoin-verse-block (verse-block contents info)
;;    (format "%s" verse-block))

;;;###autoload
(defun org-moinmoin-export-as-moinmoin
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org MoinMoin Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (set-auto-mode t)
	      (org-export-add-to-stack (current-buffer) 'moinmoin)))
	`(org-export-as 'moinmoin ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'moinmoin "*Org MoinMoin Export*"
		   subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))
