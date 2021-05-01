;;; escad-mode.el --- Escad mode
;; Copyright (C) 2020, 2021 Markus Kollmar (email: markuskollmar@onlinehome.de)
;;
;; This file is part of ESCAD.
;;
;; ESCAD is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ESCAD is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with ESCAD.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; This file provides tools for a more convenient working with emacs as a escad via slime.
;; Step1: make buffer right, execute (pdf-tools-install), load view-0.pdf, (auto-revert-mode)
;; Step2: make buffer below, start slime, load escad, set repl-width to frame-width, hide headline
;; Step3: make buffer left, execute (escad-taxonomy-browser)
;; Step4: create escad-lisp mode for editing escad-lisp files. Thus allow support for special auto-complete functionality of taxonomy (e.g. with https://github.com/auto-complete/auto-complete/blob/master/doc/manual.md#omni-completion)
;;
;; Provide:
;; - ido.mode for taxonomy-list fast selection:
;;  (setq mylist (list "red" "blue" "yellow" "clear" "i-dont-know"))
;;  (ido-completing-read "What, ... is your favorite color? " mylist)
;; - drawing preview for 2d sketch preview
;;(require 'dom)
;;(require 'svg)
;; (image-type-available-p 'imagemagick)
;; image-types
;; (setq svg (svg-create 50 50 :stroke "orange" :stroke-width 1))
;; (svg-gradient svg "gradient" 'linear '(0 . "red") '(100 . "blue"))
;; (save-excursion (goto-char (+ 2(point))) (svg-insert-image svg))
;; (svg-rectangle svg 1 1 15 15 :gradient "gradient" :id "rec1")
;; (svg-circle svg 5 5 10 :id "circle1")
;; (svg-ellipse svg 100 100 50 90 :stroke "red" :id "ellipse1")
;; (svg-line svg 100 190 50 100 :id "line1" :stroke "yellow")
;; (svg-polyline svg '((200 . 100) (500 . 450) (80 . 100))
;; 	      :stroke "green" :id "poly1")
;; (svg-polygon svg '((100 . 100) (200 . 150) (150 . 90))
;; 	     :stroke "blue" :fill "red" :id "gon1")
;; (svg-print svg)
;; (dom-attributes svg)
;; (dom-by-id svg "rec1")
;; (dom-set-attribute (dom-by-id svg "rec1") 'height 45)
;; (svg-possibly-update-image svg)

;;; Code:

(define-minor-mode escad-mode
  "Toggle escad minor mode which provides tools for a escad-session.
While in a escad (https://github.com/mkollmar/escad) session with slime-mode you get some convenience key-strokes and views to get a better supported escad-environment than a simple lisp repl."
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " Escad"
 ;; The minor mode bindings.
 :keymap
 '(([C-backspace] . hungry-electric-delete)
   ([M-l] . goto-line)
   ([C-M-backspace]
    . (lambda ()
        (interactive)
        (hungry-electric-delete t)))))

(defun escad-make-repl (load-file)
  "Generate in current buffer a slime-repl."
  (slime)
  (slime-load-file load-file))

(defun escad-make-view (view-file)
  "Generate in current buffer a view."
  (find-file view-file)
  (pdf-tools-install)
  (auto-revert-mode))

(defun escad-make-taxonomy-browser (taxonomy-file)
  "Generate in current buffer a org-mode-structure of the taxonomy file."
  (let* ((taxonomy (read (with-temp-buffer
			   (insert-file-contents taxonomy-file)
			   (buffer-string)))))
    (switch-to-buffer (set-buffer (generate-new-buffer "Escad Taxonomy")))
    (insert "* ESCAD Taxonomy")(newline)
    (insert "** Description")(newline)
    (dolist (help-list (car taxonomy))
      (insert (format "%s=%s."
		      (car help-list)
		      (cadr help-list))) (newline))
    
    (insert "** Taxonomies")(newline)
    (dolist (taxonomies-list (cadr taxonomy))
      (insert (format "%s: %s."
		      (nth 1 taxonomies-list)
		      (nth 3 taxonomies-list))) (newline)))
  (org-mode))

(defun escad-make-layout ()
  "Creates basic window layout for escad. Bottom is for repl, upper left for graphical view and upper right for additional view/help/taxonomy-browser. Should be called once by beginning of session."
  (let ((repl-win (split-window nil nil 'below)))
    (window-resize repl-win (- 5 (window-body-height repl-win)))
    (split-window nil nil 'right)
    (escad-make-view "./public/view/view-0.pdf")
    (next-window)
    (escad-make-taxonomy-browser "../../lib/escad_taxonomy.lisp")
    (next-window)
    (escad-make-repl "../../package.lisp")))

(defun escad-make-
  
;; (easy-menu-define words-menu global-map
;;        "Menu for word navigation commands."
;;        '("Escad"
;;           ["Forward word" forward-word]
;;           ["Backward word" backward-word]))

;; (define-key global-map [menu-bar Escad] 'undefined)

(provide 'escad-mode)

;;; escad-mode.el ends here
