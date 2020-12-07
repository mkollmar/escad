;;; escad-mode.el --- Escad mode
;; Copyright (C) 2020 Markus Kollmar (email: markuskollmar@onlinehome.de)
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

(defun escad-make-repl ()
  "Generate in current buffer a slime-repl."
  (slime))

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
  (split-window nil nil 'below)
  (escad-make-repl)
  (previous-window)
  (escad-make-view "./public/view/view-0.pdf")
  (split-window nil nil 'right)
  (escad-make-taxonomy-browser "../../lib/escad_taxonomy.lisp"))
  
;; (easy-menu-define words-menu global-map
;;        "Menu for word navigation commands."
;;        '("Escad"
;;           ["Forward word" forward-word]
;;           ["Backward word" backward-word]))

;; (define-key global-map [menu-bar Escad] 'undefined)

(provide 'escad-mode)

;;; escad-mode.el ends here
