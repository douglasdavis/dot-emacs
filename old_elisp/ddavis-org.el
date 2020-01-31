;;; ddavis-org.el --- org mode configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-mode setup.

;;; Code:

(require 'use-package)
(require 'ddavis-vars)

(if ddavis-v-is-mac
    ;; mac
    (use-package org
      :bind (("<A-down>" . 'org-move-subtree-down)
             ("<A-up>" . 'org-move-subtree-up)
             ("<A-left>" . 'org-promote-subtree)
             ("<A-right>" . 'org-demote-subtree))
      :init
      (setq org-src-fontify-natively t)
      :config
      (setq org-structure-template-alist
            (append org-structure-template-alist
                    '(("el" . "src emacs-lisp :results silent")
                      ("py" . "src python :results silent")
                      ("cpp" . "src C++")))))

  ;; not mac
  (use-package org
    :bind (("<s-down>" . 'org-move-subtree-down)
           ("<s-up>" . 'org-move-subtree-up)
           ("<s-left>" . 'org-promote-subtree)
           ("<s-right>" . 'org-demote-subtree))
    :init
    (setq org-src-fontify-natively t)
    :config
    (setq org-structure-template-alist
          (append org-structure-template-alist
                  '(("el" . "src emacs-lisp :results silent")
                    ("py" . "src python :results silent")
                    ("cpp" . "src C++"))))))


(require 'ox-md)

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-reveal
  :ensure t
  :after ox)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((python . t)))


(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/Dropbox/org/agenda/"))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

(use-package htmlize
  :ensure t)



(provide 'ddavis-org)
;;; ddavis-org.el ends here
