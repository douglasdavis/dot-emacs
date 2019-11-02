;;; ddavis-cpp.el --- c++ configuration              -*- lexical-binding: t; -*-

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

;; C++ dev specific setup

;;; Code:

(require 'use-package)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.icc\\'" . c++-mode))

(defun ddavis/cpp-eglot-setup ()
  "enable variables and hooks for eglot cpp IDE"
  (interactive)
  (use-package eglot
    :ensure t
    :config
    (require 'eglot))
  (delete 'company-capf company-backends)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'eglot-server-programs
               `((c++-mode cc-mode) ,ddavis-v-clangd-exe)))


(provide 'ddavis-cpp)
;;; ddavis-cpp.el ends here
