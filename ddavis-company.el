;;; ddavis-company.el --- company configuration      -*- lexical-binding: t; -*-

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

;; Helm setup -- see: https://github.com/company-mode/company-mode

;;; Code:


(require 'use-package)

(use-package company
  :init
  (setq company-clang-executable ddavis-v-clang-exe)
  :config
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'mu4e-compose-mode-hook 'company-mode)
  (add-hook 'sh-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode))


(provide 'ddavis-company)
;;; ddavis-company.el ends here
