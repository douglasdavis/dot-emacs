;;; ddavis-lsp.el --- lsp configuration              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Doug Davis

;; Author: Doug Davis <douglas.davis.092@gmail.com>
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

;;

;;; Code:

(require 'use-package)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-clients-clangd-executable ddavis-v-clangd-exe)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-show-hover nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(provide 'ddavis-lsp)
;;; ddavis-lsp.el ends here
