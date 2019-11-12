;;; ddavis-package.el --- setting up packaging       -*- lexical-binding: t; -*-

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

;; Setting up use-package -- see: https://github.com/jwiegley/use-package

;;; Code:


(require 'package)
(require 'ddavis-vars)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa" "https://orgmode.org/elpa/") t)

(when (version< emacs-version "27.0.50")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 5)
  (auto-package-update-maybe))


(provide 'ddavis-package)
;;; ddavis-package.el ends here
