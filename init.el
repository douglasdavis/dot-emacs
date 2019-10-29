;;; init.el --- Emacs init.el                        -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; Use .el if it is newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(fset 'yes-or-no-p 'y-or-n-p)
(when (yes-or-no-p "start server?")
  (server-start))

(add-to-list 'load-path "~/.emacs.d/ddavis-elisp")

(setq custom-file (concat user-emacs-directory "/custom.el"))

(require 'ddavis-vars)
(require 'ddavis-package)

(require 'ddavis-utils)
(require 'ddavis-org)
(require 'ddavis-helm)

(when ddavis-v-is-mac
  (require 'ddavis-macos))
(when ddavis-v-enable-mu4e
  (require 'ddavis-mu4e))
(when ddavis-v-enable-irc
  (require 'ddavis-irc))

(require 'ddavis-projectile)
(require 'ddavis-spell)
(require 'ddavis-eshell)
(require 'ddavis-company)
(require 'ddavis-python)
(require 'ddavis-cpp)
(require 'ddavis-misc)
(require 'ddavis-looks)


(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.001)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq tramp-default-method "ssh")
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(setq column-number-mode t)

(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 248))

(setq auto-save-list-file-prefix nil
      create-lockfiles nil
      auto-save-list-file-prefix nil
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook
             (lambda ()
               (add-to-list 'write-file-functions 'delete-trailing-whitespace))))
 '(text-mode-hook
   c-mode-common-hook
   python-mode-hook
   markdown-mode-hook
   bash-mode-hook
   sh-mode-hook
   cmake-mode-hook
   fundamental-mode-hook
   LaTeX-mode-hook))

(setq tex-fontify-script nil
      font-latex-fontify-script nil)

(when ddavis-v-is-cc7
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/home/ddavis/Software/localbase/bin/firefox"))

(when ddavis-v-is-pion
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))
