;;; macos.el --- mac specific init                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: lisp

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

;; mac specific emacs init configuration

;;; Code:


(when (memq window-system '(mac ns))
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq-default ns-alternate-modifier 'meta)
  (setq-default mac-option-modifier 'meta)
  (setq-default ns-right-alternate-modifier nil)
  (setq-default ns-command-modifier 'super)
  (setq-default mac-command-modifier 'super)
  (setq-default ns-function-modifier 'hyper)
  (setq-default mac-function-modifier 'hyper)
  (global-set-key [(meta shift up)]  #'dd/move-line-up)
  (global-set-key [(meta shift down)]  #'dd/move-line-down))

(bind-key (kbd "s-\\") #'dd/toggle-window-split)
(bind-key (kbd "s-/") #'previous-buffer)
(bind-key (kbd "s-1") #'delete-other-windows)
(bind-key (kbd "s-2") #'split-window-below)
(bind-key (kbd "s-3") #'split-window-right)
(bind-key (kbd "s-5") #'projectile-find-file-in-known-projects)
(bind-key (kbd "s-4") #'mu4e)
(bind-key (kbd "s-d") #'dd/open-init)
(bind-key (kbd "s-b") #'helm-buffers-list)
(bind-key (kbd "s-f") #'helm-find-files)
(bind-key (kbd "s-g") #'magit-status)
(bind-key (kbd "s-o") #'other-window)
(bind-key (kbd "s-p") #'helm-projectile)
(bind-key (kbd "s-r") #'dd/helm-project-search)
(bind-key (kbd "s-t") #'dd/work-on-thesis)
(bind-key (kbd "s-u") #'gnus)
(bind-key (kbd "s-w") #'dd/delete-frame-or-window)

;;; end of macos.el
