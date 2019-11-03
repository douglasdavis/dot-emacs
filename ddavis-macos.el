;;; ddavis-macos.el --- Emacs on macOS config        -*- lexical-binding: t; -*-

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

;; some mac specific things

;;; Code:

(require 'use-package)
(require 'ddavis-helm)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH"  "MANPATH" "SHELL"))
  (exec-path-from-shell-initialize))

(when (display-graphic-p)
  (setq-default ns-alternate-modifier 'meta)
  (setq-default mac-option-modifier 'meta)
  (setq-default ns-right-alternate-modifier nil))

(when (display-graphic-p)
  (setq-default ns-command-modifier 'super)
  (setq-default mac-command-modifier 'super))

(when (display-graphic-p)
  (setq-default ns-function-modifier 'hyper)
  (setq-default mac-function-modifier 'hyper))

(global-unset-key (kbd "s-t"))
(global-set-key [(meta shift up)]  'ddavis/move-line-up)
(global-set-key [(meta shift down)]  'ddavis/move-line-down)
(global-set-key (kbd "s-\\") 'ddavis/toggle-window-split)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-.") 'other-window)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-b") 'helm-buffers-list)
(global-set-key (kbd "s-t") 'neotree)
(global-set-key (kbd "s-w") (lambda ()
                              (interactive)
                              (if (< (count-windows) 2)
                                  (delete-frame)
                                (delete-window))))

(setq browse-url-browser-function 'browse-url-default-macosx-browser)



(provide 'ddavis-macos)
;;; ddavis-macos.el ends here
