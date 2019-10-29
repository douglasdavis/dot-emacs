;;; ddavis-helm.el --- helm configuration            -*- lexical-binding: t; -*-

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

(require 'use-package)

(use-package helm
  :ensure t
  :init
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 30)
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-ff-skip-boring-files t)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-t" . find-file)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-bookmarks)
         ("C-x m" . helm-M-x)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)))

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (require 'helm-projectile)
  (helm-projectile-on)
  (define-key global-map (kbd "C-c h p") 'helm-projectile)
  (setq helm-split-window-in-side-p t))

(use-package helm-rg
  :ensure t
  :init
  (setq helm-rg-ripgrep-executable ddavis-v-rg-exe)
  :config
  (require 'helm-rg)
  (define-key global-map (kbd "C-c s r") 'helm-projectile-rg))


(provide 'ddavis-helm)
;;; ddavis-helm.el ends here
