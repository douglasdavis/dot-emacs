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

;; helm setup -- see: https://github.com/emacs-helm/helm

;;; Code:

(require 'use-package)
(require 'ddavis-projectile)

(use-package helm
  :ensure t
  :init (setq helm-autoresize-max-height 50
              helm-autoresize-min-height 30)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-t" . find-file)
         ("C-x r b" . helm-bookmarks)
         ("C-x m" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-ff-skip-boring-files t)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :init (setq projectile-completion-system 'helm
              helm-split-window-in-side-p t)
  :bind (:map helm-command-map
              ("p" . helm-projectile))
  :demand)

(use-package helm-fd
  :ensure t
  :demand
  :init (setq helm-fd-cmd ddavis-v-fd-exe)
  :bind (:map helm-command-map
              ("/" . helm-fd)
              ("f" . helm-fd-project)))

(use-package helm-rg
  :ensure t
  :demand
  :init (setq helm-rg-ripgrep-executable ddavis-v-rg-exe)
  :bind (("C-c s r" . helm-projectile-rg)))

(use-package helm-descbinds
  :ensure t
  :demand
  :bind (("C-h b" . helm-descbinds)))

(provide 'ddavis-helm)
;;; ddavis-helm.el ends here
