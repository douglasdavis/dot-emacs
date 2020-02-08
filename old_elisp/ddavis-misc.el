;;; ddavis-misc.el --- misc Emacs config             -*- lexical-binding: t; -*-

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

;; Misc. stuff that doesn't fall into another category. Many setq's
;; and keybinding definitions, Also some various use-package calls
;; that do not require any more configuration.

;;; Code:

(require 'use-package)
(require 'ddavis-vars)
(require 'ddavis-utils)

;; authinfo
(setq auth-sources '((:source "~/.emacs.d/.authinfo.gpg")))

;; misc seq's
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.001)
(setq inhibit-startup-screen t)
(setq tramp-default-method "ssh")
(setq-default show-paren-delay 0)
(setq-default indent-tabs-mode nil)

;; we don't like Emacs backups
(setq auto-save-list-file-prefix nil
      create-lockfiles nil
      auto-save-list-file-prefix nil
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)

;; some key bindings which don't require any external packages.
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-x \\") 'ddavis/toggle-window-split)

(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook
             (lambda ()
               (add-to-list 'write-file-functions 'delete-trailing-whitespace))))
 '(text-mode-hook
   c-mode-common-hook
   emacs-lisp-mode-hook
   python-mode-hook
   markdown-mode-hook
   bash-mode-hook
   sh-mode-hook
   cmake-mode-hook
   fundamental-mode-hook
   LaTeX-mode-hook))

(when ddavis-v-is-cc7
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/home/ddavis/Software/localbase/bin/firefox"))

(when ddavis-v-is-pion
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox-developer-edition"))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-project-dir)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh nil))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))

(use-package cmake-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package ace-window
  :ensure t
  :bind ("M-o" . 'ace-window))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . 'elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.scipy.org/feed.xml"
          "https://planet.emacslife.com/atom.xml"
          "https://sachachua.com/blog/feed/"
          "https://ddavis.io/index.xml"
          "https://www.npr.org/rss/podcast.php?id=510318"
          "http://pragmaticemacs.com/feed/"))
  (setq-default elfeed-search-filter "@4-weeks-ago"))


(provide 'ddavis-misc)
;;; ddavis-misc.el ends here