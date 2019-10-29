;;; ddavis-misc.el --- misc Emacs config             -*- lexical-binding: t; -*-

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

(setq vc-follow-symlinks t)
(use-package magit
  :ensure t
  :bind ("C-c m s" . 'magit-status))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-project-dir)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh nil))

(use-package which-key
  :ensure t
  :hook
  (after-init . which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :config
  (require 'yasnippet-snippets))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . 'elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.scipy.org/feed.xml"
          "http://sachachua.com/blog/feed/"
          "http://pragmaticemacs.com/feed/"
          "https://ddavis.io/index.xml"))
  ;;"http://planet.emacsen.org/atom.xml"))
  (setq-default elfeed-search-filter "@4-weeks-ago"))

(use-package deadgrep
  :ensure t
  :config
  (require 'deadgrep))


(use-package ace-window
  :ensure t
  :config
  (define-key global-map (kbd "M-o") 'ace-window))


(provide 'ddavis-misc)
;;; ddavis-misc.el ends here
