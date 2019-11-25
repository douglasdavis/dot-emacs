;;; ddavis-vc.el --- version control                 -*- lexical-binding: t; -*-

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

;; version control specific things; i.e. magit
;; see: https://magit.vc/

;;; Code:

(require 'use-package)

(setq vc-follow-symlinks t)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  :demand)


(defun ddavis/magit-kill-buffers ()
  "See `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))
(bind-key "q" #'ddavis/magit-kill-buffers magit-status-mode-map)


(provide 'ddavis-vc)
;;; ddavis-vc.el ends here
