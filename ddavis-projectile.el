;;; ddavis-projectile.el --- projectile configuration  -*- lexical-binding: t; -*-

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

(use-package projectile
  :ensure t
  :hook
  (after-init . projectile-mode)
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))

(defun ddavis/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(provide 'ddavis-projectile)
;;; ddavis-projectile.el ends here
