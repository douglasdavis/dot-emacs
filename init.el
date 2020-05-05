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

;; My personal Emacs configuration.

;;; Code:

;; plenty of RAM
(let ((gc-cons-threshold (* 1000 1024 1024)))
  (setq comp-deferred-compilation t
        comp-async-jobs-number 5)
  (require 'ob-tangle)
  (org-babel-load-file (concat user-emacs-directory "dot-emacs/emacs-init.org"))
  (garbage-collect))

(setq gc-cons-threshold (* 100 1024 1024))
