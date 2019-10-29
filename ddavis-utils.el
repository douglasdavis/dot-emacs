;;; ddavis-utils.el --- random utilities             -*- lexical-binding: t; -*-

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

(defun ddavis/enable-cua-selection ()
  "turn on cua-selection-mode"
  (interactive)
  (cua-selection-mode t))

;; See the following for more details
;;     https://emacs.stackexchange.com/a/50215/8887
;; and also see the following on a recent GNU/Linux or similar system:
;;     /usr/share/doc/fontconfig/fontconfig-user.html
;; for the explanation of spacing=100
;; also see the following UNIX StackExchange answer:
;;    https://unix.stackexchange.com/a/363368/13105
(defun ddavis/compare-monospace-font-families ()
  "Display a list of all monospace font faces. Tested on GNU/Linux."
  (interactive)
  (pop-to-buffer "*Monospace Fonts*")
  (erase-buffer)
  (dolist (font-name (seq-filter (lambda (font)
                                   (when-let ((info (font-info font)))
                                     (string-match-p "spacing=100" (aref info 1))))
                                 (font-family-list)))
    (insert
     (propertize
      (concat "1 l; 0 O o [ < = > ] " font-name ")\n")
      'font-lock-face `((:family
                         ,(format "%s" (font-get (font-spec :name font-name) :family))))))))

(defun ddavis/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ddavis/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun ddavis/copy-lines-matching-re (re)
  "put lines matching re in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))


(defun ddavis/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x \\") 'ddavis/toggle-window-split)


(provide 'ddavis-utils)
;;; ddavis-utils.el ends here
