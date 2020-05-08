;;; rss.el --- rss feed setup                        -*- lexical-binding: t; -*-

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

;; rss feed setup with elfeed

;;; Code:


(use-package elfeed
  :commands elfeed
  :ensure t
  :bind ("C-x w" . 'elfeed)
  :init
  (setq shr-use-fonts nil)
  (setq elfeed-feeds
        '(("https://planet.scipy.org/feed.xml" python)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://ddavis.io/index.xml" blog)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("http://feeds.podtrac.com/zKq6WZZLTlbM" nyt podcast)))
  :config
  ;; Entries older than 3 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "3 weeks ago"
                                :remove 'unread))
  (setq-default elfeed-search-filter "@21-days-ago")


  (defvar dd-podcast-speed "1.33"
    "mpv --speed argument for podcasts")

  (defun dd/elfeed-play-enclosure-with-mpv ()
    "Play enclosure link with mpv."
    (interactive)
    (let ((speed dd-podcast-speed)
          (podcast-link (nth 0 (car (elfeed-entry-enclosures elfeed-show-entry)))))
      (message "Opening %s with with mpv..." podcast-link)
      (start-process "elfeed-mpv" nil "mpv"
                     "--speed" speed
                     podcast-link))))


;;; end of rss.el
