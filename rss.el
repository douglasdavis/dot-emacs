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
