;;; dot-gnus.el --- Gnus config                       -*- lexical-binding: t; -*-

(use-package gnus
  :config
  (add-hook 'gnus-before-startup-hook #'delete-other-windows)
  (add-hook 'gnus-before-resume-hook #'delete-other-windows)
  (setq gnus-startup-file "~/.emacs.d/gnusnews/newsrc")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nntp "news.gmane.io")
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))
          ;; (nnimap "dukemail"
          ;;         (nnimap-address "mail.phy.duke.edu")
          ;;         (nnimap-stream ssl)
          ;;         (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))
          (nnimap "anaconda"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-stream ssl)
                  (nnimap-server-port "imaps")
                  (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))))
  (setq gnus-message-archive-group "nnimap+fastmail:Sent")

  (setq message-directory "~/.emacs.d/gnusmail/"
        gnus-home-directory "~/.emacs.d/gnusnews/"
        gnus-dribble-directory "~/.emacs.d/gnusnews/"
        gnus-article-save-directory "~/.emacs.d/gnusnews/"
        gnus-directory "~/.emacs.d/gnusnews/")

  (setq nnfolder-directory "~/.emacs.d/gnusmail/archive/")
  (setq nndraft-directory "~/.emacs.d/gnusmail/drafts/")
  (setq gnus-parameters
        '(("fastmail"
           (posting-style
            (name "Doug Davis")
            (address "ddavis@ddavis.io")
            (gcc "nnimap+fastmail:Sent")
            (body "")
            (eval (setq mail-host-address "ddavis.io"
                        message-sendmail-extra-arguments '("--read-envelope-from")))
            (user-mail-address "ddavis@ddavis.io")))
          ("fastmailwedding"
           (posting-style
            (name "Doug Davis")
            (address "us@dougandkristiebecome.one")
            (gcc "nnimap+fastmail:Sent")
            (body "")
            (eval (setq mail-host-address "dougandkristiebecome.one"
                        message-sendmail-extra-arguments '("--read-envelope-from")))
            (user-mail-address "us@dougandkristiebecome.one")))
          ;; ("dukemail"
          ;;  (posting-style
          ;;   (name "Doug Davis")
          ;;   (address "ddavis@phy.duke.edu")
          ;;   (body "")
          ;;   (gcc "nnimap+dukemail:Sent")
          ;;   (eval (setq mail-host-address "phy.duke.edu"
          ;;               message-sendmail-extra-arguments '("--read-envelope-from")))
          ;;   (user-mail-address "ddavis@phy.duke.edu")))))
          ("anaconda"
           (posting-style
            (address "Doug Davis <ddavis@anaconda.com>")
            (gcc nil)
            (body "")
            (name "Doug Davis")
            (eval (setq message-send-mail-function 'smtpmail-send-it
                        smtpmail-default-smtp-sever "imap.gmail.com"
                        smtpmail-smtp-service 587))
            ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 ddavis@anaconda.com")))))

  ;; keys
  ;; (define-key gnus-group-mode-map (kbd "q") 'quit-window)
  ;; (define-key gnus-group-mode-map (kbd "Q") 'gnus-group-exit)

  (setq mail-user-agent 'gnus-user-agent) ; also works with `sendmail-user-agent'
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-novice-user nil)

  ;; agent
  (setq gnus-agent t)
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/.emacs.d/gnusagent")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil)

  ;; article
  (setq gnus-article-browse-delete-temp t)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.3)
  (setq gnus-mode-line-image-cache nil)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "Gnus: %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))

  ;; asynchronous
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 20)

  ;; checking sources
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)

  ;; dribble
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)

  ;; groups
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-level-zombie 9)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '(gnus-group-sort-by-rank
          gnus-group-sort-by-unread
          gnus-group-sort-by-alphabet))
  (setq gnus-topic-display-empty-topics t)
  (setq gnus-group-mode-line-format "Gnus: %%b")
  ;; summary
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject t)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format "%U%R%z %-16,16&user-date;  %4L:%-20,20f  %B%S\n")
  (setq gnus-summary-mode-line-format "Gnus: %p (%U)")
  (when window-system
    (setq gnus-summary-display-arrow t)
    (setq-default
     ;; gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     ;;gnus-thread-sort-functions '(gnus-thread-sort-by-date)
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
     gnus-sum-thread-tree-root            ""
     gnus-sum-thread-tree-false-root      ""
     gnus-sum-thread-tree-indent          " "
     gnus-sum-thread-tree-vertical        "│"
     gnus-sum-thread-tree-leaf-with-other "├► "
     gnus-sum-thread-tree-single-leaf     "╰► "))
  (set-face-attribute 'gnus-summary-selected nil :weight 'regular)
  (set-face-attribute 'gnus-summary-normal-unread nil :weight 'regular)
  (set-face-attribute 'gnus-summary-high-unread nil :weight 'regular)
  (set-face-attribute 'gnus-summary-normal-ancient nil :inherit 'regular)
  (set-face-attribute 'gnus-summary-low-ancient nil :inherit 'regular)
  (set-face-attribute 'gnus-summary-high-ancient nil :inherit 'regular)
  :hook ((gnus-summary-mode-hook . hl-line-mode)
         (gnus-group-mode-hook . hl-line-mode)
         (gnus-server-mode-hook . hl-line-mode)
         (gnus-group-mode-hook . gnus-topic-mode))
  :custom-face
  (gnus-summary-selected-face ((t (:weight regular)))))
