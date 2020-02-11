(use-package gnus
  :config
  (setq user-full-name "Doug Davis"
        user-mail-address "ddavis@ddavis.io")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))
          (nnimap "cernmail"
                  (nnimap-address "imap.cern.ch")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))
          (nnimap "dukemail"
                  (nnimap-address "mail.phy.duke.edu")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg"))))
  (setq gnus-message-archive-group "nnimap+fastmail:Sent")
  (setq message-directory "~/.emacs.d/gnusmail/")
  (setq gnus-directory "~/.emacs.d/gnusnews/")
  (setq nnfolder-directory "~/.emacs.d/gnusmail/archive")
  (setq nndraft-directory "~/.emacs.d/gnusmail/drafts/")
  (setq gnus-parameters
        '(("fastmail"
           (posting-style
            (name "Doug Davis")
            (address "ddavis@ddavis.io")
            (gcc "nnimap+fastmail:Sent")
            (body "")
            (eval (setq message-sendmail-extra-arguments '("-a" "fastmail")))
            (user-mail-address "ddavis@ddavis.io")))
          ("dukemail"
           (posting-style
            (name "Doug Davis")
            (address "ddavis@phy.duke.edu")
            (body "")
            (gcc "nnimap+dukemail:Sent")
            (eval (setq message-sendmail-extra-arguments '("-a" "duke")))
            (user-mail-address "ddavis@phy.duke.edu")))
          ("cernmail"
           (posting-style
            (name "Doug Davis")
            (address "ddavis@cern.ch")
            (body "")
            (gcc "nnimap+cernmail:Sent")
            (eval (setq message-sendmail-extra-arguments '("-a" "cern")))
            (user-mail-address "ddavis@cern.ch")))))
  ;; agent
  (setq mail-user-agent 'gnus-user-agent) ; also works with `sendmail-user-agent'
  (setq gnus-agent t)
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/.News/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil)
  ;; article
  (setq gnus-article-browse-delete-temp 'ask)
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
  ;; asynchronous
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
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
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "") ;; "● ")
    (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
    (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
    (setq gnus-sum-thread-tree-vertical        "│")
    (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
    (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
  (setq gnus-summary-display-arrow t)

  ;; (setq gnus-sum-thread-tree-false-root "")
  ;; (setq gnus-sum-thread-tree-indent " ")
  ;; (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  ;; (setq gnus-sum-thread-tree-root "")
  ;; (setq gnus-sum-thread-tree-single-leaf "└─➤ ")
  ;; (setq gnus-sum-thread-tree-vertical "│")
  :hook ((gnus-summary-mode . hl-line-mode)
         (gnus-group-mode . hl-line-mode)
         (gnus-server-mode . hl-line-mode)
         (gnus-group-mode . gnus-topic-mode)))


(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))

     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("misc" visible))
                                 (("Email" visible nil nil)
                                  (("Duke" visible nil nil))
                                  (("CERN" visible nil nil))
                                  (("Fastmail" visible nil nil)))
                                 (("Python" visible nil nil))
                                 (("Emacs" visible nil nil))))

     (setq gnus-topic-alist '(("Python"
                               "nntp+news.gwene.org:gmane.comp.python.numba.user"
                               "nntp+news.gwene.org:gmane.comp.python.numeric.general"
                               "nntp+news.gwene.org:gmane.comp.python.pydata"
                               "nntp+news.gwene.org:gmane.comp.python.c++"
                               "nntp+news.gwene.org:gmane.comp.python.devel"
                               "nntp+news.gwene.org:gmane.comp.python.announce"
                               "nntp+news.gwene.org:gmane.comp.python.matplotlib.devel"
                               "nntp+news.gwene.org:gmane.comp.python.matplotlib.general"
                               "nntp+news.gwene.org:gmane.comp.python.matplotlib.announce"
                               "nntp+news.gwene.org:gwene.org.scipy.planet")
                              ("Emacs"
                               "nntp+news.gwene.org:gmane.emacs.devel"
                               "nntp+news.gwene.org:gmane.emacs.help"
                               "nntp+news.gwene.org:gmane.emacs.emacsconf"
                               "nntp+news.gwene.org:gmane.emacs.macintosh.osx"
                               "nntp+news.gwene.org:gmane.emacs.gnus.user"
                               "nntp+news.gwene.org:gmane.emacs.cc-mode.general"
                               "nntp+news.gwene.org:gmane.emacs.orgmode")
                              ("misc"
                               "nntp+news.gwene.org:gmane.announce"
                               "nntp+news.gwene.org:gmane.org.fsf.announce")
                              ("Fastmail"
                               "nnimap+fastmail:Trash"
                               "nnimap+fastmail:Spam"
                               "nnimap+fastmail:Sent"
                               "nnimap+fastmail:Lists/sr.ht"
                               "nnimap+fastmail:Lists/mu-discuss"
                               "nnimap+fastmail:Lists/llvm"
                               "nnimap+fastmail:Lists/clangd-dev"
                               "nnimap+fastmail:Lists"
                               "nnimap+fastmail:Drafts"
                               "nnimap+fastmail:Archive"
                               "nnimap+fastmail:INBOX")
                              ("CERN"
                               "nnimap+cernmail:Trash"
                               "nnimap+cernmail:Tasks"
                               "nnimap+cernmail:sent-mail"
                               "nnimap+cernmail:Sent Messages"
                               "nnimap+cernmail:Sent Items"
                               "nnimap+cernmail:Sent"
                               "nnimap+cernmail:Outbox"
                               "nnimap+cernmail:Notes"
                               "nnimap+cernmail:Mailing Lists/TRT mail lists"
                               "nnimap+cernmail:Mailing Lists/Top lists"
                               "nnimap+cernmail:Mailing Lists/Tau"
                               "nnimap+cernmail:Mailing Lists/SW help mail lists"
                               "nnimap+cernmail:Mailing Lists/SW Computing"
                               "nnimap+cernmail:Mailing Lists/Stats"
                               "nnimap+cernmail:Mailing Lists/Machine Learning"
                               "nnimap+cernmail:Mailing Lists/JEDI notifications"
                               "nnimap+cernmail:Mailing Lists/InDet"
                               "nnimap+cernmail:Mailing Lists/HistFitter"
                               "nnimap+cernmail:Mailing Lists/Exits"
                               "nnimap+cernmail:Mailing Lists/Current Physicists"
                               "nnimap+cernmail:Mailing Lists/CERN-JIRA"
                               "nnimap+cernmail:Mailing Lists"
                               "nnimap+cernmail:Junk E-Mail"
                               "nnimap+cernmail:Journal"
                               "nnimap+cernmail:INBOX"
                               "nnimap+cernmail:Drafts"
                               "nnimap+cernmail:Deleted Messages"
                               "nnimap+cernmail:Deleted Items"
                               "nnimap+cernmail:Contacts"
                               "nnimap+cernmail:Calendar"
                               "nnimap+cernmail:Archive"
                               "nnimap+cernmail:2018"
                               "nnimap+cernmail:2017"
                               "nnimap+cernmail:2016"
                               "nnimap+cernmail:2015"
                               "nnimap+cernmail:2014")
                              ("Duke"
                               "nnimap+dukemail:INBOX"
                               "nnimap+dukemail:Archive"
                               "nnimap+dukemail:Archives/2016"
                               "nnimap+dukemail:Archives/2015"
                               "nnimap+dukemail:Archives"
                               "nnimap+dukemail:Trash"
                               "nnimap+dukemail:Spam"
                               "nnimap+dukemail:Sent"
                               "nnimap+dukemail:Sent Messages"
                               "nnimap+dukemail:Notes"
                               "nnimap+dukemail:Drafts"
                               "nnimap+dukemail:Deleted Messages")
                              ("Email"
                               "nndraft:drafts")
                              ("Gnus")))))
