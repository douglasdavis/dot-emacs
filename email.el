;;; email.el --- Emacs init email configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: mail, init

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

;; my email setup with mu4e

;;; Code:


(use-package message
  :init
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-kill-buffer-on-exit t
        message-dont-reply-to-names '("ddavis@phy.duke.edu"
                                      "ddavis@ddavis.io"
                                      "ddavis@anaconda.com"
                                      "ddavis@cern.ch"))
  :demand t)

(use-package sendmail
  :init
  (defconst dd/sendmail-exe
    (cond (dd/on-m1-p "/Users/ddavis/software/specific/msmtp/1.8.14/bin/msmtp")
          (dd/on-cc7-p "/usr/local/bin/msmtp")
          (t (executable-find "msmtp")))
    "Machine dependent msmtp executable string.")
  (setq sendmail-program dd/sendmail-exe)
  :demand t)

(defconst dd/mu-exe
  (cond (dd/on-m1-p "/opt/homebrew/Cellar/mu/1.6.3/bin/mu")
        (dd/on-cc7-p "/home/ddavis/software/specific/mu/1.4.15/bin/mu")
        (t (executable-find "mu")))
  "Machine dependent mu executable string.")

(defconst dd/mu4e-dir
  (cond (dd/on-m1-p "/opt/homebrew/Cellar/mu/1.6.3/share/emacs/site-lisp/mu/mu4e")
        (dd/on-cc7-p "/home/ddavis/software/specific/mu/1.4.15/share/emacs/site-lisp/mu4e")
        (dd/on-mac-p "/usr/local/Cellar/mu/1.6.3/share/emacs/site-lisp/mu/mu4e"))
  "Machine dependent mu4e installation location string.")

(defun dd/mu4e-byte-comp ()
  (interactive)
  (byte-recompile-directory dd/mu4e-dir 0 t))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(use-package mu4e
  :load-path dd/mu4e-dir
  :commands (mu4e mu4e-update-mail-and-index)
  :bind (("C-c 4" . mu4e)
         :map mu4e-headers-mode-map
         ("j" . dd/mu4e-jump-via-comp-read)
         ("d" . mu4e-headers-mark-for-delete)
         ("D" . mu4e-headers-mark-for-trash)
         :map mu4e-main-mode-map
         ("j" . dd/mu4e-jump-via-comp-read)
         :map mu4e-view-mode-map
         ("V" . visual-fill-column-mode)
         ("d" . mu4e-view-mark-for-delete)
         ("D" . mu4e-view-mark-for-trash)
         ("M" . mu4e-action-view-in-w3m)
         ("j" . dd/mu4e-jump-via-comp-read))
  :config
  (add-hook 'mu4e-main-mode-hook 'delete-other-windows)

  (defun dd/dont-auto-save ()
    (interactive)
    (auto-save-mode -1))
  (add-hook 'mu4e-compose-mode-hook #'dd/dont-auto-save)

  (defun dd/mu4e-toggle-gnus ()
    "Toggle Gnus view mode in mu4e."
    (interactive)
    (setq mu4e-view-use-gnus (not mu4e-view-use-gnus)))

  (defun dd/reset-standard-name-and-email ()
    "Reset mail address and name to default."
    (interactive)
    (setq user-mail-address "ddavis@ddavis.io"
          user-full-name "Doug Davis"))

  (defun dd/mu4e-jump-via-comp-read ()
    "Jump maildirs using `completing-read'."
    (interactive)
    (let ((maildir (completing-read "Maildir: " (mu4e-get-maildirs))))
      (mu4e-headers-search (format "maildir:\"%s\"" maildir))))

  (defun mu4e-action-view-in-w3m ()
    "View the body of the message in Emacs w3m."
    (interactive)
    (w3m-browse-url (concat "file://"
                            (mu4e~write-body-to-html (mu4e-message-at-point t)))))

  (set-face-attribute 'mu4e-unread-face nil :weight 'regular)
  (set-face-attribute 'mu4e-header-highlight-face nil :weight 'regular)
  ;; (setq  mu4e-use-fancy-chars nil
  ;;        mu4e-headers-thread-connection-prefix '("│"   . "│")
  ;;        mu4e-headers-thread-orphan-prefix     '("├► " . "├► ")
  ;;        mu4e-headers-thread-child-prefix      '("├► " . "├► ")
  ;;        mu4e-headers-thread-last-child-prefix '("╰► " . "╰► "))
  (setq mu4e-mu-binary dd/mu-exe
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "true"
        mu4e-update-interval 120
        mu4e-maildir "~/.mail"
        mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-user-mail-address-list '()
        mu4e-attachment-dir (expand-file-name "~/Downloads/")
        mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-reply-ignore-address
        '("notifications@github\\.com"
          "ddavis@ddavis\\.io"
          "ddavis@phy\\.duke\\.edu"
          "douglas\\.davis\\.092@gmail\\.com"
          "douglas\\.davis@duke\\.edu"
          "douglas\\.davis@cern\\.ch"
          "ddavis@cern\\.ch"))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "cern"
             :enter-func (lambda () (mu4e-message "Entering CERN context"))
             :leave-func (lambda () (dd/reset-standard-name-and-email))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/cern" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address      . "ddavis@cern.ch" )
                      ( user-email-address     . "ddavis@cern.ch" )
                      ( mail-host-address      . "cern.ch" )
                      ( user-full-name         . "Doug Davis" )
                      ( mu4e-trash-folder      . "/cern/Trash" )
                      ( mu4e-sent-folder       . "/cern/Sent" )
                      ( mu4e-drafts-folder     . "/cern/Drafts" )
                      ( mu4e-reply-to-address  . "ddavis@cern.ch" )))

           ,(make-mu4e-context
             :name "duke"
             :enter-func (lambda () (mu4e-message "Entering Duke context"))
             :leave-func (lambda () (dd/reset-standard-name-and-email))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/duke" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address       . "ddavis@phy.duke.edu" )
                      ( user-email-address      . "ddavis@phy.duke.edu" )
                      ( user-full-name          . "Doug Davis" )
                      ( mail-host-address       . "phy.duke.edu" )
                      ( mu4e-trash-folder       . "/duke/Trash" )
                      ( mu4e-sent-folder        . "/duke/Sent" )
                      ( mu4e-drafts-folder      . "/duke/Drafts" )
                      ( mu4e-reply-to-address   . "ddavis@phy.duke.edu" )))))
  (when (or dd/on-mac-p dd/on-cc7-p)
    (add-to-list 'mu4e-contexts
                 (make-mu4e-context
                  :name "fastmail"
                  :enter-func (lambda () (mu4e-message "Entering FastMail context"))
                  :leave-func (lambda () (mu4e-message "Leaving FastMail context"))
                  :match-func (lambda (msg)
                                (when msg
                                  (string-match-p "^/fastmail" (mu4e-message-field msg :maildir))))
                  :vars '( ( user-mail-address      . "ddavis@ddavis.io" )
                           ( user-email-address     . "ddavis@ddavis.io" )
                           ( user-full-name         . "Doug Davis" )
                           ( mail-host-address      . "ddavis.io" )
                           ( mu4e-trash-folder      . "/fastmail/Trash" )
                           ( mu4e-sent-folder       . "/fastmail/Sent" )
                           ( mu4e-drafts-folder     . "/fastmail/Drafts" )
                           ( mu4e-reply-to-address  . "ddavis@ddavis.io" )))))
  (setq mu4e-bookmarks '())
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread short"
                :query "flag:unread and (m:/duke* or m:/cern* or m:/fastmail/INBOX)"
                :key ?U))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread all"
                :query "flag:unread AND NOT flag:trashed"
                :key ?u))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Recent personal"
                :query "date:1w..now AND m:/fastmail*"
                :key ?f))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "INBOXes"
                :query "m:/duke/INBOX or m:/cern/INBOX or m:/fastmail/INBOX"
                :key ?i))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last day's work"
                :query (concat "date:1d..now "
                               "and not m:/fastmail* "
                               "and not m:/cern/Mailing\\ Lists/CERN-JIRA "
                               "and not m:/cern/Mailing\\ Lists/JEDI*")
                :key ?w))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Recent work"
                :query "date:3d..now AND NOT m:/fastmail* "
                :key ?r))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Duke recent"
                :query "date:5d..now AND m:/duke*"
                :key ?d))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "CERN recent"
                :query "date:2d..now AND m:/cern*"
                :key ?c))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 1 day"
                :query "date:1d..now"
                :key ?1))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 3 days"
                :query "date:3d..now"
                :key ?3))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 7 days"
                :query "date:1w..now"
                :key ?7)))


;;; end of email.el
