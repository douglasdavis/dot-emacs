;;; ddavis-mu4e.el --- mu4e configuration            -*- lexical-binding: t; -*-

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

;; Email setup -- see: https://www.djcbsoftware.nl/code/mu/mu4e.html

;;; Code:

(require 'ddavis-vars)

(when ddavis-v-is-mac
  (add-to-list 'load-path "~/Software/mu/releases/master/share/emacs/site-lisp/mu4e")
  (setq sendmail-program "~/Software/localbase/bin/msmtp"
        mu4e-mu-binary "~/Software/mu/releases/master/bin/mu"))

(when ddavis-v-is-grads-18
  (add-to-list 'load-path "~/Software/localbase/share/emacs/site-lisp/mu4e")
  (setq sendmail-program "/usr/bin/msmtp"
        mu4e-mu-binary "~/Software/localbase/bin/mu"))

;; (when ddavis-v-is-pion
;;   (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;;   (setq sendmail-program "/usr/bin/msmtp"
;;         mu4e-mu-binary "/usr/bin/mu"))

(when ddavis-v-is-cc7
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (setq sendmail-program "~/Software/localbase/bin/msmtp"
        mu4e-mu-binary "/usr/local/bin/mu"))

(require 'mu4e)

(setq mu4e-change-filenames-when-moving t
      mu4e-get-mail-command "true"
      mu4e-update-interval 75
      mu4e-maildir "~/.mail"
      mu4e-confirm-quit nil
      mu4e-context-policy 'pick-first
      mu4e-change-filenames-when-moving t
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-kill-buffer-on-exit t)


(defun ddavis/mu4e-jump-via-helm ()
  (interactive)
  (let ((maildir (helm-comp-read "Maildir: " (mu4e-get-maildirs))))
    (mu4e-headers-search (format "maildir:\"%s\"" maildir))))

(define-key global-map (kbd "C-c 4") 'mu4e)
(define-key mu4e-headers-mode-map "j" 'ddavis/mu4e-jump-via-helm)
(define-key mu4e-view-mode-map "j" 'ddavis/mu4e-jump-via-helm)
(define-key mu4e-main-mode-map "j" 'ddavis/mu4e-jump-via-helm)
(define-key mu4e-headers-mode-map (kbd "C-c k") 'mu4e-kill-update-mail)
(define-key mu4e-view-mode-map (kbd "C-c k") 'mu4e-kill-update-mail)
(define-key mu4e-main-mode-map (kbd "C-c k") 'mu4e-kill-update-mail)
(define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-delete)
(define-key mu4e-headers-mode-map "D" 'mu4e-headers-mark-for-trash)

(defun ddavis/set-standard-name-and-email ()
  (interactive)
  (setq user-mail-address "ddavis@ddavis.io"
        user-email-address "ddavis@ddavis.io"
        user-full-name "Doug Davis"))

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "cern"
           :enter-func (lambda () (mu4e-message "Entering CERN context"))
           :leave-func (lambda () (ddavis/set-standard-name-and-email))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/cern" (mu4e-message-field msg :maildir))))
           :vars '( ( user-mail-address      . "ddavis@cern.ch" )
                    ( user-email-address     . "ddavis@cern.ch" )
                    ( user-full-name         . "Douglas Davis" )
                    ( mu4e-trash-folder      . "/cern/Trash" )
                    ( mu4e-sent-folder       . "/cern/Sent" )
                    ( mu4e-drafts-folder     . "/cern/Drafts" )
                    ( mu4e-reply-to-address  . "ddavis@cern.ch" )))

         ,(make-mu4e-context
           :name "duke"
           :enter-func (lambda () (mu4e-message "Entering Duke context"))
           :leave-func (lambda () (ddavis/set-standard-name-and-email))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/duke" (mu4e-message-field msg :maildir))))
           :vars '( ( user-mail-address       . "ddavis@phy.duke.edu" )
                    ( user-email-address      . "ddavis@phy.duke.edu" )
                    ( user-full-name          . "Douglas Davis" )
                    ( mu4e-trash-folder       . "/duke/Trash" )
                    ( mu4e-sent-folder        . "/duke/Sent" )
                    ( mu4e-drafts-folder      . "/duke/Drafts" )
                    ( mu4e-reply-to-address   . "ddavis@phy.duke.edu" )))))

(when (or ddavis-v-is-mac ddavis-v-is-cc7)
  (add-to-list 'mu4e-contexts
               (make-mu4e-context
                :name "gmail"
                :enter-func (lambda () (mu4e-message "Entering Gmail context"))
                :leave-func (lambda () (ddavis/set-standard-name-and-email))
                :match-func (lambda (msg)
                              (when msg
                                (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
                :vars '( ( user-mail-address           . "douglas.davis.092@gmail.com" )
                         ( user-email-address          . "douglas.davis.092@gmail.com" )
                         ( user-full-name              . "Doug Davis" )
                         ( mu4e-trash-folder           . "/gmail/_blackhole" )
                         ( mu4e-sent-folder            . "/gmail/[Gmail]/Sent Mail" )
                         ( mu4e-drafts-folder          . "/gmail/_blackhole" )
                         ( mu4e-reply-to-address       . "douglas.davis.092@gmail.com" ))))
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
                         ( mu4e-trash-folder      . "/fastmail/Trash" )
                         ( mu4e-sent-folder       . "/fastmail/Sent" )
                         ( mu4e-drafts-folder     . "/fastmail/Drafts" )
                         ( mu4e-reply-to-address  . "ddavis@ddavis.io" )))))


(setq mu4e-bookmarks ())
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Unread short"
              :query "flag:unread and not flag:trashed and (m:/duke* or m:/cern* or m:/fastmail/INBOX or m:/gmail/INBOX*)"
              :key ?u))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Unread all"
              :query "flag:unread and not flag:trashed"
              :key ?U))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "INBOXes"
              :query "m:/duke/INBOX or m:/cern/INBOX or m:/fastmail/INBOX or m:/gmail/INBOX"
              :key ?i))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Last day's work"
              :query "date:1d..now and not m:/fastmail* and not m:/cern/Mailing\\ Lists/JEDI* and not m:/gmail*"
              :key ?w))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Recent work"
              :query "date:3d..now and not m:/fastmail* and not m:/cern/Mailing\\ Lists/JEDI* and not m:/gmail*"
              :key ?r))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Duke recent"
              :query "date:5d..now and m:/duke*"
              :key ?d))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "FastMail recent"
              :query "date:2d..now and m:/fastmail*"
              :key ?f))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "CERN recent"
              :query "date:2d..now and m:/cern*"
              :key ?c))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Emacs"
              :query "m:/fastmail/Lists/emacs-devel or m:/fastmail/Lists/help-gnu-emacs or m:/fastmail/Lists/emacs-orgmode"
              :key ?e))
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
              :key ?7))

(setq mu4e-compose-reply-ignore-address
      '("notifications@github\\.com"
        "ddavis@ddavis\\.io"
        "ddavis@phy\\.duke\\.edu"
        "douglas\\.davis\\.092@gmail\\.com"
        "douglas\\.davis@duke\\.edu"
        "ddavis@cern\\.ch"))

(setq w3m-default-desplay-inline-images t)

(defun mu4e-action-view-in-w3m ()
  "View the body of the message in emacs w3m."
  (interactive)
  (w3m-browse-url (concat "file://"
                          (mu4e~write-body-to-html (mu4e-message-at-point t)))))
(define-key mu4e-view-mode-map (kbd "M") 'mu4e-action-view-in-w3m)

(defun ddavis/mu4e-toggle-gnus ()
  (interactive)
  (setq mu4e-view-use-gnus (not mu4e-view-use-gnus)))

(define-key mu4e-headers-mode-map (kbd "C-c g") 'ddavis/mu4e-toggle-gnus)


;; (when ddavis-v-enable-mu4e
;;   (use-package visual-fill-column
;;     :ensure t
;;     :after mu4e
;;     :hook ((visual-line-mode . visual-fill-column-mode)
;;            (mu4e-view-mode . visual-line-mode))
;;     :config
;;     (add-hook 'mu4e-view-mode-hook
;;               (lambda () (setq-local fill-column 98))))

;; (use-package mu4e-maildirs-extension
;;   :ensure t
;;   :after mu4e
;;   :config
;;   (mu4e-maildirs-extension)))


(provide 'ddavis-mu4e)
;;; ddavis-mu4e.el ends here
