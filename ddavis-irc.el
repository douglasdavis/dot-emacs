;;; ddavis-irc.el --- irc configuration              -*- lexical-binding: t; -*-

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

;; IRC setup -- see: https://github.com/jorgenschaefer/circe

;;; Code:

(require 'epa-file)
(require 'erc)
(require 'use-package)
(require 'ddavis-vars)


(if ddavis-v-is-mac
    (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
  (custom-set-variables '(epg-gpg-program "/usr/bin/gpg2")))

(use-package password-store
  :when ddavis-v-enable-irc
  :ensure t
  :config
  (require 'password-store))

(defun ddavis/irc-pw-freenode (server)
  (password-store-get "Freenode"))

(defun ddavis/irc-pw-gitter (server)
  (password-store-get "Gitter"))

(use-package circe
  :when ddavis-v-enable-irc
  :ensure t
  :config
  (setq circe-reduce-lurker-spam t
        circe-network-options
        `(("Freenode"
           :nick "ddavis"
           :nickserv-password ddavis/irc-pw-freenode
           :tls t
           :channels (:after-auth "#emacs" "#sr.ht" "#python" "#pydata" "#scipy")
           )
          ("Gitter"
           :server-buffer-name "Gitter"
           :host "irc.gitter.im"
           :port "6697"
           :nick "douglasdavis"
           :pass ddavis/irc-pw-gitter
           :tls t
           )
          ))
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-use-cycle-completion t
        circe-format-say "<{nick}> {body}")
  ;; (setq lui-fill-column 79
  ;;       lui-fill-type 18)
  (setq helm-mode-no-completion-in-region-in-modes
        '(circe-channel-mode
          circe-query-mode
          circe-server-mode)))

(defun ddavis/circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) " >>> ")
                       'face 'circe-prompt-face)
           " ")))
(add-hook 'circe-chat-mode-hook 'ddavis/circe-prompt)

;; ERC nick color from EmacsWiki
(setq nick-face-list '())
(setq-default erc-colors-list '("blue" "green" "yellow"
                                "gray" "brown" "red"
                                "purple" "white" "cyan"))
(defun build-nick-face-list ()
  (setq i -1)
  (setq nick-face-list
        (mapcar
         (lambda (COLOR)
           (setq i (1+ i))
           (list (custom-declare-face
                  (make-symbol (format "erc-nick-face-%d" i))
                  (list (list t (list :foreground COLOR)))
                  (format "Nick face %d" i))))
         erc-colors-list)))

(defun my-insert-modify-hook ()
  (if (null nick-face-list) (build-nick-face-list))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
        (let ((nick (match-string 1)))
          (put-text-property (match-beginning 1) (match-end 1)
                             'face (nth
                                    (mod (string-to-number
                                          (substring (md5 nick) 0 4) 16)
                                         (length nick-face-list))
                                    nick-face-list))))))
(add-hook 'erc-insert-modify-hook 'my-insert-modify-hook)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))


(provide 'ddavis-irc)
;;; ddavis-irc.el ends here
