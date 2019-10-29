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

;;

;;; Code:

(require 'epa-file)
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
  (enable-circe-color-nicks))


(provide 'ddavis-irc)
;;; ddavis-irc.el ends here
