;;; plover-websocket.el --- Interact with Plover through a websocket   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: plover steno stenography
;; Version: 0.10
;; Homepage: https://github.com/sachac/plover-websocket-el
;; Package-Requires: ((emacs "26.1") (websocket))

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
;; You will also need to install:
;; https://github.com/openstenoproject/plover
;; https://github.com/user202729/plover_websocket_server
;;
;; Follow the instructions at plover_websocket_server in order to
;; install the plugin, and then enable it from Plover > Configure > Plugins.
;;
;; When I installed it, there was an error about
;; ModuleNotFoundError: No module named 'plover_engine_server.websocket'
;;
;; I got around it by installing the plugin from source:
;; git clone https://github.com/user202729/plover_websocket_server
;; cd plover_websocket_server
;; plover -s plover_plugins install -e .
;;
;; Connect with M-x plover-websocket-connect
;; 
;;; Code:

(defvar plover-websocket-url "ws://localhost:8086/websocket" "Plover websocket URL.")
(defvar plover-websocket nil "Plover websocket connection.")
(defvar plover-websocket-debug nil "Debug messages")
(defvar plover-websocket-on-message-payload-functions nil "Functions to call when messages arrive.")
(defvar plover-websocket-messages nil "Messages from Plover.")
(defvar plover-websocket-plover-command "plover" "Command to run Plover.")
(defvar plover-websocket-zero-last-stroke-length t "Set to t if using 'keyboard' as the machine for Plover.")

(defun plover-websocket-on-message (_ frame)
  "Handle Plover websocket sending FRAME."
  (let* ((payload (json-parse-string (websocket-frame-payload frame) :object-type 'plist :array-type 'list)))
    (when plover-websocket-debug
      (setq plover-websocket-messages (cons frame plover-websocket-messages)))
    (run-hook-with-args 'plover-websocket-on-message-payload-functions payload)))

(defun plover-websocket-on-close (&rest args)
  "Display a message when the connection has closed."
  (setq plover-websocket nil)
  (message "Plover connection closed."))

(defun plover-websocket-connect (&optional url)
  "Connect to a Plover instance."
  (interactive (list (or plover-websocket-url (read-string "URL: "))))
  (if (websocket-openp plover-websocket)
      (message "Already connected to Plover.")
    (setq plover-websocket (websocket-open (or url plover-websocket-url)
                                           :on-message #'plover-websocket-on-message
                                           :on-close #'plover-websocket-on-close))))
(defun plover-websocket-disconnect ()
  "Disconnect from an OBS instance."
  (interactive)
  (when plover-websocket (websocket-close plover-websocket)))

(defun plover-websocket-send (&rest args)
  "Send a message of type REQUEST-TYPE."
  (let ((msg (json-encode-plist args)))
    (websocket-send-text plover-websocket msg)
    (when plover-websocket-debug (prin1 msg))))

(defun plover-websocket-start ()
  "Start the Plover executable."
  (interactive)
  (start-process-shell-command plover-websocket-plover-command))

(defmacro defplover (command docstring &rest args)
  `(defun ,command ()
     ,docstring
     (interactive)
     (apply 'plover-websocket-send
            :zero_last_stroke_length plover-websocket-zero-last-stroke-length
            (quote ,args))))
;; test

(defplover plover-websocket-toggle-plover "Toggle Plover." :translation "{PLOVER:TOGGLE}")
(defplover plover-websocket-suspend-plover "Suspend Plover." :translation "{PLOVER:SUSPEND}")
(defplover plover-websocket-resume-plover "Resume Plover." :translation "{PLOVER:RESUME}")
(defplover plover-websocket-add-translation "Add translation using the Plover interface." :translation "{PLOVER:ADD_TRANSLATION}")
(defplover plover-websocket-lookup "Look up outline using the Plover interface." :translation "{PLOVER:LOOKUP}")
(defplover plover-websocket-configure "Configure Plover." :translation "{PLOVER:CONFIGURE}")
(defplover plover-websocket-focus "Focus Plover." :translation "{PLOVER:FOCUS}")
(defplover plover-websocket-quit "Quit Plover." :translation "{PLOVER:QUIT}")

(provide 'plover-websocket)
;;; plover-websocket.el ends here
