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
;; Set your plover-websocket-password with M-x custom-variable or (setq ...)
;; Connect with M-x plover-websocket-connect
;; 
;; 
;; See https://github.com/sachac/plover-websocket-el for notes and updates.
;; 
;;; Code:

(defvar plover-websocket-url "ws://localhost:8086/websocket" "Plover websocket URL.")
(defvar plover-websocket nil "Plover websocket connection.")
(defgroup plover-websocket nil "Plover websocket")
(defcustom plover-websocket-password nil "Password/secret key. See plover_websocket_server's README.md for configuration instructions."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Password"))
  :group 'plover-websocket)
(defcustom plover-websocket-debug nil "If non-nil, capture some debugging info."
  :type '(choice (const :tag "On" t)
                 (const :tag "Off" nil)))
(defcustom plover-websocket-stroke-buffer-name nil "If non-nil, add stroke information to the named buffer."
  :type '(choice (const :tag "ff" t)
                 (string :tag "Buffer name")))

(defvar plover-websocket-on-message-payload-functions '(plover-websocket-display-lookups) "Functions to call when messages arrive.")
(defvar plover-websocket-messages nil "Messages from Plover.")
(defvar plover-websocket-plover-command "plover" "Command to run Plover.")
(defvar plover-websocket-zero-last-stroke-length t "Set to t if using 'keyboard' as the machine for Plover.")
(defvar plover-websocket-message-callback-once-functions nil "List of callbacks that will be removed after they return non-nil.")
(defun plover-websocket-log-stroke (payload)
	"Add stroke info from PAYLOAD to the end of `plover-websocket-stroke-buffer-name`."
	(when (and plover-websocket-stroke-buffer-name (get-buffer-create plover-websocket-stroke-buffer-name))
		(let (last-text
					point)
			(when (plist-get payload :stroked)
				(with-current-buffer (get-buffer plover-websocket-stroke-buffer-name)
					(setq point (point))
					(goto-char (point-max))
					(setq last-text
								(mapconcat (lambda (o) (plist-get o :text))
													 (plist-get (plist-get plover-websocket-last-translation-payload :translated) :new) ""))
					(when (or (plist-get payload :rtfcre)
										(plist-get
										 (plist-get payload :stroked) :rtfcre))
						(insert
						 (or (plist-get payload :rtfcre)
								 (plist-get
									(plist-get payload :stroked) :rtfcre))
						 " "))
					(when (and last-text (string-match "^\n+$" last-text)) (insert last-text))
					(when (get-buffer-window)
						(with-selected-window (get-buffer-window)
							(goto-char (point-max)))))))))

(defvar plover-websocket-last-translation-payload nil "Last translation.")

(defun plover-websocket-on-message (_ frame)
  "Handle Plover websocket sending FRAME."
  (let* ((payload (let ((json-object-type 'plist)
                        (json-array-type 'list))
                    (json-read-from-string (websocket-frame-payload frame)))))
    (when plover-websocket-debug
      (setq plover-websocket-messages (cons frame plover-websocket-messages)))
    (when (plist-get payload :translated)
      (setq plover-websocket-last-translation-payload payload))
    (when plover-websocket-stroke-buffer-name
      (plover-websocket-log-stroke payload))
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
  (unless (websocket-openp plover-websocket) (plover-websocket-connect))
  (let ((msg (json-encode-plist (append args (list :secretkey plover-websocket-password)))))
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

(defplover plover-websocket-toggle-plover "Toggle Plover." :translation "{PLOVER:TOGGLE}")
(defplover plover-websocket-suspend-plover "Suspend Plover." :translation "{PLOVER:SUSPEND}")
(defplover plover-websocket-resume-plover "Resume Plover." :translation "{PLOVER:RESUME}")
(defplover plover-websocket-add-translation-with-interface "Add translation using the Plover interface. :translation {PLOVER:ADD_TRANSLATION}")
(defplover plover-websocket-lookup-with-interface "Look up outline using the Plover interface." :translation "{PLOVER:LOOKUP}")
(defplover plover-websocket-configure "Configure Plover." :translation "{PLOVER:CONFIGURE}")
(defplover plover-websocket-focus "Focus Plover." :translation "{PLOVER:FOCUS}")
(defplover plover-websocket-quit "Quit Plover." :translation "{PLOVER:QUIT}")

(defmacro with-plover-always (&rest body)
  `(progn
     (plover-websocket-send :translation "{PLOVER:ALWAYS:START}" :zero_last_stroke_length t)
     (prog1 (progn ,@body))
     (plover-websocket-send :translation "{PLOVER:ALWAYS:END}")))

(defmacro with-plover-never (&rest body)
  `(progn
     (plover-websocket-send :translation "{PLOVER:NEVER:START}" :zero_last_stroke_length t)
     (prog1 (progn ,@body)
       (plover-websocket-send :translation "{PLOVER:NEVER:END}" :zero_last_stroke_length t))))

(defmacro with-plover-plain (&rest body)
  `(unwind-protect
       (progn
         (plover-websocket-send :translation "{PLOVER:SOLO_DICT:+commands.json}")
         ,@body)
     (plover-websocket-send :translation "{PLOVER:END_SOLO_DICT}")))

(defun plover-websocket-add-translation (key translation &optional dictionary)
  "Add KEY and TRANSLATION in Plover's default dictionary.
KEY should be a steno string (ex: SKP-B)."
  (interactive (list (with-plover-plain (read-string "Key (ex: SKP-B): ")) (with-plover-never (read-string "Translation: "))))
  (plover-websocket-send :add_translation `(:key ,key :translation ,translation :dictionary ,dictionary)))

(defun plover-websocket-display-lookups (payload)
  (when-let ((result (plist-get payload :look_up_result)))
    (message (mapconcat (lambda (group)
                          (format "Suggestions for %s: %s"
                                  (plist-get group :text)
                                  (mapconcat (lambda (suggestion) (string-join suggestion "/"))
                                             (plist-get group :steno_list)
                                             "; ")))
                        result
                        ";;")))
  (when-let ((result (plist-get payload :get_translation_result)))
    (message "Plover: %s -> %s"
             (string-join (plist-get result :key) "/")
             (mapconcat (lambda (entry) (format "%s (%s)" (car entry) (file-name-nondirectory (cadr entry))))
                        (plist-get result :result)           
                        "; "))))

;; TODO: Figure out how to add callbacks
(defun plover-websocket-look-up (translation)
  "Show the possible strokes that result in TRANSLATION."
  (interactive (list (with-plover-never (read-string "Translation: "))))
  (plover-websocket-send :look_up translation))

(defun plover-websocket-get-translation (strokes)
  "Show the translation for STROKES.
STROKES should be a string."
  (interactive (list (replace-regexp-in-string " " "/" (string-trim (with-plover-plain (read-string "Strokes: "))))))
  (plover-websocket-send :get_translation strokes :forced t))

;; (plover-websocket-look-up "and" (lambda (result) (pp result)))

(provide 'plover-websocket)
;;; plover-websocket.el ends here
