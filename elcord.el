;;; elcord.el --- Allows you to integrate Rich Presence from Discord

;;; Copyright 2017 heatingdevice

;;; Version: 0.0.2
;;; Author: heatingdevice
;;; URL: https://github.com/mstrodl/elcord
;;; License: MIT

;;; Commentary:
;; elcord allows you to show off your buffer with all your Discord friends via the new rich presence feature

;;; Code:

(require 'json)
(eval-when-compile (require 'cl-lib))
(require 'bindat)

(defgroup elcord nil
  "Options for elcord."
  :prefix "elcord-")

(defcustom elcord-client-id 'nil
  "ID of elcord client."
  :type '(choice (const :tag "No ID" nil)
                 (string :length :tag "Use the specified ID")
                 (function :tag "Call the function with no args to get the ID."))
  :group 'elcord)

(defun elcord-on-connect (process evnt)
  "Debug function used to log connection state change.
Argument PROCESS Process the event occured on.
Argument EVNT The reason the connection state changed."
  (case (process-status process)
    (exit
     (elcord-handle-disconnect))
    (t
     (message (prin1-to-string evnt)))))

(defvar elcord-connected nil)

(defun elcord-create-presence ()
  "Create a new status and set it."
  (elcord-setpresence (buffer-name) (+ 1 (count-lines (point-min) (point))) (+ 1 (count-lines (point-min) (point-max)))))

(defun elcord-handle-disconnect ()
  "Handles reconnecting when socket disconnects..."
  (setq elcord-connected nil)
  (setq elcord-first-message nil)
  (elcord-connect))

(defvar elcord-first-message nil)

(defun elcord-on-message (process evnt)
  "Debug function used to output all incoming packets.
Argument PROCESS The process the message was recieved on.
Argument EVNT The message recieved on the socket."
  (unless elcord-first-message
    (progn
      (elcord-create-presence)
      (setq elcord-first-message t))))

(defvar elcord-jsonstr "")
(defvar elcord-datalen 0)
(defvar elcord-message-spec '())
(defvar elcord-packet '())

(defun elcord-send-packet (opcode obj)
  "Packs and sends a packet to the IPC server.
Argument OPCODE OP code to send.
Argument OBJ The data to send to the IPC server."
  (setq elcord-jsonstr (json-encode obj))
  (setq elcord-datalen (length elcord-jsonstr))
  (setq elcord-message-spec
        `((:op u32r)
          (:len u32r)
          (:data str ,elcord-datalen)))
  (setq elcord-packet (bindat-pack
                       elcord-message-spec
                       `((:op . ,opcode)
                         (:len . ,elcord-datalen)
                         (:data . ,elcord-jsonstr))))
  (process-send-string elcord-sock elcord-packet))

(defvar elcord-activity '())
(defvar elcord-nonce "")
(defvar elcord-presence '())
(defvar elcord-pid (emacs-pid))
(defvar elcord-join-and-spectate t)

(defcustom elcord-mode-icon-alist '((emacs-lisp-mode . "emacs_icon"))
  "alist of major modes to icon names to have elcord use.
Note, these icon names must be available as 'small_image' in Discord."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defcustom elcord-mode-text-alist '()
  "alist of major modes to text labels to have elcord use."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defun elcord--mode-icon ()
  (let ((mode major-mode)
        (ret "emacs_icon"))
    (while mode
      (if-let ((icon (assoc mode elcord-mode-icon-alist)))
          (progn
            (setq ret (cdr icon))
            (setq mode nil))
        (setq mode (get mode 'derived-mode-parent))))

    ret))

(defun elcord--mode-text ()
  (let ((mode major-mode)
        (ret mode-name))
    (while mode
      (if-let ((text (assoc mode elcord-mode-text-alist)))
          (progn
            (setq ret (cdr text))
            (setq mode nil))
        (setq mode (get mode 'derived-mode-parent))))

    ret))

(defun elcord--mode-icon-and-text ()
  (if-let ((icon (elcord--mode-icon))
           (text (elcord--mode-text)))
      (list
       (cons "small_text" text)
       (cons "small_image" icon))
    nil))

(defun elcord-setpresence (filename line-num line-count)
  "Set presence.
Argument FILENAME Name of current buffer.
Argument LINE-NUM Line number the pointer is located at.
Argument LINE-COUNT Total number of lines in buffer."
  (setq elcord-activity `(
                          ("assets" . (
                                       ("large_image" . "emacs_icon")
                                       ("large_text" . "Emacs")
                                       ,@ (elcord--mode-icon-and-text)))
                          ("details" . ,(concat "Editing " filename))
                          ("state" . ,(concat "Line " (number-to-string line-num)))
                          ("party" . (
                                      ("id" . "theonlyeditor")
                                      ("size" . [,line-num ,line-count])
                                      ))
                          ("secrets" . ,(if elcord-join-and-spectate '(
                                                                       ("join" . "yesuseemacswithmepls")
                                                                       ("match" . "emacsisbest")
                                                                       ("spectate" . "stupidvimuseruseemacs"))
                                          '(("match" . "emacsisbest"))))
                          ))
  (setq elcord-nonce (format-time-string "%s%N"))
  (setq elcord-presence `(
                          ("cmd" . "SET_ACTIVITY")
                          ("args" . (("activity" . ,elcord-activity)
                                     ("pid" . ,elcord-pid)))
                          ("nonce" . ,elcord-nonce)
                          ))
  (elcord-send-packet 1 elcord-presence))

(defvar elcord-discord-socket (concat (or (getenv "XDG_RUNTIME_DIR") (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp") "/discord-ipc-0"))

(defvar elcord-timer nil)
(defvar elcord-sock nil)
(defun elcord--resolve-client-id ()
  (typecase elcord-client-id
    (nil
     nil)
    (string
     elcord-client-id)
    (function
     (funcall elcord-client-id))))

(defun elcord-connect ()
  "Connects to the Discord socket."
  (condition-case err
      (unless (or elcord-connected
                  (null elcord-client-id))
        (setq elcord-sock
              (if (eq system-type 'windows-nt)
                  (make-process
                   :name "elcord-sock"
                   :command (list "pipeman.exe" "discord-ipc-0")
                   :connection-type nil
                   :sentinel 'elcord-on-connect
                   :filter 'elcord-on-message)
                (make-network-process
                 :name "elcord-sock"
                 :remote elcord-discord-socket
                 :sentinel 'elcord-on-connect
                 :filter 'elcord-on-message)))
        (set-process-query-on-exit-flag elcord-sock nil)
        (message "Sending Discord IPC handshake...")
        (elcord-send-packet 0 `(("v" . 1) ("client_id" . ,(elcord--resolve-client-id))))
        (setq elcord-connected t)
        (setq elcord-timer (run-at-time 0 15 'elcord-timer-hook))
        elcord-connected)
    (file-error
     (unless elcord-connected
       (sleep-for 1)
       (elcord-connect)))))

(defun elcord-disconnect ()
  (when elcord-connected
    (progn
      (cancel-timer elcord-timer)
      (setq elcord-timer nil)

      (setq elcord-connected nil)
      (delete-process elcord-sock)
      (setq elcord-sock nil))))

(defvar elcord-last-known-position (count-lines (point-min) (point)))
(defvar elcord-last-known-buffer-name (buffer-name))

(defun elcord-update-precense ()
  "Check if we changed our current line..."
  (when (and (or (not (eq (count-lines (point-min) (point))
                          elcord-last-known-position))
                 (not (string= (buffer-name) elcord-last-known-buffer-name)))
             elcord-connected)
    (progn
      (setq elcord-last-known-buffer-name (buffer-name))
      (setq elcord-last-known-position (count-lines (point-min) (point)))
      (elcord-create-presence))))

(defun elcord-timer-hook ()
  (elcord-update-precense))

(provide 'elcord)
;;; elcord.el ends here