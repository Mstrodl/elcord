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


(defvar elcord-client_id "373861544456486913")
(defun elcord-on-connect (process evnt)
  "Debug function used to log connection state change.
Argument PROCESS Process the event occured on.
Argument EVNT The reason the connection state changed."
  (if (string= evnt "connection broken by remote peer\n")
      (elcord-handle-disconnect)
    (message (prin1-to-string evnt)))
  )

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
  ; (message evnt)
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
  (process-send-string elcord-sock elcord-packet)
  )

(defvar elcord-activity '())
(defvar elcord-nonce "")
(defvar elcord-presence '())
(defvar elcord-pid (emacs-pid))
(defvar elcord-join-and-spectate t)

(defun elcord-setpresence (filename line-num line-count)
  "Set presence.
Argument FILENAME Name of current buffer.
Argument LINE-NUM Line number the pointer is located at.
Argument LINE-COUNT Total number of lines in buffer."
  (setq elcord-activity `(
                   ("assets" . (
                                ("large_image" . ,(if (boundp 'spacemacs-version) "spacemacs_icon" "emacs_icon"))
                                ("large_text" . "Use this!")
                                ("small_image" . "vim_small")
                                ("small_text" . "Not this!")))
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
  (elcord-send-packet 1 elcord-presence)
  )

(defvar elcord-discord-socket (concat (or (getenv "XDG_RUNTIME_DIR") (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp") "/discord-ipc-0"))

(when (eq system-type 'windows-nt)
    (setq elcord-discord-socket "\\\\?\\pipe\\discord-ipc-0"))

(defvar elcord-sock nil)
(defun elcord-connect ()
  "Connects to the Discord socket."
  (condition-case err
      (unless elcord-connected
        (progn
          (setq elcord-sock
                (make-network-process :name "elcord-sock"
                                      :remote elcord-discord-socket
                                      :sentinel 'elcord-on-connect
                                      :filter 'elcord-on-message))
          (set-process-query-on-exit-flag elcord-sock nil)
          (message "Sending Discord IPC handshake...")
          (elcord-send-packet 0 `(("v" . 1) ("client_id" . ,elcord-client_id)))
          (setq elcord-connected t)))
    (file-error (unless elcord-connected
        (progn
          (sleep-for 1)
          (elcord-connect))))))

(defvar elcord-last-known-position (count-lines (point-min) (point)))
(defvar elcord-last-known-buffer-name (buffer-name))
(defun elcord-command-hook ()
  "Check if we changed our current line..."
  (when (and (or (not (eq (count-lines (point-min) (point))
                        elcord-last-known-position))
               (not (string= (buffer-name) elcord-last-known-buffer-name))) elcord-connected)
      (progn
        (setq elcord-last-known-buffer-name (buffer-name))
        (setq elcord-last-known-position (count-lines (point-min) (point)))
        (elcord-create-presence))))
;; We have this hook which is called whenever like anything at all happens and we check if it changed the line#...
(add-hook 'post-command-hook 'elcord-command-hook)

(provide 'elcord)
;;; elcord.el ends here
