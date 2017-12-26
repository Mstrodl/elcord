;;; elcord.el --- Allows you to integrate Rich Presence from Discord

;;; Copyright 2017 heatingdevice

;;; Version: 0.0.2
;;; Author: heatingdevice
;;; URL: https://github.com/mstrodl/elcord
;;; License: MIT

;;; Commentary:
;; elcord allows you to show off your buffer with all your Discord friends via the new rich presence feature

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'json)
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

(defcustom elcord-mode-icon-alist '((emacs-lisp-mode . "emacs_icon"))
  "alist of major modes to icon names to have elcord use.
Note, these icon names must be available as 'small_image' in Discord."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defcustom elcord-mode-text-alist '()
  "alist of major modes to text labels to have elcord use."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defcustom elcord-auto-reconnect 't
  "When enabled, elcord will automatically reconnect
if the connection to Discord is lost."
  :type 'boolean
  :group 'elcord)

(defun elcord-connect ()
  "Connects to the Discord socket."
  (interactive)
  (unless (or elcord--connected
              (null (elcord--resolve-client-id))
              (and (eq system-type 'windows-nt)
                   (or
                    (not (executable-find "powershell"))
                    (not (file-exists-p elcord--stdpipe-path)))))
    (setq elcord--sock
          (if (eq system-type 'windows-nt)
              (make-process
               :name "*elcord-sock*"
               :command (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" elcord--stdpipe-path "." elcord--discord-ipc-pipe)
               :connection-type nil
               :sentinel 'elcord--connection-sentinel
               :filter 'elcord--connection-filter)
            (make-network-process
             :name "*elcord-sock*"
             :remote (concat (or (getenv "XDG_RUNTIME_DIR")
                                 (getenv "TMPDIR")
                                 (getenv "TMP")
                                 (getenv "TEMP")
                                 "/tmp")
                             "/" elcord--discord-ipc-pipe)
             :sentinel 'elcord--connection-sentinel
             :filter 'elcord--connection-filter)))
    (set-process-query-on-exit-flag elcord--sock nil)
    (elcord--send-packet 0 `(("v" . 1) ("client_id" . ,(elcord--resolve-client-id))))
    (setq elcord--connected t)
    (setq elcord--elcord-update-presence-timer (run-at-time 0 15 'elcord--update-presence)))
  elcord--connected)

(defun elcord-disconnect ()
  (interactive)
  (when elcord--connected
    (progn
      (when elcord--elcord-update-presence-timer
        (cancel-timer elcord--elcord-update-presence-timer)
        (setq elcord--elcord-update-presence-timer nil))
      (setq elcord--first-message nil)
      (setq elcord--connected nil)
      (delete-process elcord--sock)
      (setq elcord--sock nil)))
  (when elcord--reconnect-timer
    (cancel-timer elcord--reconnect-timer)
    (setq elcord--reconnect-timer nil))
  (not elcord--connected))

(defvar elcord--discord-ipc-pipe "discord-ipc-0")
(defvar elcord--elcord-update-presence-timer nil)
(defvar elcord--reconnect-timer nil)
(defvar elcord--sock nil)
(defvar elcord--connected nil)
(defvar elcord--first-message nil)
(defvar elcord--last-known-position (count-lines (point-min) (point)))
(defvar elcord--last-known-buffer-name (buffer-name))

(when (eq system-type 'windows-nt)
  (defvar elcord--stdpipe-path (expand-file-name
                                "stdpipe.ps1"
                                (file-name-directory (file-truename load-file-name)))))

(defun elcord--resolve-client-id ()
  "Get the client ID to use for elcord by looking at
`elcord-client-id' and extracting its value."
  (typecase elcord-client-id
    (nil
     nil)
    (string
     elcord-client-id)
    (function
     (funcall elcord-client-id))))

(defun elcord--connection-sentinel (process evnt)
  "Track connection state change on Discord connection."
  (case (process-status process)
    (exit
     (elcord--handle-disconnect))
    (t)))

(defun elcord--connection-filter (process evnt)
  "Track incoming data from Discord connection."
  (unless elcord--first-message
    (progn
      (elcord--create-presence)
      (setq elcord--first-message t))))

(defun elcord--reconnect ()
  "Attempt to reconnect elcord."
  (when elcord-auto-reconnect
    (unless (elcord-connect)
      (setq elcord--reconnect-timer (run-at-time 15 nil 'elcord--reconnect)))))

(defun elcord--handle-disconnect ()
  "Handles reconnecting when socket disconnects..."
  (setq elcord--connected nil)
  (setq elcord--first-message nil)
  (elcord--reconnect))

(defun elcord--create-presence ()
  "Create a new status and set it."
  (elcord--setpresence (buffer-name) (+ 1 (count-lines (point-min) (point))) (+ 1 (count-lines (point-min) (point-max)))))

(defun elcord--send-packet (opcode obj)
  "Packs and sends a packet to the IPC server.
Argument OPCODE OP code to send.
Argument OBJ The data to send to the IPC server."
  (when (process-live-p elcord--sock)
    (let* ((jsonstr (json-encode obj))
           (datalen (length jsonstr))
           (message-spec
            `((:op u32r)
              (:len u32r)
              (:data str ,datalen)))
           (packet
            (bindat-pack
             message-spec
             `((:op . ,opcode)
               (:len . ,datalen)
               (:data . ,jsonstr)))))
      (process-send-string elcord--sock packet))))

(defun elcord--mode-icon ()
  "Figure out what icon to use for the current major mode.
If an icon is mapped by `elcord-mode-icon-alist', then that is used.
Otherwise, if the mode is a derived mode, try to find an icon for it.
If no icon is available, use the default icon."
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
  "Figure out what text to use for the current major mode.
If an icon is mapped by `elcord-mode-text-alist', then that is used.
Otherwise, if the mode is a derived mode, try to find text for its parent,
If no text is available, use the value of `mode-name'."
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
  "Obtain the icon & text to use for the small icon, using current major mode.
Get either a list
  ((\"small_text\" . <text>) (\"small_image\" . <icon-name>))
or nil, if no text/icon are available for the current major mode."
  (if-let ((icon (elcord--mode-icon))
           (text (elcord--mode-text)))
      (list
       (cons "small_text" text)
       (cons "small_image" icon))
    nil))

(defun elcord--setpresence (filename line-num line-count)
  "Set presence.
Argument FILENAME Name of current buffer.
Argument LINE-NUM Line number the pointer is located at.
Argument LINE-COUNT Total number of lines in buffer."
  (let* ((activity
          `(("assets" . (("large_image" . "emacs_icon")
                         ("large_text" . "Emacs")
                         ,@(elcord--mode-icon-and-text)))
            ("details" . ,(concat "Editing " filename))
            ("state" . ,(concat "Line " (number-to-string line-num)))
            ("party" . (("id" . "theonlyeditor")
                        ("size" . [,line-num ,line-count])))
            ("secrets" . (("match" . "emacsisbest")))))
         (nonce (format-time-string "%s%N"))
         (presence
           `(("cmd" . "SET_ACTIVITY")
             ("args" . (("activity" . ,activity)
                        ("pid" . ,(emacs-pid))))
             ("nonce" . ,nonce))))
    (elcord--send-packet 1 presence)))

(defun elcord--update-presence ()
  "Check if we changed our current line..."
  (when (and (or (not (eq (count-lines (point-min) (point))
                          elcord--last-known-position))
                 (not (string= (buffer-name) elcord--last-known-buffer-name)))
             elcord--connected)
    (progn
      (setq elcord--last-known-buffer-name (buffer-name))
      (setq elcord--last-known-position (count-lines (point-min) (point)))
      (elcord--create-presence))))

(provide 'elcord)
;;; elcord.el ends here
