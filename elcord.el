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
(eval-when-compile (require 'subr-x))

(require 'json)
(require 'bindat)

(defgroup elcord nil
  "Options for elcord."
  :prefix "elcord-")

(defcustom elcord-client-id '"388338871475240965"
  "ID of elcord client."
  :type '(choice (const :tag "No ID" nil)
                 (string :tag "Use the specified ID")
                 (function :tag "Call the function with no args to get the ID."))
  :group 'elcord)

(defcustom elcord-mode-icon-alist '((c-mode . "c-mode_icon")
                                    (c++-mode . "cpp-mode_icon")
                                    (csharp-mode . "csharp-mode_icon")
                                    (comint-mode . "comint-mode_icon")
                                    (emacs-lisp-mode . "emacs_icon")
                                    (java-mode . "java-mode_icon")
                                    (lisp-mode . "lisp-mode_icon")
                                    (slime-repl-mode . "lisp-mode_icon")
                                    (sly-mrepl-mode . "lisp-mode_icon")
                                    (python-mode . "python-mode_icon"))
  "alist of major modes to icon names to have elcord use.
Note, these icon names must be available as 'small_image' in Discord."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defcustom elcord-mode-text-alist '((c-mode . "C  ")
                                    (c++-mode . "C++")
                                    (csharp-mode . "C#")
                                    (java-mode . "Java")
                                    (lisp-mode . "Common-Lisp")
                                    (slime-repl-mode . "SLIME-REPL")
                                    (sly-mrepl-mode . "Sly-REPL"))
  "alist of major modes to text labels to have elcord use."
  :type '(alist :key-type symbol :value-type string)
  :group 'elcord)

(defcustom elcord-display-buffer-details 't
  "When enabled, Discord status will display buffer name and line numbers:
  \"Editing <buffer-name>\"
  \"Line <line-number> (<line-number> of <line-count>)\"

Otherwise, it will display:
  \"Editing\"
  \"<elcord-mode-text>\"

The mode text is the same found by `elcord-mode-text-alist'"
  :type 'boolean
  :group 'elcord)

(define-minor-mode elcord-mode
  "Global minor mode for displaying Rich Pressence in Discord."
  nil nil nil
  :global t
  :group 'elcord
  :after-hook
  (progn
    (cond
     (elcord-mode
      (elcord--enable))
     (t
      (elcord--disable)))))

(defvar elcord--discord-ipc-pipe "discord-ipc-0")
(defvar elcord--update-presence-timer nil)
(defvar elcord--reconnect-timer nil)
(defvar elcord--sock nil)
(defvar elcord--last-known-position (count-lines (point-min) (point)))
(defvar elcord--last-known-buffer-name (buffer-name))

(when (eq system-type 'windows-nt)
  (defvar elcord--stdpipe-path (expand-file-name
                                "stdpipe.ps1"
                                (file-name-directory (file-truename load-file-name)))))

(defun elcord--make-process ()
  (cl-case system-type
    (windows-nt
     (make-process
      :name "*elcord-sock*"
      :command (list
                "PowerShell"
                "-NoProfile"
                "-ExecutionPolicy" "Bypass"
                "-Command" elcord--stdpipe-path "." elcord--discord-ipc-pipe)
      :connection-type nil
      :sentinel 'elcord--connection-sentinel
      :filter 'elcord--connection-filter
      :noquery nil))
    (t
     (make-network-process
      :name "*elcord-sock*"
      :remote (expand-file-name
               elcord--discord-ipc-pipe
               (file-name-as-directory
                (or (getenv "XDG_RUNTIME_DIR")
                    (getenv "TMPDIR")
                    (getenv "TMP")
                    (getenv "TEMP")
                    "/tmp")))
      :sentinel 'elcord--connection-sentinel
      :filter 'elcord--connection-filter
      :noquery nil))))

(defun elcord--enable ()
  (unless (elcord--resolve-client-id)
    (warn "elcord: no elcord-client-id available"))
  (when (eq system-type 'windows-nt)
    (unless (executable-find "powershell")
      (warn "elcord: powershell not available"))
    (unless (file-exists-p elcord--stdpipe-path)
      (warn "elcord: 'stdpipe' script does not exist (%s)" elcord--stdpipe-path)))

  ;;Start trying to connect
  (elcord--start-reconnect))

(defun elcord--disable ()
  ;;Cancel updates
  (elcord--cancel-updates)
  ;;Cancel any reconnect attempt
  (elcord--cancel-reconnect)
  (elcord--disconnect))

(defun elcord--resolve-client-id ()
  "Get the client ID to use for elcord by looking at
`elcord-client-id' and extracting its value."
  (cl-typecase elcord-client-id
    (nil
     nil)
    (string
     elcord-client-id)
    (function
     (funcall elcord-client-id))))

(defun elcord--connection-sentinel (process evnt)
  "Track connection state change on Discord connection."
  (cl-case (process-status process)
    ((closed exit)
     (elcord--handle-disconnect))
    (t)))

(defun elcord--connection-filter (process evnt)
  "Track incoming data from Discord connection."
  (elcord--start-updates))

(defun elcord--connect ()
  "Connects to the Discord socket."
  (condition-case nil
      (progn
        (setq elcord--sock (elcord--make-process))
        (condition-case nil
            (progn
              (elcord--send-packet 0 `(("v" . 1) ("client_id" . ,(elcord--resolve-client-id))))
              (setq success t))
          (error
           (delete-process elcord--sock)
           (setq elcord--sock nil)))
        t)
    (error
     nil)))

(defun elcord--disconnect ()
  (when elcord--sock
    (delete-process elcord--sock)
    (setq elcord--sock nil)))

(defun elcord--reconnect ()
  "Attempt to reconnect elcord."
  (message "elcord: attempting reconnect..")
  (when (elcord--connect)
    ;;Reconnected.
    (message "elcord: conecting...")
    (elcord--cancel-reconnect)))

(defun elcord--start-reconnect ()
  "Starts attempting to reconnect"
  (unless elcord--reconnect-timer
    (setq elcord--reconnect-timer (run-at-time 0 15 'elcord--reconnect))))

(defun elcord--cancel-reconnect ()
  "Starts attempting to reconnect"
  (when elcord--reconnect-timer
    (cancel-timer elcord--reconnect-timer)
    (setq elcord--reconnect-timer nil)))

(defun elcord--handle-disconnect ()
  "Handles reconnecting when socket disconnects..."
  (message "elcord: disconnected")
  ;;Stop updating presence for now
  (elcord--cancel-updates)
  ;;Start trying to reconnect
  (elcord--start-reconnect))

(defun elcord--send-packet (opcode obj)
  "Packs and sends a packet to the IPC server.
Argument OPCODE OP code to send.
Argument OBJ The data to send to the IPC server."
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
    (process-send-string elcord--sock packet)))

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

(defun elcord--details-and-state ()
  "Obtain the details and state to use for Discord's Rich Presence."
  (if elcord-display-buffer-details
      (list
       (cons "details" (format "Editing %s" (buffer-name)))
       (cons "state" (format "Line %s (%s of %S)"
                             (format-mode-line "%l")
                             (format-mode-line "%l")
                             (+ 1 (count-lines (point-min) (point-max))))))
    (list
     (cons "details" "Editing")
     (cons "state" (elcord--mode-text)))))

(defun elcord--set-presence ()
  "Set presence."
  (let* ((activity
          `(("assets" . (("large_image" . "emacs_icon")
                         ("large_text" . "Emacs")
                         ,@(elcord--mode-icon-and-text)))
            ,@(elcord--details-and-state)
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
  (when (or (not (eq (count-lines (point-min) (point))
                     elcord--last-known-position))
            (not (string= (buffer-name) elcord--last-known-buffer-name)))
    (progn
      (setq elcord--last-known-buffer-name (buffer-name))
      (setq elcord--last-known-position (count-lines (point-min) (point)))
      (condition-case nil
          ;;Try and set the presence
          (elcord--set-presence)
        (error
         ;;If we hit an error, cancel updates
         (elcord--cancel-updates)
         ;; and try reconnecting
         (elcord--start-reconnect))))))

(defun elcord--start-updates ()
  (unless elcord--update-presence-timer
    (message "elcord: connected. starting updates")
    ;;Start sending updates now that we've heard from discord
    (setq elcord--update-presence-timer (run-at-time 0 15 'elcord--update-presence))))

(defun elcord--cancel-updates ()
  (when elcord--update-presence-timer
    (cancel-timer elcord--update-presence-timer)
    (setq elcord--update-presence-timer nil)))

(provide 'elcord)
;;; elcord.el ends here
