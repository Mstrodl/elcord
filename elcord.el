;;; elcord.el --- Allows you to integrate Rich Presence from Discord

;; Copyright (C) 2017 heatingdevice

;; Author: heatingdevice
;;      Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;; Created: 21 Nov 2017
;; Version: 1.1.0
;; Keywords: games
;; Homepage: https://github.com/Mstrodl/elcord
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:
;; elcord allows you to show off your buffer with all your Discord friends via the new rich presence feature
;; To use, enable the global minor mode `elcord-mode'
;; When enabled, elcord will communicate with the local Discord client in order to display information under
;; the 'Playing a Game' status, updating this information at a regular interval.
;;
;; elcord will display an Emacs title, icon, as well as information about your current buffer:
;; 1) The name and an icon (if available) for the current major mode
;; 2) The name of the current buffer
;; 3) The line number of the cursor, as well as total line count for the buffer
;; `elcord-display-buffer-details' can be customized so that buffer name and line number are omitted.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup elcord nil
  "Options for elcord."
  :prefix "elcord-"
  :group 'external)

(defcustom elcord-client-id '"388338871475240965"
  "ID of elcord client (Application ID).
See <https://discordapp.com/developers/applications/me>."
  :type '(choice (const :tag "'Native' Application ID" "388338871475240965")
                 (string :tag "Use the specified ID")
                 (function :tag "Call the function with no args to get the ID."))
  :group 'elcord)

(defcustom elcord-icon-base
  '"https://raw.githubusercontent.com/Mstrodl/elcord/master/icons/"
  "Base URL for icon images. Mode icons will be loaded from this URL + the icon name + '.png'"
  :type '(choice (const :tag "Elcord GitHub Repository"
                        "https://raw.githubusercontent.com/Mstrodl/elcord/master/icons/")
                 (string :tag "Use the specified URL base")
                 (function :tag "Call the function with icon name as an arg to get the URL base."))
  :group 'elcord)

(defcustom elcord-refresh-rate 15
  "How often to send updates to Discord, in seconds."
  :type 'integer
  :group 'elcord)

(defcustom elcord-idle-timer 300
  "How long to wait before setting the status to idle."
  :type 'integer
  :group 'elcord)

(defcustom elcord-idle-message "Getting something to drink..."
  "Message to show when elcord status is idle."
  :type 'string)

(defcustom elcord-quiet 'nil
  "Whether or not to supress elcord messages (connecting, disconnecting, etc.)"
  :type 'boolean
  :group 'elcord)

(defcustom elcord-mode-icon-alist '((agda-mode . "agda-mode_icon")
                                    (assembly-mode . "assembly-mode_icon")
                                    (c-mode . "c-mode_icon")
                                    (c++-mode . "cpp-mode_icon")
                                    (clojure-mode . "clojure-mode_icon")
                                    (csharp-mode . "csharp-mode_icon")
                                    (comint-mode . "comint-mode_icon")
                                    (cperl-mode . "cperl-mode_icon")
                                    (elixir-mode . "elixir-mode_icon")
                                    (emacs-lisp-mode . (elcord--editor-icon))
                                    (enh-ruby-mode . "ruby-mode_icon")
                                    (erc-mode . "irc-mode_icon")
                                    (erlang-mode . "erlang-mode_icon")
                                    (forth-mode . "forth-mode_icon")
                                    (fsharp-mode . "fsharp-mode_icon")
                                    (gdscript-mode . "gdscript-mode_icon")
                                    (haskell-mode . "haskell-mode_icon")
                                    (haskell-interactive-mode . "haskell-mode_icon")
                                    (hy-mode . "hy-mode_icon")
                                    (java-mode . "java-mode_icon")
                                    (julia-mode . "julia-mode_icon")
                                    (js-mode . "javascript-mode_icon")
                                    (kotlin-mode . "kotlin-mode_icon")
                                    (go-mode . "go-mode_icon")
                                    (latex-mode . "latex-mode_icon")
                                    (lisp-mode . "lisp-mode_icon")
                                    (magit-mode . "magit-mode_icon")
                                    (markdown-mode . "markdown-mode_icon")
                                    (meson-mode . "meson-mode_icon")
				    (nim-mode . "nim-mode_icon")
                                    (nix-mode . "nix-mode_icon")
                                    (ocaml-mode . "ocaml-mode_icon")
                                    (org-mode . "org-mode_icon")
                                    (pascal-mode . "pascal-mode_icon")
                                    (php-mode . "php-mode_icon")
                                    (puml-mode . "puml-mode_icon")
                                    (puppet-mode . "puppet-mode_icon")
                                    (python-mode . "python-mode_icon")
                                    (racket-mode . "racket-mode_icon")
                                    (ruby-mode . "ruby-mode_icon")
                                    (rust-mode . "rust-mode_icon")
                                    (rustic-mode . "rust-mode_icon")
                                    (solidity-mode . "solidity-mode_icon")
                                    (sh-mode . "comint-mode_icon")
                                    (terraform-mode . "terraform-mode_icon")
                                    (typescript-mode . "typescript-mode_icon")
                                    (zig-mode . "zig-mode_icon")
                                    ("^slime-.*" . "lisp-mode_icon")
                                    ("^sly-.*$" . "lisp-mode_icon"))
  "Mapping alist of major modes to icon names to have elcord use.
Note, these icon names must be available as 'small_image' in Discord."
  :type '(alist :key-type (choice (symbol :tag "Mode name")
                                  (regexp :tag "Regex"))
                :value-type (choice (string :tag "Icon name")
                                    (function :tag "Mapping function")))
  :group 'elcord)

(defcustom elcord-mode-text-alist '((agda-mode . "Agda")
                                    (assembly-mode . "Assembly")
                                    (c-mode . "C  ")
                                    (c++-mode . "C++")
                                    (csharp-mode . "C#")
                                    (cperl-mode . "Perl")
                                    (elixir-mode . "Elixir")
                                    (enh-ruby-mode . "Ruby")
                                    (erlang-mode . "Erlang")
                                    (fsharp-mode . "F#")
                                    (gdscript-mode . "GDScript")
                                    (hy-mode . "Hy")
                                    (java-mode . "Java")
                                    (julia-mode . "Julia")
                                    (lisp-mode . "Common Lisp")
                                    (markdown-mode . "Markdown")
                                    (magit-mode . "It's Magit!")
                                    ("mhtml-mode" . "HTML")
				    (nim-mode . "Nim")
                                    (ocaml-mode . "OCaml")
                                    (pascal-mode . "Pascal")
                                    (puml-mode . "UML")
                                    (sh-mode . "Shell")
                                    (slime-repl-mode . "SLIME-REPL")
                                    (sly-mrepl-mode . "Sly-REPL")
                                    (solidity-mode . "Solidity")
                                    (terraform-mode . "Terraform")
                                    (typescript-mode . "Typescript")
                                    (php-mode "PHP"))
  "Mapping alist of major modes to text labels to have elcord use."
  :type '(alist :key-type (choice (symbol :tag "Mode name")
                                  (regexp :tag "Regex"))
                :value-type (choice (string :tag "Text label")
                                    (function :tag "Mapping function")))
  :group 'elcord)

(defcustom elcord-display-elapsed 't
  "When enabled, Discord status will display the elapsed time since Emacs \
has been started."
  :type 'boolean
  :group 'elcord)

(defvar elcord--startup-time (string-to-number (format-time-string "%s" (current-time))))

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

(defcustom elcord-buffer-details-format-function 'elcord-buffer-details-format
  "Function to return the buffer details string shown on discord.
Swap this with your own function if you want a custom buffer-details message."
  :type 'function
  :group 'elcord)

(defcustom elcord-use-major-mode-as-main-icon 'nil
  "When enabled, the major mode determines the main icon, rather than it being the editor."
  :type 'boolean
  :group 'elcord)

(defcustom elcord-show-small-icon 't
  "When enabled, show the small icon as well as the main icon."
  :type 'boolean
  :group 'elcord)

(defcustom elcord-editor-icon 'nil
  "Icon to use for the text editor. When nil, use the editor's native icon."
  :type '(choice (const :tag "Editor Default" nil)
                 (const :tag "Emacs" "emacs_icon")
                 (const :tag "Emacs (Pen)" "emacs_pen_icon")
                 (const :tag "Emacs (Material)" "emacs_material_icon")
                 (const :tag "Emacs (Legacy)" "emacs_legacy_icon")
                 (const :tag "Spacemacs" "spacemacs_icon")
                 (const :tag "Doom" "doom_icon"))
  :group 'elcord)

(defcustom elcord-boring-buffers-regexp-list '("^ "
                                               "\\\\*Messages\\\\*")
  "A list of regexp's to match boring buffers.
When visiting a boring buffer, it will not show in the elcord presence."
  :type '(repeat regexp)
  :group 'elcord)

;;;###autoload
(define-minor-mode elcord-mode
  "Global minor mode for displaying Rich Presence in Discord."
  nil nil nil
  :require 'elcord
  :global t
  :group 'elcord
  :after-hook
  (progn
    (cond
     (elcord-mode
      (elcord--enable))
     (t
      (elcord--disable)))))

(defvar elcord--editor-name
  (cond
   ((boundp 'spacemacs-version) "Spacemacs")
   ((boundp 'doom-version) "DOOM Emacs")
   (t "Emacs"))
  "The name to use to represent the current editor.")

(defvar elcord--discord-ipc-pipe "discord-ipc-0"
  "The name of the discord IPC pipe.")

(defvar elcord--update-presence-timer nil
  "Timer which periodically updates Discord Rich Presence.
nil when elcord is not active.")

(defvar elcord--reconnect-timer nil
  "Timer used by elcord to attempt connection periodically, when active but disconnected.")

(defvar elcord--sock nil
  "The process used to communicate with Discord IPC.")

(defvar elcord--last-known-position (count-lines (point-min) (point))
  "Last known position (line number) recorded by elcord.")

(defvar elcord--last-known-buffer-name (buffer-name)
  "Last known buffer recorded by elcord.")

(defvar elcord--stdpipe-path (expand-file-name
                              "stdpipe.ps1"
                              (file-name-directory (file-truename load-file-name)))
  "Path to the 'stdpipe' script.
On Windows, this script is used as a proxy for the Discord named pipe.
Unused on other platforms.")

(defvar elcord--idle-status nil
  "Current idle status.")

(defun elcord--make-process ()
  "Make the asynchronous process that communicates with Discord IPC."
  (let ((default-directory "~/"))
    (cl-case system-type
      (windows-nt
       (make-process
        :name "*elcord-sock*"
        :command (list
                  "PowerShell"
                  "-NoProfile"
                  "-ExecutionPolicy" "Bypass"
                  "-Command" elcord--stdpipe-path "." elcord--discord-ipc-pipe)
        :connection-type 'pipe
        :sentinel 'elcord--connection-sentinel
        :filter 'elcord--connection-filter
        :noquery t))
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
        :noquery t)))))

(defun elcord--enable ()
  "Called when variable ‘elcord-mode’ is enabled."
  (setq elcord--startup-time (string-to-number (format-time-string "%s" (current-time))))
  (unless (elcord--resolve-client-id)
    (warn "elcord: no elcord-client-id available"))
  (when (eq system-type 'windows-nt)
    (unless (executable-find "powershell")
      (warn "elcord: powershell not available"))
    (unless (file-exists-p elcord--stdpipe-path)
      (warn "elcord: 'stdpipe' script does not exist (%s)" elcord--stdpipe-path)))
  (when elcord-idle-timer
    (run-with-idle-timer
     elcord-idle-timer t 'elcord--start-idle))

  ;;Start trying to connect
  (elcord--start-reconnect))

(defun elcord--disable ()
  "Called when variable ‘elcord-mode’ is disabled."
  ;;Cancel updates
  (elcord--cancel-updates)
  ;;Cancel any reconnect attempt
  (elcord--cancel-reconnect)

  ;;If we're currently connected
  (when elcord--sock
    ;;Empty our presence
    (elcord--empty-presence))

  ;;Stop running idle hook
  (cancel-function-timers 'elcord--start-idle)

  (elcord--disconnect))

(defun elcord--empty-presence ()
  "Sends an empty presence for when elcord is disabled."
  (let* ((nonce (format-time-string "%s%N"))
         (presence
          `(("cmd" . "SET_ACTIVITY")
            ("args" . (("activity" . nil)
                       ("pid" . ,(emacs-pid))))
            ("nonce" . ,nonce))))
    (elcord--send-packet 1 presence)))

(defun elcord--resolve-client-id ()
  "Evaluate `elcord-client-id' and return the client ID to use."
  (cl-typecase elcord-client-id
    (null
     nil)
    (string
     elcord-client-id)
    (function
     (funcall elcord-client-id))))

(defun elcord--resolve-icon-base (icon)
  "Evaluate `elcord-icon-base' and return the URL to use.
Argument ICON the name of the icon we're resolving."
  (cl-typecase elcord-icon-base
    (null
     nil)
    (string
     (concat elcord-icon-base icon ".png"))
    (function
     (funcall elcord-icon-base icon))))

(defun elcord--connection-sentinel (process evnt)
  "Track connection state change on Discord connection.
Argument PROCESS The process this sentinel is attached to.
Argument EVNT The event which triggered the sentinel to run."
  (cl-case (process-status process)
    ((closed exit)
     (elcord--handle-disconnect))
    (t)))

(defun elcord--connection-filter (process evnt)
  "Track incoming data from Discord connection.
Argument PROCESS The process this filter is attached to.
Argument EVNT The available output from the process."
  (elcord--start-updates))

(defun elcord--connect ()
  "Connects to the Discord socket."
  (or elcord--sock
      (ignore-errors
        (unless elcord-quiet
          (message "elcord: attempting reconnect.."))
        (setq elcord--sock (elcord--make-process))
        (condition-case nil
            (elcord--send-packet 0 `(("v" . 1) ("client_id" . ,(elcord--resolve-client-id))))
          (error
           (delete-process elcord--sock)
           (setq elcord--sock nil)))
        elcord--sock)))

(defun elcord--disconnect ()
  "Disconnect elcord."
  (when elcord--sock
    (delete-process elcord--sock)
    (setq elcord--sock nil)))

(defun elcord--reconnect ()
  "Attempt to reconnect elcord."
  (when (elcord--connect)
    ;;Reconnected.
    ;; Put a pending message unless we already got first handshake
    (unless (or elcord--update-presence-timer elcord-quiet)
      (message "elcord: connecting..."))
    (elcord--cancel-reconnect)))

(defun elcord--start-reconnect ()
  "Start attempting to reconnect."
  (unless (or elcord--sock elcord--reconnect-timer)
    (setq elcord--reconnect-timer (run-at-time 0 15 'elcord--reconnect))))

(defun elcord--cancel-reconnect ()
  "Cancels any ongoing reconnection attempt."
  (when elcord--reconnect-timer
    (cancel-timer elcord--reconnect-timer)
    (setq elcord--reconnect-timer nil)))

(defun elcord--handle-disconnect ()
  "Handles reconnecting when socket disconnects."
  (unless elcord-quiet
    (message "elcord: disconnected by remote host"))
  ;;Stop updating presence for now
  (elcord--cancel-updates)
  (setq elcord--sock nil)
  ;;Start trying to reconnect
  (when elcord-mode
    (elcord--start-reconnect)))

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

(defun elcord--test-match-p (test mode)
  "Test `MODE' against `TEST'.
if `test' is a symbol, it is compared directly to `mode'.
if `test' is a string, it is a regex to compare against the name of `mode'."
  (cl-typecase test
    (symbol (eq test mode))
    (string (string-match-p test (symbol-name mode)))))

(defun elcord--entry-value (entry mode)
  "Test `ENTRY' against `MODE'.  Return the value of `ENTRY'.
`entry' is a cons who's `car' is `elcord--test-match-p' with `mode''
When `mode' matches, if the `cdr' of `entry' is a string, return that,
otherwise if it is a function, call it with `mode' and return that value."
  (when (elcord--test-match-p (car entry) mode)
    (let ((mapping (cdr entry)))
      (cl-typecase mapping
        (string mapping)
        (function (funcall mapping mode))))))

(defun elcord--find-mode-entry (alist mode)
  "Get the first entry in `ALIST' matching `MODE'.
`alist' Should be an alist like `elcord-mode-icon-alist' where each value is
 either a string,or a function of one argument `mode'.
 If it is a function, it should return a string, or nil if no match."
  (let ((cell alist)
        (result nil))
    (while cell
      (setq result (elcord--entry-value (car cell) mode)
            cell (if result nil (cdr cell))))
    result))

(defun elcord--editor-icon ()
  "The icon to use to represent the current editor."
  (cond
   ((progn elcord-editor-icon) elcord-editor-icon)
   ((boundp 'spacemacs-version) "spacemacs_icon")
   ((boundp 'doom-version) "doom_icon")
   (t "emacs_icon")))

(defun elcord--mode-icon ()
  "Figure out what icon to use for the current major mode.
If an icon is mapped by `elcord-mode-icon-alist', then that is used.
Otherwise, if the mode is a derived mode, try to find an icon for it.
If no icon is available, use the default icon."
  (let ((mode major-mode)
        (ret (elcord--editor-icon)))
    (while mode
      (if-let ((icon (elcord--find-mode-entry elcord-mode-icon-alist mode)))
          (setq ret (elcord--resolve-icon-base icon)
                mode nil)
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
      (if-let ((text (elcord--find-mode-entry elcord-mode-text-alist mode)))
          (setq ret text
                mode nil)
        (setq mode (get mode 'derived-mode-parent))))
    (unless (stringp ret)
      (setq ret (format-mode-line ret)))
    ret))

(defun elcord--mode-icon-and-text ()
  "Obtain the icon & text to use for the current major mode.
\((\"large_text\" . <text>)
  (\"large_image\" . <icon-name>)
  (\"small_text\" . <text>)
  (\"small_image\" . <icon-name>))"
  (let ((text (elcord--mode-text))
        (icon (elcord--mode-icon))
        large-text large-image
        small-text small-image)
    (cond
     (elcord-use-major-mode-as-main-icon
      (setq large-text text
            large-image icon
            small-text elcord--editor-name
            small-image (elcord--editor-icon)))
     (t
      (setq large-text elcord--editor-name
            large-image (elcord--editor-icon)
            small-text text
            small-image icon)))
    (cond
     (elcord-show-small-icon
      (list
       (cons "large_text" large-text)
       (cons "large_image" large-image)
       (cons "small_text" small-text)
       (cons "small_image" small-image)))
     (t
      (list
       (cons "large_text" large-text)
       (cons "large_image" large-image)
       (cons "small_text" small-text))))))

(defun elcord-buffer-details-format ()
  "Return the buffer details string shown on discord."
  (format "Editing %s" (buffer-name)))

(defun elcord--details-and-state ()
  "Obtain the details and state to use for Discord's Rich Presence."
  (let ((activity (if elcord-display-buffer-details
                      (list
                       (cons "details" (funcall elcord-buffer-details-format-function))
                       (cons "state" (format "Line %s of %S"
                                             (format-mode-line "%l")
                                             (+ 1 (count-lines (point-min) (point-max))))))
                    (list
                     (cons "details" "Editing")
                     (cons "state" (elcord--mode-text))))))
    (when elcord-display-elapsed
      (push (list "timestamps" (cons "start" elcord--startup-time)) activity))
    activity))

(defun elcord--set-presence ()
  "Set presence."
  (let* ((activity
          `(("assets" . (,@(elcord--mode-icon-and-text)))
            ,@(elcord--details-and-state)))
         (nonce (format-time-string "%s%N"))
         (presence
          `(("cmd" . "SET_ACTIVITY")
            ("args" . (("activity" . ,activity)
                       ("pid" . ,(emacs-pid))))
            ("nonce" . ,nonce))))
    (elcord--send-packet 1 presence)))

(defun elcord--buffer-boring-p (buffer-name)
  "Return non-nil if `BUFFER-NAME' is non-boring per `ELCORD-BORING-BUFFERS-REGEXP-LIST'."
  (let ((cell elcord-boring-buffers-regexp-list)
        (result nil))
    (while cell
      (if (string-match-p (car cell) buffer-name)
          (setq result t
                cell nil)
        (setq cell (cdr cell))))
    result))

(defun elcord--find-non-boring-window ()
  "Try to find a live window displaying a non-boring buffer."
  (let ((cell (window-list))
        (result nil))
    (while cell
      (let ((window (car cell)))
        (if (not (elcord--buffer-boring-p (buffer-name (window-buffer window))))
            (setq result window
                  cell nil)
          (setq cell (cdr cell)))))
    result))

(defun elcord--try-update-presence (new-buffer-name new-buffer-position)
  "Try updating presence with `NEW-BUFFER-NAME' and `NEW-BUFFER-POSITION' while handling errors and disconnections."
  (setq elcord--last-known-buffer-name new-buffer-name
        elcord--last-known-position new-buffer-position)
  (condition-case err
      ;;Try and set the presence
      (elcord--set-presence)
    (error
     (message "elcord: error setting presence: %s" (error-message-string err))
     ;;If we hit an error, cancel updates
     (elcord--cancel-updates)
     ;; Disconnect
     (elcord--disconnect)
     ;; and try reconnecting
     (elcord--start-reconnect))))

(defun elcord--update-presence ()
  "Conditionally update presence by testing the current buffer/line.
If there is no 'previous' buffer attempt to find a non-boring buffer to initialize to."
  (if (= elcord--last-known-position -1)
      (when-let ((window (elcord--find-non-boring-window)))
        (with-current-buffer (window-buffer window)
          (elcord--try-update-presence (buffer-name) (count-lines (point-min) (point)))))
    (let ((new-buffer-name (buffer-name (current-buffer))))
      (unless (elcord--buffer-boring-p new-buffer-name)
        (let ((new-buffer-position (count-lines (point-min) (point))))
          (unless (and (string= new-buffer-name elcord--last-known-buffer-name)
                       (= new-buffer-position elcord--last-known-position))
            (elcord--try-update-presence new-buffer-name new-buffer-position)))))))

(defun elcord--start-updates ()
  "Start sending periodic update to Discord Rich Presence."
  (unless elcord--update-presence-timer
    (unless elcord-quiet
      (message "elcord: connected. starting updates"))
    ;;Start sending updates now that we've heard from discord
    (setq elcord--last-known-position -1
          elcord--last-known-buffer-name ""
          elcord--update-presence-timer (run-at-time 0 elcord-refresh-rate 'elcord--update-presence))))

(defun elcord--cancel-updates ()
  "Stop sending periodic update to Discord Rich Presence."
  (when elcord--update-presence-timer
    (cancel-timer elcord--update-presence-timer)
    (setq elcord--update-presence-timer nil)))

(defun elcord--start-idle ()
  "Set presence to idle, pause update and timer."
  (unless elcord--idle-status
    (unless elcord-quiet
      (message (format "elcord: %s" elcord-idle-message )))

    ;;hacky way to stop updates and store elapsed time
    (cancel-timer elcord--update-presence-timer)
    (setq elcord--startup-time (string-to-number (format-time-string "%s" (time-subtract nil elcord--startup-time)))

          elcord--idle-status t)

    (let* ((activity
            `(("assets" . (,@(elcord--mode-icon-and-text)))
              ("timestamps" ("start" ,@(string-to-number (format-time-string "%s" (current-time)))))
              ("details" . "Idle") ("state" .  ,elcord-idle-message)))
           (nonce (format-time-string "%s%N"))
           (presence
            `(("cmd" . "SET_ACTIVITY")
              ("args" . (("activity" . ,activity)
                         ("pid" . ,(emacs-pid))))
              ("nonce" . ,nonce))))
      (elcord--send-packet 1 presence))
    (add-hook 'pre-command-hook 'elcord--cancel-idle)))

(defun elcord--cancel-idle ()
  "Resume presence update and timer."
  (when elcord--idle-status
    (remove-hook 'pre-command-hook 'elcord--cancel-idle)

    ;;resume timer with elapsed time
    (setq elcord--startup-time (string-to-number (format-time-string "%s" (time-subtract nil elcord--startup-time)))
          elcord--idle-status nil
          ;;hacky way to resume updates
          elcord--update-presence-timer nil)
    (elcord--start-updates)

    (unless elcord-quiet
      (message "elcord: welcome back"))))


(provide 'elcord)
;;; elcord.el ends here
