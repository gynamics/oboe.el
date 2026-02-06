;;; oboe.el --- A simple temporary buffer management framework -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/gynamics/oboe.el
;; Keywords: convenience


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Emacs has *scratch* buffer for temporary elisp scripting.  However, sometimes
;; you may want to get a temporary buffer with some other specific configuration
;; to do something immediately.  That is not something difficult to do but
;; usually diversed in various packages and sometimes there is no such support,
;; which can be really annoying.  Therefore oboe.el be.

;; The idea of oboe.el is just as simple as following steps:
;; 1. prompt for a buffer configuration to load
;; 2. create a buffer and load selected configuration
;; 3. display it according to configuration

;; The idea looks like a trival version of org-capture, but they have different
;; target.  oboe is designed as a temporary buffer management framework.  It
;; will track created oboe buffers in queues, assigning each new buffer with a
;; unique ID.  You can manage all these buffers in a menu.  It also provides a
;; plist-based framework for creating customized configurations.

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup oboe nil
  "A simple temporary buffer management framework."
  :group 'convenience)

(defcustom oboe-loaders
  (list
   '((lambda (f)
       (when f (funcall f)))
     :major)
   '((lambda (l)
       (mapc (lambda (f) (funcall f)) l))
     :minor-list)
   '((lambda (s)
       (cond ((stringp s) (insert s))
             ((functionp s) (insert (funcall s)))))
     :init-content))
  "A list of loader functions used by `oboe-load'.
All list members should be in format (FUNC KEY1 KEY2 ...).
`oboe-use' will call FUNC on (VALUE1 VALUE2 ...).

You may want to write your own loaders and add them to this list.
Be aware that there may be nil values."
  :type '(list (cons function (list symbol)))
  :group 'oboe)

(defun oboe-use (config loader)
  "Load one item in CONFIG with LOADER."
  (apply (car loader)
         (mapcar (lambda (key)
                   (plist-get config key))
                 (cdr loader))))

(defun oboe-load (config buf)
  "Load CONFIG in BUF, return configured BUF."
  (with-current-buffer buf
    (mapc (lambda (loader)
            (oboe-use config loader))
          oboe-loaders))
  buf)

(defcustom oboe-config-alist
  (list
   `(elisp
     :major emacs-lisp-mode
     :init-content
     ,(concat
       ";; This buffer is a temporary buffer for elisp scripting by OBOE.\n"
       ";; you can save it somewhere by pressing C-x C-w.\n"))
   '(eshell
     :major eshell-mode
     :display
     (lambda (buffer)
       (split-window)
       (switch-to-buffer buffer)))
   '(org :major org-mode)
   '(text :major text-mode))
  "Alist of oboe config, each item value is a plist of key-value pairs.

Keys:

:create : A function to create the buffer.  Its default value is
`oboe-default-create-method'.  Normally you don't need to tweak it.

:display : A function to display given buffer.  Its default value
is `oboe-default-display-method'.

:assoc-file : A path as a string.  This allows us to associate temporary
buffers to files to solve some path env problems when loading a
project-wide major-mode.  It can also be used for creating persistent
storage.
  - If it is a regular file, all buffers created will be associated to
this file.  This file will never be truncated.
  - If it is a directory, a unique temporary file will be created in
that directory for each new oboe buffer to be associated to.  These
files may be deleted when associated buffers are killed.  You can
control this behavior via toggle `oboe-delete-temp-file-on-kill`.
  - If it is nil (default) or anything else, do not associate temporary
buffer to file.

:major : Major mode to be loaded in the buffer.  Actually, it doesn't
need to be a major-mode at all, just a function to be called once.

:minor-list : A list of minor-modes to be loaded in the buffer.  The
order of minor-mode calls is determined by `mapc'.

:init-content : A string to be inserted into the buffer.  It may be a
function that returns a string as well.

:revive : A function to find and display a buried buffer with
given config.  This function works on a specific buffer config
rather than a specific buffer, so you can choose which buffer to
revive, even concat all existing buffers.  Its default value is
`oboe-default-revive-method'.

:return : A function to extract a value from current buffer to
be provided for other usages.  For example, a `oboe-pipe'.

These keys are just fine to construct a simple preset to make the
whole system work.  You may extend this with your own loader
functions, everything in elisp as your wish."
  :type '(list (alist :key-type symbol
                      :value-type (plist :value-type sexp)))
  :group 'oboe)

(defvar oboe--buffers (make-hash-table)
  "A hashmap to record backward reference from buffer to its class.
Each key is a buffer.
Each value is a config symbol.")

(defvar oboe--classes (make-hash-table)
  "A hashmap to record all living oboe buffers.
Each key is a config symbol.
Each value is a list (COUNTER BUF1 BUF2 ...) where
COUNTER is the accumulated value of buffers.")

(defun oboe-register-buffer (config buffer-or-name)
  "Register BUFFER-OR-NAME with CONFIG to `oboe--classes'."
  (cond
   ((or (bufferp buffer-or-name)
        (stringp buffer-or-name))
    (let ((class (plist-get config :name))
          (buf (get-buffer buffer-or-name)))
      ;; register buffer to buffer list of this class
      (let ((buffers
             (or (gethash class oboe--classes)
                 (puthash class (list 0) oboe--classes))))
        (setcdr buffers (cons buf (cdr buffers))))
      ;; keep backward reference
      (puthash buf class oboe--buffers)))
   (t (error "%S is not a valid buffer or buffer name" buffer-or-name))))

(defun oboe-unregister-buffer (buffer-or-name)
  "Unregister BUFFER-OR-NAME from `oboe--classes'."
  (cond
   ((or (bufferp buffer-or-name)
        (stringp buffer-or-name))
    (when-let* ((buf (get-buffer buffer-or-name))
                (class (gethash buf oboe--buffers))
                (buffers (gethash class oboe--classes)))
      ;; remove buf from buffer list of this class
      (setcdr buffers (delq buf (cdr buffers)))
      ;; remove backward reference to release the buffer
      (remhash buf oboe--buffers)))
   (t (error "%S is not a valid buffer or buffer name" buffer-or-name))))

(defcustom oboe-default-display-method
  'switch-to-buffer
  "Default method to display newly created oboe buffer."
  :type 'function
  :group 'oboe)

(defun oboe-display-buffer (buffer)
  "Display a temporary BUFFER according to CONFIG.
BUFFER must be a valid oboe buffer."
  (let* ((config
          (or (alist-get
               (gethash (or buffer (error "Invalid buffer!"))
                        oboe--buffers)
               oboe-config-alist)))
         (display-method
          (or (plist-get config :display)
              oboe-default-display-method
              (error "No available display method!"))))
    (funcall display-method buffer)))

(defun oboe-make-buffer-name (config)
  "Return a buffer name string according to CONFIG.

We use an accumulator counter to assign unique value for temporary buffers.
Theoretically it won't overflow for normal usage."
  (let* ((class (plist-get config :name))
         (buffers (or (gethash class oboe--classes)
                      (puthash class (list 0) oboe--classes))))
    ;; update accumulator to make sure buffer id ever repeats
    (puthash class (cons (1+ (car buffers)) (cdr buffers))
             oboe--classes)
    ;; use old id before update
    (format "*oboe:%s<%d>*" (symbol-name class) (car buffers))))

(defcustom oboe-delete-temp-file-on-kill t
  "Whether delete associated temp file when kill a oboe buffer."
  :type 'boolean
  :group 'oboe)

(defcustom oboe-default-create-method
  (lambda (config)
    (let ((path (plist-get config :assoc-file)))
      (cond
       ((and (stringp path)
             (file-regular-p path))
        (find-file-noselect path))
       ((and (stringp path)
             (file-directory-p path))
        (find-file-noselect
         ;; `make-temp-file' ensures no duplicate files generated.
         (make-temp-file
          (format "%s%s-"
                  (file-name-as-directory path)
                  (current-time-string)))))
       (t
        (get-buffer-create (oboe-make-buffer-name config))))))
  "Default method to create a oboe buffer."
  :type 'function
  :group 'oboe)

(defun oboe-make-buffer (config)
  "Create a temporary buffer according to CONFIG."
  (let* ((creator (or (plist-get config :create)
                      oboe-default-create-method))
         (buf (funcall creator config)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook
                (lambda () (oboe-unregister-buffer (current-buffer))))
      (when (and oboe-delete-temp-file-on-kill
                 (and-let* ((path (plist-get config :assoc-file)))
                   (file-directory-p path)))
        (add-hook 'kill-buffer-hook
                  (lambda () (delete-file (buffer-file-name))))
        (make-local-variable 'kill-buffer-query-functions)
        (setq kill-buffer-query-functions nil))
      (oboe-register-buffer config buf)
      (oboe-load config buf))))

(defun oboe-find-last-buffer (&optional config)
  "Find the last buried buffer with CONFIG.
If CONFIG is nil, find one from all oboe buffers."
  (let ((matches
         (or (and config
                  (cdr (gethash (plist-get config :name) oboe--classes)))
             (hash-table-keys oboe--buffers)))
        (buffers (cdr (buffer-list)))
        (found nil))
    ;; loop through buffers and check if it's with CONFIG
    (while buffers
      (let ((buf (car buffers)))
        (when (and (buffer-live-p buf)
                   (member buf matches))
          (setq found buf
                buffers nil))
        (setq buffers (cdr buffers))))
    ;; if no buffer found, simply print a hint
    (unless found
      (message "No matched living oboe buffer found!"))
    found))

(defun oboe-revive-last-buffer (&optional config)
  "Find and display last buried buffer with CONFIG.
The behavior is user-defined when CONFIG is nil."
  (when-let ((display-method
              (or (and config
                       (plist-get config :display))
                  oboe-default-display-method))
             (buffer (oboe-find-last-buffer config)))
    (funcall display-method buffer)))

(defcustom oboe-default-revive-method
  'oboe-revive-last-buffer
  "Default method to revive created oboe buffer."
  :type 'function
  :group 'oboe)

(defun oboe-recall-buffer (&optional config)
  "Compose a temporary from existing buffers with CONFIG."
  (let ((revive-method
         (or (plist-get config :revive)
             oboe-default-revive-method)))
    (if revive-method
        (funcall revive-method config)
      (error "No available method to revive buffer!"))))

(defun oboe-read-config (prompt)
  "A helper for reading oboe config with PROMPT, return a symbol."
  (intern (completing-read prompt oboe-config-alist)))

;;;###autoload
(defun oboe-new (config)
  "Create a temporary buffer by selecting CONFIG interactively.

If not called interactively, CONFIG can be either a symbol for the
config name registered in `oboe-config-alist' or a config plist."
  (interactive
   (list (oboe-read-config
          "Create a temporary buffer with config: ")))
  (let* ((ac (assoc config oboe-config-alist))
         (cf (cond (ac (cons :name ac))
                   ((plistp config) config)
                   (t (error "Invalid config %s" config))))
         (buffer (oboe-make-buffer cf)))
    (oboe-display-buffer buffer)
    buffer))

;;;###autoload
(defun oboe-recall (config)
  "Recall a buried temporary buffer, bring it to front.
The revived buffer is selected by `oboe-default-revive-method'.

If called interactively, with prefix argument given, prompt for CONFIG
to select a specific config and use its `:revive' property.  CONFIG must
be a symbol for the config name registed in `oboe-config-alist'."
  (interactive
   (list
    (when current-prefix-arg
      (oboe-read-config "Revive a temporary buffer with config: "))))
  (oboe-recall-buffer
   (and config
        (if-let ((config (assoc config oboe-config-alist)))
            (cons :name config)
          (error "Unknown config name %s" config)))))

(defcustom oboe-default-absorb-method
  'insert-buffer-substring
  "How `oboe-absorb' insert captured text to created buffer.
It should have the same arguments with `insert-buffer-substring'"
  :type 'function
  :group 'oboe)

;;;###autoload
(defun oboe-absorb (buffers &optional region-only config)
  "Absorb selected BUFFERS into a temporary buffer.

If prefix argument as REGION-ONLY is given, only absorb active regions
in selected buffers.

If CONFIG is given, do not prompt for config name.  CONFIG can be either
a symbol which is a valid key in `oboe-config-alist', or a plist.

P. S.  You can absorb on one buffer for multiple times."
  (interactive
   (list
    (mapcar
     #'get-buffer
     (completing-read-multiple
      "Absorb buffer(s): "
      (mapcar (lambda (buf) (buffer-name buf)) (buffer-list))))
    current-prefix-arg))
  (let ((buf (if config
                 (oboe-new config)
               (call-interactively #'oboe-new))))
    (dolist (in buffers)
      (let ((args (cons in (when region-only
                             (with-current-buffer in
                               (if (use-region-p)
                                   (list (region-beginning) (region-end))
                                 (list (point) (point))))))))
        (with-current-buffer buf
          (apply oboe-default-absorb-method args))))
    (oboe-display-buffer buf)
    buf))

;;;###autoload
(defun oboe-menu (config-names)
  "Display temporary buffers in a menu, filtered by CONFIG-NAMES.

If prefix argument is given, prompt for CONFIG-NAMES."
  (interactive
   (list
    (when current-prefix-arg
      (mapcar (lambda (s) (intern s))
              (completing-read-multiple
               "Display temporary buffers with config(s): "
               oboe-config-alist)))))
  (switch-to-buffer
   (list-buffers-noselect
    nil
    (hash-table-keys oboe--buffers)
    (lambda (buf)
      (member (gethash buf oboe--buffers)
              (or config-names (hash-table-keys oboe--classes)))))))

(defcustom oboe-pipe-commit-keybinding "C-c C-c"
  "Default keybinding for \\[oboe-pipe-commit] in an oboe pipe."
  :type 'key-sequence
  :group 'oboe)

(defcustom oboe-pipe-abort-keybinding "C-c C-k"
  "Default keybinding for \\[kill-buffer] in an oboe pipe."
  :type 'key-sequence
  :group 'oboe)

(defcustom oboe-pipe-reset-keybinding "C-c C-r"
  "Default keybinding to reselect command in an oboe pipe."
  :type 'key-sequence
  :group 'oboe)

(defun oboe-pipe-commit (command ctxt-buf pipe-buf)
  "Commit PIPE-BUF to COMMAND to be called in CTXT-BUF.
PIPE-BUF should be an oboe buffer."
  (let* ((class (gethash pipe-buf oboe--buffers))
         (config (alist-get class oboe-config-alist))
         (arg (funcall (or (plist-get config :return) #'list) pipe-buf)))
    (with-current-buffer ctxt-buf
      (apply command arg)))
  (kill-buffer pipe-buf))

(defun oboe-pipe-setup (command ctxt-buf pipe-buf)
  "Set up keybindings in PIPE-BUF for a oboe pipe.
CTXT-BUF, PIPE-BUF and COMMAND are provided to `oboe-pipe-commit'."
  (with-current-buffer pipe-buf
    (setq-local header-line-format
                (concat
                 (format "Command: %s " (propertize (help-fns-function-name command)
                                                    'face font-lock-function-name-face))
                 "Commit: " (propertize oboe-pipe-commit-keybinding 'face 'highlight)
                 " Abort: " (propertize oboe-pipe-abort-keybinding 'face 'highlight)
                 " Reselect: " (propertize oboe-pipe-reset-keybinding 'face 'highlight)))
    (keymap-local-set oboe-pipe-commit-keybinding
                      (lambda ()
                        (interactive)
                        (oboe-pipe-commit command ctxt-buf pipe-buf)))
    (keymap-local-set oboe-pipe-abort-keybinding
                      (lambda ()
                        (interactive)
                        (kill-buffer pipe-buf)))
    (keymap-local-set oboe-pipe-reset-keybinding
                      (lambda (new-cmd)
                        (interactive "CReselect command: ")
                        (setq command new-cmd)))))

;;;###autoload
(defun oboe-pipe (command)
  "Create a temporary buffer for intermediate text editing.
After editing that buffer, pipe buffer as the argument to COMMAND with
`oboe-pipe-commit-keybinding' or abort it with
`oboe-pipe-abort-keybinding'.  You can also reselect COMMAND before
commit with `oboe-pipe-reset-keybinding'."
  (interactive "CCommand: ")
  (oboe-pipe-setup command (current-buffer) (call-interactively #'oboe-new)))

(defun oboe-replace-region (buf)
  "Replace active region with content in BUF."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-buffer-substring buf))

(defcustom oboe-blow-default-bellend
  #'oboe-replace-region
  "Default command used by `oboe-pipe-commit' in `oboe-blow'.
This function will be called with current buffer as ctxt-buf."
  :type 'function
  :group 'oboe)

;;;###autoload
(defun oboe-blow ()
  "Capture selected region and absorb it into a oboe pipe.

If prefix argument is given, prompt for command.  Otherwise use
`oboe-blow-default-bellend', which replaces selected region with buffer
content.

If double `C-u' prefix is given, prompt for buffer config name.
Otherwise simply use the major mode of parent buffer."
  (interactive)
  (oboe-pipe-setup
   (if current-prefix-arg
       (read-command "Command: " oboe-blow-default-bellend)
     oboe-blow-default-bellend)
   (current-buffer)
   (oboe-absorb (list (current-buffer)) t
                (unless (and current-prefix-arg
                             (>= (car current-prefix-arg) 16))
                  `(:name blow :major ,major-mode)))))

(provide 'oboe)
;;; oboe.el ends here
