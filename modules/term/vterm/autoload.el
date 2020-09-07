;;; term/vterm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vterm/toggle (arg)
  "Toggles a terminal popup window at project root.

If prefix ARG is non-nil, recreate vterm buffer in the current project's root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let ((buffer-name
         (format "*doom:vterm-popup:%s*"
                 (if (bound-and-true-p persp-mode)
                     (safe-persp-name (get-current-persp))
                   "main")))
        confirm-kill-processes
        current-prefix-arg)
    (when arg
      (let ((buffer (get-buffer buffer-name))
            (window (get-buffer-window buffer-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (window-live-p window)
          (delete-window window))))
    (if-let (win (get-buffer-window buffer-name))
        (if (eq (selected-window) win)
            (delete-window win)
          (select-window win)
          (when (bound-and-true-p evil-local-mode)
            (evil-change-to-initial-state))
          (goto-char (point-max)))
      (setenv "PROOT" (or (doom-project-root) default-directory))
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (unless (eq major-mode 'vterm-mode)
            (vterm-mode))
          (+vterm--change-directory-if-remote))
        (pop-to-buffer buffer)))))

;;;###autoload
(defun +vterm/here (arg)
  "Open a terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (require 'vterm)
  ;; This hack forces vterm to redraw, fixing strange artefacting in the tty.
  (save-window-excursion
    (pop-to-buffer "*scratch*"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
           (if arg
               default-directory
             project-root))
         display-buffer-alist)
    (setenv "PROOT" project-root)
    (vterm)
    (+vterm--change-directory-if-remote)))

(defun +vterm--change-directory-if-remote ()
  "When `default-directory` is remote, use the corresponding
method to prepare vterm at the corresponding remote directory."
  (when (and (featurep 'tramp)
             (tramp-tramp-file-p default-directory))
    (message "default-directory is %s" default-directory)
    (with-parsed-tramp-file-name default-directory nil
      (let* ((sh-file-name-handler-p (tramp-sh-file-name-handler-p v))
             (login-program
              (tramp-get-method-parameter v 'tramp-login-program))
             (login-args
              (tramp-get-method-parameter v 'tramp-login-args))
             (async-args
              (tramp-get-method-parameter v 'tramp-async-args))
             (direct-async-args
              (tramp-get-method-parameter v 'tramp-direct-async-args))
             ;; We don't create the temporary file.  In fact, it
             ;; is just a prefix for the ControlPath option of
             ;; ssh; the real temporary file has another name, and
             ;; it is created and protected by ssh.  It is also
             ;; removed by ssh when the connection is closed.  The
             ;; temporary file name is cached in the main
             ;; connection process, therefore we cannot use
             ;; `tramp-get-connection-process'.
             (tmpfile
              (when sh-file-name-handler-p
                (with-tramp-connection-property
                    (tramp-get-process v) "temp-file"
                  (tramp-compat-make-temp-name))))
             (options
              (when sh-file-name-handler-p
                (tramp-compat-funcall
                 'tramp-ssh-controlmaster-options v)))
             spec p)

        ;; Replace `login-args' place holders.
        (setq
         spec (format-spec-make ?t tmpfile)
         options (format-spec (or options "") spec)
         spec (format-spec-make
               ?h (or host "") ?u (or user "") ?p (or port "")
               ?c options ?l "")
         ;; Add arguments for asynchronous processes.
         login-args (append async-args direct-async-args login-args)
         ;; Expand format spec.
         login-args
         (tramp-compat-flatten-tree
          (mapcar
           (lambda (x)
             (setq x (mapcar (lambda (y) (format-spec y spec)) x))
             (unless (member "" x) x))
           login-args))
         ;; Split ControlMaster options.
         login-args
         (tramp-compat-flatten-tree
          (mapcar (lambda (x) (split-string x " ")) login-args))
         p
         (mapconcat  'identity `(,login-program ,@login-args) " "))
        (vterm-send-string p)
        (vterm-send-return)
        (sleep-for 2)
        (save-window-excursion
          (pop-to-buffer "*scratch*"))
        (setq current-pos 0)
        (while (or (re-search-backward tramp-password-prompt-regexp (line-beginning-position) t)
                   (re-search-backward tramp-wrong-passwd-regexp (line-beginning-position) t)
                   (equal (point) (line-beginning-position)))

          (if (equal current-pos (point))
              (progn
                (message "Sittin for 0"))
            (progn
              (message "at %s" (point))
              (setq current-pos (point))
              (vterm-send-string (read-passwd "password:"))
              (vterm-send-return))))
        (message "Changing to %s" localname)
        (vterm-send-string
         (concat "cd " localname))
        (vterm-send-return)))))


(defvar +vterm--insert-point nil)

;;;###autoload
(defun +vterm-remember-insert-point-h ()
  "Remember point when leaving insert mode."
  (setq-local +vterm--insert-point (point)))

;;;###autoload
(defun +vterm-goto-insert-point-h ()
  "Go to the point we were at when we left insert mode."
  (when +vterm--insert-point
    (goto-char +vterm--insert-point)
    (setq-local +vterm--insert-point nil)))
(line-beginning-position)
