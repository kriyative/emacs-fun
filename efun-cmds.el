;;; efun-cmds.el --- basic elisp commands

(require 'efun-base)

(defvar *x-find-file-fallback* 'find-file)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (funcall (or *x-find-file-fallback* 'find-file) tramp-file-name)))

(defun x-find-file (&optional arg)
  "Extend find-file to invoke sudo-find-file if prefix arg is
provided. Bind it to \"\C-x\C-f\" to override the built-in
`find-file`.

 (global-set-key \"\C-x\C-f\" 'x-find-file)
"
  (interactive "p")
  (call-interactively
   (if (and arg (< 1 arg))
       'sudo-find-file
     (or *x-find-file-fallback* 'find-file))))

(defun tail-f (file)
  "Create a COMINT mode buffer running the `tail -f` command on
specified FILE. If FILE is a ssh/scp style remote file spec,
e.g.,

  user@remote.host.com:/path/to/file.txt

then a ssh connection is opened to remote.host.com, and `tail -f`
is invoked on the remote server."
  (interactive "Flocal or remote file: ")
  (let ((buf-name (concat "tail-f " file))
	(re "\\(\\w+\\)@\\([^:]+\\):\\(.*\\)"))
    (if (string-match re file)
	(let ((user (match-string 1 file))
	      (host (match-string 2 file))
	      (file1 (match-string 3 file)))
	  (make-comint buf-name "ssh" nil
		       "-l" user
		       host
		       "tail" "-f" file1))
      (make-comint buf-name "tail" nil "-f" (expand-file-name file)))
    (pop-to-buffer (concat "*" buf-name "*"))))

(defun kill-buffers-matching (pattern)
  "Kill all buffers matching specified regexp"
  (interactive "sRegexp: ")
  (dolist (buffer (remove-if-not (lambda (x)
                                   (let ((fname (or (buffer-file-name x)
                                                    (with-current-buffer x
                                                      (ibuffer-buffer-file-name)))))
                                     (when fname
                                       (string-match pattern fname))))
                                 (buffer-list)))
    (kill-buffer buffer)))

(defun run (command)
  "Spawn a long running process in a buffer named *run:
command+args* and swtich to it."
  (interactive "sRun program: ")
  (destructuring-bind (program &rest args)
      (split-string command " ")
    (let* ((buf-name (concat "*run:" command "*"))
           (buf-proc (get-buffer-process buf-name)))
      (if buf-proc
          (message "Process is already running.")
        (let ((buf (get-buffer buf-name)))
          (when buf (kill-buffer buf))
          (let ((name (file-name-nondirectory program))
                (buf (get-buffer-create buf-name)))
            (switch-to-buffer (apply 'make-comint-in-buffer name buf program nil args))
            (run-hooks (intern-soft (concat "comint-" name "-hook")))))))))

(defun python-webserver (&optional port)
  (interactive "sPort (8000): ")
  (let ((port (or (when (not (zerop (length port)))
                    (string-to-int port))
                  8000)))
    (run (format "python -m SimpleHTTPServer %d" port))))

(defvar *find-function-stack* nil)

(defun find-function-do-it-around (orig-fun symbol type switch-fn)
  (let ((marker (point-marker))
        (res (funcall orig-fun symbol type switch-fn)))
    (unless (equal marker (point-marker))
      (push marker *find-function-stack*))
    res))

(when (fboundp 'advice-add)
  (advice-add 'find-function-do-it :around #'find-function-do-it-around))

(defun pop-find-function ()
  (interactive)
  (let ((marker (pop *find-function-stack*)))
    (if marker
        (let ((buf (marker-buffer marker))
              (point (marker-position marker)))
          (if (buffer-live-p buf)
              (progn
                (unless (equal buf (current-buffer))
                  (switch-to-buffer buf))
                (goto-char point))
            (message "Buffer for previous location is unavailable" buf)))
      (message "No previous location"))))

(define-key emacs-lisp-mode-map "\M-," 'pop-find-function)

(defun exec! (command)
  "Run COMMAND in a sub-process and wait for it to complete, log
output to the *Console* buffer."
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (args (cdr program-and-args))
         (console (get-buffer-create "*Console*"))
         (pt (with-current-buffer console
               (goto-char (point-max))
               (insert "$ "
                       (mapconcat 'identity program-and-args " ")
                       "\n")
               (point)))
         (ret (apply 'call-process program nil console t args))
         (out (with-current-buffer console
                (buffer-substring-no-properties pt (point)))))
    (if (and (numberp ret) (= 0 ret))
        out
      (throw 'exec!-error (list ret out)))))

(defun spawn& (command)
  "Spawn COMMAND as a separate process."
  (interactive (list (read-shell-command "$ ")))
  (let* ((program-and-args (if (stringp command)
                               (split-string command)
                             command))
         (program (car program-and-args))
         (program-name (file-name-nondirectory program))
         (program-buffer (concat " *" program-name))
         (args (cdr program-and-args)))
    (apply 'start-process program-name program-buffer program args)))

(provide 'efun-cmds)
