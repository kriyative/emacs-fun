;;; efun-cmds.el --- basic elisp commands

(require 'efun-base)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

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
     'find-file)))

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
  (dolist (buffer (remove-if-not
                   (lambda (x) (string-match pattern (buffer-name x)))
                   (buffer-list)))
    (kill-buffer buffer)))

(provide 'efun-cmds)
