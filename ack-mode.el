;;; ack-mode ---    [sj--11/10/16]

(defvar ack-program-name "ack")

(defvar ack-color-filename "bold green")
(defvar ack-color-match "bold red")
(defvar ack-color-lineno "bold yellow")

(defvar ack-arguments `("--nopager" "--group" "--color"
			,(concat "--color-filename=" ack-color-filename)
			,(concat "--color-match=" ack-color-match)
			,(concat "--color-lineno=" ack-color-lineno)
			))

(defvar ack-mode-directory-function
  (defun sj/project-root-dir ()
    (let ((root-dir (project-root-fetch)))
      (when root-dir
	(cdar root-dir)))))

(define-derived-mode ack-mode special-mode "Ack"
  "Major mode for ack search results."
  ;; End of last fully processed line
  (set (make-local-variable 'ack-last-processed-mark) (point-min-marker))
  (set (make-local-variable 'ack-in-group-p) nil)
  (set (make-local-variable 'ack-current-group-file-name) nil)
  (set (make-local-variable 'ack-current-group-start-marker) (make-marker)))

(defun ack (search-string)
  (interactive "sSearch For: ")
  (let* ((pwd (or (and (functionp ack-mode-directory-function)
		       (funcall ack-mode-directory-function))
		  default-directory))
	 (buf (ack-start-ack-process search-string pwd)))
    (switch-to-buffer-other-window buf)))

(defun ack-start-ack-process (search-string dir)
  (let ((buf (generate-new-buffer (format "*ack \"%s\"*" search-string))))
    (save-current-buffer
      (set-buffer buf)
      (ack-mode)
      (setq default-directory dir)
      (let ((proc (apply 'start-process (buffer-name buf) buf 
			 ack-program-name (append ack-arguments (list search-string)))))
	(set-process-filter proc 'ack-process-filter)))
    buf))

(defun ack-process-filter (proc string)
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (ack-process-new-input))))))

(defun ack-process-new-input ()
  (goto-char ack-processed-mark)
  (while (= 0 (forward-line 100)) t)
  (beginning-of-line)
  (unless (equal ack-processed-mark (point))
    (ansi-color-apply-on-region ack-processed-mark (point))
    (move-marker ack-processed-mark (point))))
