;;; ack-mode ---    [sj--11/10/16]

(defvar ack-program "ack")
(defvar ack-arguments '("--color" "--nopager" "--group"))

(defvar ack-processed-mark nil)
(make-variable-buffer-local 'ack-processed-mark)

(defvar ack-mode-directory-function
  (defun sj/project-root-dir ()
    (cdar (project-root-fetch))))

(defun ack (string)
  (interactive "sSearch For: ")
  (let ((pwd (or (and (functionp ack-mode-directory-function)
		      (funcall ack-mode-directory-function))
		 default-directory)))
    (ack-start-ack-process pwd)))

(defun ack-start-ack-process (dir)
  (let ((buf (generate-new-buffer "*ack*")))
    (save-current-buffer
      (set-buffer buf)
      (setq default-directory dir)
      (setq ack-processed-mark (point-min-marker))
      (let ((proc (apply 'start-process (buffer-name buf) buf 
			 ack-program (append ack-arguments (list string)))))
	(set-process-filter proc 'ack-process-filter)))))

(defun ack-process-filter (proc string)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (display-buffer buf)
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
