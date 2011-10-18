;;; ack-mode --- run ack and display the results in a browsable buffer

;; Copyright (C) 2011 Sudish Joseph <sudish@gmail.com>

;; Licensed under the same terms as GNU Emacs.

;; Keywords: ack grep search
;; Created: 16 Oct 2011
;; Author: Sudish Joseph <sudish@gmail.com>
;; Version: 1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This mode exists to run ack and similar recursive file search
;; tools, displaying the results in a browsable buffer.  The results
;; of the search are grouped by file with individual lines prefixed
;; only by the line number, making it easier to sift through the
;; results.

(require 'ansi-color)

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

(defvar ack-last-processed-mark)
(defvar ack-in-group-p)
(defvar ack-current-group-file-name)
(defvar ack-current-group-start-marker)

(define-derived-mode ack-mode special-mode "Ack"
  "Major mode for ack search results."
  ;; End of last fully processed line
  (set (make-local-variable 'ack-last-processed-mark) (point-min-marker))
  (set (make-local-variable 'ack-in-group-p) nil)
  (set (make-local-variable 'ack-current-group-file-name) nil)
  (set (make-local-variable 'ack-current-group-start-marker) (make-marker)))

(define-key ack-mode-map [return] 'ack-visit-result)
(define-key ack-mode-map [left]   'ack-previous-file)
(define-key ack-mode-map [right]  'ack-next-file)

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

(defsubst ack-last-line-in-buffer-p ()
  (save-excursion
    ;; we're on the last line if
    ;; - we can't move forward a line
    ;; - moving forward leaves us at the end of the buffer
    (or (/= 0 (forward-line))
	(eobp))))

(defun ack-process-new-input ()
  (goto-char ack-last-processed-mark)
  (while (not (ack-last-line-in-buffer-p))
    (cond ((looking-at-p "^$")
	   (setq ack-in-group-p nil)
	   (when ack-current-group-file-name
	     (put-text-property ack-current-group-start-marker (point)
				'ack-file-name
				(expand-file-name ack-current-group-file-name default-directory))
	     (setq ack-current-group-file-name nil)))
	  ((and (not ack-in-group-p)
		(looking-at "^\\([^[:blank:]]+\\)$"))
	   (setq ack-in-group-p t)
	   (setq ack-current-group-file-name
		 (ansi-color-filter-apply (substring-no-properties (match-string 1))))
	   (set-marker ack-current-group-start-marker (point-marker))))
    (forward-line))
  (unless (equal ack-last-processed-mark (point))
    (ansi-color-apply-on-region ack-last-processed-mark (point))
    (set-marker ack-last-processed-mark (point-marker))))

(defun ack-visit-result ()
  (interactive)
  (let* ((file (get-text-property (point) 'ack-file-name))
	 (buf (and file (get-file-buffer file)))
	 (line (save-excursion
		 (save-match-data
		   (beginning-of-line)
		   (when (looking-at "^\\([[:digit:]]+\\):")
		     (string-to-number (match-string 1)))))))
    (cond ((bufferp buf)
	   (ack-display-buffer buf line))
	  ((and file (file-readable-p file))
	   (ack-display-buffer (find-file-noselect file) line))
	  (error "Couldn't show file %s" file))))

(defun ack-display-buffer (buf line)
  (when (bufferp buf)
    (save-selected-window
      (select-window
       (display-buffer-use-some-window buf '((inhibit-same-window . t))))
      (with-current-buffer buf
	(goto-char (point-min))
	(forward-line (1- line))
	(recenter nil)))))

(defun ack-skip-property-changes (prop direction times)
  (let ((skip-func (cond ((eq direction 'forward) 'next-single-property-change)
			 (t 'previous-single-property-change)))
	(position (point)))
    (while (and position (>= times 0))
      (goto-char position)
      (setq position (funcall skip-func position prop))
      (setq times (1- times)))
    (unless position
      (goto-char
       (cond ((eq direction 'forward) (point-max))
	     (t                       (point-min)))))))

(defun ack-next-file ()
  (interactive)
  (cond ((get-text-property (point) 'ack-file-name)
	 (ack-skip-property-changes 'ack-file-name 'forward 2))
	(t
	 (ack-skip-property-changes 'ack-file-name 'forward 1)))
  (beginning-of-line))

(defun ack-previous-file ()
  (interactive)
  (cond ((get-text-property (point) 'ack-file-name)
	 (ack-skip-property-changes 'ack-file-name 'backward 3))
	(t
	 (ack-skip-property-changes 'ack-file-name 'backward 2)))
  (forward-line))
