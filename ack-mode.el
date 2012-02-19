;;; ack-mode --- run ack and display the results in a browsable buffer

;; Copyright (C) 2011-2012 Sudish Joseph <sudish@gmail.com>

;; Licensed under the same terms as GNU Emacs.

;; Keywords: ack grep search
;; Created: 16 Oct 2011
;; Author: Sudish Joseph <sudish@gmail.com>
;; Version: 1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This mode provides a browsable buffer for the results from ack, the
;; recursive search tool.  The results of the search are grouped by
;; file with individual lines prefixed only by the line number, making
;; it easy to sift through the results.
;;
;; To use, bind `ack-mode' to a key of your choice.
;;
;; You can move between file blocks in results buffers using `TAB' and
;; `shift-TAB'.  `RETURN' visits the file and line mentioned on the
;; line the cursor is currently on.


;; Customizable values
(defgroup ack-mode nil
  "A mode for browsing ack recursive file search results."
  :link '(url-link :tag "ack" "http://betterthangrep.com/")
  :package-version "1"
  :group 'ack-mode)

(defcustom ack-program-name "ack"
  "The name of the ack binary."
  :type '(string)
  :require 'ack-mode
  :group 'ack-mode)

(defcustom ack-arguments '("--group" "--nopager" "--nocolor")
  "Arguments for ack, used on each search."
  :type '(repeat string)
  :require 'ack-mode
  :group 'ack-mode)

(defun sj/project-root-dir ()
  (let ((root-dir (project-root-fetch)))
    (when root-dir
      (cdar root-dir))))

(defcustom ack-root-directory-function 'ack-default-directory
  "Function returning the directory to start the search in.

Useful in conjunction with a project root package - use this to
have ack begin its search at the root of your project."
  :type '(function)
  :require 'ack-mode
  :group 'ack-mode)

;; shouldn't need to tweak these, but have at
(defcustom ack-mode-file-regexp "^\\([^:[:blank:]][^:[:blank:]]+\\)$"
  "Regular expression matching the first line of a file results block."
  :type '(regexp)
  :require 'ack-mode
  :group 'ack-mode)

(defcustom ack-mode-line-regexp "^\\([[:digit:]]+\\):"
  "Regular expression matching a results line."
  :type '(regexp)
  :require 'ack-mode
  :group 'ack-mode)


(defvar ack-font-lock-keywords `((,ack-mode-file-regexp . (0 font-lock-keyword-face t))
				 (,ack-mode-line-regexp . font-lock-variable-name-face)))

;; The buffer-locals in an ack-mode buffer
(defvar ack-local-font-lock-keywords)
(defvar ack-last-processed-mark)
(defvar ack-in-group-p)
(defvar ack-current-group-file-name)
(defvar ack-current-group-start-marker)

(defvar ack-use-text-properties nil)

(define-derived-mode ack-mode special-mode "Ack"
  "Major mode for ack search results."
  ;; State tracking async input chunks
  (set (make-local-variable 'ack-last-processed-mark) (point-min-marker))
  (set (make-local-variable 'ack-in-group-p) nil)
  (set (make-local-variable 'ack-current-group-file-name) nil)
  (set (make-local-variable 'ack-current-group-start-marker) (make-marker)))

(define-key ack-mode-map [return]      'ack-visit-result)
(define-key ack-mode-map [(shift tab)] 'ack-previous-file)
(define-key ack-mode-map [tab]         'ack-next-file)

;;;###autoload
(defun ack (search-string)
  "Runs `ack-program-name' and displays the result in a browsable buffer."
  (interactive (list (grep-read-regexp)))
  (let* ((pwd (or (and (functionp ack-root-directory-function)
		       (funcall ack-root-directory-function))
		  default-directory))
	 (buf (ack-start-ack-process search-string pwd)))
    (switch-to-buffer-other-window buf)))

(defun ack-default-directory ()
  "Returns default (current) directory of the current buffer.

Useful as value for `ack-root-directory-function'."
  default-directory)

(defun ack-start-ack-process (search-string dir)
  (let ((buf (generate-new-buffer (format "*ack \"%s\"*" search-string))))
    (save-current-buffer
      (set-buffer buf)
      (ack-mode)
      (hack-dir-local-variables-non-file-buffer)
      (setq default-directory dir)

      (set (make-local-variable 'ack-local-font-lock-keywords)
	   (cons `(,search-string . font-lock-string-face)
		 ack-font-lock-keywords))
      (setq font-lock-defaults '(ack-local-font-lock-keywords t t))
      (font-lock-mode 1)

      (let ((proc (apply 'start-process (buffer-name buf) buf
			 ack-program-name (append ack-arguments (list search-string)))))
	(set-process-filter   proc 'ack-process-filter)
	(set-process-sentinel proc 'ack-process-sentinel)))
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
	  (when ack-use-text-properties
	    (ack-process-new-input)))))))

(defun ack-process-sentinel (proc event)
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc))
	  (inhibit-read-only t))
      (when (buffer-live-p buf)
	(with-current-buffer buf
	  (save-excursion
	    (goto-char (point-max))
	    (insert "\n")
	    (when ack-use-text-properties
	      (ack-process-new-input))))))))

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
		(looking-at ack-mode-file-regexp))
	   (setq ack-in-group-p t)
	   (setq ack-current-group-file-name (substring-no-properties (match-string 1)))
	   (set-marker ack-current-group-start-marker (point-marker))))
    (forward-line))
  (set-marker ack-last-processed-mark (point-marker)))

(defun ack-visit-result ()
  "Visit file and line displayed on current line in search results buffer.

The file is visited in a separate window with the current line centered."
  (interactive)
  (let* ((file (car (ack-find-file-group 'current)))
	 (buf (and file (get-file-buffer file)))
	 (line (save-excursion
		 (save-match-data
		   (beginning-of-line)
		   (when (looking-at ack-mode-line-regexp)
		     (string-to-number (match-string 1)))))))
    (cond ((bufferp buf)
	   (ack-display-buffer buf line))
	  ((and file (file-readable-p file))
	   (ack-display-buffer (find-file-noselect file) line))
	  (t (when file (error "Couldn't show file %s" file))))))

(defun ack-display-buffer (buf line)
  "Display line number `line' of buffer `buf' in some visible window."
  (when (bufferp buf)
    (save-selected-window
      (select-window
       (display-buffer-use-some-window buf '((inhibit-same-window . t))))
      (with-current-buffer buf
	(goto-char (point-min))
	(forward-line (1- (or line 0)))
	(recenter nil)))))

(defun ack-find-file-group (which)
  "Returns (PATH . LOCATION) for the file group specified by `which'.
`which' may be 'next, 'current. Any other value is treated as 'previous.

PATH is the path to the file.  LOCATION is the character location in the
buffer where the file group begins."
  (let* ((forward-p (eq which 'next))
	 (search-func (if forward-p 're-search-forward 're-search-backward))
	 (old-point (point)))
    (save-excursion
      (when (looking-at ack-mode-file-regexp)
	(forward-line (if forward-p nil -1)))
      (cond ((funcall search-func ack-mode-file-regexp nil t)
	     (beginning-of-line)
	     (cons (substring-no-properties (match-string 1)) (point)))
	    (t (cons nil old-point))))))

(defun ack-next-file ()
  "Go to the next file in the search results buffer."
  (interactive)
  (let ((location (cdr (ack-find-file-group 'next))))
    (when location
      (goto-char location))))

(defun ack-previous-file ()
  "Go to the last file in the search results buffer."
  (interactive)
  (let ((location (cdr (ack-find-file-group 'previous))))
    (when location
      (goto-char location))))
