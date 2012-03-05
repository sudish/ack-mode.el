;;; ack-mode --- run ack and display the results in a browsable buffer

;; Copyright (C) 2011-2012 Sudish Joseph <sudish@gmail.com>

;; Licensed under the same terms as GNU Emacs.

;; Keywords: ack grep search
;; Created: 16 Oct 2011
;; Author: Sudish Joseph <sudish@gmail.com>
;; Version: 2

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

(defcustom ack-mode-program-name "ack"
  "The name of the ack binary."
  :type '(string)
  :require 'ack-mode
  :group 'ack-mode)

(defcustom ack-mode-arguments '("--group" "--nopager" "--nocolor")
  "Arguments for ack, used on each search."
  :type '(repeat string)
  :require 'ack-mode
  :group 'ack-mode)

(defcustom ack-mode-root-directory-function 'ack-mode-default-directory
  "Function returning the directory to start the search in.

Useful in conjunction with a project root package - use this to
have ack begin its search at the root of your project."
  :type '(function)
  :require 'ack-mode
  :group 'ack-mode)

(defcustom ack-mode-header-line-face 'mode-line-buffer-id
  "Face used for header lines in ack buffers."
  :type '(face)
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


(defvar ack-mode-font-lock-keywords
  `((,ack-mode-file-regexp . (0 font-lock-keyword-face t))
    (,ack-mode-line-regexp . font-lock-variable-name-face)))

;; The buffer-locals in an ack-mode buffer
(defvar ack-mode-local-font-lock-keywords)

(define-derived-mode ack-mode special-mode "Ack"
  "Major mode for ack search results.")

(define-key ack-mode-map [return]      'ack-mode-visit-result)
(define-key ack-mode-map [(shift tab)] 'ack-mode-previous-file)
(define-key ack-mode-map [tab]         'ack-mode-next-file)

;;;###autoload
(defun ack (search-string)
  "Runs `ack-mode-program-name' and displays the result in a browsable buffer."
  (interactive (list (grep-read-regexp)))
  (let* ((pwd (or (and (functionp ack-mode-root-directory-function)
		       (funcall ack-mode-root-directory-function))
		  default-directory))
	 (buf (ack-mode-start-ack-process search-string pwd)))
    (switch-to-buffer-other-window buf)))

(defun ack-mode-default-directory ()
  "Returns default (current) directory of the current buffer.

Useful as value for `ack-mode-root-directory-function'."
  default-directory)

(defun ack-mode-start-ack-process (search-string dir)
  (let ((buf (generate-new-buffer (format "*ack \"%s\"*" search-string))))
    (save-current-buffer
      (set-buffer buf)
      (ack-mode)
      (setq header-line-format (ack-mode-header-line dir))
      (hack-dir-local-variables-non-file-buffer)
      (setq default-directory dir)

      (set (make-local-variable 'ack-mode-local-font-lock-keywords)
	   (cons `(,search-string . font-lock-string-face)
		 ack-mode-font-lock-keywords))
      (setq font-lock-defaults '(ack-mode-local-font-lock-keywords t t))
      (font-lock-mode 1)

      (let ((proc (apply 'start-process (buffer-name buf) buf
			 ack-mode-program-name
			 (append ack-mode-arguments (list search-string)))))
	(set-process-filter proc 'ack-mode-process-filter)))
    buf))

(defun ack-mode-process-filter (proc string)
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point)))))))

(defun ack-mode-visit-result ()
  "Visit file and line displayed on current line in search results buffer.

The file is visited in a separate window with the current line centered."
  (interactive)
  (let* ((file (car (ack-mode-find-file-group 'current)))
	 (buf (and file (get-file-buffer file)))
	 (line (save-excursion
		 (save-match-data
		   (beginning-of-line)
		   (when (looking-at ack-mode-line-regexp)
		     (string-to-number (match-string 1)))))))
    (cond ((bufferp buf)
	   (ack-mode-display-buffer buf line))
	  ((and file (file-readable-p file))
	   (ack-mode-display-buffer (find-file-noselect file) line))
	  (t (when file (error "Couldn't show file %s" file))))))

(defun ack-mode-display-buffer (buf line)
  "Display line number `line' of buffer `buf' in some visible window."
  (when (bufferp buf)
    (save-selected-window
      (select-window
       (display-buffer-use-some-window buf '((inhibit-same-window . t))))
      (with-current-buffer buf
	(goto-char (point-min))
	(forward-line (1- (or line 0)))
	(recenter)
	(ack-mode-temporarily-highlight-line)))))

(defun ack-mode-find-file-group (which)
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

(defun ack-mode-next-file ()
  "Go to the next file in the search results buffer."
  (interactive)
  (let ((location (cdr (ack-mode-find-file-group 'next))))
    (when location
      (goto-char location))))

(defun ack-mode-previous-file ()
  "Go to the last file in the search results buffer."
  (interactive)
  (let ((location (cdr (ack-mode-find-file-group 'previous))))
    (when location
      (goto-char location))))

(defun ack-mode-header-line (dir)
  "Returns a value suitable for `header-line-format'."
  (let* ((shortened (replace-regexp-in-string dir ".../" default-directory))
	 (header (format "root: %s, pwd: %s, %%s" dir shortened)))
    `(:propertize ,header face ,ack-mode-header-line-face)))

;; Support for temporarily highlighting current line.  The highlight is
;; removed when the window configuration changes.
(defvar ack-mode-overlay (make-overlay 0 0))
(overlay-put ack-mode-overlay 'face isearch-face)

(defun ack-mode-temporarily-highlight-line ()
  (move-overlay ack-mode-overlay (line-beginning-position) (line-end-position))
  (add-hook 'window-configuration-change-hook 'ack-mode-remove-highlight))

(defun ack-mode-remove-highlight ()
  (delete-overlay ack-mode-overlay)
  (remove-hook 'window-configuration-change-hook 'ack-mode-remove-highlight))
