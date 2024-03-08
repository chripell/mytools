;;; chri -- chripell's elisp snippets.

;;; Commentary:

;;; elisp snippets.

;;; Code:

(require 'dired)
(require 'dired-aux)

(defcustom chri/ig-dir "/mnt/fast/scambio2/ig"
  "Directory where to create folders with photos for IG."
  :type 'string
  :group 'chri)

(defun chri/ig-get-next-dir ()
  "Get next directory to use for IG photos."
  (let ((max-num 0)
        (files (directory-files chri/ig-dir)))
    (dolist (f files)
      (let ((fn (string-to-number f)))
        (if (> fn max-num) (setq max-num fn))))
    (concat (file-name-as-directory chri/ig-dir) (format "%07d" (+ max-num 1)))))

(defun chri/ig-from-dired ()
  "Copies and squares marked files in Dired to next folder in chri/ig-dir."
  (interactive)
  (let ((files (dired-get-marked-files))
        (dest (file-name-as-directory (chri/ig-get-next-dir))))
    (make-directory dest)
    (dolist (f files)
      (copy-file f dest))
    (let ((default-directory dest))
      (shell-command "ig_square.py *.jpg"))
    (dolist (f files)
      (delete-file (concat dest (file-name-nondirectory f))))))

(defun chri/eww-from-dired ()
  "Opens a HTML file with eww from Dired."
  (interactive)
  (eww-open-file (dired-get-file-for-visit)))

(defun chri/uncompress-one-file (file dest)
  "Uncompress a single FILE to DEST."
  (let ((default-directory dest)
        (handler (find-file-name-handler file 'dired-compress-file))
        suffix newname
        (suffixes dired-compress-file-suffixes)
        command)
    ;; See if any suffix rule matches this file name.
    (while suffixes
      (let (case-fold-search)
        (if (string-match (car (car suffixes)) file)
            (setq suffix (car suffixes) suffixes nil))
        (setq suffixes (cdr suffixes))))
    ;; If so, compute desired new name.
    (if suffix
        (setq newname dest))
    (cond (handler
           (funcall handler 'dired-compress-file file))
          ((file-symlink-p file)
           nil)
          ((and suffix (setq command (nth 2 suffix)))
           (if (string-match "%[io]" command)
               (prog1 (setq newname (file-name-as-directory newname))
                 (dired-shell-command
                  (replace-regexp-in-string
                   "%o" (shell-quote-argument (file-local-name newname))
                   (replace-regexp-in-string
                    "%i" (shell-quote-argument (file-local-name file))
                    command
                    nil t)
                   nil t)))
             ;; We found an uncompression rule.
             (let ((match (string-search " " command))
                   (msg (concat "Uncompressing " file)))
               (unless (if match
                           (dired-check-process msg
                                                (substring command 0 match)
                                                (substring command (1+ match))
                                                (file-local-name file))
                         (dired-check-process msg
                                              command
                                              (file-local-name file)))
                 newname))))
          (t (error "File format not uncompressable")))))


(defun chri/uncompress-from-dired ()
  "Decompress an archive to a given directory."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (rfn-list (mapcar #'dired-make-relative files))
         (target-dir (dired-dwim-target-directory))
	 (dired-one-file	; fluid variable inside dired-create-files
	  (and (consp files) (null (cdr files)) (car files)))
         (default (and dired-one-file
		       (not dired-dwim-target) ; Bug#25609
		       (expand-file-name (file-name-nondirectory (car files))
					 target-dir)))
	 (defaults (dired-dwim-target-defaults files target-dir))
	 (target (expand-file-name ; fluid variable inside dired-create-files
		  (minibuffer-with-setup-hook
		      (lambda ()
                        (setq-local minibuffer-default-add-function nil)
			(setq minibuffer-default defaults))
		    (dired-mark-read-file-name "Destination directory:"
		                               target-dir 'uncompress 1 rfn-list default)))))
    (dolist (file files)
      (chri/uncompress-one-file file target))))

(defun chri/generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))
(global-set-key (kbd "s-n") 'chri/generate-buffer)

(provide 'chri)
;;; chri.el ends here.
