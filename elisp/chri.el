;;; chri -- chripell's elisp snippets.

;;; Commentary:

;;; elisp snippets.

;;; Code:

(require 'dired)

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

(provide 'chri)
;;; chri.el ends here.
