;; -*- lexical-binding: t -*-
;;; .emacs --- chripell's chaotic dot emacs file.
;;; Commentary:

;;; Code:

;; Optimizations for LSP mode (run lsp-doctor):
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Default packaging.
;; problems to connect go GNU ELPA.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add ~/elisp/ to directories path.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/elisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.8)
 '(ac-auto-start nil)
 '(ac-set-trigger-key "<backtab>")
 '(ac-trigger-key "<backtab>")
 '(auto-coding-regexp-alist
   '(("^BABYL OPTIONS:[ 	]*-\\*-[ 	]*rmail[ 	]*-\\*-" . no-conversion)
     ("<meta\\b[^>]*\\bcontent=\"text/html; charset=UTF-8\"[^>]*>" . utf-8)
     ("<\\?xml\\b[^>]*\\bencoding=\"utf-8\"[^>]*\\?>" . utf-8)))
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "English")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string nil)
 '(elpy-eldoc-show-current-function nil)
 '(global-font-lock-mode t nil (font-lock))
 '(gofmt-command "goimports")
 '(indicate-buffer-boundaries '((t . right) (top . left)))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(jde-jdk-registry '(("1.6" . "/opt/java")))
 '(lsp-ui-sideline-enable nil)
 '(mouse-yank-at-point t)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(ivy-rich use-package lsp-pyright counsel lsp-ivy lsp-ui which-key-posframe which-key bazel elpher lsp-dart flymake ido-completing-read+ flycheck-ocaml flycheck-pyflakes ein ipython-shell-send highlight-indentation ht wanderlust web-server websocket ccls lsp-jedi async company company-posframe dash dash-functional find-file-in-project flim flycheck flycheck-posframe ghub git-commit haskell-mode lsp-treemacs magit markdown-mode posframe project projectile semi transient treemacs with-editor xref yaml-mode rustic cargo rust-mode toml-mode forge company-coq proof-general iedit bpftrace-mode company-lsp lsp-mode flycheck-yamllint flycheck-mypy company-lua magit-gh-pulls elpy go-guru go-mode go-autocomplete markdown-preview-mode browse-kill-ring+ android-mode))
 '(projectile-tags-command "make_TAGS \"%s\" %s")
 '(rustic-display-spinner nil)
 '(rustic-format-trigger 'on-save)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(speedbar-show-unknown-files t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make sure selected packages are installed (cross-systems).
(package-install-selected-packages)

;; Enable Lua mode.
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

;; Use aspell for spell checking.
(setq-default ispell-program-name "aspell")

;; I am not scarred by narrow to region. ;-)
(put 'narrow-to-region 'disabled nil)

;; UTF-8 all the way!
(set-default-coding-systems 'utf-8)

;; Enable Javascript mode.
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; Enable Python mode.
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))
;; Enable elpy.
(elpy-enable)
;; Use ipython as a shell.
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt -i")

;; Load Go guru.
(require 'go-guru)
;; Keybindings for Go guru.
(defun my-go-mode-hook ()
  "Hook for Go mode."
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (go-guru-hl-identifier-mode)
  (auto-complete-mode 1)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; Go LSP
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Sppedbar stuff, on F12 open, S-F12 close.
(require 'sr-speedbar)
(defun sr-speedbar-open-and-select ()
 "Opens and select SR Speedbar."
 (interactive) (sr-speedbar-open) (sr-speedbar-select-window))

;; Shift + arrows move point between windows..
;(eval-when-compile (require 'cl)) ;; lexical-let
(require 'windmove)
;; The next macro ignores error if there is no windows on the left.
(defun ignore-error-wrapper (fn)
  "Function return new function that ignore errors.
The function wraps a function FN with `ignore-errors' macro."
  ;(lexical-let ((fn fn))
  (let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))
(global-set-key [M-left] (ignore-error-wrapper 'windmove-left))
(global-set-key [M-right] (ignore-error-wrapper 'windmove-right))
(global-set-key [M-up] (ignore-error-wrapper 'windmove-up))
(global-set-key [M-down] (ignore-error-wrapper 'windmove-down))

;; ivy for fancy M-X and friends completion.
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
;; Ivy-resume and other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)
;; use ivy lsp-ivy-workspace-symbol or
;; lsp-ivy-global-workspace-symbol:
(require 'lsp-ivy)
;; rebind C-M-.
(define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)

;; Sound is annoying, just flash the screen on error.
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Enable projectile mode.
(projectile-mode)
(require 'projectile-speedbar)

;; Enable spelling everywhere.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; C-c y shows the content of the kill ring.
(global-set-key "\C-cy" 'browse-kill-ring)

;; Move to/from *clipboard*:
;; S-Inset Paste
;; C-Insert Copy
;; S-Delete Cut
(require 'simpleclip)
(simpleclip-mode 1)

;; rcirc IRC client setup.
(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls)))
(setq rcirc-default-nick "chripell")
(load "~/elisp/pass")

;;; lsp mode everywhere!
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)
;; Note that pyls is the default
;; Foce jedi lsp for Python
(require 'lsp-jedi)
;; flycheck lsp doesn't work with Jedi.
;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-flake8)))))))))
(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'python-flake8 'python-mypy))
;; Force pyright
;; (require 'lsp-pyright)
;; C/C++:
(require 'ccls)
(setq ccls-executable "/usr/bin/ccls")

;;; Enable Debugger Adapter Mode.
;; CHRI20200713, seems broken: 
;; (dap-mode 1)
;; (dap-ui-mode 1)
;; (dap-tooltip-mode 1)
;; (tooltip-mode 1)
;; (dap-ui-controls-mode 1)
;; (require 'dap-go)

;;; Enable bpftrace mode.
(require 'bpftrace-mode)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; insertion of argument to go functions and such
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/git/yasnippet-snippets/snippets"))
(yas-global-mode 1)
(global-set-key (kbd "C-c y") 'company-yasnippet)

;; posframe to show suggestions.
(require 'company-posframe)
(company-posframe-mode 1)
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; ws_butler for unobtrusively delete  whitespace
(require 'ws-butler)
(ws-butler-global-mode)

;; customize company mode, no delay and active everywhere
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
;; TAB -> complete current, C-TAB -> complete common
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<C-tab>") 'company-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; My personalized shortcuts:
(global-set-key [f5] 'compile)
;; TODO: move both to projectile (global-set-key [f6] 'test)
(global-set-key [kp-left] 'backward-sexp)
(global-set-key [kp-right] 'forward-sexp)
(global-set-key [kp-up] 'backward-up-list)
(global-set-key [kp-down] 'down-list)
(global-set-key [kp-prior] 'beginning-of-defun)
(global-set-key [kp-next] 'end-of-defun)
(global-set-key [kp-begin] 'beginning-of-buffer)
(global-set-key [kp-end] 'end-of-buffer)
(global-set-key [kp-insert] 'set-mark-command)
(global-set-key [kp-delete] 'pop-global-mark)

;; Default font
(setq default-frame-alist '((font . "Source Code Pro-12")))

;; Proof General
;; from: https://endlessparentheses.com/proof-general-configuration-for-the-coq-software-foundations-tutorial.html
;; No splash screen.
(setq proof-splash-seen t)
;; Hybrid mode is by far the best.
(setq proof-three-window-mode-policy 'hybrid)
;;; I don't know who wants to evaluate comments
;;; one-by-one, but I don't.
(setq proof-script-fly-past-comments t)
(with-eval-after-load 'coq
  ;; The most common command by far. Having a 3(!)
  ;; keys long sequence for this command is just a
  ;; crime.
  (define-key coq-mode-map "\M-n"
    #'proof-assert-next-command-interactive)
  (define-key coq-mode-map "\M-p"
    #'proof-undo-last-successful-command))
(when (fboundp 'company-coq-initialize)
  (add-hook 'coq-mode-hook #'company-coq-initialize))
;; End of Proof General

;; Rust
(require 'toml-mode)
;; (require 'rust-mode)
;; (setq rust-format-on-save t)
;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
(require 'rustic)

;; org mode
(setq org-agenda-files
      '("~/chripell-org/inbox.org"
	"~/chripell-org/tasks.org"
	"~/chripell-org/projects.org"))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/chripell-org/inbox.org" "Tasks")
                               "* TODO %i%?")
			      ("n" "Note" entry
			       (file "~/chripell-org/notes.org")
			       "* Entered on %U\n  %i%?")))
(setq org-refile-targets '(("~/chripell-org/tasks.org" :maxlevel . 3)
                           ("~/chripell-org/someday.org" :level . 1)))
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))

;; notmuch email
(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "chripell@gmail.com"
      user-full-name "Christian Pellegrin")
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it)
(setq smtpmail-debug-info t)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-auto-save-directory "~/mail/draft")
(setq message-kill-buffer-on-exit t)
(setq message-directory "~/mail/")

;; Support C-x C-e from bash:
(add-to-list 'auto-mode-alist '("/bash-fc" . shell-script-mode))

;; I prefer zap-up-to-char
(global-set-key "\M-z" 'zap-up-to-char)

;; dart/flutter
(require 'lsp-dart)
(add-hook 'dart-mode-hook #'lsp)

;; Enable which-key for learing new keybindings:
(require 'which-key)
(which-key-setup-side-window-bottom)
;; (which-key-setup-side-window-right-bottom)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook 'which-key-mode)
(which-key-mode)

(provide '.emacs)
;;; .emacs ends here
