;;; init.el -- chripell's initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; chripell's initialization file

;;; Code:

;;; Load machine specific initialization. It is expected to define as t/nil:
;;; chri/proglang
;;; chri/notmuch
;;; chri/projectile-global
(defvar chri/proglang)
(defvar chri/notmuch)
(defvar chri/projectile-global)
(require 'init-mach "~/.emacs.d/init-mach.el")

;; Optimizations for LSP mode (run lsp-doctor):
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Add ~/elisp/ and sub-directories to load path.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/elisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Initialize package sources.
(require 'package)
(require 'gnutls)

;; Default packaging.
;; problems to connect go GNU ELPA.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Use-package.
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
(setq use-package-verbose t)

;; Make sure packages are downloaded.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-coding-regexp-alist
   '(("^BABYL OPTIONS:[ 	]*-\\*-[ 	]*rmail[ 	]*-\\*-" . no-conversion)
     ("<meta\\b[^>]*\\bcontent=\"text/html; charset=UTF-8\"[^>]*>" . utf-8)
     ("<\\?xml\\b[^>]*\\bencoding=\"utf-8\"[^>]*\\?>" . utf-8)))
 '(bookmark-save-flag 1)
 '(calendar-latitude 53.35)
 '(calendar-longitude -6.26)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(current-language-environment "English")
 '(custom-enabled-themes '(deeper-blue))
 '(debug-on-error nil)
 '(dired-auto-revert-buffer t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string nil)
 '(elpy-eldoc-show-current-function nil)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(indicate-buffer-boundaries '((t . right) (top . left)))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(lsp-diagnostics-disabled-modes '(python-mode))
 '(lsp-ui-sideline-enable nil)
 '(magit-auto-revert-mode t)
 '(mouse-yank-at-point t)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(unicode-fonts ivy-yasnippet yasnippet-snippets lsp coq js-mode notmuch coq-mode go-playground javascript-mode diminish yaml-imenu ws-butler which-key-posframe wanderlust use-package typescript-mode tree-mode toml-mode toml smex simpleclip rustic rust-mode proof-general projectile-speedbar menu-bar+ markdown-preview-mode magit-gh-pulls lsp-ui lsp-pyright lsp-jedi lsp-ivy lsp-dart kotlin-mode jsonrpc jedi ivy-rich ipython-shell-send iedit ido-completing-read+ haskell-mode go-projectile go-autocomplete ghub+ forge flymake flycheck-yamllint flycheck-pyflakes flycheck-posframe flycheck-ocaml flycheck-mypy flycheck-kotlin flx-ido find-file-in-project elpy elpher ein dash-functional counsel company-posframe company-lua company-lsp company-coq ccls cargo browse-kill-ring+ bpftrace-mode bazel async android-mode))
 '(projectile-tags-command "make_TAGS \"%s\" %s")
 '(rustic-display-spinner nil)
 '(rustic-format-trigger 'on-save)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(unicode-fonts-debug-availability t)
 '(unicode-fonts-use-prepend nil)
 '(warning-suppress-types '((lsp-mode)))
 '(wdired-allow-to-change-permissions t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Use aspell for spell checking.
(setq-default ispell-program-name "aspell")

;; I am not scarred by narrow to region. ;-)
(put 'narrow-to-region 'disabled nil)

;; UTF-8 all the way!
(set-default-coding-systems 'utf-8)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Allow disable showing on modeline with :diminish tag
(use-package diminish
 :diminish abbrev-mode
 :diminish org-indent-mode
 :diminish apheleia-mode
 :diminish auto-revert-mode
 :diminish hungry-delete-mode
 :diminish hungry-delete
 :diminish lisp-interaction-mode
 :diminish visual-line-mode
 :diminish subword-mode
 :diminish auto-fill-function)

;; ivy & C for fancy M-X and friends completion.
(use-package ivy
  :demand
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 ("C-c C-r" . ivy-resume)
         :map ivy-mode-map
         ("M-s" . ivy-yank-symbol))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))
(use-package counsel
  :diminish
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("M-y" . counsel-yank-pop)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("<f2> j" . counsel-set-variable)
	 ("C-c TAB" . counsel-company)
	 ("C-c c" . counsel-compile)
	 ("C-c a" . counsel-ag)
	 ("C-c g" . counsel-git)
	 ("C-c G" . counsel-search)
	 ("C-c e" . counsel-flycheck)
	 ("C-c f" . counsel-fonts)
	 ("C-c j" . counsel-git-grep)
	 ("C-c L" . counsel-git-log)
	 ("C-c M" . counsel-linux-app)
	 ("C-c b" . counsel-bookmark)
	 ("C-c B" . bookmark-set)
	 ("C-c d" . counsel-descbinds)
	 ("C-c o" . counsel-outline)
	 ("C-c r" . counsel-rg)
	 ("C-c t" . counsel-load-theme)
	 ("C-c F" . counsel-org-file))
  :config (counsel-mode))
(use-package swiper
  :diminish
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c s" . swiper-all)))
(use-package ivy-rich
  :diminish
  :after ivy
  :config
  (ivy-rich-mode 1))

;; yasnippet for pasting snippets.
(use-package yasnippet
  :diminish
  :bind
  ([f9] . yas-expand)
  ([C-f9] . yas-describe-tables)
  :config
  (add-to-list 'yas-snippet-dirs "~/git/yasnippet-snippets/snippets")
  (yas-global-mode 1)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
(use-package yasnippet-snippets
  :after yasnippet
  :diminish)
(use-package ivy-yasnippet
  :diminish
  :after yasnippet
  :bind
  ("s-y" . ivy-yasnippet)
  ([S-f9] . ivy-yasnippet))

;; ws_butler for unobtrusively delete  whitespace.
(use-package ws-butler
  :diminish
  :config
  (ws-butler-global-mode))

;; Use company mode for completion.
(use-package company
  :diminish
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :hook (after-init . global-company-mode)
  :init
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [f9] 'yas-expand)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "<C-tab>") 'company-complete-common)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)))
(use-package company-posframe
  :diminish
  :after company
  :init
  (company-posframe-mode 1))

;; flycheck mode for showing errors/warnings.
(use-package flycheck
  :config
  ;; Always split vertically list of errors
  (defun chri/list-errors ()
    "Always split vertically."
    (interactive)
    (let ((split-width-threshold nil)
          (split-height-threshold 0))
      (call-interactively 'flycheck-list-errors)))
  (define-key flycheck-command-map "l" 'chri/list-errors))
(use-package flycheck-posframe
  :init
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-position 'window-bottom-left-corner)
  (setq flycheck-posframe-border-width 1)
  (set-face-attribute 'flycheck-posframe-background-face nil :inherit 'default)
  (set-face-attribute 'flycheck-posframe-border-face nil :foreground "gray50")
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'flycheck-error-list-info)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'flycheck-error-list-warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'flycheck-error-list-error)
  :hook (flycheck-mode . flycheck-posframe-mode))

;; magit
;; I get errors if I defer this.
(use-package magit
  :demand
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; Default font
(setq default-frame-alist '((font . "Source Code Pro-12")))

(if (not chri/proglang)
  (use-package which-key
    :diminish
    :init
    (which-key-setup-side-window-bottom)
    (which-key-mode))
  ;; chri/proglang branch:

  ;; LSP mode.
  (use-package lsp-mode
    :init
    (setq lsp-keymap-prefix "s-l")
    :hook ((python-mode . lsp)
           (c++-mode . lsp)
           (c-mode .lsp)
           (go-mode . lsp))
    ;; rebind C-M-.
    :bind (:map lsp-mode-map ("C-M-." . lsp-find-references))
    :commands lsp)
  (use-package lsp-ivy
    :after lsp-mode)
  (use-package lsp-ui
    :commands lsp-ui-mode
    :after lsp-mode)
  (use-package lsp-treemacs
    :after lsp-mode
    :commands lsp-treemacs-errors-list
    :bind (:map lsp-mode-map
                ("s-t s" . lsp-treemacs-symbols)
                ("s-t r" . lsp-treemacs-references)))
  (use-package dap-mode
    :defer t
    :after lsp-mode
    :init
    (require 'dap-python))

  ;; Enable which-key for learing new keybindings:
  (use-package which-key
    :diminish
    :demand
    :config
    (which-key-setup-side-window-bottom)
    (which-key-mode)
    :hook ((lsp-mode . lsp-enable-which-key-integration)
	   (lsp-mode . which-key-mode)))

  ;; Lua mode.
  (use-package lua-mode
    :mode "\\.lua\\'"
    :interpreter "lua"
    :hook (lua-mode . turn-on-font-lock))

  ;; Javascript mode.
  ;; (use-package js-mode
  ;;  :mode "\\.js\\'")

  ;; Python mode.
  (use-package python
    :mode
    ("\\.py\\'" . python-mode)
    ("\\.wsgi$" . python-mode)
    :interpreter ("python" . python-mode))
  ;; Enable elpy.
  (use-package elpy
    :diminish
    :after python
    :init
    (elpy-enable))
  ;; Use ipython as a shell.
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "--simple-prompt -i")

  ;; Go mode
  (use-package go-mode
    :config
    (defun chri/lsp-go-install-save-hooks ()
      "Go Save hook
       Set up before-save hooks to format buffer and add/delete imports.
       Make sure you don't have other gofmt/goimports hooks enabled."
      (add-hook 'before-save-hook 'lsp-format-buffer t t)
      (add-hook 'before-save-hook 'lsp-organize-imports t t))
    (add-hook 'go-mode 'chri/lsp-go-install-save-hooks)
    :mode "\\.go\\'")
  (use-package go-playground
    :if (executable-find "go")
    :defines go-playground-basedir
    :init
    (setq go-playground-basedir (expand-file-name "~/t"))
    :commands go-playground go-playground-ask-file-name)
  (use-package go-guru
    :if (executable-find "guru")
    :hook (go-mode . go-guru-hl-identifier-mode))

  ;; C/C++:
  (use-package ccls
    :defer t
    :init
    (setq ccls-executable "/usr/bin/ccls")
    :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

  ;; bpftrace mode
  (use-package bpftrace-mode
    :mode "\\.bt\\'"
    :interpreter "bpftrace")

  ;; Ocaml
  (when (file-readable-p (expand-file-name "~/.emacs.d/opam-user-setup.el"))
    (defun opam-setup ()
      "Loads Ocaml stuff. Call it before using Ocaml, otherwise it slows down startup."
      (interactive)
      ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
      (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
      ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
      ))

  ;; coq / Proof General
  ;; (use-package coq)
  (use-package proof-general
    :defines proof-splash-seen proof-three-window-mode-policy proof-script-fly-past-comments coq-mode-map
    :init
    (setq proof-splash-seen t)
    (setq proof-three-window-mode-policy 'hybrid)
    (setq proof-script-fly-past-comments t)
    :bind (:map coq-mode-map
		("M-n" . proof-assert-next-command-interactive)
		("M-p" . proof-undo-last-successful-command)))
  (use-package company-coq
    :hook coq-mode company-coq-initialize)

  ;; Rust
  (use-package rustic
    :defer t
    :init
    ;; to use rustic-mode even if rust-mode also installed
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)))

  ;; dart/flutter
  (use-package lsp-dart
    :hook (dart-mode . lsp))

  ;; devicetree mode
  (use-package dts-mode
    :mode "\\.dt[si]\\'")

  ;; markdown mode.
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "pandoc"))

  ;; My personalized shortcuts:
  (global-set-key [f5] 'projectile-compile-project)
  (global-set-key [S-f5] 'projectile-run-project)
  (global-set-key [f6] 'projectile-test-project)
  (global-set-key [S-f6] 'projectile-regenerate-tags)
  )

;; The next macro ignores error if there is no windows on the left.
(defun chri/ignore-error-wrapper (fn)
  "Function return new function that ignore errors.
The function wraps a function FN with `ignore-errors' macro."
  (let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))
(use-package windmove
  :init
  (global-set-key [M-left] (chri/ignore-error-wrapper 'windmove-left))
  (global-set-key [M-right] (chri/ignore-error-wrapper 'windmove-right))
  (global-set-key [M-up] (chri/ignore-error-wrapper 'windmove-up))
  (global-set-key [M-down] (chri/ignore-error-wrapper 'windmove-down))
  (defun windmove-python ()
    "Redefines keys in Python mode for windmove"
    (define-key elpy-mode-map [M-left] (chri/ignore-error-wrapper 'windmove-left))
    (define-key elpy-mode-map [M-right] (chri/ignore-error-wrapper 'windmove-right))
    (define-key elpy-mode-map [M-up] (chri/ignore-error-wrapper 'windmove-up))
    (define-key elpy-mode-map [M-down] (chri/ignore-error-wrapper 'windmove-down)))
  (add-hook 'elpy-mode-hook 'windmove-python)
  :diminish)

;; Sound is annoying, just flash the screen on error.
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Projectile mode.
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (when chri/projectile-global (projectile-mode +1))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Counsel Projectile.
;; I have this disabled because I use C-c commands for counsel. I use Projectile
;; commands for having immediately output in a separate window.
;; (use-package counsel-projectile
;;  :config
;;  (counsel-projectile-mode)
;;  :after projectile)

;; ag and rg packages to support projectile.
(use-package ag
  :commands ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp ag-dired ag-dired-regexp ag-project-dired ag-project-dired-regexp
  :after projectile)
(use-package rg
  :custom
  (rg-keymap-prefix "\C-cS")
  :commands rg rg-menu
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu)
  :bind-keymap
  ("C-c S" . rg-global-map)
  :after projectile)


;; Enable spelling and flycheck everywhere.
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Move to/from *clipboard*:
;; S-Inset Paste
;; C-Insert Copy
;; S-Delete Cut
(use-package simpleclip
  :config
  (simpleclip-mode 1))

;; org mode.
(use-package org
  :defines org-capture-templates org-refile-targets
  :init
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
  :mode ("\\.org\\'" . org-mode))

;; notmuch email.
(when chri/notmuch
  (use-package notmuch
    :commands notmuch
    :defines smtpmail-smtp-server message-send-mail-function message-default-mail-headers smtpmail-debug-info message-auto-save-directory message-kill-buffer-on-exit message-directory
    :init
    (setq notmuch-search-oldest-first nil)
    (setq mail-user-agent 'message-user-agent)
    (setq user-mail-address "chripell@gmail.com"
	  user-full-name "Christian Pellegrin")
    (setq smtpmail-smtp-server "smtp.gmail.com"
	  message-send-mail-function 'message-smtpmail-send-it)
    (setq smtpmail-debug-info t)
    (setq message-default-mail-headers "Cc: \nBcc: \n")
    (setq message-auto-save-directory "~/mail/draft")
    (setq message-kill-buffer-on-exit t)
    (setq message-directory "~/mail/")))

;; Support C-x C-e from bash.
(add-to-list 'auto-mode-alist '("/bash-fc" . shell-script-mode))

;; I prefer zap-up-to-char.
(global-set-key "\M-z" 'zap-up-to-char)

;; Overwrite what is currently selected.
(delete-selection-mode 1)

;; Highlight Delimiters
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :functions color-saturate-name
  :config
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))
  :hook(
        (prog-mode . rainbow-delimiters-mode)
        (text-mode . rainbow-delimiters-mode)))

;; Show Hex Color Codes
(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook (
         (web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

;; My personalized shortcuts:
(global-set-key [kp-left] 'backward-sexp)
(global-set-key [kp-right] 'forward-sexp)
(global-set-key [kp-up] 'backward-up-list)
(global-set-key [kp-down] 'down-list)
(global-set-key [kp-prior] 'beginning-of-defun)
(global-set-key [kp-next] 'end-of-defun)
(global-set-key [kp-home] 'beginning-of-buffer)
(global-set-key [kp-end] 'end-of-buffer)
(global-set-key [kp-insert] 'set-mark-command)
(global-set-key [kp-delete] 'pop-global-mark)
(global-set-key [kp-divide] 'copy-to-register)
(global-set-key [kp-multiply] 'insert-register)
(global-set-key [kp-subtract] 'point-to-register)
(global-set-key [kp-add] 'jump-to-register)
(global-set-key [f7] 'delete-other-windows)
(global-set-key [f8] 'delete-window)

;; Show startup time.
(defun chri/display-startup-time ()
  "Show startup time."
  (message "🚀 Emacs loaded in %s with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'chri/display-startup-time)

(provide 'init.el)
;;; init.el ends here
