;;; init.el -- chripell's initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; chripell's initialization file

;;; Code:

;;; Load machine specific initialization. It is expected to define as t/nil:
(defvar chri/proglang)
(defvar chri/notmuch)
(defvar chri/projectile-global)
(defvar chri/enable-tabnine)
(defvar chri/prefer-eglot)
(require 'init-mach "~/.emacs.d/init-mach.el")

;; Optimizations for LSP mode (run lsp-doctor):
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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
 '(dired-dwim-target t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
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
   '(emacsql-sqlite-module lsp-mode flycheck-rust dumb-jump projectile go-guru go-mode lua-mode which-key flycheck company yasnippet ivy magit scad-mode counsel-gtags gtags js2-mode eglot company-tabnine rainbow-mode rainbow-delimiters rg ag dts-mode ucs-utils font-utils persistent-soft unicode-fonts ivy-yasnippet yasnippet-snippets lsp coq js-mode notmuch coq-mode go-playground javascript-mode diminish yaml-imenu ws-butler which-key-posframe wanderlust use-package typescript-mode tree-mode toml-mode toml smex simpleclip rustic rust-mode proof-general projectile-speedbar menu-bar+ markdown-preview-mode magit-gh-pulls lsp-ui lsp-pyright lsp-jedi lsp-ivy lsp-dart kotlin-mode jsonrpc jedi ivy-rich ipython-shell-send iedit ido-completing-read+ haskell-mode go-projectile go-autocomplete ghub+ forge flymake flycheck-yamllint flycheck-pyflakes flycheck-posframe flycheck-ocaml flycheck-mypy flycheck-kotlin flx-ido find-file-in-project elpy elpher ein dash-functional counsel company-posframe company-lua company-lsp company-coq ccls cargo browse-kill-ring+ bpftrace-mode bazel async android-mode))
 '(projectile-tags-command "make_TAGS \"%s\" %s")
 '(rustic-display-spinner nil)
 '(rustic-format-trigger 'on-save)
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "make LLVM=1 CC=\"ccache clang\" -j4 && make LLVM=1 bzImage CC=\"ccache clang\" -j4")
     (projectile-project-compilation-cmd . "make CC=\"ccache gcc\" -j4 && make bzImage CC=\"ccache gcc\" -j4")))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode 'right)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(unicode-fonts-block-font-mapping
   '(("Aegean Numbers"
      ("Noto Sans Symbols" "Aegean" "Symbola" "Quivira" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Ahom"
      ("AhomUnicode"))
     ("Alchemical Symbols"
      ("Noto Sans Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Alphabetic Presentation Forms"
      ("DejaVu Sans:width=condensed" "Arial Unicode MS" "Cardo" "Code2000" "Quivira" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Anatolian Hieroglyphs"
      ("Anatolian"))
     ("Ancient Greek Musical Notation"
      ("Cardo" "Noto Sans Symbols" "Aegean" "New Athena Unicode" "Musica" "Symbola" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Ancient Greek Numbers"
      ("Noto Sans Symbols" "Apple Symbols" "New Athena Unicode" "Cardo" "Aegean" "Quivira" "Symbola" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Ancient Symbols"
      ("Noto Sans Symbols" "Analecta" "New Athena Unicode" "Cardo" "Aegean" "Quivira" "Symbola" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Arabic"
      ("Courier New" "Simplified Arabic Fixed" "Simplified Arabic" "Amiri" "Aldhabi" "Adobe Arabic" "Urdu Typesetting" "Geeza Pro" "Baghdad" "Damascus" "Al Bayan" "Andalus" "Arabic Typesetting" "Traditional Arabic" "Scheherazade" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Nadeem" "Microsoft Uighur" "Tahoma" "Microsoft Sans Serif" "MPH 2B Damase" "KufiStandardGK" "DecoType Naskh" "Koodak" "FreeMono" "Code2000"))
     ("Arabic Extended-A"
      ("Scheherazade" "Amiri"))
     ("Arabic Mathematical Alphabetic Symbols"
      ("Amiri"))
     ("Arabic Presentation Forms-A"
      ("Geeza Pro" "Amiri" "Arial Unicode MS" "Microsoft Sans Serif" "Tahoma" "KufiStandardGK" "Andalus" "Arabic Typesetting" "Urdu Typesetting" "Adobe Arabic" "DecoType Naskh" "Al Bayan" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "MPH 2B Damase" "Code2000"))
     ("Arabic Presentation Forms-B"
      ("DejaVu Sans Mono" "Geeza Pro" "Amiri" "Adobe Arabic" "Traditional Arabic" "Urdu Typesetting" "Arial Unicode MS" "Microsoft Sans Serif" "KufiStandardGK" "DejaVu Sans:width=condensed" "FreeMono" "DecoType Naskh" "Code2000"))
     ("Arabic Supplement"
      ("Courier New" "Simplified Arabic Fixed" "Amiri" "Simplified Arabic" "Geeza Pro" "Damascus" "Andalus" "Arabic Typesetting" "Traditional Arabic" "Scheherazade" "Adobe Arabic" "Microsoft Uighur" "Tahoma" "Microsoft Sans Serif" "MPH 2B Damase"))
     ("Armenian"
      ("DejaVu Sans Mono" "Noto Sans Armenian" "Mshtakan" "Sylfaen" "DejaVu Sans:width=condensed" "Quivira" "MPH 2B Damase" "Code2000" "Arial Unicode MS" "Everson Mono:weight=bold" "FreeMono"))
     ("Arrows"
      ("DejaVu Sans Mono" "Apple Symbols" "Cambria Math" "Segoe UI Symbol" "DejaVu Sans:width=condensed" "Asana Math" "Arial Unicode MS" "BabelStone Modern" "Symbola" "Quivira" "Code2000" "Noto Sans Symbols" "Everson Mono:weight=bold" "FreeMono"))
     ("Avestan"
      ("Noto Sans Avestan" "Ahuramzda:weight=bold" "ALPHABETUM Unicode"))
     ("Balinese"
      ("Noto Sans Balinese:weight=bold" "Aksara Bali"))
     ("Bamum"
      ("Noto Sans Bamum"))
     ("Bamum Supplement"
      ("Noto Sans Bamum"))
     ("Batak"
      ("Batak-Unicode" "Noto Sans Batak"))
     ("Bengali"
      ("Bangla Sangam MN" "Noto Sans Bengali" "Noto Sans Bengali UI" "Nirmala UI" "Vrinda" "Mukti Narrow" "Akaash" "Arial Unicode MS" "Code2000" "ALPHABETUM Unicode"))
     ("Block Elements"
      ("DejaVu Sans Mono" "Noto Sans Symbols" "FreeMono" "DejaVu Sans:width=condensed" "Apple Symbols" "Segoe UI Symbol" "BabelStone Modern" "Symbola" "Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Bopomofo"
      ("Lantinghei TC" "MingLiU" "SimHei" "LiSong Pro" "FangSong" "SimSun" "DFKai-SB" "WenQuanYi Zen Hei Mono" "Microsoft JhengHei" "Microsoft JhengHei UI" "Microsoft YaHei" "Microsoft YaHei UI" "Lantinghei SC" "HAN NOM A" "Arial Unicode MS" "BabelStone Han" "Code2000" "ALPHABETUM Unicode"))
     ("Bopomofo Extended"
      ("MingLiU" "SimHei" "FangSong" "SimSun" "DFKai-SB" "Microsoft JhengHei" "Microsoft JhengHei UI" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "Code2000"))
     ("Box Drawing"
      ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans" "Everson Mono" "Quivira" "Code2000" "Noto Sans Symbols" "Segoe UI Symbol" "Symbola"))
     ("Brahmi"
      ("Segoe UI Historic" "Noto Sans Brahmi" "Adinatha Tamil Brahmi" "ALPHABETUM Unicode"))
     ("Braille Patterns"
      ("Quivira" "Apple Braille" "DejaVu Sans:width=condensed" "Apple Symbols" "Segoe UI Symbol" "Symbola" "Noto Sans Symbols" "FreeMono" "Code2000" "Everson Mono:weight=bold"))
     ("Buginese"
      ("Noto Sans Buginese" "MPH 2B Damase" "Monlam Uni Sans Serif" "Code2000"))
     ("Buhid"
      ("Noto Sans Buhid" "Quivira" "Code2000"))
     ("Byzantine Musical Symbols"
      ("Noto Sans Symbols" "Musica" "Symbola" "FreeSerif"))
     ("CJK Compatibility"
      ("SimHei" "FangSong" "SimSun" "MingLiU" "Meiryo" "Microsoft JhengHei" "Microsoft JhengHei UI" "Lantinghei SC" "Lantinghei TC" "HAN NOM A" "Arial Unicode MS" "WenQuanYi Zen Hei Mono" "HanaMinA" "BabelStone Han" "Code2000"))
     ("CJK Compatibility Forms"
      ("WenQuanYi Zen Hei Mono" "Lantinghei SC" "SimHei" "FangSong" "SimSun" "LiSong Pro" "Baoli SC" "Microsoft YaHei" "Microsoft YaHei UI" "Lantinghei TC" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "Symbola" "Xingkai SC" "DFKai-SB" "Code2000"))
     ("CJK Compatibility Ideographs"
      ("SimHei" "FangSong" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "WenQuanYi Zen Hei Mono" "BabelStone Han" "UnBatang" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "Arial Unicode MS" "Lantinghei SC" "HanaMinA"))
     ("CJK Compatibility Ideographs Supplement"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "SimSun" "MingLiU" "HanaMinA" "Hiragino Kaku Gothic Pro" "Hiragino Maru Gothic Pro" "Hiragino Mincho Pro" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM B" "LiSong Pro"))
     ("CJK Radicals Supplement"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "HanaMinA" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "DFKai-SB" "Apple Symbols" "Code2000"))
     ("CJK Strokes"
      ("WenQuanYi Zen Hei Mono" "HanaMinA" "BabelStone Han" "Code2000"))
     ("CJK Symbols and Punctuation"
      ("Lantinghei SC" "SimHei" "FangSong" "SimSun" "HanaMinA" "WenQuanYi Zen Hei Mono" "LiSong Pro" "STFangsong" "Microsoft YaHei" "Microsoft YaHei UI" "Lantinghei TC" "MingLiU" "HAN NOM A" "Arial Unicode MS" "PCMyungjo" "BabelStone Han" "Osaka:spacing=m" "Code2000"))
     ("CJK Unified Ideographs"
      ("WenQuanYi Zen Hei Mono" "Lantinghei SC" "Songti SC" "SimHei" "FangSong" "STFangsong" "SimSun" "LiSong Pro" "Baoli SC" "HanaMinA" "BabelStone Han" "Apple LiGothic" "Lantinghei TC" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "DFKai-SB" "Arial Unicode MS" "Xingkai SC" "GB18030 Bitmap" "UnBatang"))
     ("CJK Unified Ideographs Extension A"
      ("SimHei" "FangSong" "STFangsong" "SimSun" "Songti SC" "Microsoft YaHei" "Microsoft YaHei UI" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HanaMinA" "HAN NOM A" "Code2000" "DFKai-SB" "BabelStone Han" "GB18030 Bitmap"))
     ("CJK Unified Ideographs Extension B"
      ("SimHei" "FangSong" "SimSun" "LiSong Pro" "Microsoft YaHei" "Microsoft YaHei UI" "HanaMinB" "HAN NOM B" "Code2002" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "BabelStone Han" "DFKai-SB"))
     ("CJK Unified Ideographs Extension C"
      ("HanaMinB" "BabelStone Han" "HAN NOM B"))
     ("CJK Unified Ideographs Extension D"
      ("HanaMinB" "BabelStone Han"))
     ("CJK Unified Ideographs Extension E"
      ("HanaMinB" "BabelStone Han"))
     ("Carian"
      ("Segoe UI Historic" "Noto Sans Carian" "Aegean" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Chakma"
      ("Ribeng"))
     ("Cham"
      ("Noto Sans Cham" "Cham OI_Tangin" "Cham OI_Kulbleng" "Cham OI_Kul" "Code2000"))
     ("Cherokee"
      ("Aboriginal Sans" "Aboriginal Serif" "Plantagenet Cherokee" "Noto Sans Cherokee" "Gadugi" "MPH 2B Damase" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Code2000"))
     ("Cherokee Supplement"
      ("Everson Mono:weight=bold"))
     ("Combining Diacritical Marks"
      ("Monaco" "Consolas" "Noto Sans" "Cambria Math" "Charis SIL" "Doulos SIL" "Courier New" "DejaVu Sans:width=condensed" "DejaVu Sans Mono" "Cardo" "Code2000" "Gentium Plus" "Junicode" "Tahoma" "Microsoft Sans Serif" "Arial" "Quivira" "Symbola" "Everson Mono" "FreeMono" "Arial Unicode MS" "ALPHABETUM Unicode"))
     ("Combining Diacritical Marks Extended"
      ("Monlam Uni Sans Serif"))
     ("Combining Diacritical Marks Supplement"
      ("Cardo" "FreeSerif" "Junicode" "Doulos SIL" "DejaVu Sans:width=condensed" "Noto Sans" "Segoe UI" "Code2000" "Everson Mono" "ALPHABETUM Unicode"))
     ("Combining Diacritical Marks for Symbols"
      ("Cambria Math" "Segoe UI Symbol" "Noto Sans Symbols" "Symbola" "Code2000" "Everson Mono" "Arial Unicode MS"))
     ("Combining Half Marks"
      ("Consolas" "DejaVu Sans:width=condensed" "Everson Mono:weight=bold" "Symbola"))
     ("Common Indic Number Forms"
      ("Noto Sans Kaithi" "Nirmala UI" "Siddhanta"))
     ("Control Pictures"
      ("Apple Symbols" "BabelStone Modern" "Noto Sans Symbols" "Segoe UI Symbol" "Arial Unicode MS" "Symbola" "Quivira" "FreeMono" "Code2000" "Everson Mono:weight=bold"))
     ("Coptic"
      ("Noto Sans Coptic" "Antinoou" "New Athena Unicode" "Segoe UI Historic" "Segoe UI Symbol" "Quivira" "Analecta" "Nilus" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Coptic Epact Numbers"
      ("Nilus" "Symbola"))
     ("Counting Rod Numerals"
      ("WenQuanYi Zen Hei Mono" "Noto Sans Symbols" "BabelStone Modern" "Symbola" "Quivira" "Apple Symbols" "Code2001"))
     ("Cuneiform"
      ("Segoe UI Historic" "Noto Sans Cuneiform" "Noto Sans Sumero-Akkadian Cuneiform" "Akkadian"))
     ("Cuneiform Numbers and Punctuation"
      ("Akkadian" "Segoe UI Historic" "Noto Sans Cuneiform" "Noto Sans Sumero-Akkadian Cuneiform"))
     ("Currency Symbols"
      ("Monaco" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Consolas" "Noto Sans Symbols" "Noto Sans" "Segoe UI" "Apple Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Cypriot Syllabary"
      ("Segoe UI Historic" "Noto Sans Cypriot" "Aegean" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Cyrillic"
      ("Consolas" "Monaco" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Noto Sans" "Courier New" "Calibri" "Microsoft Sans Serif" "Code2000" "Arial Unicode MS" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Charcoal CY" "Geneva CY" "ALPHABETUM Unicode"))
     ("Cyrillic Extended-A"
      ("Quivira" "Everson Mono:weight=bold" "FreeSerif" "ALPHABETUM Unicode"))
     ("Cyrillic Extended-B"
      ("Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Cyrillic Supplement"
      ("Consolas" "Courier New" "Calibri" "Noto Sans" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Deseret"
      ("Noto Sans Deseret" "Apple Symbols" "Segoe UI Symbol" "Analecta" "Code2001" "Everson Mono:weight=bold"))
     ("Devanagari"
      ("Annapurna SIL" "Noto Sans Devanagari" "Devanagari Sangam MN" "Devanagari MT" "Nirmala UI" "Mangal" "Samyak Devanagari" "Samyak" "Siddhanta" "Aparajita" "Code2000" "Arial Unicode MS" "ALPHABETUM Unicode"))
     ("Devanagari Extended"
      ("Annapurna SIL" "Siddhanta" "FreeSerif"))
     ("Dingbats"
      ("Apple Color Emoji" "DejaVu Sans Mono" "Segoe UI Symbol" "Zapf Dingbats" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Code2000" "Noto Sans Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Domino Tiles"
      ("DejaVu Sans:width=condensed" "Symbola" "Quivira" "Segoe UI Symbol" "Noto Sans Symbols" "Code2001" "Everson Mono:weight=bold"))
     ("Early Dynastic Cuneiform"
      ("Akkadian"))
     ("Egyptian Hieroglyphs"
      ("Segoe UI Historic:weight=bold" "Noto Sans Egyptian Hieroglyphs:weight=bold" "Aegyptus:weight=bold" "Gardiner"))
     ("Elbasan"
      ("Albanian" "Everson Mono:weight=bold"))
     ("Emoticons"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Quivira"))
     ("Enclosed Alphanumeric Supplement"
      ("Segoe UI Symbol" "Noto Sans Symbols" "Symbola" "Quivira" "BabelStone Han" "BabelStone Modern"))
     ("Enclosed Alphanumerics"
      ("Noto Sans Symbols" "Segoe UI Symbol" "Junicode" "Arial Unicode MS" "Symbola" "Quivira" "Code2000" "BabelStone Han" "WenQuanYi Zen Hei Mono" "BabelStone Modern" "HAN NOM A" "Everson Mono:weight=bold"))
     ("Enclosed CJK Letters and Months"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "MingLiU" "Arial Unicode MS" "HanaMinA" "Meiryo" "BabelStone Han" "Quivira" "Code2000" "UnBatang" "HAN NOM A"))
     ("Enclosed Ideographic Supplement"
      ("Segoe UI Symbol" "Noto Sans Symbols" "HanaMinA" "BabelStone Han" "Symbola"))
     ("Ethiopic"
      ("Kefa" "Noto Sans Ethiopic" "Nyala" "Abyssinica SIL" "Ethiopia Jiret" "Ethiopic WashRa SemiBold" "Ethiopic Yebse" "Code2000"))
     ("Ethiopic Extended"
      ("Kefa" "Noto Sans Ethiopic" "Nyala" "Abyssinica SIL" "Code2000"))
     ("Ethiopic Extended-A"
      ("Kefa" "Noto Sans Ethiopic" "Abyssinica SIL"))
     ("Ethiopic Supplement"
      ("Kefa" "Noto Sans Ethiopic" "Nyala" "Abyssinica SIL" "Code2000"))
     ("General Punctuation"
      ("Monaco" "Apple Symbols" "Segoe UI Symbol" "Cambria Math" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Antinoou" "Symbola" "Code2000" "Quivira" "Noto Sans" "Everson Mono:weight=bold" "FreeMono" "BabelStone Modern"))
     ("Geometric Shapes"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Segoe UI Symbol" "Arial Unicode MS" "Symbola" "Noto Sans Symbols" "Quivira" "BabelStone Modern" "Everson Mono" "FreeMono" "Code2000"))
     ("Geometric Shapes Extended"
      ("Symbola" "Quivira"))
     ("Georgian"
      ("DejaVu Sans Mono" "Noto Sans Georgian" "Noto Serif Georgian" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Code2000" "Quivira" "Sylfaen" "MPH 2B Damase" "Everson Mono:weight=bold"))
     ("Georgian Supplement"
      ("Noto Sans Georgian" "Noto Serif Georgian" "DejaVu Serif:width=condensed" "MPH 2B Damase" "Quivira" "Everson Mono:weight=bold"))
     ("Glagolitic"
      ("Noto Sans Glagolitic" "Segoe UI Historic" "Segoe UI Symbol" "MPH 2B Damase" "Quivira" "FreeSerif" "ALPHABETUM Unicode"))
     ("Gothic"
      ("Noto Sans Gothic" "Segoe UI Historic" "Segoe UI Symbol" "Analecta" "Junicode" "Sadagolthina" "MPH 2B Damase" "FreeSerif" "Code2001" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Greek Extended"
      ("Consolas" "DejaVu Sans Mono" "Courier New" "Antinoou" "Noto Sans" "DejaVu Sans:width=condensed" "Cardo" "Junicode" "New Athena Unicode" "Microsoft Sans Serif" "Gentium Plus Compact" "Gentium Plus" "Arial Unicode MS" "Arial" "Tahoma" "Aegean" "Code2000" "Quivira" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Greek and Coptic"
      ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Antinoou" "Noto Sans" "Segoe UI Historic" "Segoe UI Symbol" "New Athena Unicode" "Calibri" "Microsoft Sans Serif" "Gentium Plus Compact" "Gentium Plus" "Lucida Console" "Arial Unicode MS" "Cardo" "Aegean" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode" "Noto Sans Coptic"))
     ("Gujarati"
      ("Nirmala UI" "Noto Sans Gujarati" "Noto Sans Gujarati UI" "Gujarati MT" "Shruti" "Samyak Gujarati" "Samyak" "Gujarati Sangam MN" "Code2000" "Arial Unicode MS"))
     ("Gurmukhi"
      ("Gurmukhi Sangam MN" "Gurmukhi MN" "Nirmala UI" "Noto Sans Gurmukhi" "Noto Sans Gurmukhi UI" "Raavi" "Code2000" "Arial Unicode MS" "AnmolUni"))
     ("Halfwidth and Fullwidth Forms"
      ("Meiryo" "Arial Unicode MS" "Microsoft JhengHei" "Microsoft JhengHei UI" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "Apple Symbols" "Quivira" "Code2000" "HAN NOM A"))
     ("Hangul Compatibility Jamo"
      ("PCMyungjo" "Malgun Gothic" "Gulim" "Dotum" "Batang" "Gungsuh" "AppleMyungjo" "UnBatang" "WenQuanYi Zen Hei Mono" "HAN NOM A" "Arial Unicode MS" "Code2000" "HeadLineA"))
     ("Hangul Jamo"
      ("UnBatang" "WenQuanYi Zen Hei Mono" "PCMyungjo" "Malgun Gothic" "Gulim" "Dotum" "Batang" "Gungsuh" "Arial Unicode MS" "Code2000"))
     ("Hangul Jamo Extended-A"
      ("Malgun Gothic" "HanaMinA" "UnBatang"))
     ("Hangul Jamo Extended-B"
      ("Malgun Gothic" "HanaMinA" "UnBatang"))
     ("Hangul Syllables"
      ("AppleGothic" "Malgun Gothic" "Gulim" "Dotum" "Batang" "Gungsuh" "UnBatang" "WenQuanYi Zen Hei Mono" "Arial Unicode MS" "Code2000"))
     ("Hanunoo"
      ("Noto Sans Hanunoo" "MPH 2B Damase" "Quivira" "FreeSerif"))
     ("Hebrew"
      ("Miriam Fixed" "Ezra SIL" "Ezra SIL SR" "Arial Hebrew" "Raanana" "New Peninim MT" "Aharoni" "David" "FrankRuehl" "Gisha" "Levenim MT" "Narkisim" "Rod" "Cardo" "Courier New" "Adobe Hebrew" "Code2000" "Aramaic Imperial Yeb" "Microsoft Sans Serif" "Tahoma" "Lucida Sans Unicode" "Arial Unicode MS" "Arial" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Hiragana"
      ("Osaka:spacing=m" "MS Gothic" "MS Mincho" "MingLiU" "Hiragino Kaku Gothic Pro" "Meiryo" "Arial Unicode MS" "HanaMinA" "BabelStone Han" "Microsoft JhengHei" "Microsoft YaHei" "Microsoft YaHei UI" "HAN NOM A" "Code2000" "ALPHABETUM Unicode"))
     ("IPA Extensions"
      ("Monaco" "Consolas" "DejaVu Sans Mono" "Courier New" "Noto Sans" "Arial Unicode MS" "Arial" "Tahoma" "Microsoft Sans Serif" "Aboriginal Sans" "Cardo" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Code2000" "ALPHABETUM Unicode"))
     ("Ideographic Description Characters"
      ("SimHei" "FangSong" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "AppleMyungjo" "HanaMinA" "HAN NOM A" "Quivira" "DFKai-SB" "Code2000"))
     ("Imperial Aramaic"
      ("Aramaic Imperial Yeb" "Quivira" "Segoe UI Historic" "Noto Sans Imperial Aramaic" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Inscriptional Pahlavi"
      ("ZH Mono" "Segoe UI Historic" "Noto Sans Inscriptional Pahlavi" "ALPHABETUM Unicode" "Ahuramzda:weight=bold"))
     ("Inscriptional Parthian"
      ("ZH Mono" "Segoe UI Historic" "Noto Sans Inscriptional Parthian" "ALPHABETUM Unicode"))
     ("Javanese"
      ("Noto Sans Javanese" "Tuladha Jejeg"))
     ("Kaithi"
      ("Noto Sans Kaithi"))
     ("Kana Supplement"
      ("Meiryo UI" "HanaMinA" "BabelStone Han"))
     ("Kanbun"
      ("SimHei" "FangSong" "SimSun" "Meiryo" "Arial Unicode MS" "WenQuanYi Zen Hei Mono" "HanaMinA" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft YaHei" "Microsoft YaHei UI" "HAN NOM A" "Code2000"))
     ("Kangxi Radicals"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "Meiryo" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "HanaMinA" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "DFKai-SB" "AppleMyungjo" "Apple Symbols" "Code2000"))
     ("Kannada"
      ("Kannada Sangam MN" "Noto Sans Kannada" "Noto Sans Kannada UI" "Tunga" "Akshar Unicode" "Kedage" "Nirmala UI" "Kannada MN" "Arial Unicode MS" "Code2000"))
     ("Katakana"
      ("Osaka:spacing=m" "MS Gothic" "MingLiU" "Meiryo" "HanaMinA" "Arial Unicode MS" "BabelStone Han" "Microsoft JhengHei" "Microsoft YaHei" "Microsoft YaHei UI" "HAN NOM A" "Code2000" "ALPHABETUM Unicode"))
     ("Katakana Phonetic Extensions"
      ("MS Gothic" "MingLiU" "Meiryo" "HanaMinA" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "HAN NOM A" "Code2000"))
     ("Kayah Li"
      ("Noto Sans Kayah Li" "Code2000" "FreeMono"))
     ("Kharoshthi"
      ("Segoe UI Historic" "Noto Sans Kharoshthi" "MPH 2B Damase" "ALPHABETUM Unicode"))
     ("Khmer"
      ("Noto Sans Khmer" "Noto Sans Khmer UI" "Noto Serif Khmer" "Khmer Sangam MN" "DaunPenh" "Code2000" "MoolBoran" "Khmer Mondulkiri" "Khmer Busra"))
     ("Khmer Symbols"
      ("Noto Sans Khmer" "Noto Sans Khmer UI" "Noto Serif Khmer" "Khmer Sangam MN" "MoolBoran" "Khmer Mondulkiri" "Khmer Busra" "Code2000"))
     ("Khojki"
      ("KhojkiUnicodeOT"))
     ("Khudawadi"
      ("OldSindhi"))
     ("Lao"
      ("Noto Sans Lao" "Noto Sans Lao UI" "Noto Serif Lao" "Lao Sangam MN" "DokChampa" "DejaVu Sans Mono" "Arial Unicode MS" "Saysettha MX" "DejaVu Sans:width=condensed" "Code2000"))
     ("Latin Extended-C"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Noto Sans" "Cambria Math" "Gentium Plus" "Charis SIL" "Doulos SIL" "Code2000" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Latin Extended-D"
      ("FreeMono" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Junicode" "Cardo" "Quivira" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Latin Extended-E"
      ("Quivira" "Everson Mono:weight=bold" "HanaMinA"))
     ("Lepcha"
      ("Mingzat" "Noto Sans Lepcha"))
     ("Letterlike Symbols"
      ("Monaco" "Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "Cambria Math" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Code2000" "Symbola" "Quivira" "HAN NOM A" "Everson Mono:weight=bold"))
     ("Limbu"
      ("Noto Sans Limbu" "Namdhinggo SIL" "MPH 2B Damase" "Code2000"))
     ("Linear A"
      ("Aegean"))
     ("Linear B Ideograms"
      ("Noto Sans Linear B" "Aegean" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode" "MPH 2B Damase"))
     ("Linear B Syllabary"
      ("Noto Sans Linear B" "Aegean" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode" "MPH 2B Damase" "Penuturesu"))
     ("Lisu"
      ("Lisu Unicode" "Miao Unicode" "Noto Sans Lisu" "Lisu Tzimu" "Quivira" "Everson Mono:weight=bold"))
     ("Lycian"
      ("Segoe UI Historic" "Noto Sans Lycian" "Aegean" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Lydian"
      ("Segoe UI Historic" "Noto Sans Lydian" "Aegean" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Mahjong Tiles"
      ("Segoe UI Symbol" "Symbola" "Noto Sans Symbols" "Quivira" "Everson Mono"))
     ("Malayalam"
      ("Malayalam Sangam MN" "Nirmala UI" "Kartika" "Code2000" "Akshar Unicode" "Samyak Malayalam" "Samyak" "Arial Unicode MS"))
     ("Mandaic"
      ("Noto Sans Mandaic"))
     ("Mathematical Alphanumeric Symbols"
      ("Cambria Math" "Noto Sans Symbols" "Asana Math" "Code2001" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Mathematical Operators"
      ("Monaco" "DejaVu Sans Mono" "Segoe UI Symbol" "Cambria Math" "DejaVu Sans:width=condensed" "Noto Sans Symbols" "Apple Symbols" "Asana Math" "Arial Unicode MS" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Meetei Mayek"
      ("Noto Sans Meetei Mayek" "Eeyek Unicode" "Meetei Mayek"))
     ("Meetei Mayek Extensions"
      ("Noto Sans Meetei Mayek"))
     ("Meroitic Cursive"
      ("Nilus" "Segoe UI Historic" "Segoe UI Symbol"))
     ("Meroitic Hieroglyphs"
      ("Nilus"))
     ("Miao"
      ("Miao Unicode" "Albanian"))
     ("Miscellaneous Mathematical Symbols-A"
      ("Noto Sans Symbols" "Apple Symbols" "Segoe UI Symbol" "Asana Math" "Code2000" "Symbola" "Quivira" "Cambria Math" "Everson Mono:weight=bold"))
     ("Miscellaneous Mathematical Symbols-B"
      ("Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "Cambria Math" "Asana Math" "Code2000" "Symbola" "Quivira"))
     ("Miscellaneous Symbols"
      ("Apple Color Emoji" "Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Symbola" "Quivira" "MS Reference Sans Serif" "Cardo" "Code2000" "Everson Mono:weight=bold"))
     ("Miscellaneous Symbols and Arrows"
      ("Symbola" "Quivira" "Asana Math" "Code2000" "Segoe UI Symbol" "Noto Sans Symbols"))
     ("Miscellaneous Symbols and Pictographs"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Quivira"))
     ("Miscellaneous Technical"
      ("Segoe UI Symbol" "Noto Sans Symbols" "Apple Symbols" "Cambria Math" "DejaVu Sans Mono" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Modi"
      ("MarathiCursiveG"))
     ("Modifier Tone Letters"
      ("Apple Symbols" "Noto Sans Symbols" "Gentium Plus" "Code2000" "Quivira" "Charis SIL" "Doulos SIL" "DejaVu Sans Mono"))
     ("Mongolian"
      ("STFangsong" "STHeiti" "STKaiti" "STSong" "Noto Sans Mongolian" "Mongolian Baiti" "Daicing Xiaokai" "Code2000"))
     ("Mro"
      ("Mro Unicode"))
     ("Musical Symbols"
      ("Noto Sans Symbols" "Musica" "FreeSerif" "Symbola" "Quivira"))
     ("Myanmar"
      ("Noto Sans Myanmar" "Noto Sans Myanmar UI" "Myanmar Text" "Myanmar Sangam MN" "Myanmar MN" "TharLon" "Yunghkio" "Myanmar3" "Masterpiece Uni Sans" "Padauk" "Code2000" "Tai Le Valentinium"))
     ("Myanmar Extended-A"
      ("Noto Sans Myanmar" "Noto Sans Myanmar UI" "Myanmar Text" "Padauk" "TharLon" "Yunghkio"))
     ("Myanmar Extended-B"
      ("TharLon" "Yunghkio"))
     ("NKo"
      ("Ebrima" "Conakry" "DejaVu Sans:width=condensed" "Noto Sans NKo" "Code2000"))
     ("Nabataean"
      ("Everson Mono:weight=bold"))
     ("New Tai Lue"
      ("Noto Sans New Tai Lue" "Microsoft New Tai Lue" "Dai Banna SIL Book" "Dai Banna SIL Book:style=Regular"))
     ("Number Forms"
      ("DejaVu Sans:width=condensed" "Asana Math" "Arial Unicode MS" "Junicode" "Symbola" "Quivira" "Charis SIL" "Doulos SIL" "Code2000" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Ogham"
      ("Segoe UI Historic" "Segoe UI Symbol" "Noto Sans Ogham" "DejaVu Sans:width=condensed" "BabelStone Modern" "Code2000" "Aboriginal Serif" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Ol Chiki"
      ("Nirmala UI" "Noto Sans Ol Chiki" "Code2000"))
     ("Old Hungarian"
      ("OldHungarian"))
     ("Old Italic"
      ("Segoe UI Historic" "Segoe UI Symbol" "DejaVu Sans:width=condensed" "Cardo" "New Athena Unicode" "Aegean" "Noto Sans Old Italic" "Albanian" "Code2001" "Quivira" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Old North Arabian"
      ("Marib"))
     ("Old Permic"
      ("Everson Mono:weight=bold"))
     ("Old Persian"
      ("Segoe UI Historic" "Noto Sans Old Persian" "MPH 2B Damase" "Aegean" "Code2001" "FreeSans" "ALPHABETUM Unicode"))
     ("Old South Arabian"
      ("Segoe UI Historic" "Noto Sans Old South Arabian" "Quivira" "Qataban" "Everson Mono:weight=bold"))
     ("Old Turkic"
      ("Noto Sans Old Turkic" "Segoe UI Historic" "Segoe UI Symbol" "Quivira" "Everson Mono:weight=bold"))
     ("Optical Character Recognition"
      ("Apple Symbols" "Segoe UI Symbol" "Noto Sans Symbols" "Arial Unicode MS" "Symbola" "Quivira" "FreeMono" "BabelStone Modern" "Code2000" "Everson Mono"))
     ("Oriya"
      ("Noto Sans Oriya" "Oriya Sangam MN" "Nirmala UI" "Kalinga" "Samyak Oriya" "Samyak" "Code2000" "Arial Unicode MS"))
     ("Ornamental Dingbats"
      ("Symbola"))
     ("Osmanya"
      ("Noto Sans Osmanya" "Ebrima" "Andagii" "MPH 2B Damase" "Code2001" "Everson Mono:weight=bold"))
     ("Phags-pa"
      ("BabelStone Phags-pa Book" "BabelStone Phags-pa Book:style=Regular" "Noto Sans Phags-pa" "Microsoft PhagsPa" "Code2000"))
     ("Phaistos Disc"
      ("Aegean" "Noto Sans Symbols" "Symbola" "Everson Mono:weight=bold" "Code2001" "ALPHABETUM Unicode"))
     ("Phoenician"
      ("Segoe UI Historic" "Noto Sans Phoenician" "Aegean" "Quivira" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Phonetic Extensions"
      ("Monaco" "Consolas" "Calibri" "Noto Sans" "Aboriginal Sans" "Charis SIL" "Doulos SIL" "Quivira" "Courier New" "DejaVu Sans:width=condensed" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Phonetic Extensions Supplement"
      ("Consolas" "Calibri" "Courier New" "Noto Sans" "Aboriginal Sans" "Charis SIL" "Doulos SIL" "Quivira" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Playing Cards"
      ("DejaVu Sans:width=condensed" "Symbola" "Noto Sans Symbols" "Segoe UI Symbol" "Quivira"))
     ("Rejang"
      ("Noto Sans Rejang" "Code2000" "Everson Mono:weight=bold"))
     ("Rumi Numeral Symbols"
      ("HanaMinA"))
     ("Runic"
      ("Noto Sans Runic" "Segoe UI Historic" "Segoe UI Symbol" "Aboriginal Serif" "Junicode" "FreeMono" "Quivira" "Code2000" "Cardo" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Samaritan"
      ("Noto Sans Samaritan" "Quivira" "Everson Mono:weight=bold"))
     ("Saurashtra"
      ("Noto Sans Saurashtra" "Code2000" "Sourashtra"))
     ("Sharada"
      ("Albanian"))
     ("Shavian"
      ("Segoe UI Historic" "Noto Sans Shavian" "Andagii" "MPH 2B Damase" "Apple Symbols" "Code2001" "Everson Mono:weight=bold"))
     ("Siddham"
      ("MuktamsiddhamG"))
     ("Sinhala"
      ("Noto Sans Sinhala" "Nirmala UI" "Iskoola Pota" "Akshar Unicode" "Sinhala Sangam MN"))
     ("Small Form Variants"
      ("Apple Symbols" "Arial Unicode MS" "WenQuanYi Zen Hei Mono" "Microsoft YaHei" "Microsoft YaHei UI" "Code2000"))
     ("Sora Sompeng"
      ("Nirmala UI"))
     ("Specials"
      ("BabelStone Modern" "Noto Sans Symbols" "Apple Symbols" "Arial Unicode MS" "Symbola" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Quivira" "FreeMono" "BabelStone Han"))
     ("Sundanese"
      ("Noto Sans Sundanese" "Sundanese Unicode"))
     ("Sundanese Supplement"
      ("Noto Sans Sundanese"))
     ("Superscripts and Subscripts"
      ("Consolas" "Monaco" "Apple Symbols" "Cambria Math" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Segoe UI Symbol" "Asana Math" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Supplemental Arrows-A"
      ("Segoe UI Symbol" "Cambria Math" "DejaVu Sans:width=condensed" "Asana Math" "Quivira" "Symbola" "Apple Symbols" "Noto Sans Symbols" "Code2000" "Everson Mono:weight=bold" "FreeMono" "BabelStone Modern"))
     ("Supplemental Arrows-B"
      ("Cambria Math" "Segoe UI Symbol" "Apple Symbols" "Noto Sans Symbols" "Asana Math" "Quivira" "Symbola" "Code2000" "Everson Mono:weight=bold"))
     ("Supplemental Arrows-C"
      ("Symbola"))
     ("Supplemental Mathematical Operators"
      ("Cambria Math" "Segoe UI Symbol" "Noto Sans Symbols" "Apple Symbols" "Asana Math" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Supplemental Punctuation"
      ("DejaVu Sans Mono" "Segoe UI Symbol" "Noto Sans Symbols" "Antinoou" "New Athena Unicode" "Cardo" "Aegean" "Symbola" "Quivira" "Everson Mono:weight=bold" "Code2000" "ALPHABETUM Unicode"))
     ("Supplemental Symbols and Pictographs"
      ("Symbola"))
     ("Syloti Nagri"
      ("Noto Sans Syloti Nagri" "MPH 2B Damase"))
     ("Syriac"
      ("Segoe UI Historic" "Estrangelo Edessa" "Estrangelo Nisibin" "Code2000"))
     ("Tagalog"
      ("Quivira" "Noto Sans Tagalog"))
     ("Tagbanwa"
      ("Noto Sans Tagbanwa" "Quivira"))
     ("Tags"
      ("BabelStone Modern" "BabelStone Han"))
     ("Tai Le"
      ("Microsoft Tai Le" "TharLon" "Noto Sans Tai Le" "Yunghkio" "Tai Le Valentinium" "MPH 2B Damase" "FreeSerif"))
     ("Tai Tham"
      ("Noto Sans Tai Tham" "Lanna Alif" "Chiangsaen Alif" "Lanna Unicode UI" "Monlam Uni Sans Serif"))
     ("Tai Viet"
      ("Tai Heritage Pro" "Noto Sans Tai Viet"))
     ("Tai Xuan Jing Symbols"
      ("WenQuanYi Zen Hei Mono" "Apple Symbols" "Noto Sans Symbols" "Segoe UI Symbol" "BabelStone Han" "DejaVu Sans:width=condensed" "Symbola" "Quivira" "BabelStone Modern" "Code2001" "Everson Mono:weight=bold"))
     ("Takri"
      ("Albanian"))
     ("Tamil"
      ("Latha" "Noto Sans Tamil" "Noto Sans Tamil UI" "Nirmala UI" "Tamil MN" "Tamil Sangam MN" "InaiMathi" "Vijaya" "Maduram" "Akshar Unicode" "Samyak Tamil" "Samyak" "Code2000" "Arial Unicode MS"))
     ("Telugu"
      ("Noto Sans Telugu" "Noto Sans Telugu UI" "Telugu Sangam MN" "Vani" "Nirmala UI" "Gautami" "Akshar Unicode" "Code2000" "Arial Unicode MS"))
     ("Thaana"
      ("MV Boli" "Noto Sans Thaana" "MPH 2B Damase" "Code2000" "Everson Mono:weight=bold"))
     ("Thai"
      ("Thonburi" "DokChampa" "Noto Sans Thai" "Noto Sans Thai UI" "Noto Serif Thai" "Ayuthaya" "Silom" "Krungthep" "Sathu" "Angsana New" "AngsanaUPC" "Code2000" "Tahoma" "Arial Unicode MS" "Quivira" "Everson Mono:weight=bold"))
     ("Tibetan"
      ("Noto Sans Tibetan" "Kailasa" "Kokonor" "Tibetan Machine Uni" "Microsoft Himalaya" "Jomolhari" "Monlam Uni Sans Serif" "Arial Unicode MS"))
     ("Tifinagh"
      ("Noto Sans Tifinagh" "Ebrima" "DejaVu Sans:width=condensed" "Code2000" "Quivira" "Everson Mono:weight=bold"))
     ("Transport and Map Symbols"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola"))
     ("Ugaritic"
      ("Segoe UI Historic" "Noto Sans Ugaritic" "Aegean" "Code2001" "Andagii" "Quivira" "Everson Mono:weight=bold" "FreeSans" "ALPHABETUM Unicode"))
     ("Unified Canadian Aboriginal Syllabics"
      ("Aboriginal Sans" "Aboriginal Serif" "Noto Sans Canadian Aboriginal" "Gadugi" "Euphemia UCAS" "Euphemia" "Code2000" "Quivira" "Everson Mono:weight=bold"))
     ("Unified Canadian Aboriginal Syllabics Extended"
      ("Aboriginal Sans" "Aboriginal Serif" "Noto Sans Canadian Aboriginal" "Gadugi" "Euphemia UCAS" "Euphemia" "Quivira" "Everson Mono:weight=bold"))
     ("Vai"
      ("Ebrima" "Noto Sans Vai" "Dukor" "Wakor" "Code2000" "Quivira"))
     ("Variation Selectors"
      ("BabelStone Modern" "BabelStone Han" "Code2000"))
     ("Variation Selectors Supplement"
      ("BabelStone Modern" "BabelStone Han"))
     ("Vedic Extensions"
      ("Siddhanta"))
     ("Vertical Forms"
      ("Microsoft YaHei" "Microsoft YaHei UI" "Symbola"))
     ("Yi Radicals"
      ("Noto Sans Yi" "Nuosu SIL" "Microsoft Yi Baiti" "STFangsong" "Code2000"))
     ("Yi Syllables"
      ("Noto Sans Yi" "Nuosu SIL" "Microsoft Yi Baiti" "STFangsong" "Code2000"))
     ("Yijing Hexagram Symbols"
      ("WenQuanYi Zen Hei Mono" "Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "DejaVu Sans:width=condensed" "BabelStone Han" "Symbola" "Quivira" "BabelStone Modern" "Code2000" "Everson Mono:weight=bold"))))
 '(unicode-fonts-debug-availability t)
 '(unicode-fonts-skip-font-groups '(low-quality-glyphs))
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

;;; Prevent Extraneous Tabs.
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

;; Enable easy-gpg.
(use-package epa
  :config
  ;; Let Emacs query the passphrase through the minibuffer
  ;; Add 'allow-loopback-pinentry' to ~/.gnupg/gpg-agent.conf
  (setq epg-pinentry-mode 'loopback)
  (require 'epa-file)
  (epa-file-enable))

;; magit.
(use-package magit
  :defines magit-bind-magit-project-status
  :commands magit-process-git
  :config
  ;; This is needed to avoid a bug with project.el integration.
  (setq magit-bind-magit-project-status nil)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)
         ("s-m" . magit-file-dispatch)))
(use-package forge
  :after magit)

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
	 ("C-c C-a" . ag)
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
         ("C-r" . swiper)))
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

;; Default font
(setq default-frame-alist '((font . "S0urce Code Pro-12")))

;; Improved unicode support. Note that the font above takes
;; precedence. Use a variant which doesn't mess with colored emoji and
;; such.
(use-package persistent-soft)
(use-package font-utils)
(use-package ucs-utils)
(use-package unicode-fonts
  :functions unicode-fonts-setup
  :config
  (unicode-fonts-setup))


(if (not chri/proglang)
  (use-package which-key
    :diminish
    :init
    (which-key-setup-side-window-bottom)
    (which-key-mode))
  ;; chri/proglang branch:

  (if chri/prefer-eglot
      ;; eglot mode.
      (use-package eglot
        :commands (eglot eglot-ensure)
        :hook ((swift-mode . eglot-ensure)
               (rust-mode . eglot-ensure)
               (c-mode . eglot-ensure)
               (c++-mode . eglot-ensure)
               (python-mode . eglot-ensure)
               (js2-mode . eglot-ensure)
               (go-mode . eglot-ensure)
               (obc-c-mode . eglot-ensure)
               ;; Show all documentations, in practice both definitions and errors.
               (eglot-managed-mode . (lambda () (setq eldoc-documentation-strategy 'eldoc-documentation-compose))))
        :config
        (when chri/enable-tabnine
          (add-to-list 'eglot-stay-out-of 'company))
        (define-key eglot-mode-map (kbd "s-e r") 'eglot-rename)
        (define-key eglot-mode-map (kbd "s-e f") 'eglot-format)
        (define-key eglot-mode-map (kbd "s-e G") 'eglot-format-buffer)
        (define-key eglot-mode-map (kbd "s-e o") 'eglot-code-action-organize-imports)
        (define-key eglot-mode-map (kbd "s-e a") 'eglot-code-actions)
        (define-key eglot-mode-map (kbd "s-e q") 'eglot-code-action-quickfix)
        (define-key eglot-mode-map (kbd "s-h") 'eldoc)
        (define-key eglot-mode-map (kbd "s-e x") 'xref-find-references)
        (define-key eglot-mode-map (kbd "C-M-.") 'xref-find-references)
        (define-key eglot-mode-map (kbd "s-e t") 'eglot-find-typeDefinition)
        (define-key eglot-mode-map (kbd "s-e i") 'eglot-find-implementation)
        (define-key eglot-mode-map (kbd "s-e d") 'eglot-find-declaration)
        (define-key eglot-mode-map (kbd "s-e R") 'eglot-reconnect)
        (define-key eglot-mode-map (kbd "s-e S") 'eglot-shutdown-all))

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
      (require 'dap-python)))

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
  (use-package js2-mode
    :mode
    (("\\.js\\'" . js2-mode))
    :custom
    (js2-include-node-externs t)
    (js2-global-externs '("window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))
    (js2-highlight-level 3)
    (js2r-prefer-let-over-var t)
    (js2r-prefered-quote-type 2)
    (js-indent-align-list-continuation t)
    (global-auto-highlight-symbol-mode t)
    :config
    (setq js-indent-level 2))


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
    (unless chri/prefer-eglot
      (add-hook 'go-mode 'chri/lsp-go-install-save-hooks))
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
  (unless chri/prefer-eglot
    (use-package ccls
      :defer t
      :init
      (setq ccls-executable "/usr/bin/ccls")
      :hook ((c-mode c++-mode objc-mode) .
             (lambda ()
               (require 'ccls)
               (lsp)))))

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
  (unless chri/prefer-eglot
    (use-package lsp-dart
      :hook (dart-mode . lsp)))

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

  (when chri/enable-tabnine
    ;; Tabnine for AI completion
    (customize-set-variable 'lsp-completion-provider :none)
    (use-package company-tabnine
      :hook ((python-mode . chri/tabnine-on)
             (c-mode . chri/tabnine-on)))
    (defun chri/tabnine-off ()
      "turn off TabNine for this buffer"
      (interactive)
      (setq-local company-backends (delete 'company-tabnine company-backends)))
    (defun chri/tabnine-on ()
      "turn on TabNine for this buffer"
      (interactive)
      (setq-local company-backends (add-to-list 'company-backends 'company-tabnine))))

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
  (defun windmove-over (kmap)
    "Redefines keys for windmove"
    (define-key kmap [M-left] (chri/ignore-error-wrapper 'windmove-left))
    (define-key kmap [M-right] (chri/ignore-error-wrapper 'windmove-right))
    (define-key kmap [M-up] (chri/ignore-error-wrapper 'windmove-up))
    (define-key kmap [M-down] (chri/ignore-error-wrapper 'windmove-down)))
  (add-hook 'elpy-mode-hook (lambda ()
                              (windmove-over elpy-mode-map)
                              (define-key elpy-mode-map (kbd "C-c C-s") `rg-menu)))
  (add-hook 'org-mode-hook (lambda () (windmove-over org-mode-map)))
  (add-hook 'c-mode-hook (lambda ()
                           (define-key c-mode-map (kbd "C-c C-s") `rg-menu)))
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
  (rg-keymap-prefix "\C-cs")
  :commands rg rg-menu
  :config
  (rg-enable-default-bindings)
  :bind ("C-c C-s" . rg-menu)
  :bind-keymap ("C-c s" . rg-global-map))


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

;; Highlight Delimiters.
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

;; Show Hex Color Codes.
(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook (
         (web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

;; Have ediff in a single frame.
(use-package ediff
  :commands ediff ediff-files ediff-buffers ediff-backup ediff-current-file ediff-directories ediff-revision ediff-patch-file ediff-patch-buffer ediff-merge ediff-merge-buffers-with-ancestor
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq diff-switches "-u"
        ediff-custom-diff-options "-U3"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook ((ediff-startup . ediff-toggle-wide-display)
         (ediff-cleanup . ediff-toggle-wide-display)
         (ediff-suspend . ediff-toggle-wide-display)))

;; Enable imenu
(add-hook 'prog-mode-hook 'imenu-add-menubar-index)

;; Don't warn about large files.
(setq large-file-warning-threshold nil)

;; Gtags
(use-package counsel-gtags
  :bind (("C-c C-t d" . counsel-gtags-find-definition)
         ("C-c C-t r" . counsel-gtags-find-reference)
         ("C-c C-t s" . counsel-gtags-find-symbol)
         ("C-c C-t f" . counsel-gtags-find-file)
         ("C-c C-t C-b" . counsel-gtags-go-backward)
         ("C-c C-t C-f" . counsel-gtags-go-forward)
         ("C-c C-t C" . counsel-gtags-create-tags)
         ("C-c C-t U" . counsel-gtags-update-tags)
         ("C-c C-t j" . counsel-gtags-dwim)
         ("C-c C-t a" . tags-apropos)))

;; Frame title
(setq-default frame-title-format '("%b [%m]"))

;; OpenSCAD mode
(use-package scad-mode
  :mode "\\.scad$")

;; Dumb Jump
(use-package dumb-jump
  :ensure t
  :bind (("s-j o" . dumb-jump-go-other-window)
         ("s-j s-o" . dumb-jump-go-other-window)
         ("s-j j" . dumb-jump-go)
         ("s-j s-j" . dumb-jump-go)
         ("s-j b" . dumb-jump-back)
         ("s-j s-b" . dumb-jump-back)
         ("s-j q" . dumb-jump-quick-look)
         ("s-j s-q" . dumb-jump-quick-look)
         ("s-j x" . dumb-jump-go-prefer-external)
         ("s-j s-x" . dumb-jump-go-prefer-external)
         ("s-j z" . dumb-jump-go-prefer-external-other-window)
         ("s-j s-z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; Load only if exists
(defun load-if-exists (fname)
  "Load if exists."
 (when (file-exists-p fname)
   (load-file fname)))
(load-if-exists "~/elisp/pass.el")
(load-if-exists "~/git/aide.el/aide.el")

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
(global-set-key [f2] 'split-window-right)
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
