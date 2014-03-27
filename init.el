;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; language configurations
(set-language-environment 'Japanese)
(prefer-coding-system  'utf-8-unix)
;; show the corresponding paren
(show-paren-mode t)
;; no start up message
(setq inhibit-startup-screen t)
;; hide tool-bar and menu-bar
(when window-system
  (tool-bar-mode 0))
(menu-bar-mode 0)
;; show line and column number
(line-number-mode t)
(column-number-mode t)
;;; The number of lines to scroll a window by when point moves out.
(setq scroll-step 1)
;; follow version controlled symbolic link
(setq vc-follow-symlinks t)
;; auto revert changes in version controlled file
(setq auto-revert-check-vc-info t)
;; set background color
(setq frame-background-mode 'dark)
;; use tango-dark theme
(load-theme 'tango-dark t)
;; input special and control characters by "Option"
(setq ns-option-modifier 'none)
;; set recenter-position
(setq recenter-positions '(middle))
;; indent settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(add-hook 'makefile-mode-hook
          (function (lambda () (setq indent-tabs-mode t))))
(define-key global-map (kbd "RET") 'newline-and-indent)
;; indent style for C
(add-hook 'c-mode-common-hook
          '(lambda() (setq c-default-style "k&r"
                           c-basic-offset tab-width)))

;; linum: display line numbers to the left of buffers
(require 'linum)
(global-linum-mode t)
;; dynamic linum-format
(setq linum-format "%5d ")

;; save-place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/emacs-places.txt")

;; flyspell
(require 'flyspell)
(setq ispell-program-name "aspell")
;; skip Japanese characters in ispell
(eval-after-load
    "ispell" '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; flymake
(require 'flymake)
;; flymake without Makefile
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))
;; flymake with/without Makefile
(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))
;; flymake for C/C++
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "clang" '("-W" "-Wall" "-Wextra" "-std=c99" "-fsyntax-only")))
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "clang++" '("-W" "-Wall" "-Wextra" "-fsyntax-only")))
(defun flymake-header-init ()
  (flymake-simple-generic-init
   "clang" '("-W" "-Wall" "-Wextra" "-std=c99" "-x" "c-header")))
(defun flymake-cc-header-init ()
  (flymake-simple-generic-init
   "clang++" '("-W" "-Wall" "-Wextra" "-x" "c++-header")))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-header-init) flymake-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cc-header-init) flymake-allowed-file-name-masks)
(dolist (hook (list 'c-mode-hook 'c++-mode-hook))
  (add-hook hook '(lambda () (flymake-mode t))))

;; autoinsert
(require 'autoinsert)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.emacs.d/templates")
(setq auto-insert-alist
      (append '(("[Mm]akefile$" . "Makefile")
                ("\\.c$" . "template.c")
                ("\\.cpp$" . "template.cpp")
                ("\\.f$" . "template.f")
                ("\\.sh$" . "template.sh")
                ("\\.plt$" . "template.plt")
                ("\\.py$" . "template.py")
                ("\\.rb$" . "template.rb")
                ("\\.pl$" . "template.pl")
                ("\\.html$" . "template.rst")
                ("\\.tex$" . "template.tex")
                ("\\.rst$" . "template.rst"))
              auto-insert-alist))
(add-hook 'find-file-hooks 'auto-insert)

;; disable init-log
(setq init-loader-show-log-after-init nil)

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(defvar my/packages
  '(init-loader
    popup flymake-cursor
    undohist volatile-highlights
    anzu rainbow-delimiters
    yasnippet auto-complete
    undo-tree evil evil-leader evil-numbers
    auto-complete-clang jedi
    clojure-mode cider ac-cider-compliment
    gist
    mew w3m))
(require 'cl-lib)
(let ((not-installed (cl-remove-if (lambda (x) (package-installed-p x)) my/packages)))
  (when not-installed (package-refresh-contents)
    (dolist (pkg not-installed) (package-install pkg))))

;; el-get: allows you to install and manage elisp code for Emacs
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
;; additional recipe for el-get
(add-to-list 'el-get-recipe-path (expand-file-name "~/.emacs.d/recipes"))
;; local sources
(setq my-packages
      (append '(ddskk
                auto-complete-latex
                smart-dnd
                rst-mode
                yatex
                auto-save-buffers-enhanced)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)

;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/init")
(unless (equal (init-loader-error-log) "") (init-loader-show-log))

;; undo-tree: treat undo history as a tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; yasnippet: yet another snippet extension for Emacs.
;; http://capitaomorte.github.com/yasnippet/
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-indent-line 'fixed)
(setq yas-wrap-around-region 'nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/plugins/yasnippet/extras/imported"))
;; autoinsert header file with include guard
(setq auto-insert-alist
      (append '(("\\.h$" . '(yas-expand-snippet "once")))
              auto-insert-alist))

;; auto-complete: The most intelligent auto-completion extension for GNU Emacs
;; http://cx4a.org/software/auto-complete/index.html
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-auto-show-menu t)
;; use auto-complete menu map
(setq ac-use-menu-map t)
(setq ac-set-trigger-key "TAB")

;; auto-complete-clang: auto complete source for clang. AC+Clang+Yasnippet!
;; https://github.com/brianjcj/auto-complete-clang
(require 'auto-complete-clang)
(add-hook 'c-mode-common-hook
          '(lambda () (setq ac-sources
                            (append '(ac-source-clang ac-source-yasnippet)
                                    ac-sources))))

;; jedi: Python auto-completion for Emacs
;; https://github.com/tkf/emacs-jedi
(setq jedi:setup-keys t)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; rst-mode:
(require 'rst)
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))
(add-to-list 'ac-modes 'rst-mode)
(add-hook 'rst-mode-hook
          '(lambda () (setq ac-sources
                            (append '(ac-source-yasnippet) ac-sources))))

;; rainbow-delimiters: which highlights parens, brackets, and braces according to their depth
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; evil: An extensible vi layer for Emacs
;; http://gitorious.org/evil
(require 'evil)
(evil-mode 1)
(setq evil-want-C-u-scroll t)
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)
;;(modify-syntax-entry ?_ "w")
(define-key evil-normal-state-map (kbd "M-j") nil)
(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  ;; SKKによるカーソル色変更を, 挿入ステートかつ日本語モードの場合に限定
  "Allow ccc to update cursor color only when we are in insert state
and in `skk-j-mode'."
  (when (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  ;; Evilによるカーソルの変更を, 挿入ステートかつ日本語モードではない場合に限定
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (unless (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-ex-search-update-pattern
  (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
  ;; SKKの未確定状態(skk-henkan-mode)ではない場合だけ, 検索パターンをアップデート
  "Inhibit search pattern update during `skk-henkan-mode'.
This is reasonable since inserted text during `skk-henkan-mode'
is a kind of temporary one which is not confirmed yet."
  (unless (and (boundp 'skk-henkan-mode) skk-henkan-mode)
    ad-do-it))

;; show flymake error message in popup menu
(require 'popup)
(defun flymake-popup-error ()
  (unless (evil-visual-state-p) ;; disable in evil-visual-state
    (interactive)
    (let* ((line-no (flymake-current-line-no))
           (line-err-info-list
            (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count (length line-err-info-list)))
      (while (> count 0)
        (when line-err-info-list
          (let* ((file
                  (flymake-ler-file (nth (1- count) line-err-info-list)))
                 (full-file
                  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                 (text
                  (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line
                  (flymake-ler-line (nth (1- count) line-err-info-list))))
            (popup-tip (format "[%s] %s" line text))))
        (setq count (1- count))))))
;; show pop-up menu on error line
(defadvice flymake-mode
  (before post-command-stuff activate compile)
  (set (make-local-variable 'post-command-hook)
       (add-hook 'post-command-hook 'flymake-popup-error)))
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;; anzu: emacs port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(require 'anzu)
(custom-set-variables
 '(anzu-mode-lighter "") ;; minor-mode name
 '(anzu-deactivate-region t) ;; deactivate region at anzu replace command
 '(anzu-search-threshold 1000))

;; volatile-highlights.el: minor mode for visual feedback on some operations.
;; http://www.emacswiki.org/emacs/VolatileHighlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-pop)

;; undohist: persistent undo history for gnu emacs
;; https://github.com/m2ym/undohist-el
(require 'undohist)
(undohist-initialize)

;; yatex: Yet Another TeX mode for Emacs
;; http://www.yatex.org/
(if (file-exists-p "~/.emacs.d/yatex.el")
    (load-file "~/.emacs.d/yatex.el"))

;; mew: a mail reader for Emacs
;; http://www.mew.org/
(if (file-exists-p "~/.emacs.d/mew.el")
    (load-file "~/.emacs.d/mew.el"))

;; SKK: Simple Kana to Kanji conversion program Japanese input method on Emacs
;; http://openlab.ring.gr.jp/skk/
(require 'skk-autoloads)
(setq skk-user-directory "~/.emacs.d/skk")
(setq skk-show-annotation t)
(setq skk-auto-insert-paren nil)
(setq skk-henkan-strict-okuri-precedence t)
;; dynamic completion
(setq skk-dcomp-activate t)
;; hooking skk-latin-mode 
(dolist (hook (list 'find-file-hook 'minibuffer-setup-hook
                    'evil-insert-state-entry-hook))
  (add-hook hook (lambda ()
                   (when (not (cl-remove-if-not (lambda (x) (eq major-mode x))
                                                (list 'lisp-interaction-mode
                                                      'cider-mode)))
                     (skk-mode t) (skk-latin-mode t)))))

;; cider: CIDER is a Clojure IDE and REPL for Emacs
;; https://github.com/clojure-emacs/cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; hide the *nrepl-connection* and *nrepl-server* buffers in switch-to-buffer
(setq nrepl-hide-special-buffers t)
;; display the port on which the REPL server is running in buffer name
(setq nrepl-buffer-name-show-port t)

;; ac-cider-compliment: Emacs auto-complete client for Compliment
;; https://github.com/alexander-yakushev/ac-cider-compliment
(require 'ac-cider-compliment)
(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes cider-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; coffee-mode: emacs major mode for coffeescript
;; https://github.com/defunkt/coffee-mode
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))

;; auto-save-buffers-enhanced: enables auto-saving along with vcs
;; https://github.com/kentaro/auto-save-buffers-enhanced
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-interval 2)
;;; local settings
(if (file-exists-p (expand-file-name "~/.emacs.d/init-local.el"))
    (load (expand-file-name "~/.emacs.d/init-local.el") nil t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
