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
(when window-system (tool-bar-mode 0))
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
;; enable C-w in minibuffer
(define-key minibuffer-local-completion-map (kbd "\C-w") 'backward-kill-word)
;; set recenter-position
(setq recenter-positions '(middle))
;; change path to auto-save-list files
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/save-")
;; indent settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defun use-tabs-in-makefile-mode () (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'use-tabs-in-makefile)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; indent style for C
(defun set-indent-for-c ()
  (setq c-default-style "k&r"
        c-basic-offset tab-width))
(add-hook 'c-mode-common-hook 'set-indent-for-c)

;; linum: display line numbers to the left of buffers
(require 'linum)
(global-linum-mode t)
;; dynamic linum-format
(setq linum-format "%5d ")

;; save-place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/var/emacs-places.txt")

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
(defun enable-flymake-mode () (flymake-mode t))
(dolist (hook (list 'c-mode-hook 'c++-mode-hook))
  (add-hook hook 'enable-flymake-mode))

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
(setq my-el-get-packages
      (append '(ddskk
                auto-complete-latex
                rst-mode
                yatex
                auto-save-buffers-enhanced)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-el-get-packages)

;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(defvar my-elpa-packages
  '(init-loader
    popup flymake-cursor
    diminish
    session undohist volatile-highlights
    anzu rainbow-delimiters
    smartparens
    yasnippet auto-complete
    undo-tree evil evil-leader evil-numbers
    auto-complete-clang jedi
    clojure-mode cider ac-cider
    go-mode go-autocomplete go-errcheck
    yaml-mode
    coffee-mode
    php-mode
    exec-path-from-shell
    mew))
(require 'cl-lib)
(let ((not-installed
       (cl-remove-if (lambda (x) (package-installed-p x)) my-elpa-packages)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;; init-loader: Loader for configuration files
;; https://github.com/emacs-jp/init-loader
(setq init-loader-show-log-after-init nil) ;; disable init-log
(require 'init-loader)
(init-loader-load "~/.emacs.d/init")
(unless (equal (init-loader-error-log) "") (init-loader-show-log))

;; session: use variables, registers and buffer places across sessions
;; https://github.com/emacsmirror/session
(require 'session)
(setq session-save-file (expand-file-name "~/.emacs.d/var/session"))
(add-hook 'after-init-hook 'session-initialize)

;; undo-tree: treat undo history as a tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; yasnippet: yet another snippet extension for Emacs.
;; http://capitaomorte.github.com/yasnippet/
(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)
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
(diminish 'auto-complete-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-comphist-file "~/.emacs.d/var/ac-comphist.dat")
(setq ac-auto-show-menu t)
;; use auto-complete menu map
(setq ac-use-menu-map t)
(setq ac-set-trigger-key "TAB")

;; auto-complete-clang: auto complete source for clang. AC+Clang+Yasnippet!
;; https://github.com/brianjcj/auto-complete-clang
(require 'auto-complete-clang)
(defun add-ac-source-for-c ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'add-ac-source-for-c)

;; jedi: Python auto-completion for Emacs
;; https://github.com/tkf/emacs-jedi
(setq jedi:setup-keys t)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; rst-mode: Emacs Support for reStructuredText
;; http://docutils.sourceforge.net/docs/user/emacs.html
(require 'rst)
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))
(add-to-list 'ac-modes 'rst-mode)
(defun add-ac-source-for-rst ()
 (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'rst-mode-hook 'add-ac-source-for-rst)

;; rainbow-delimiters: which highlights parens, brackets,
;; and braces according to their depth
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; smartparens: automatic insertion, wrapping and paredit-like navigation
;; with user defined pairs.
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(diminish 'smartparens-mode)

;; evil: An extensible vi layer for Emacs
;; http://gitorious.org/evil
(require 'evil)
(evil-mode 1)
;; reset evil-insert-state-map, use emacs keybind in evil-insert-state
(setcdr evil-insert-state-map nil)
;; use [escape] to switch normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(setq evil-want-C-u-scroll t)
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)
(define-key evil-normal-state-map (kbd "M-j") nil)
(define-key evil-motion-state-map (kbd "\\") nil)
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
    (let* ((line-no (line-number-at-pos))
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
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;; anzu: emacs port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(require 'anzu)
(custom-set-variables
 '(anzu-deactivate-region t) ;; deactivate region at anzu replace command
 '(anzu-search-threshold 1000))
(diminish 'anzu-mode)

;; volatile-highlights.el: minor mode for visual feedback on some operations.
;; http://www.emacswiki.org/emacs/VolatileHighlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-pop)

;; undohist: persistent undo history for gnu emacs
;; https://github.com/m2ym/undohist-el
(require 'undohist)
(setq undohist-directory "~/.emacs.d/var/undohist")
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
(defun enable-skk-latin-mode ()
  (unless (cl-remove-if-not (lambda (x) (eq major-mode x))
                            (list 'lisp-interaction-mode 'cider-mode))
    (skk-mode t) (skk-latin-mode t)))
(dolist (hook (list 'find-file-hook 'minibuffer-setup-hook
                    'evil-insert-state-entry-hook))
  (add-hook hook 'enable-skk-latin-mode))

;; cider: CIDER is a Clojure IDE and REPL for Emacs
;; https://github.com/clojure-emacs/cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; hide the *nrepl-connection* and *nrepl-server* buffers in switch-to-buffer
(setq nrepl-hide-special-buffers t)
;; display the port on which the REPL server is running in buffer name
(setq nrepl-buffer-name-show-port t)
;; store cider-repl-history
(setq cider-repl-history-file "~/.emacs.d/var/cider-repl-history")

;; ac-cider: Emacs auto-complete client for CIDER
;; https://github.com/clojure-emacs/ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; coffee-mode: emacs major mode for coffeescript
;; https://github.com/defunkt/coffee-mode
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style
      '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))

;; auto-save-buffers-enhanced: enables auto-saving along with vcs
;; https://github.com/kentaro/auto-save-buffers-enhanced
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-interval 2)

;; yaml-mode: Simple major mode to edit YAML file for emacs
;; https://github.com/yoshiki/yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; local settings
(if (file-exists-p "~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
