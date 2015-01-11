;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
;; language configurations
(set-language-environment 'Japanese)
(prefer-coding-system  'utf-8-unix)
;; show the corresponding paren
(show-paren-mode +1)
;; no start up message
(setq inhibit-startup-screen t)
;; hide tool-bar and menu-bar
(when window-system (tool-bar-mode -1))
(menu-bar-mode -1)
;; show line and column number
(line-number-mode +1)
(column-number-mode +1)
;;; The number of lines to scroll a window by when point moves out.
(setq scroll-step 1)
;; follow version controlled symbolic link
(setq vc-follow-symlinks t)
;; auto revert changes in version controlled file
(unless (require 'autorevert)
  (defvar auto-revert-check-vc-info t))
;; set background color
(setq frame-background-mode 'dark)
;; use tango-dark theme
(load-theme 'tango-dark t)
;; input special and control characters by "Option"
(setq ns-option-modifier 'none)
;; enable C-w in minibuffer
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)
;; set recenter-position
(setq recenter-positions '(middle))
;; change path to auto-save-list files
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/save-")
;; indent settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defun use-tabs-in-makefile-mode () (setq indent-tabs-mode +1))
(add-hook 'makefile-mode-hook 'use-tabs-in-makefile)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; linum: display line numbers to the left of buffers
(global-linum-mode +1)
(setq linum-format "%5d ")

;; save-place
(setq save-place t)
(setq save-place-file "~/.emacs.d/var/emacs-places.txt")

;; ispell
(eval-after-load 'ispell
 '(progn
    ;; use aspell
    (setq-default ispell-program-name "aspell")
    ;; skip Japanese characters in ispell
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

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

;; build recipes with elpa
(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
   (el-get-elpa-build-local-recipes))

;; additional recipe for el-get
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;; init-loader: Loader for configuration files
;; https://github.com/emacs-jp/init-loader
(el-get-bundle init-loader)
(custom-set-variables '(init-loader-show-log-after-init 'error-only))
(init-loader-load "~/.emacs.d/init")

;; diminish: Diminished modes are minor modes with no modeline display
;; https://github.com/emacsmirror/diminish
(el-get-bundle diminish)

;; session: use variables, registers and buffer places across sessions
;; https://github.com/emacsmirror/session
(el-get-bundle session)
(setq session-save-file (expand-file-name "~/.emacs.d/var/session"))
(add-hook 'after-init-hook 'session-initialize)

;; undohist: persistent undo history for gnu emacs
;; https://github.com/m2ym/undohist-el
(el-get-bundle undohist)
(require 'undohist)
(setq undohist-directory "~/.emacs.d/var/undohist")
(undohist-initialize)
(setq undohist-ignored-files
      (append '("COMMIT_EDITMSG" "NOTES_EDITMSG" "MERGE_MSG" "TAG_EDITMSG")
              undohist-ignored-files))

;; undo-tree: treat undo history as a tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
(el-get-bundle undo-tree)
(global-undo-tree-mode +1)
(diminish 'undo-tree-mode)

;; anzu: emacs port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(el-get-bundle anzu)
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-deactivate-region t) ;; deactivate region at anzu replace command
 '(anzu-search-threshold 1000))
(diminish 'anzu-mode)

;; volatile-highlights.el: minor mode for visual feedback on some operations.
;; http://www.emacswiki.org/emacs/VolatileHighlights
(el-get-bundle volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode +1)
(diminish 'volatile-highlights-mode)
(eval-after-load 'evil
  '(progn (vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
          (vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
          (vhl/give-advice-to-make-vhl-on-changes evil-paste-pop)))

;; rainbow-delimiters: which highlights parens, brackets,
;; and braces according to their depth
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(el-get-bundle rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; auto-save-buffers-enhanced: enables auto-saving along with vcs
;; https://github.com/kentaro/auto-save-buffers-enhanced
(el-get-bundle auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-interval 2)

;; ddskk: Simple Kana to Kanji conversion program Japanese input method on Emacs
;; http://openlab.ring.gr.jp/skk/
(el-get-bundle ddskk)
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
    (skk-mode +1) (skk-latin-mode +1)))
(dolist (hook (list 'find-file-hook 'minibuffer-setup-hook
                    'evil-insert-state-entry-hook))
  (add-hook hook 'enable-skk-latin-mode))

;; flycheck: Modern on-the-fly syntax checking for GNU Emacs
;; https://github.com/flycheck/flycheck
(el-get-bundle flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "M-n") 'flycheck-next-error)

;; flycheck-pos-tip: Flycheck errors display in tooltip
;; https://github.com/flycheck/flycheck-pos-tip
(el-get-bundle flycheck-pos-tip)
(eval-after-load 'flycheck
  '(progn
     (custom-set-variables
       '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages))))

;; evil: An extensible vi layer for Emacs
;; http://gitorious.org/evil
(el-get-bundle evil)
(el-get-bundle evil-leader)
(el-get-bundle evil-numbers)
(setq evil-want-C-u-scroll t)
(evil-mode +1)
;; reset evil-insert-state-map, use emacs keybind in evil-insert-state
(setcdr evil-insert-state-map nil)
;; use [escape] to switch normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)
(define-key evil-normal-state-map (kbd "M-j") nil)
(define-key evil-motion-state-map (kbd "\\") nil)
(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  ;; SKKによるカーソル色変更を, 挿入ステートかつ日本語モードの場合に限定
  "Allow ccc to update cursor color only when we are in insert state and in `skk-j-mode'."
  (when (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  ;; Evilによるカーソルの変更を, 挿入ステートかつ日本語モードではない場合に限定
  "Allow ccc to update cursor color only when we are in insert state and in `skk-j-mode'."
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

;; git-commit-mode: Major mode for editing git commit messages
;; https://github.com/magit/git-modes
(el-get-bundle git-commit-mode)
(eval-after-load 'git-commit-mode
 '(progn
    (remove-hook 'git-commit-mode-hook 'turn-on-auto-fill)
    (add-hook 'git-commit-mode-hook 'flyspell-mode)))

;; git-gutter-fringe: Fringe version of git-gutter.el
;; https://github.com/syohex/emacs-git-gutter-fringe
(el-get-bundle git-gutter-fringe)
(global-git-gutter-mode +1)
(diminish 'git-gutter-mode)

;; yasnippet: yet another snippet extension for Emacs.
;; http://capitaomorte.github.com/yasnippet/
(el-get-bundle yasnippet)
(autoload 'yas-minor-mode-on "yasnippet" nil t)
(yas-global-mode +1)
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
(el-get-bundle auto-complete)
(global-auto-complete-mode +1)
(diminish 'auto-complete-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-comphist-file "~/.emacs.d/var/ac-comphist.dat")
(setq ac-auto-show-menu t)
;; use auto-complete menu map
(setq ac-use-menu-map t)
(ac-set-trigger-key "TAB")

;; rst-mode: Emacs Support for reStructuredText
;; http://docutils.sourceforge.net/docs/user/emacs.html
(el-get-bundle rst-mode)
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))
(add-to-list 'ac-modes 'rst-mode)
(defun add-ac-source-for-rst ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'rst-mode-hook 'add-ac-source-for-rst)

;; yaml-mode: Major mode for editing YAML files
;; https://github.com/yoshiki/yaml-mode
(el-get-bundle yaml-mode)

;; coffee-mode: emacs major mode for coffeescript
;; https://github.com/defunkt/coffee-mode
(el-get-bundle coffee-mode)
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style
      '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))

;; auto-complete-clang: auto complete source for clang. AC+Clang+Yasnippet!
;; https://github.com/brianjcj/auto-complete-clang
(el-get-bundle auto-complete-clang)
(defun add-ac-source-for-c ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'add-ac-source-for-c)

;; jedi: Python auto-completion for Emacs
;; https://github.com/tkf/emacs-jedi
(el-get-bundle jedi)
(add-hook 'python-mode-hook 'jedi:setup)

;; php-mode: Major mode for editing PHP code
;; https://github.com/ejmr/php-mode
(el-get-bundle php-mode)

;; clojure-mode: Major mode for Clojure code
;; https://github.com/clojure-emacs/clojure-mode
(el-get-bundle clojure-mode)

;; cider: CIDER is a Clojure IDE and REPL for Emacs
;; https://github.com/clojure-emacs/cider
(el-get-bundle cider)
(eval-after-load 'cider
 '(progn
    ;; hide the *nrepl-connection* and *nrepl-server* buffers in switch-to-buffer
    (setq nrepl-hide-special-buffers t)
    ;; display the port on which the REPL server is running in buffer name
    (setq nrepl-buffer-name-show-port t)
    ;; store cider-repl-history
    (setq cider-repl-history-file "~/.emacs.d/var/cider-repl-history")
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)))

;; ac-cider: Emacs auto-complete client for CIDER
;; https://github.com/clojure-emacs/ac-cider
(el-get-bundle ac-cider)
(eval-after-load 'cider
 '(progn
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)))
(eval-after-load 'auto-complete
 '(progn
    (add-to-list 'ac-modes 'cider-mode)
    (add-to-list 'ac-modes 'cider-repl-mode)))

;; yatex: Yet Another TeX mode for Emacs
;; http://www.yatex.org/
(el-get-bundle yatex)
(if (file-exists-p "~/.emacs.d/yatex.el")
    (load-file "~/.emacs.d/yatex.el"))

;; mew: a mail reader for Emacs
;; http://www.mew.org/
(el-get-bundle mew)
(if (file-exists-p "~/.emacs.d/mew.el")
    (load-file "~/.emacs.d/mew.el"))

;;; local settings
(if (file-exists-p "~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
