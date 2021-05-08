(if (file-exists-p "~/.emacs.d/custom.el") nil
  (write-region "" nil "~/.emacs.d/custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; loading new files just in case lol
;; (load-file "~/.emacs2")

;; random kbd changes
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;;Packages
;; im idiot
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list the packages you want
(setq package-list '(yafolding org-superstar all-the-icons use-package lsp-mode avy monky
			       beacon cherry-blossom-theme vterm clues-theme company company-quickhelp dashboard geiser
			       doom-modeline doom-themes emojify emojify-logos go-mode go-playground
			       helpful highlight-indent-guides magit minibuffer-complete-cycle free-keys
			       paredit paredit-everywhere projectile treemacs treemacs-all-the-icons nix-mode
			       treemacs-magit rainbow-delimiters toc-org flycheck lsp-treemacs helm-lsp hl-todo
			       slime slime-company quelpa simple-mpc helm-gtags function-args clang-format highlight-escape-sequences
			       clang-format+ quelpa git-commit magit-popup meson-mode helm-projectile rainbow-identifiers unicode-fonts))
					; list the repositories containing them


;; activate all the packages (in particular autoloads)
(package-initialize)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; in line function arguments hint
(require 'function-args)
(fa-config-default)

;;Programming
;; compiling
(global-set-key (kbd "C-c M-c") 'compile)
(setq compile-command "cd .. && meson build && ninja -C build && ./build/main") 

;; lsp config
(require 'lsp-mode)
(setq lsp-enable-links nil)
(global-set-key (kbd "C-c d") 'lsp-find-definition)
(global-set-key (kbd "C-c z") 'lsp-find-declaration)
(global-set-key (kbd "C-c h") 'ff-find-other-file)

;; 
(global-display-line-numbers-mode)

;;geiser
(setq geiser-active-implementations '(guile))
(require 'geiser)

;;common lisp
(slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company))
(setq inferior-lisp-program (executable-find "sbcl"))

					;
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'clang-format+-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'before-save-hook 'gofmt-before-save)

;;ORG MODE
;;
(require 'org)
(require 'org-superstar)
(setq org-directory "~/Documents/programming/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(setq org-hide-emphasis-markers t)

;;TOC 
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (warn "toc-org not found"))

;; todo capturing
(require 'org)(setq org-capture-templates '(
			       ("c"
				"Code"
				entry
				(file+headline "must.org" "Code")
				"* TODO %^{TITLE} %^G\n:PROPERTIES:\n:Created: %U:Source: %a\n:END:\n%i%?"
				:prepend nil
				:empty-lines 1
				:create t
				:kill-buffer t)
			       )
      )
(global-set-key (kbd "C-c c") 'org-capture)
;; highlight escape sequences
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)


;;Random editing stuff
					; 
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)
(electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

(setq electric-pair-delete-adjacent-pair nil)

;;Visual stuff
(normal-erase-is-backspace-mode 1)

;;doom modeline 
;; 
(require 'doom-modeline)
(doom-modeline-mode 1)

;;indentantion
;; 
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)

;;disable tabs
;
;(setq-default indent-tabs-mode nil)

;;company mode
;
(add-hook 'after-init-hook 'global-company-mode)

;;yafolding

;;yafolding keybinds
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))
;;end of yafolding keybinds


;custom yafolding keybinds
 
;(require 'yafolding)
;(define-key yafolding-mode-map (kbd "<C-S-return>") nil)
;(define-key yafolding-mode-map (kbd "<C-M-return>") nil)
;(define-key yafolding-mode-map (kbd "<C-return>") nil)
;(define-key yafolding-mode-map (kbd "C-c <C-M-return>") 'yafolding-toggle-all)
;(define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-hide-parent-element)
;(define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)

;end of custom yafolding keybinds

(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(show-paren-mode 1)

;;Global custom keybinds

(global-set-key (kbd "C-x p") 'move-to-window-line-top-bottom)
(global-set-key (kbd "C-:") 'avy-goto-char)


;backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(ido-mode 1)
(setq ido-enable-flex-matching t)

(require 'projectile)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;;projectile shit
(setq projectile-project-search-path '("~/Documents/programming/C/pj" "~/Documents/programming/cxx/xxpj/"))
;; clang formatting
(require 'clang-format)
(setq clang-format+-offset-modified-region 'buffer)
(setq clang-format-style 'google)

;; magit error
(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)
;function peek
(require 'function-args)

;; todo highlighting

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;;slime syntax highlighting
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))

(when window-system
  (load "~/.emacs.d/gui.el"))
