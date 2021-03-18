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
(setq package-list '(yafolding org-superstar all-the-icons use-package lsp-mode
			       beacon cherry-blossom-theme vterm clues-theme company company-quickhelp dashboard 
			       doom-modeline doom-themes emojify emojify-logos go-mode go-playground
			       helpful highlight-indent-guides magit minibuffer-complete-cycle free-keys
			       paredit paredit-everywhere projectile treemacs treemacs-all-the-icons nix-mode
			       treemacs-magit rainbow-delimiters toc-org flycheck lsp-treemacs helm-lsp hl-todo
			       slime slime-company lispy rtags quelpa simple-mpc helm-gtags function-args clang-format
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

;; helm
(straight-use-package 'helm)
;;fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;(require 'unicode-fonts)
;(unicode-fonts-setup)

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
					;
;;common lisp
(slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company))
(setq inferior-lisp-program (executable-find "sbcl"))
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

					;
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'clang-format+-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'gofmt-before-save)

;;ORG MODE
;;
(require 'org)
(require 'org-superstar)
(setq org-directory "~/Documents/programming/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground "white")))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

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
					; 
(load-theme 'doom-horizon t)
(normal-erase-is-backspace-mode 1)
;;Dashboard
;; 
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/epics/satanichia.png")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
(beacon-mode 1)

;;doom modeline 
;; 
(require 'doom-modeline)
(doom-modeline-mode 1)

;;treemacs
;
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;;emojify
;; 
(add-hook 'after-init-hook #'global-emojify-mode)

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

;;rainbow delimiters
;
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(show-paren-mode 1)

;;Global custom keybinds

(global-set-key (kbd "C-x p") 'move-to-window-line-top-bottom)

;backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; (setq ido-enable-flex-matching t)
;; (ido-mode 1)
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c b") 'helm-occur)
(global-set-key (kbd "C-x b") 'helm-mini)

(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-M-x-fuzzy-match t)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


(helm-mode 1)

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
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
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#ff99d3")
        ("STUB"   . "#1E90FF")
	("XXX"    . "#FF4500")))


(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
