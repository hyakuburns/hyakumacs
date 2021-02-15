(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(fmakunbound 'gdb)
(fmakunbound 'gdb-enable-debug)

;;Packages
;; 
(require 'package)



(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; list the packages you want
(setq package-list '(yafolding org-superstar all-the-icons use-package irony nim-mode 
beacon cherry-blossom-theme vterm clues-theme company company-quickhelp dashboard 
doom-modeline doom-themes emojify emojify-logos go-mode go-playground 
haskell-mode helpful highlight-indent-guides magit minibuffer-complete-cycle 
paredit paredit-everywhere projectile treemacs treemacs-all-the-icons 
treemacs-magit rainbow-delimiters toc-org company-irony flycheck-irony company-irony-c-headers
evil evil-lisp-state slime slime-company lispy rtags quelpa simple-mpc))

					; list the repositories containing them

(quelpa '(gdb-mi :fetcher git
         :url "https://github.com/weirdNox/emacs-gdb.git"
         :files ("*.el" "*.c" "*.h" "Makefile")))
; activate all the packages (in particular autoloads)
(use-package gdb-mi)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;;Programming
;
;;common lisp
(slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company))
(setq inferior-lisp-program (executable-find "sbcl"))
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;;C
;
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


   (require 'company-irony-c-headers)
   ;; Load with `irony-mode` as a grouped backend
   (eval-after-load 'company
     '(add-to-list
       'company-backends '(company-irony-c-headers company-irony)))
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;;haskell
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

  
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;;GDB
(fmakunbound 'gdb)
(fmakunbound 'gdb-enable-debug)

;;ORG MODE
;; 
(require 'org-superstar)
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

;TOC 
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (warn "toc-org not found"))

;;Random editing stuff
; 
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)
(global-display-line-numbers-mode)
(electric-pair-mode)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'haskell-mode-hook           'enable-paredit-mode)
;; 

;;Visual stuff
; 
(load-theme 'doom-horizon t)
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
(show-paren-mode 1)

;;Global custom keybinds

(global-set-key (kbd "C-x p") 'move-to-window-line-top-bottom)

;vi mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
