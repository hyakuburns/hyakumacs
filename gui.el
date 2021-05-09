(ido-mode 0)
;; helm
(straight-use-package 'helm)

(require 'rainbow-mode)
;;Org mode++
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

(load-theme 'doom-challenger-deep t)

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

;;Global custom keybinds

(global-set-key (kbd "C-x C-f") 'helm-find-files)


;backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

(setq projectile-completion-system 'helm)
(helm-projectile-on)


;; todo highlighting
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#ff99d3")
        ("STUB"   . "#1E90FF")
	("XXX"    . "#FF4500")))

;; set transparency

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

;;;;;GOD MODEEEEE
(require 'god-mode)
(god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
;;visual indicator
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(defun my-god-mode-update-mode-line ()
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground "#d7d7d7"
                        :background "#28156E")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#efefef"
                        :background "#404148"))
   (t
    (set-face-attribute 'mode-line nil
			:foreground "#0a0a0a"
			:background "#7c318f")
    (set-face-attribute 'mode-line-inactive nil
			:foreground "#efefef"
			:background "#404148"))))

(add-hook 'post-command-hook 'my-god-mode-update-mode-line)
