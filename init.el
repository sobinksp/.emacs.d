(setq inhibit-startup-message t)

;; Disable notification sound
(setq visible-bell       nil
      ring-bell-function #'ignore)

(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(electric-pair-mode t)
(column-number-mode 1)
(ido-mode)

(setq isearch-lazy-count t)
(setq isearch-lazy-highlight t)
(setq global-auto-revert-non-file-buffers t) ;; live realod for dired
(setq compile-command "")
(setq scroll-conservatively 101)
(setq make-backup-files nil)

(global-auto-revert-mode t)                  ;; enable auto live-reload buffer if its chagned externally
(global-display-line-numbers-mode)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(define-key special-mode-map "q"
  (lambda () (interactive) (quit-restore-window nil 'kill)))

;; (global-hl-line-mode t)

;; change default find-file path for windows
(when (eq system-type 'windows-nt)
  (let ((appdata-roaming (getenv "APPDATA")))
    (when appdata-roaming
      (setq default-directory (expand-file-name ".emacs.d" appdata-roaming))
      )
    )
    (set-face-attribute 'default nil :font "Consolas-14"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  ;; (set-face-attribute 'default nil
  ;;                     :family "JetBrainsMono Nerd Font"
  ;;                     :foundry "nil"
  ;;                     :slant 'normal
  ;;                     :weight 'regular
  ;;                     :height 150
  ;;                     :width 'normal))
    (set-face-attribute 'default nil :font "Iosevka-20"))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	                 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package general
  :ensure t
  :after evil
  :config

  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)
  (tyrant-def
    ;; "SPC" '("M-x" . execute-extended-command)
    "SPC" 'counsel-M-x
    "wv" 'evil-window-vsplit
    "wh" 'evil-window-split
    "wm" 'toggle-maximize-buffer
    "we" 'balance-windows
    "wq" 'kill-buffer-and-window
    "fd" 'find-file
    "fb" 'counsel-switch-buffer
    "fs" 'counsel-rg
    "ts" 'hydra-text-scale/body
    "ff" 'projectile-find-file
    "pp" 'project-switch-project))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (setq evil-mode-line-format nil)
  (setq evil-search-module 'isearch)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-define-key 'normal 'global
    (kbd "C-h") 'windmove-left
    (kbd "C-j") 'windmove-down
    (kbd "C-k") 'windmove-up
    (kbd "C-l") 'windmove-right

    (kbd "+") 'evil-window-increase-height
    (kbd "_") 'evil-window-decrease-height
    (kbd "=") 'evil-window-increase-width
    (kbd "-") 'evil-window-decrease-width

    (kbd "[b") 'previous-buffer
    (kbd "]b") 'next-buffer
    (kbd "gd") 'lsp-find-definition

    (kbd "/")  'swiper)

  (evil-define-key 'normal flycheck-mode-map
    (kbd "]d") 'flycheck-next-error
    (kbd "[d") 'flycheck-previous-error))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
 ;; :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-minimum-prefix-length 1)
  (global-set-key (kbd "C-SPC") #'company-complete)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package swiper
  :ensure t
  :config
  (setq ivy-case-fold-search t))

(use-package diminish
  :ensure t)

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-switch-buffer-kill)
	("TAB" . ivy-done))
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-mode 1))


(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  ;; :after ivy
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package autothemer
  :ensure t)


(use-package timu-rouge-theme
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package hydra
  :ensure t)

(use-package projectile
  :custom ((projectile-completion-system 'ivy))
  :config
  (projectile-mode))

;; Function to toggle maximize buffer
(defun toggle-maximize-buffer() "Maximize buffer"
       (interactive)
       (if (= 1(length (window-list)))
	   (jump-to-register '_)
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))

(defhydra hydra-text-scale ()
	  "zoom"
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out"))

;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;; 	doom-themes-enable-italic nil)
;;   (load-theme 'doom-one t))

(load-theme 'wombat)

;; move emacs cutomize to separate file and load it
(setq custom-file (locate-user-emacs-file "customs.el"))
(load custom-file 'noerror 'nomessage)
