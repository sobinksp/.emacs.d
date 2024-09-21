;; Disabled startup screen.
;; Don't show splash screen
(setq inhibit-startup-message t)

;; Toolbar, menubar, scrollbr
(tool-bar-mode -1)
(menu-bar-mode t)
;; (scroll-bar-mode -1) 

;; Highlight the current line
;; (global-hl-line-mode t)

;; change default find-file path for windows
(when (string-equal system-type "windows-nt")
  (let ((appdata-roaming (getenv "APPDATA")))
    (when appdata-roaming
      (setq default-directory (expand-file-name ".emacs.d" appdata-roaming)))))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; displays current match and total matches isearch.
(setq isearch-lazy-count t)
(setq isearch-lazy-highlight t)

;; refresh buffers when changed externally
(global-auto-revert-mode t)

;; fullscreen on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(company evil-leader yasnippet-snippets yasnippet lsp-ui company-lsp lsp-mode diff-hl dff-hl git-gutter-fringe git-gutter anzu autothemer evil-surround evil-commentary evil))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "nil" :slant normal :thin regular :height 150 :width normal :weight regular)))))

;; Enable auto live-reload buffer if its changed externally.
(global-auto-revert-mode t)

;; Theme
;(load-theme 'no-clown-fiesta t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Ensure `use-package` is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Install and configure Evil Leader
(use-package evil-leader
  :ensure t
  :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "wq" 'evil-window-delete
      "wv" 'evil-window-vsplit
      "wh" 'evil-window-split
      "wm" 'toggle-maximize-buffer
      "we" 'balance-windows
      "ff" 'find-file)
)

;; Install and configure Evil
(use-package evil
  :ensure t
  :config
  (setq evil-mode-line-format nil)
  (setq evil-search-module 'isearch)
  (evil-mode 1)
  ;; window management
  (evil-define-key 'normal 'global
    (kbd "C-h") 'windmove-left
    (kbd "C-j") 'windmove-down
    (kbd "C-k") 'windmove-up
    (kbd "C-l") 'windmove-right

    (kbd "+") 'evil-window-increase-height
    (kbd "_") 'evil-window-decrease-height
    (kbd "=") 'evil-window-increase-width
    (kbd "-") 'evil-window-decrease-width)

  ;; (define-key evil-visual-state-map (kbd ">") (kbd ">gv"))
  (define-key evil-visual-state-map (kbd ">") (lambda()
						(interactive)
						(evil-shift-right (region-beginning) (region-end))
						(evil-normal-state)
						(evil-visual-restore)))
  (define-key evil-visual-state-map (kbd "<") (lambda()
						(interactive)
						(evil-shift-left (region-beginning) (region-end))
						(evil-normal-state)
						(evil-visual-restore)))
)
;; cursor customization example
;; (setq evil-normal-state-cursor '(box "light blue")
;;       evil-insert-state-cursor '(bar "medium sea green")
;;       evil-visual-state-cursor '(hollow "orange"))

;; Install and configure Evil Commentary
(use-package evil-commentary
  :ensure t
 ;; :after evil
  :config
  (evil-commentary-mode))

;; Install and configure Evil Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Install and configure Company mode
;; auto-complete
(use-package company
  :ensure t
  :config 
  (global-company-mode 1)
  (setq company-minimum-prefix-length 1)
  (global-set-key (kbd "C-SPC") #'company-complete)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))


;; Install and configure Anzu 
;; displays current match and total matches information in the mode-line in various search modes.
;; (use-package anzu
;;   :ensure t
;;   :config
;;   (global-anzu-mode +1))

;; Gitsigns like behvior
;; (use-package diff-hl
;;   :ensure t
;;   :config
  ;; (global-diff-hl-mode))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (global-git-gutter-mode +1)
  ;; (setq git-gutter:update-interval 0.5))
)

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (python-mode . lsp)
	 (typescript-mode . lsp)
	 (js-mode . lsp)
	 (web-mode . lsp)
	 )
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
)

;; Function to toggle maximize buffer
(defun toggle-maximize-buffer() "Maximize buffer"
       (interactive)
       (if (= 1(length (window-list)))
	   (jump-to-register '_)
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))

(use-package autothemer
  :ensure t
  )

(load-theme 'no-clown-fiesta t)
