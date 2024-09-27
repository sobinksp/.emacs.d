;; Disabled startup screen.
;; Don't show splash screen
(setq inhibit-startup-message t)

;; Disable notification sound
(setq visible-bell       nil
      ring-bell-function #'ignore)
;; Toolbar, menubar, scrollbr
(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1) 

;; blinking cursor
(blink-cursor-mode 0)

;; auto pairs
(electric-pair-mode t)
;; Highlight the current line
;; (global-hl-line-mode t)

(setq backup-directory-alist 
  '(("." . "~/.emacs.d/file-backups")))

;; change default find-file path for windows
(when (eq system-type 'windows-nt)
  (let ((appdata-roaming (getenv "APPDATA")))
    (when appdata-roaming
      (setq default-directory (expand-file-name ".emacs.d" appdata-roaming))
    )
  )
  ;; (set-face-attribute 'default nil
  ;;                     :family "JetBrainsMono NF"
  ;;                     :foundry "outline"
  ;;                     :slant 'normal
  ;;                     :weight 'regular
  ;;                     :height 143
  ;;                     :width 'normal)
    ;; (set-face-attribute 'default nil :font "Consolas-14")
    (set-face-attribute 'default nil :font "JetBrainsMono NF-13")
)

 ;; '(default ((t (:family "JetBrainsMono NF" :foundry "outline" :slant normal :thin regular :height 143 :width normal :weight regular)))))
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :foundry "nil"
                      :slant 'normal
                      :weight 'regular
                      :height 150
                      :width 'normal)
)

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
 '(custom-safe-themes
   '("622034e2b06b087d3e950652c93e465f3df6eab50bfdceddaa78077487e9bc24" default))
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(counsel consult doom-themes assemblage-theme flycheck general timu-rouge timu-rogue-theme timu-rouge-theme company evil-leader yasnippet-snippets yasnippet lsp-ui company-lsp lsp-mode diff-hl dff-hl git-gutter-fringe git-gutter anzu autothemer evil-surround evil-commentary evil))
 '(tool-bar-mode nil))

;; Enable auto live-reload buffer if its changed externally.
(global-auto-revert-mode t)

;; live reload for dired
(setq global-auto-revert-non-file-buffers t)
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
    "SPC" '("M-x" . execute-extended-command)
    "wv" 'evil-window-vsplit 
    "wh" 'evil-window-split
    "wm" 'toggle-maximize-buffer
    "we" 'balance-windows
    "ff" 'find-file
    "fs" 'consult-ripgrep
   )
 )

;; Install and configure Evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-mode-line-format nil)
  (setq evil-search-module 'isearch)
  ;; window management
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
    ;; lsp
    (kbd "gd") 'lsp-find-definition
    )
  (evil-define-key 'normal flycheck-mode-map
    (kbd "]d") 'flycheck-next-error
    (kbd "[d") 'flycheck-previous-error)
  ;; (define-key evil-visual-state-map (kbd ">") (kbd ">gv"))
  ;; (define-key evil-visual-state-map (kbd ">") (lambda()
  ;; 						(interactive)
  ;; 						(evil-shift-right (region-beginning) (region-end))
  ;; 						(evil-normal-state)
  ;; 						(evil-visual-restore)))
  ;; (define-key evil-visual-state-map (kbd "<") (lambda()
  ;; 						(interactive)
  ;; 						(evil-shift-left (region-beginning) (region-end))
  ;; 						(evil-normal-state)
  ;; 						(evil-visual-restore)))
)

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
	 (lsp-mode . lsp-diagnostics-mode)
	 (python-mode . lsp)
	 (typescript-mode . lsp)
	 (js-mode . lsp)
	 (web-mode . lsp)
	 )
         ;; (XXX-mode . lsp)
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

(use-package timu-rouge-theme
  :ensure t
)

;; (load-theme 'no-clown-fiesta t)
;; (load-theme 'timu-rouge)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


;; (load-theme 'assemblage t)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars)
)
  
(use-package flycheck
   :ensure t
   :init (global-flycheck-mode))

;; (use-package consult
;;   :ensure t)

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
)
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode 1))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
