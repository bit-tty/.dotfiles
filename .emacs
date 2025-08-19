;; Custom file handling
(setq custom-file "~/.emacs.custom.el")
(load-file custom-file)

;; Font configuration
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10"))

;; Basic UI configuration
(tool-bar-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)

;; Window size configuration
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Package setup with performance optimization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 1) ("elpa" . 2)))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t ;; Defer loading for better startup time
      use-package-expand-minimally t)

;; YASnippet for parameter completion and code snippets
(use-package yasnippet
  :ensure t
  :defer 2 ; Load after 2 seconds
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; Snippet navigation keybindings
  (define-key yas-keymap [(tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "TAB") 'yas-next-field)
  (define-key yas-keymap [(backtab)] 'yas-prev-field)
  (define-key yas-keymap (kbd "S-TAB") 'yas-prev-field))

;; Yasnippet snippets collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Which-key for discovering keybindings
(use-package which-key
  :defer 1
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; Company mode for completion
(use-package company
  :defer 1
  :config
  (global-company-mode)
  (setq company-show-numbers t
        company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package company-template
  :after company
  :config
  (define-key company-template-nav-map (kbd "TAB")
    'company-template-forward-field))

;; Enhanced Flycheck for syntax checking with inline errors
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Show errors in echo area
  (setq flycheck-display-errors-delay 0.3
        flycheck-idle-change-delay 0.5
        flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'lines
        flycheck-check-syntax-automatically '(save mode-enabled)))

;; Flycheck inline errors (shows errors right in the buffer)
(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

;; LSP Mode for language server support
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  ;; Performance optimizations
  (setq lsp-idle-delay 0.5
        lsp-log-io nil ; Disable IO logging for performance
        lsp-enable-file-watchers nil ; Disable file watchers for large projects
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-snippet t ; Enable snippets for parameter completion
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate nil ; Disable auto signature help for performance
        lsp-signature-render-documentation nil)
  
  ;; Configure clangd for C development
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"
          "--pch-storage=memory"))
  
  ;; Force C mode for .c files
  (add-to-list 'lsp-language-id-configuration '(c-mode . "c")))

;; Enhanced LSP UI for better interface and inline diagnostics
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; Sideline configuration for inline diagnostics
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.3
        lsp-ui-sideline-ignore-duplicate t)
  
  ;; Doc popup configuration
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20)
  
  ;; Peek configuration
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t))

;; C mode configuration - Linux coding style
(setq c-default-style "linux"
      c-basic-offset 8)  ; Linux kernel uses 8-space indentation

;; Enhanced Linux style with proper tab handling
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

;; Define the Linux tabs-only style
(c-add-style "linux-tabs-only"
             '("linux"
               (c-offsets-alist
                (arglist-cont-nonempty
                 c-lineup-gcc-asm-reg
                 c-lineup-arglist-tabs-only))))

;; Apply Linux coding style to all C files
(add-hook 'c-mode-hook
          (lambda ()
            ;; Use tabs, not spaces
            (setq indent-tabs-mode t)
            ;; Show trailing whitespace (good practice)
            (setq show-trailing-whitespace t)
            ;; Set the Linux tabs-only style
            (c-set-style "linux-tabs-only")
            ;; Ensure tab width is 8 (Linux standard)
            (setq tab-width 8)
            ;; Set basic offset to 8
            (setq c-basic-offset 8)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;; Optional: Add some Linux kernel specific settings
(defun setup-kernel-coding-style ()
  "Additional settings for strict Linux kernel style compliance"
  (interactive)
  ;; Maximum line length for kernel code
  (setq fill-column 80)
  ;; Highlight lines that are too long
  (require 'whitespace)
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 80)
  (whitespace-mode 1))

;; Uncomment the next line if you want kernel-specific settings for all C files
;; (add-hook 'c-mode-hook 'setup-kernel-coding-style)

;; CUA mode for familiar cut/copy/paste keybindings
(cua-mode 1)
(setq cua-auto-tabify-rectangles nil
      cua-keep-region-after-copy t)

;; Global keybindings
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "C-c e") 'flycheck-list-errors)

;; Visual enhancements for better error display
(custom-set-faces
 '(flycheck-error ((t (:underline (:color "red" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "orange" :style wave)))))
 '(flycheck-info ((t (:underline (:color "blue" :style wave))))))

;; Better defaults
(setq-default indent-tabs-mode nil ; Use spaces instead of tabs (overridden in C mode)
              tab-width 4         ; Set tab width (overridden in C mode to 8)
              fill-column 80)     ; Set line length

;; Global modes
(global-auto-revert-mode 1)   ; Auto-reload files changed outside Emacs
(show-paren-mode 1)           ; Highlight matching parentheses
(delete-selection-mode 1)     ; Replace selected text when typing
(save-place-mode 1)           ; Remember cursor position in files

;; Performance optimizations
(setq gc-cons-threshold (* 2 1000 1000)  ; Increase garbage collection threshold
      read-process-output-max (* 1024 1024) ; Increase process output buffer
      inhibit-startup-screen t             ; Skip startup screen
      initial-scratch-message nil)         ; Clear scratch buffer message

;; Backup and auto-save configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil) ; Disable lockfiles

;; Create backup and auto-save directories if they don't exist
(let ((backup-dir "~/.emacs.d/backups")
      (auto-save-dir "~/.emacs.d/auto-saves"))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Optimize startup time - reset gc threshold after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))
