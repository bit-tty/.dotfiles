;;; init.el --- Emacs Configuration

;;; Commentary:
;; Personal Emacs configuration for C, LaTeX, Rust, and Zig development

;;; Code:

;;==============================================================================
;; BASIC SETTINGS
;;==============================================================================

;; Custom file handling
(setq custom-file "~/.emacs.custom.el")
(load-file custom-file)

;; Font configuration
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-10"))

;; UI configuration
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(setq cua-enable-cua-keys nil)

;; Window size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Better defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; Global modes
(global-auto-revert-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Enable syntax highlighting globally
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Performance optimizations
(setq gc-cons-threshold (* 2 1000 1000)
      read-process-output-max (* 1024 1024)
      inhibit-startup-screen t
      initial-scratch-message nil)

;; Backup and auto-save configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil)

;; Create backup and auto-save directories
(let ((backup-dir "~/.emacs.d/backups")
      (auto-save-dir "~/.emacs.d/auto-saves"))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

;;==============================================================================
;; PACKAGE MANAGEMENT
;;==============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 1) ("elpa" . 2)))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;==============================================================================
;; THEME AND SYNTAX HIGHLIGHTING
;;==============================================================================

;; No custom theme - using default Emacs appearance

;; Rainbow delimiters - color matching parentheses
(use-package rainbow-delimiters
  :defer nil
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO, FIXME, NOTE, etc.
(use-package hl-todo
  :defer nil
  :config
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("HACK"   . "#FF8C00")
          ("NOTE"   . "#1E90FF")
          ("DEPRECATED" . "#B22222"))))

;;==============================================================================
;; COMPLETION AND SNIPPETS
;;==============================================================================

;; YASnippet
(use-package yasnippet
  :defer nil
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (define-key yas-keymap [(tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "TAB") 'yas-next-field)
  (define-key yas-keymap [(backtab)] 'yas-prev-field)
  (define-key yas-keymap (kbd "S-TAB") 'yas-prev-field))

(use-package yasnippet-snippets
  :defer nil
  :after yasnippet)

;; Which-key
(use-package which-key
  :defer nil
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; Company mode
(use-package company
  :defer nil
  :init
  (global-company-mode)
  :config
  (setq company-show-numbers t
        company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-minimum-prefix-length 1))

;;==============================================================================
;; SYNTAX CHECKING
;;==============================================================================

(use-package flycheck
  :defer nil
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.3
        flycheck-idle-change-delay 0.5
        flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'lines
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package flycheck-inline
  :defer nil
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

;; Global keybindings for flycheck
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "C-c e") 'flycheck-list-errors)

;;==============================================================================
;; LSP MODE
;;==============================================================================

(use-package lsp-mode
  :defer nil
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (zig-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-snippet t
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil)
  
  ;; Configure clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"
          "--pch-storage=memory"))
  
  (add-to-list 'lsp-language-id-configuration '(c-mode . "c")))

(use-package lsp-ui
  :defer nil
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.3
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t))

;;==============================================================================
;; C PROGRAMMING
;;==============================================================================

;; GNU C style with tabs-only indentation
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

(c-add-style "gnu-tabs-only"
             '("gnu"
               (c-offsets-alist
                (arglist-cont-nonempty
                 c-lineup-gcc-asm-reg
                 c-lineup-arglist-tabs-only))))

;; C mode configuration
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width 8
                  c-basic-offset 2)
            (c-set-style "gnu-tabs-only")))

;; File associations
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;;==============================================================================
;; ORG-ROAM
;;==============================================================================

(use-package org-roam
  :custom
  (org-roam-directory "~/org-roam/")
  :bind (("C-c n f" . org-roam-node-find))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode))

;;==============================================================================
;; LATEX CONFIGURATION
;;==============================================================================

;; AUCTeX
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-view-program-list '(("Zathura" "zathura %o"))
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex
        TeX-electric-sub-and-superscript t
        TeX-save-query nil)
  
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  
  ;; Enhanced syntax highlighting for LaTeX
  (setq font-latex-fontify-script t
        font-latex-fontify-sectioning 'color
        font-latex-script-display '((raise -0.3) . (raise 0.3)))
  
  ;; Enable font-lock mode for syntax coloring
  (add-hook 'LaTeX-mode-hook 'font-lock-mode))

;; RefTeX
(use-package reftex
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; Company-AUCTeX
(use-package company-auctex
  :defer t
  :after (company auctex)
  :config
  (company-auctex-init))

;; CDLaTeX
(use-package cdlatex
  :defer t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  (setq cdlatex-math-symbol-alist
        '((?. "\\cdot")
          (?< "\\leq")
          (?> "\\geq")
          (?* "\\times"))))

;; PDF Tools
(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
  
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward))

;; LaTeX Preview Pane
(use-package latex-preview-pane
  :defer t
  :config
  (setq latex-preview-pane-enable nil))

;; Magic LaTeX Buffer - prettify LaTeX code in buffer
(use-package magic-latex-buffer
  :defer t
  :hook (LaTeX-mode . magic-latex-buffer-mode)
  :config
  (setq magic-latex-buffer-preview-scale 1.0))

;; Highlight LaTeX
(use-package highlight-numbers
  :defer t
  :hook (LaTeX-mode . highlight-numbers-mode))

(use-package highlight-quoted
  :defer t
  :hook (LaTeX-mode . highlight-quoted-mode))

;; LaTeX mode hook
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (auto-fill-mode 1)
            (turn-on-reftex)
            (outline-minor-mode 1)
            (visual-line-mode 1)
            ;; Enable prettify symbols for LaTeX
            (prettify-symbols-mode 1)))

;; Prettify symbols in LaTeX mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  '(("\\alpha" . ?α)
                    ("\\beta" . ?β)
                    ("\\gamma" . ?γ)
                    ("\\delta" . ?δ)
                    ("\\epsilon" . ?ε)
                    ("\\zeta" . ?ζ)
                    ("\\eta" . ?η)
                    ("\\theta" . ?θ)
                    ("\\lambda" . ?λ)
                    ("\\mu" . ?μ)
                    ("\\pi" . ?π)
                    ("\\sigma" . ?σ)
                    ("\\phi" . ?φ)
                    ("\\omega" . ?ω)
                    ("\\Delta" . ?Δ)
                    ("\\Gamma" . ?Γ)
                    ("\\Lambda" . ?Λ)
                    ("\\Phi" . ?Φ)
                    ("\\Psi" . ?Ψ)
                    ("\\Omega" . ?Ω)
                    ("\\int" . ?∫)
                    ("\\sum" . ?∑)
                    ("\\prod" . ?∏)
                    ("\\sqrt" . ?√)
                    ("\\infty" . ?∞)
                    ("\\in" . ?∈)
                    ("\\notin" . ?∉)
                    ("\\subset" . ?⊂)
                    ("\\subseteq" . ?⊆)
                    ("\\supset" . ?⊃)
                    ("\\supseteq" . ?⊇)
                    ("\\cup" . ?∪)
                    ("\\cap" . ?∩)
                    ("\\forall" . ?∀)
                    ("\\exists" . ?∃)
                    ("\\neg" . ?¬)
                    ("\\wedge" . ?∧)
                    ("\\vee" . ?∨)
                    ("\\rightarrow" . ?→)
                    ("\\leftarrow" . ?←)
                    ("\\Rightarrow" . ?⇒)
                    ("\\Leftarrow" . ?⇐)
                    ("\\leftrightarrow" . ?↔)
                    ("\\Leftrightarrow" . ?⇔)
                    ("\\leq" . ?≤)
                    ("\\geq" . ?≥)
                    ("\\neq" . ?≠)
                    ("\\approx" . ?≈)
                    ("\\equiv" . ?≡)
                    ("\\times" . ?×)
                    ("\\cdot" . ?⋅)
                    ("\\partial" . ?∂)
                    ("\\nabla" . ?∇)))))

;; Spell checking configuration
(with-eval-after-load 'flyspell
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; LaTeX keybindings
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-a") 'TeX-command-run-all)
  (define-key LaTeX-mode-map (kbd "C-c p") 'latex-preview-pane-mode)
  (define-key LaTeX-mode-map (kbd "C-c C-v") 'TeX-view)
  (define-key LaTeX-mode-map (kbd "C-c C-l") 'TeX-recenter-output-buffer)
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count))

;; LaTeX word count
(defun latex-word-count ()
  "Count words in LaTeX document using texcount."
  (interactive)
  (let* ((file (buffer-file-name))
         (output (shell-command-to-string (concat "texcount -brief " file))))
    (message "Word count: %s" output)))

;; LaTeX snippet creation
(defun create-latex-snippets-directory ()
  "Create LaTeX snippets directory and add technical writing snippets."
  (let ((latex-snippet-dir "~/.emacs.d/snippets/latex-mode"))
    (unless (file-exists-p latex-snippet-dir)
      (make-directory latex-snippet-dir t))
    
    (let ((snippets '(
                     ("article" . "# -*- mode: snippet -*-
# name: article template
# key: article
# --
\\documentclass[11pt,a4paper]{article}
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{listings}
\\usepackage{booktabs}

\\title{${1:Title}}
\\author{${2:Author}}
\\date{\\today}

\\begin{document}
\\maketitle

\\section{${3:Introduction}}
$0

\\end{document}")

                     ("fig" . "# -*- mode: snippet -*-
# name: figure
# key: fig
# --
\\begin{figure}[${1:htbp}]
    \\centering
    \\includegraphics[width=${2:0.8}\\textwidth]{${3:filename}}
    \\caption{${4:caption}}
    \\label{fig:${5:label}}
\\end{figure}
$0")

                     ("table" . "# -*- mode: snippet -*-
# name: table
# key: table
# --
\\begin{table}[${1:htbp}]
    \\centering
    \\caption{${2:caption}}
    \\label{tab:${3:label}}
    \\begin{tabular}{${4:lll}}
        \\toprule
        ${5:Header 1} & ${6:Header 2} & ${7:Header 3} \\\\
        \\midrule
        $0
        \\bottomrule
    \\end{tabular}
\\end{table}")

                     ("eq" . "# -*- mode: snippet -*-
# name: equation
# key: eq
# --
\\begin{equation}
    \\label{eq:${1:label}}
    $0
\\end{equation}")

                     ("code" . "# -*- mode: snippet -*-
# name: code listing
# key: code
# --
\\begin{lstlisting}[language=${1:C}, caption={${2:caption}}, label={lst:${3:label}}]
$0
\\end{lstlisting}")

                     ("sec" . "# -*- mode: snippet -*-
# name: section
# key: sec
# --
\\section{${1:Section Title}}
\\label{sec:${2:label}}
$0")

                     ("subsec" . "# -*- mode: snippet -*-
# name: subsection
# key: subsec
# --
\\subsection{${1:Subsection Title}}
\\label{subsec:${2:label}}
$0")

                     ("enum" . "# -*- mode: snippet -*-
# name: enumerate
# key: enum
# --
\\begin{enumerate}
    \\item $0
\\end{enumerate}")

                     ("item" . "# -*- mode: snippet -*-
# name: itemize
# key: item
# --
\\begin{itemize}
    \\item $0
\\end{itemize}")

                     ("align" . "# -*- mode: snippet -*-
# name: align
# key: align
# --
\\begin{align}
    $0
\\end{align}"))))
      
      (dolist (snippet snippets)
        (let ((filename (concat latex-snippet-dir "/" (car snippet))))
          (unless (file-exists-p filename)
            (with-temp-file filename
              (insert (cdr snippet)))))))))

(with-eval-after-load 'yasnippet
  (create-latex-snippets-directory))

;; Biber configuration - Fixed version
(with-eval-after-load 'tex
  ;; Set default bibliography backend to biber
  (setq TeX-engine 'default)
  (setq-default TeX-command-extra-options "-shell-escape")
  
  ;; Ensure AUCTeX knows about Biber
  (unless (assoc "Biber" TeX-command-list)
    (add-to-list 'TeX-command-list
                 '("Biber" "biber %s" TeX-run-Biber nil
                   (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode context-mode)
                   :help "Run Biber"))))

;;==============================================================================
;; RUST PROGRAMMING
;;==============================================================================

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred)
  :config
  ;; Use rust-analyzer via LSP
  (setq rustic-lsp-client 'lsp-mode
        rustic-format-on-save nil
        rustic-lsp-server 'rust-analyzer)
  
  ;; Rust compilation settings
  (setq rustic-cargo-bin "cargo"
        rustic-rustfmt-bin "rustfmt"))

;;==============================================================================
;; ZIG PROGRAMMING
;;==============================================================================

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode)
  :hook (zig-mode . lsp-deferred)
  :config
  ;; Zig formatting on save (optional)
  (setq zig-format-on-save nil))  ; Set to t for auto-format

;; Configure zls for LSP
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "zls")
    :major-modes '(zig-mode)
    :server-id 'zls)))

;;==============================================================================
;; STARTUP OPTIMIZATION
;;==============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))

(provide 'init)
;;; init.el ends here
