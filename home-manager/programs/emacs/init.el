(require 'use-package)
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("bk" . kill-this-buffer))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
    (setq meow-use-enhanced-selection-effect t)
  (meow-setup)
  (meow-global-mode 1))

(use-package general
  :config
  ;; SPC as the global leader key
  (general-create-definer spl3g/leader-keys
    :prefix "C-c")

  (spl3g/leader-keys
    ;; Buffers
    "b" '(:ignore t :wk "Buffer")
    "bi" '(ibuffer :wk "ibuffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    "," '(consult-buffer :wk "Switch to buffer")
    "." '(find-file :wk "Find file")
    ;; Splits
    "w" '(:ignore t :wk "Splits")
    "wv" '(split-window-right :wk "Split vertical")
    "ws" '(split-window-below :wk "Split")
    "ww" '(other-window :wk "Cycle throug windows")
    "wc" '(delete-window :wk "Close window")
    "wd" '(delete-window :wk "Close window")
    "wl" '(evil-window-right :wk "")
    "wj" '(evil-window-down :wk "")
    "wk" '(evil-window-up :wk "")
    "wh" '(evil-window-left :wk "")
    "wo" '(delete-other-windows :wk "")
    ;; Files
    "f" '(:ignore t :wk "Files")
    "fr" '(consult-recent-file :wk "Resent files")
    "fc" '((lambda () (interactive) (find-file "~/.nixfiles/home-manager/programs/emacs/config.org")) :wk "Edit emacs config")
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fU" '(sudo-edit :wk "Sudo edit file")
    "l" '(lsp-keymap-prefix :wk "LSP")
    ;; Opening.. things
    "o" '(:ignore t)
    "ot" '(eat-toggle :wk "Eat terminal")))

(defun spl3g/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'spl3g/disable-scroll-bars)

(setq default-frame-alist '((font . "Source Code Pro")))
(set-face-attribute 'default nil
		    :font "Source Code Pro"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :font "Source Code Pro"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'variable-pitch nil
		    :font "Rubik"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :weight 'bold)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

(use-package autothemer)
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin t)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package good-scroll
  :init (good-scroll-mode))

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "Yep, it's emacs, not vim")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (add-to-list 'dashboard-item-generators '(config . dashboard-open-config))
  (setq dashboard-items '((recents . 5)
                          (agenda . 5))))

(setq ring-bell-function 'ignore)

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(defalias 'yes-or-no #'y-or-n-p)

(add-hook 'org-mode-hook 'org-indent-mode)
(require 'org-tempo)

(use-package toc-org
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-auto-tangle
  :init (add-hook 'org-mode-hook 'org-auto-tangle-mode))

(use-package org-download
  :hook (dired-mode-hook . org-download-enable))

(use-package direnv
  :config
  (direnv-mode))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("M-j" . vertico-next)
              ("M-k" . vertico-previous))
  :config
  (savehist-mode 1))
(use-package emacs
:init
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file)
   ;; :preview-key "M-."

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package treemacs
  :config
  (setq treemacs-no-png-images t))

(use-package treemacs-evil)

(use-package treemacs-all-the-icons)

(use-package smartparens
  :init (smartparens-global-mode)
  :hook (prog-mode-hook . turn-on-smartparens-strict-mode)
  :config
  ;; Snitched from doom
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't eagerly escape Swift style string interpolation
  (sp-local-pair 'swift-mode "\\(" ")" :when '(sp-in-string-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))
  ;; Expand C-style comment blocks.
  (defun +default-open-doc-comments-block (&rest _ignored)
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    (" | " "*")
                    ("|[i]\n[i]" "RET"))))    
;; (use-package parinfer-rust-mode
;;   :hook (lisp-mode-hook . parinfer-rust-mode))
;; (electric-pair-mode 1)

(use-package dap-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(use-package move-text
  :bind (("C-M-k" . move-text-up)
         ("C-M-j" . move-text-down)))

(set-default 'truncate-lines t)

(use-package eat
  :custom
  (eat-semi-char-non-bound-keys
   '(seq-difference eat-semi-char-non-bound-keys '([C-left] [C-right]))))

(defun eat-toggle()
  "Open eat terminal as a popup."
  (interactive)
  (if (eq major-mode 'eat-mode)
      (delete-window)
    (let ((buff (get-buffer-create eat-buffer-name)))
      (cl-assert (and buff (buffer-live-p buff)))
      (funcall #'pop-to-buffer buff)
      (with-current-buffer buff
              (setq-local split-width-threshold nil)
              (setq-local window-min-height 2)
              (unless (derived-mode-p 'eat-mode)
                (eat))))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)

  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  (add-to-list 'completion-at-point-functions  (cape-capf-buster #'lsp-completion-at-point))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))
(use-package py-autopep8
  :hook (python-mode . py-autopep8-mode))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))
(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
        (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package fish-mode
  :mode "(.fish)$")

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

(use-package yuck-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package js2-mode)

(use-package corfu
  :custom
  (corfu-cycle t) 
  (corfu-preselect 'prompt)
  (corfu-auto t)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq tab-always-indent 'complete))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

(use-package flycheck
  :init (global-flycheck-mode))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package yasnippet
  :init (yas-global-mode))
(use-package yasnippet-snippets)

;; (add-to-list 'load-path "~/telega.el")
;; (require 'telega)

(use-package magit)

;; (use-package exwm)
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :bind (:map dired-mode-map
               ("h" . 'dired-up-directory)
               ("l" . 'dired-find-file)
               ("v" . 'meow-visit)))

(use-package all-the-icons-dired
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
