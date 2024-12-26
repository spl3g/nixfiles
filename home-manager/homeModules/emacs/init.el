(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(load (expand-file-name "elpaca.el" user-emacs-directory))


;;; Basic behaviour

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))

(setq uniquify-buffer-name-style 'forward)

;; Auto save
(setq auto-save-default t)
(setq auto-save-include-big-deletions t)
(setq kill-buffer-delete-auto-save-files t)

;; Auto revert
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Save place in buffer
(setq save-place-limit 600)
(add-hook 'after-init-hook #'save-place-mode)


(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(keymap-global-set "C-g" #'prot/keyboard-quit-dwim)


(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))

(use-package which-key
  :diminish
  :ensure nil
  :init
  (which-key-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode))


(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(electric-pair-mode t)


(global-set-key [remap list-buffers] 'ibuffer)

;;; Tweak the looks of Emacs
(setq-default tab-width 4)

(global-word-wrap-whitespace-mode t)
(global-visual-line-mode t)


(let ((mono-spaced-font "FiraCode Nerd Font")
      (proportionately-spaced-font "Noto Serif"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0)
  (set-face-attribute 'italic nil :underline nil))


;; Mode Line
(use-package mood-line
  ;; Enable mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-segment-modal-meow-state-alist
   '((normal "N" . mood-line-meow-normal)
     (insert "I" . mood-line-meow-insert)
     (keypad "K" . mood-line-meow-keypad)
     (beacon "B" . mood-line-meow-beacon)
     (motion "M" . mood-line-meow-motion)))
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  :custom-face
  (mood-line-meow-beacon ((t (:inherit 'font-lock-function-name-face :weight bold))))
  (mood-line-meow-insert ((t (:inherit 'font-lock-string-face :weight bold))))
  (mood-line-meow-keypad ((t (:inherit 'font-lock-keyword-face :weight bold))))
  (mood-line-meow-motion ((t (:inherit 'font-lock-constant-face :weight bold))))
  (mood-line-meow-normal ((t (:inherit 'font-lock-variable-use-face :weight bold)))))


(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun spl3g/after-load-theme-advice (&rest r)
  "Run `after-load-theme-hook' and ignore R."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'spl3g/after-load-theme-advice)

(defun widen-mode-line ()
  "Widen the mode-line."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :inherit 'mode-line
                      :box '(:line-width 8 :style flat-button))
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'mode-line-inactive
                      :box '(:line-width 8 :style flat-button)))

(add-hook 'after-load-theme-hook 'widen-mode-line)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package diminish)


;; Scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 5)
(setq mouse-wheel-progressive-speed nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)

;; Annoyances
(blink-cursor-mode -1)
(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(setq delete-pair-blink-delay 0.03)
(setq blink-matching-paren nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))



;;; Configure the minibuffer and completions
(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-j" . vertico-next)
              ("M-k" . vertico-previous)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

(use-package prescient
  :config
  (prescient-persist-mode)
  (setq completion-styles '(prescient basic)
        completion-category-overrides '((file (styles basic partial-completion))))
  :custom-face
  (prescient-primary-highlight ((t (:inherit 'font-lock-keyword-face)))))

(use-package corfu-prescient
  :after corfu
  :config
  (corfu-prescient-mode))

(use-package vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-file "~/.config/emacs/var/savehist.el")
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring search-ring)))

(defun cape--dabbrev-project ()
  (let* ((project (project-current))
         (buffers (when project
                    (project-buffers project))))
    (if project
        (butlast buffers (- (length buffers) 4))
      (cape--buffers-major-mode))))

(use-package cape
  :after corfu
  :custom
  (dabbrev-ignored-buffer-modes '(archive-mode image-mode eshell-mode))
  (cape-dabbrev-check-other-buffers #'cape--dabbrev-project)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu
  :hook (elpaca-after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous))
  :custom
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-min-width 20)
  (corfu-scroll-margin 4)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode 1))

;;; The file manager (Dired)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-listing-switches "-hal --group-directories-first"))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Additional Packages


;;; Eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)))
(add-hook 'eshell-exec-hook (lambda (p)
                              (buffer-disable-undo)))
(add-hook 'eshell-kill-hook (lambda (p s)
                              (buffer-enable-undo)))
(setq eshell-history-size 500
      eshell-history-append t)

(defun spl3g/eshell-dwim ()
  (interactive)
  (defvar current-prefix-arg)
  (let* ((project (project-current))
         (func (if project
                   'project-eshell
                 'eshell))
         (buffer-name (if project
                          (format "*%s-eshell*" (project-name project))
                        "*eshell*"))
         (current-prefix-arg t))
    (if (not (get-buffer buffer-name))
        (let ((buf (funcall func)))
          (switch-to-buffer (other-buffer buf))
          (switch-to-buffer-other-window buf))
      (switch-to-buffer-other-window buffer-name))))

(defun spl3g/select-eshell ()
  (interactive)
  (let* ((eshell-buffers (seq-filter (lambda (buffer)
  									   (eq (with-current-buffer buffer major-mode)
  										   'eshell-mode))
  									 (buffer-list)))
  		 (eshell-names (mapcar (lambda (buffer) (buffer-name buffer)) eshell-buffers))
         (eshell-windows (remove nil (mapcar (lambda (buffer)
                                               (let* ((window (get-buffer-window buffer))
                                                      (name (buffer-name buffer)))
                                                 (when window
                                                   (cons name window))))
                                             eshell-buffers)))
         (selected-buffer (if (length> eshell-buffers 1)
  							  (completing-read "Select eshell buffer: " eshell-names)
  							(car eshell-buffers)))
         (selected-window (if (length> eshell-windows 1)
                              (cdr (assoc (completing-read "Select window to place the buffer in: " eshell-windows) eshell-windows))
                            (cdar eshell-windows))))
    (if selected-window
        (progn
          (select-window selected-window)
          (switch-to-buffer selected-buffer))
      (switch-to-buffer-other-window selected-buffer))))

(defun spl3g/rename-current-eshell ()
  "Add NAME in <> to the current project eshell buffer"
  (interactive)
  (let* ((eshell-buffers (seq-filter (lambda (buffer)
									   (eq (with-current-buffer buffer major-mode)
										   'eshell-mode))
									 (buffer-list)))
		 (eshell-names (mapcar (lambda (buffer) (buffer-name buffer)) eshell-buffers))
         (eshell-windows (remove nil (mapcar (lambda (buffer)
                                               (let* ((window (get-buffer-window buffer))
                                                      (name (buffer-name buffer)))
                                                 (when window
                                                   (cons name window))))
                                             eshell-buffers)))
         (selected-window (if (and (sequencep eshell-windows) (length> eshell-windows 1))
                              (assoc (completing-read "Select window to place the buffer in: " eshell-windows) eshell-windows)
                            (car eshell-windows)))
         (additional-name (when selected-window
                            (read-string "Additional name: ")))
         (buffer-name (when selected-window
                        (car (split-string (car selected-window) "<"))))
         (formatted-name (if (length> additional-name 0)
                             (format "%s<%s>" buffer-name additional-name)
                           buffer-name))
         )
    (if selected-window
        (with-current-buffer (car selected-window)
          (rename-buffer formatted-name))
      (message "No eshell buffers"))))

(keymap-global-set "C-c o t" 'spl3g/eshell-dwim)
(keymap-global-set "C-c o s" 'spl3g/select-eshell)
(keymap-global-set "C-c o r" 'spl3g/rename-current-eshell)


;; Save history on every command
(with-eval-after-load 'eshell
  (setq eshell-save-history-on-exit nil)
  (defun eshell-append-history ()
	"Call `eshell-write-history' with the `append' parameter set to `t'."
	(when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
		(ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
		(let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))

  (add-hook 'eshell-pre-command-hook 'eshell-append-history))


(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp))


(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :custom
  (eat-enable-auto-line-mode t)
  :custom-face
  (ansi-color-bright-blue ((t (:inherit 'ansi-color-blue))))
  (ansi-color-bright-red ((t (:inherit 'ansi-color-red))))
  (ansi-color-bright-red ((t (:inherit 'ansi-color-red))))
  (ansi-color-bright-cyan ((t (:inherit 'ansi-color-cyan))))
  (ansi-color-bright-black ((t (:inherit 'ansi-color-black))))
  (ansi-color-bright-green ((t (:inherit 'ansi-color-green))))
  (ansi-color-bright-white ((t (:inherit 'ansi-color-white))))
  (ansi-color-bright-yellow ((t (:inherit 'ansi-color-yellow))))
  (ansi-color-bright-magenta ((t (:inherit 'ansi-color-magenta)))))


(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))


;;; Programming things
(use-package tempel
  :bind (:map tempel-map
              ("M-TAB" . tempel-next))
  :custom
  (tempel-trigger-prefix "<")
  :init
  (add-hook 'completion-at-point-functions #'tempel-complete))

(use-package tempel-collection
  :ensure t
  :after tempel)


(use-package apheleia
  :diminish
  :hook (prog-mode-hook . apheleia-mode))


(use-package direnv
  :hook 
  (elpaca-after-init . direnv-mode))


(use-package scratch
  :commands scratch)


(use-package magit
  :after transient
  :bind (("C-c o g" . magit)))


(use-package treesit-auto
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (delete 'html treesit-auto-langs))


;; LSP shit
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
  			  ("C-c s a" . eglot-code-actions)
  			  ("C-c s r" . eglot-rename)
  			  ("C-c s h" . eldoc)
  			  ("C-c s f" . eglot-format)
  			  ("C-c s F" . eglot-format-buffer)
  			  ("C-c s d" . xref-find-definitions-at-mouse)
  			  ("C-c s R" . eglot-reconnect))
  :custom
  (completion-category-overrides '((eglot (styles prescient))
                                   (eglot-capf (styles prescient))))
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (setq-default
   eglot-workspace-configuration
   `(:nixd ( :nixpkgs (:expr "import <nixpkgs> { }")
			 :formatting (:command ["nixpkgs-fmt"])
			 :options ( :nixos (:expr "(builtins.getFlake \"/home/jerpo/nixfiles\").nixosConfigurations.ltrr-mini.options")
						:home-manager (:expr "(builtins.getFlake \"/home/jerpo/nixfiles\").homeConfigurations.\"jerpo@ltrr-mini\".options"))))))

(use-package eldoc
  :ensure nil
  :diminish)


(use-package lsp-snippet-tempel
  :ensure (:host github :repo "tpeacock19/lsp-snippet")
  :config
  (lsp-snippet-tempel-eglot-init))


(use-package flycheck-eglot
  :hook (global-flycheck-mode . global-flycheck-eglot-mode))

(use-package flycheck
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.35)))
  (add-to-list 'flycheck-checkers 'python-ruff)
  :hook (elpaca-after-init . global-flycheck-mode))


(use-package sideline
  :custom
  (sideline-truncate t))

(use-package sideline-flycheck
  :after (flycheck sideline)
  :hook
  (flycheck-mode . sideline-mode)
  (flycheck-mode . sideline-flycheck-setup)
  :custom
  (flycheck-display-errors-function nil)
  (sideline-flycheck-display-mode 'line)
  :init
  (add-to-list 'sideline-backends-right 'sideline-flycheck))


;;; Languages
(use-package web-mode
  :mode
  ("\\.html\\'"
   "\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'")
  :hook
  (eb-mode . (lambda () (electric-pair-local-mode -1)))
  :custom
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-script-padding tab-width)
  (web-mode-style-padding tab-width)
  :config
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))


(use-package org-mode
  :ensure nil
  :custom
  (org-startup-indented t)
  :mode "\\.org\\'")


(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))


(use-package typescript-ts-mode
  :ensure nil
  :hook (typescript-ts-mode . (lambda () (setq forward-sexp-function nil)))
  :custom (typescript-ts-mode-indent-offset tab-width))

(provide 'init)
;;; init.el ends here.
