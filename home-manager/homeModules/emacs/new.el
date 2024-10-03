(load (expand-file-name "elpaca.el" user-emacs-directory))

(use-package emacs
  :ensure nil
  :demand t
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  
  ;; Scrolling
  (scroll-margin 5)
  (scroll-conservatively 101)
  (mouse-wheel-progressive-speed nil)
  
  ;; Backups
  (create-lockfiles nil)
  (make-backup-files nil)
  (backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (tramp-backup-directory-alist backup-directory-alist)
  (backup-by-copying-when-linked t)
  (backup-by-copying t)  ; Backup by copying rather renaming
  (delete-old-versions t)  ; Delete excess backup versions silently
  (version-control t)  ; Use version numbers for backup files
  (kept-new-versions 5)
  (kept-old-versions 5)
  (vc-make-backup-files nil)
  
  ;; Auto saving
  (auto-save-default t)
  (auto-save-include-big-deletions t)
  (kill-buffer-delete-auto-save-files t)

  ;; Ediff
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   e                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  (set-face-attribute 'default nil
                      :font "FiraCode Nerd Font"
                      :height 110
                      :weight 'medium))

;; Auto reverting buffers
(use-package auto-revert
  :ensure nil
  :no-require t
  :custom
  (revert-without-query (list "."))
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t)

  ;; Revert other buffers (e.g, Dired)
  (global-auto-revert-non-file-buffers t)
  :init
  (global-auto-revert-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 500) ; default is 20
  (recentf-auto-cleanup 'mode)
  :init
  (recentf-mode))

;; Save history
(use-package savehist :ensure nil
  :custom
  (savehist-file "~/.config/emacs/var/savehist.el")
  (history-length 500)
  (savehist-additional-variables '(kill-ring search-ring))
  :config
  (savehist-mode t))

;; Annoyances
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

;; Emacs no littering
(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))

;; Minibuffer completions
(use-package prescient
  :config
  (prescient-persist-mode)
  (setq completion-styles '(prescient basic)
        completion-category-overrides '((file (styles basic partial-completion))))
  :custom-face
  (prescient-primary-highlight ((t (:inherit 'font-lock-keyword-face)))))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("M-j" . vertico-next)
              ("M-k" . vertico-previous)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; In buffer completions
(use-package corfu
  :hook (emacs-startup . global-corfu-mode)
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous)
              ([remap previous-line] . nil)
              ([remap next-line] . nil))
  :custom
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-popupinfo-delay 0.5)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-scroll-margin 4)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :after corfu
  :custom
  (dabbrev-ignored-buffer-modes '(archive-mode image-mode eshell-mode))
  :config
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key
  :ensure nil
  :init
  (which-key-mode))

;; Theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defadvice consult-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

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

;; Some things for programming
(electric-pair-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers 'relative)
(visual-line-mode 1)

;; My eshell functions
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
                            (car eshell-windows))))
    (if selected-window
        (progn
          (select-window selected-window)
          (switch-to-buffer selected-buffer))
      (switch-to-buffer-other-window selected-buffer))))

(defun spl3g/open-eshell ()
  (interactive)
  (let* ((project (project-current))
         (func (if project
                   'project-eshell
                 'eshell))
         (buffer-name (if project
                          (format "*%s-eshell*" (project-name project))
                        "*eshell*")))
    (if (not (get-buffer buffer-name))
        (let ((buf (funcall func)))
          (switch-to-buffer (other-buffer buf))
          (switch-to-buffer-other-window buf))
      (switch-to-buffer-other-window buffer-name))))

(keymap-global-set "C-c o s" 'spl3g/select-eshell)
(keymap-global-set "C-c o t" 'spl3g/open-eshell)

;; LSP
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-suggest-server-download nil)
  (lsp-keymap-prefix "C-c s")
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(prescient)))

  :hook
  (lsp-completion-mode . lsp-mode-setup-completion)
  (nix-mode . lsp-mode)
  (typescript-ts-mode . lsp-mode)
  (go-ts-mode . lsp-mode)
  (web-mode . lsp-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.35))))     (use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-doc-enable nil))

;; Templates
(use-package tempel
  :bind (:map tempel-map
              ("M-TAB" . tempel-next))
  :custom
  (tempel-trigger-prefix "<")
  :init
  (add-to-list 'completion-at-point-functions #'tempel-complete))

(use-package tempel-collection
  :ensure t
  :after tempel)

;; Formatting
(use-package apheleia
  :hook (prog-mode . apheleia-mode))

;; Direnv
(use-package direnv
  :config
  (direnv-mode))

;; Dired
(use-package dired :ensure nil
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-hal --group-directories-first")
  (dired-dwim-target t))

;; Calc
(use-package casual-calc
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))

;; Magit
(use-package magit
  :bind (("C-c o g" . magit)))

;; Window switching
(use-package ace-window
  :bind ("M-o" . ace-window))

(provide 'new)
;;; new.el ends here
