(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


(load (expand-file-name "elpaca.el" user-emacs-directory))



;;; Basic behaviour


(setopt uniquify-buffer-name-style 'forward)


(setopt enable-recursive-minibuffers t)


;; Auto save
(setopt auto-save-default t)
(setopt auto-save-include-big-deletions t)
(setopt kill-buffer-delete-auto-save-files t)


;; Auto revert
(setopt revert-without-query (list ".")  ; Do not prompt
		auto-revert-stop-on-user-input nil
		auto-revert-verbose t)


;; Revert other buffers (e.g, Dired)
(setopt global-auto-revert-non-file-buffers t)
(add-hook 'elpaca-after-init-hook #'global-auto-revert-mode)


;; Save place in buffer
(setopt save-place-limit 600)
(add-hook 'elpaca-after-init-hook #'save-place-mode)


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
  :config
  (no-littering-theme-backups))


(use-package which-key
  :ensure nil
  :init
  (which-key-mode))


(use-package vundo
  :commands (vundo)
  :hook (vundo-mode . (lambda () (visual-line-mode -1) (setopt truncate-lines t))))


(use-package winner
  :ensure nil
  :bind ("C-0" . winner-undo)
  :custom
  (winner-dont-bind-my-keys t)
  :hook (elpaca-after-init . winner-mode))


(use-package repeat
  :ensure nil
  :hook (elpaca-after-init . repeat-mode))


(use-package avy
  :bind ("M-j" . avy-goto-char-timer))


(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))


(setopt ediff-window-setup-function #'ediff-setup-windows-plain
		ediff-split-window-function #'split-window-horizontally)


(electric-pair-mode t)


(global-set-key [remap list-buffers] 'ibuffer)

;;; Tweak the looks of Emacs
(setq-default tab-width 4)

(global-word-wrap-whitespace-mode t)
(global-visual-line-mode t)


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


;; Scrolling
(setopt scroll-conservatively 101)
(setopt scroll-margin 5)
(setopt mouse-wheel-progressive-speed nil)
(setopt fast-but-imprecise-scrolling t)
(setopt scroll-error-top-bottom t)
(setopt scroll-preserve-screen-position t)


;; Annoyances
(blink-cursor-mode -1)
(setopt visible-bell nil)
(setopt ring-bell-function #'ignore)
(setopt delete-pair-blink-delay 0.03)
(setopt blink-matching-paren nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)

(kill-ring-deindent-mode)
;; Overwrite the default function with a patched one
(defun kill-ring-deindent-buffer-substring-function (beg end delete)
  "Save the text within BEG and END to kill-ring, decreasing indentation.
Delete the saved text if DELETE is non-nil.

In the saved copy of the text, remove some of the indentation, such
that the buffer position at BEG will be at column zero when the text
is yanked."
  (let ((a beg)
		(b end))
	(setq beg (min a b)
		  end (max a b)))
  (let ((indentation (save-excursion (goto-char beg)
									 (current-column)))
		(i-t-m indent-tabs-mode)
		(text (if delete
				  (delete-and-extract-region beg end)
				(buffer-substring beg end))))
	(with-temp-buffer
	  ;; Indent/deindent the same as the major mode in the original
	  ;; buffer.
	  (setq indent-tabs-mode i-t-m)
	  (insert text)
	  (indent-rigidly (point-min) (point-max)
					  (- indentation))
	  (buffer-string))))


;;; Configure the minibuffer and completions
(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))


(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


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
              ("M-k" . corfu-previous)
			  ([remap previous-line] . nil)
              ([remap next-line] . nil))
  :custom
  (corfu-preselect 'prompt)
  (corfu-auto nil)
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
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))


(use-package completion-preview
  :ensure nil
  :hook (elpaca-after-init . global-completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
			  ("TAB" . (lambda ()
						 (interactive)
						 (completion-preview-complete)
						 (completion-at-point)))

			  ("M-i" . (lambda ()
						 (interactive)
						 (let ((com (completion-preview--get 'completion-preview-common))
							   (ind (completion-preview--get 'completion-preview-index))
							   (all (completion-preview--get 'completion-preview-suffixes)))
						   (add-to-history 'corfu-history
										   (substring-no-properties
										    (concat com (nth ind all)))))
						 (completion-preview-insert)))
			  ("M-n" . completion-preview-next-candidate)
			  ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 2))



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



;;; Eshell


(use-package eshell
  :ensure nil
  :hook
  (eshell-exec . (lambda (p) (buffer-disable-undo)))
  (eshell-kill . (lambda (p s) (buffer-enable-undo)))
  (eshell-pre-command . eshell-append-history)

  :bind (("C-c o t" . spl3g/eshell-dwim)
		 ("C-c o s" . spl3g/select-eshell)
		 ("C-c o r" . spl3g/rename-current-eshell))
  
  :custom
  (eshell-history-size 500)
  (eshell-history-append t)
  (eshell-save-history-on-exit nil)
  
  :config
  ;; (add-to-list 'eshell-modules-list 'eshell-tramp)
  
  ;; Save history on every command
  (defun eshell-append-history ()
	"Call `eshell-write-history' with the `append' parameter set to `t'."
	(when eshell-history-ring
	  (let ((newest-cmd-ring (make-ring 1)))
		(ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
		(let ((eshell-history-ring newest-cmd-ring))
		  (eshell-write-history eshell-history-file-name t)))))


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

		   (eshell-windows (remove nil (mapcar (lambda (buffer)
												 (let* ((window (get-buffer-window buffer))
														(name (buffer-name buffer)))
												   (when window
													 (cons name window))))
											   eshell-buffers)))

		   (eshell-names (seq-filter (lambda (buffer) (not (eq buffer (buffer-name (current-buffer)))))
									 (mapcar (lambda (buffer) (buffer-name buffer)) eshell-buffers)))

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
		(message "No eshell buffers")))))


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



;;; Programming things


(use-package yasnippet
  :hook (elpaca-after-init . yas-global-mode))


(use-package yasnippet-snippets
  :after yasnippet)


(use-package apheleia
  :hook (prog-mode-hook . apheleia-mode))


(use-package envrc
  :hook (elpaca-after-init . envrc-global-mode)
  :bind-keymap
  ("C-c e" . envrc-command-map))


(use-package scratch
  :commands scratch)


(use-package transient)

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


(keymap-global-set "C-c c c" 'compile)
(keymap-global-set "C-c c r" 'recompile)


(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



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


(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(use-package dape)



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
   "\\.djhtml\\'"
   "\\.vue\\'")
  :hook
  (eb-mode . (lambda () (electric-pair-local-mode -1)))
  :custom
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-script-padding tab-width)
  (web-mode-style-padding tab-width))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))


(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))


(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'")
  :hook (typescript-ts-mode . (lambda () (setq-local forward-sexp-function nil)))
  :custom
  (typescript-ts-mode-indent-offset tab-width))


(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-mode-indent-offset tab-width))


(use-package markdown-mode
  :mode ("\\.md\\'"))


(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(setopt sql-connection-alist
		'(("postgres-sirius"
           (sql-product 'postgres)
           (sql-user "college")
           (sql-server "127.0.0.1")
           (sql-database "coll")
           (sql-port 5432))))

(setopt sql-sqlite-program "sqlite3")


(use-package elm-mode
  :mode "\\.elm\\'")


(use-package verb
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))



;; Notetaking


(use-package org-mode
  :ensure nil
  :hook (org-mode . variable-pitch-mode)
  :custom
  (org-startup-indented t)
  :mode "\\.org\\'"
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  (org-mode . (lambda () (set-fill-column 90))))


(use-package denote
  :commands (denote denote-create-note denote-journal-extras-new-entry))


(provide 'init)
;;; init.el ends here.
