;;;; Optimisations from https://github.com/jamescherti/minimal-emacs.d

;;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 1024 1024))  ; 512kb

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (unless noninteractive
    (progn
      ;; Disable mode-line-format during init
      (defun minimal-emacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))

      (defvar minimal-emacs--default-mode-line-format mode-line-format
        "Default value of `mode-line-format'.")
      (setq-default mode-line-format nil)

      (defun minimal-emacs--startup-load-user-init-file (fn &rest args)
        "Advice for startup--load-user-init-file to reset mode-line-format."
        (let (init)
          (unwind-protect
              (progn
                (apply fn args)  ; Start up as normal
                (setq init t))
            (unless init
              ;; If we don't undo inhibit-{message, redisplay} and there's an
              ;; error, we'll see nothing but a blank Emacs frame.
              (minimal-emacs--reset-inhibited-vars-h))
            (unless (default-toplevel-value 'mode-line-format)
              (setq-default mode-line-format
                            minimal-emacs--default-mode-line-format)))))

      (advice-add 'startup--load-user-init-file :around
                  #'minimal-emacs--startup-load-user-init-file))

    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)))

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors 'silent)

;;; UI elements

;; Disable startup screens and messages
(setq inhibit-splash-screen t)


(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))

(unless (daemonp)
  (unless noninteractive
    (when (fboundp 'tool-bar-setup)
      ;; Temporarily override the tool-bar-setup function to prevent it from
      ;; running during the initial stages of startup
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file
          (:before (&rest _) minimal-emacs-setup-toolbar)
        (advice-remove #'tool-bar-setup #'ignore)
        (tool-bar-setup)))))

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;;; And that's mine

(setq package-enable-at-startup nil)
(setq-default pgtk-wait-for-event-timeout 0)

(let ((mono-spaced-font "FiraCode Nerd Font")
	  (proportionately-spaced-font "Inconsonata"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0)
  (set-face-attribute 'italic nil :underline nil))


(provide 'early-init)
;;; early-init.el ends here
