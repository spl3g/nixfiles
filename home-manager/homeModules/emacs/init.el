;;; Code:
;; (org-babel-load-file
;; (expand-file-name
;;  "config.org"
;;  user-emacs-directory))
(load (expand-file-name
       "new.el"
       user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992"
     "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" default))
 '(eshell-modules-list
   '(eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-tramp
                  eshell-extpipe eshell-glob eshell-hist eshell-ls eshell-pred
                  eshell-prompt eshell-script eshell-term eshell-unix))
 '(org-agenda-files '("~/prog/js/agenda.org"))
 '(package-selected-packages '(eglot marginalia embark-consult))
 '(safe-local-variable-values '((engine . go)))
 '(warning-suppress-log-types '((emacs) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic)))))
