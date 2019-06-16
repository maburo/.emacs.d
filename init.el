; (require 'package)
; (setq package-enable-at-at-startup nil)
; (setq package-archives '(("org"   . "http://orgmode.org/elpa/")
;                          ("gnu"   . "http://elpa.gnu.org/packages/")
;                          ("melpa" . "https://melpa.org/packages/")))
; (package-initialize)
; (setq package-enable-at-startup nil)

(require 'org)

(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-thing sublimity evil symon dmenu avy idle-highlight-mode beacon smex company-tern which-key use-package tern spaceline projectile neotree magit js2-refactor git-gutter doom-themes company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#282c34")))))
