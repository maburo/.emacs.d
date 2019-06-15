(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
;; save session
;; (desktop-save-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)
;; fonst mononoki size 12
(add-to-list 'default-frame-alist '(font . "mononoki-12"))
(set-frame-font "Source Code Pro for Powerline-12")
;; frame dimension 80x24
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(ido-mode t)
(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; package manager
(require 'package)
(setq package-enable-at-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; use-package macro
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
  (setq projectile-require-project-root nil)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

; ;;(use-package doom-modeline
; ;;  :ensure t
; ;;  :hook (after-init . doom-modeline-mode))

(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (progn
    (spaceline-define-segment buffer-id
      (if (buffer-file-name)
          (let ((project-root (projectile-project-p)))
            (if project-root
                (file-relative-name (buffer-file-name) project-root)
              (abbreviate-file-name (buffer-file-name))))
        (powerline-buffer-id)))
    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off)))

; ;; All The Icons
; (use-package all-the-icons)

;; NeoTree
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle))

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (setq doom-modeline-icon t))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode))

;; Javascript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;; https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
;; sudo npm i -g tern
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :hook ((js2-mode . (lambda () company-mode)))
  :interpreter "node"
  :config
  (setq js-indent-level 2
	js2-basic-indent 2
	js-chain-indent t))
;;  :init
;;  (setq js-basic-indent 2)
;;  (setq-default js2-basic-indent 2
;;		js2-basic-offset 2
;;		js2-auto-indent-p t
;;		js2-enter-indents-newline t
;;		js2-indent-on-enter-key t
;;		js2-global-externs (list "window" "require")))

(use-package tern
  :requires company
  :after company
  :ensure t
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (use-package company-tern
    :ensure t
    :init (add-to-list 'company-backends 'company-tern)))

(use-package js2-refactor
  :after js2-mode
  :ensure t
  :hook ((js2-mode . js2-refactor-mode)))
;;  :init (add-hook 'js2-mode-hook 'js2-refactor-mode))
;; Javascript end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-lenght 2)
  ;;(add-hook 'js2-mode-hook 'company-mode)
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company git-gutter doom-themes spaceline which-key magit projectile use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
