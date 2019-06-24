(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
;; line numbers
(global-display-line-numbers-mode t)
;; Highlight current line
(global-hl-line-mode 1)
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)



;; IDO
(ido-mode t)
;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(defalias 'list-buffers 'ibuffer)
;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)
;; Includes buffer names of recently open files, even if they're not open now
(setq ido-use-virtual-buffers t)

(let ((gc-cons-threshold most-positive-fixnum))
  (require 'package)
  (setq package-enable-at-at-startup nil)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (setq package-enable-at-startup nil)

  ; encoding
  (when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)

  (setq inhibit-startup-screen t)
  (setq-default create-lockfiles nil
                make-backup-files nil
                initial-scratch-message nil
                ;; no beeping or blinking please
                ring-bell-function #'ignore)

  ;; use package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)

  ;; font
  (add-to-list 'default-frame-alist '(font . "mononoki-12"))
  (set-frame-font "Source Code Pro for Powerline-12")
  ;; frame dimension 80x24
  (add-to-list 'default-frame-alist '(height . 24))
  (add-to-list 'default-frame-alist '(width . 80))

  ;; doom theme
  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-one t)
    (setq doom-modeline-icon t))

  (use-package rainbow-delimiters
    :ensure t
    :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  (use-package smex
    :ensure t
    :init (smex-initialize)
    :bind ("M-x" . smex))
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

  (use-package avy
    :ensure t
    :bind
    ("M-s" . avy-goto-char))

  (use-package company
    :ensure t
;;    :hook (prog-mode . (lambda () company-mode))
    :init (global-company-mode)
    :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-lenght 2))
  
  (use-package symon
    :ensure t
    :bind
    ("s-h" . symon-mode))

  ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
	  (filename (buffer-file-name)))
      (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

  ;; spaceline
  ; (use-package spaceline
  ;   :ensure t
  ;   :init
  ;   (require 'spaceline-config)
  ;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))
;    :config
;    (progn
;      (spaceline-define-segment buffer-id
;	       (if (buffer-file-name)
;	        (let ((project-root (projectile-project-p)))
;	         (if project-root
;	            (file-relative-name (buffer-file-name) project-root)
;	             (abbreviate-file-name (buffer-file-name))))
;               (powerline-buffer-id)))
;               (spaceline-spacemacs-theme)
;               (spaceline-toggle-minor-modes-off)))
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-themes ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tern sql-indent spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode coffee-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
