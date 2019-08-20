(let ((gc-cons-threshold most-positive-fixnum))
  (require 'org)
  (org-babel-load-file
   (expand-file-name "config.org" user-emacs-directory)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (smartparens glsl-mode irony modern-cpp-font-lock drag-stuff smart-hungry-delete emmet-mode skeletor docker-compose-mode company-c-headers workgroups2 ranger helpful cider clojure-mode visual-regexp visual-replace undo-tree anzu try company-tern flycheck git-timemachine yasnippet-snippets use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
