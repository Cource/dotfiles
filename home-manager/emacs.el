(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;--------------------------------------------------------------------

(use-package emacs
  :custom-face
  (default     ((t (:family "JetBrains Mono" :height 120))))
  (fixed-pitch ((t (:family "JetBrains Mono" :height 120))))
  (variable-pitch ((t (:font "Atkinson Hyperlegible" :height 140))))
  :hook
  (before-save . whitespace-cleanup)
  :custom
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (straight-use-package-by-default t)
  :config
  (scroll-bar-mode 0)
  (tooltip-mode  0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (delete-selection-mode)
  (electric-pair-mode)
  (set-fill-column 79)
  (global-display-fill-column-indicator-mode t)
  (set-frame-parameter nil 'alpha-background 94))


(use-package constant-theme
  :config (load-theme 'constant t))
;; (use-package wildcharm-theme
;;   :config (load-theme 'wildcharm t))

(use-package smooth-scrolling
  :config (smooth-scrolling-mode t))

;; minimal modeline
(use-package mood-line
  :config (mood-line-mode))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" ":<|>" "----"))
  (global-ligature-mode t))

;; Vertical minibuffer completion items
(use-package vertico
  :config
  (savehist-mode)
  (vertico-mode))

;; Fuzzy+someOtherStuff completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-catergory-overr '((file (styles basic partial-completion)))))

;; Common actions at point
(use-package embark
  :bind ("C-." . embark-act))

;; Extra info in minibuffer
(use-package marginalia
  :config (marginalia-mode))

;; Popup completion at point
(use-package corfu
  :custom (corfu-auto t)
  :config (global-corfu-mode))

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

(use-package magit)

(use-package dirvish
  :config (dirvish-override-dired-mode))

(use-package direnv
  :config (direnv-mode))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
             '(odin-mode . ("ols" "--stdio"))))

;; Org mode stuff
(use-package visual-fill-column)
(use-package org
  :custom-face
  (org-level-1 ((t (:height 1.728 :weight bold :inherit variable-pitch))))
  (org-level-2 ((t (:weight bold))))
  (org-level-3 ((t (:weight bold))))
  (org-level-4 ((t (:weight bold))))
  :custom (org-ellipsis "▾")
  :hook (org-mode . my-org-startup)
  :config
    (defun my-org-startup ()
    (org-indent-mode)
    (visual-fill-column-mode)
    (set-fill-column 78)
    (setq visual-fill-column-center-text t)
    (visual-line-mode)
    (display-fill-column-indicator-mode -1)))

(use-package nix-mode)

(use-package odin-mode
  :straight (odin-mode :type git :host github
                       :repo "mattt-b/odin-mode"))

(use-package typescript-mode)

(use-package markdown-mode)

(use-package haskell-mode)

(use-package web-mode)
