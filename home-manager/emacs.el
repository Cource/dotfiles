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
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (straight-use-package-by-default t)
  (treesit-language-source-alist
   '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
  :config
  (scroll-bar-mode 0)
  (tooltip-mode  0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (delete-selection-mode)
  (electric-pair-mode)
  (set-fill-column 79))
  ;; (global-display-fill-column-indicator-mode t)
  ;; (add-to-list 'default-frame-alist '(alpha-background . 94)))


(use-package majapahit-themes
  :config (load-theme 'majapahit-dark t))
;; (use-package constant-theme
;;   :config (load-theme 'constant t))
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

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package eldoc-box)

(use-package eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols" "--stdio"))))

(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (display-fill-column-indicator-mode -1))))

(defun jujutsu ()
  "Minimal jj (Jujutsu) interface for Emacs"
  (interactive)
  (let ((command ""))
    (while (not (string= command "q"))
      (compile "jj log")
      (setq command (read-string "Enter a command (or 'q' to quit): "))
      (cond
       ((or (string= "" command)
            (string= "q" command))
        nil)
       ((or (string-prefix-p "help" command)
            (string-prefix-p "diff" command))
        (compile (concat "jj " command)))
       (t (shell-command (concat "jj " command)))))))

;; Org mode stuff
(use-package visual-fill-column)
(use-package org
  :custom-face
  (org-level-1 ((t (:weight bold))))
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

(use-package org-roam
  :bind
  ("C-c i" . org-roam-node-insert)
  ("C-c f" . org-roam-node-find)
  ("C-c d" . org-roam-dailies-find-date)
  ("C-c r" . org-roam-buffer-toggle)
  :custom
  (org-roam-directory "~/Documents/roam")
  :config
  (org-roam-db-autosync-mode))

(use-package nix-mode)

(use-package odin-mode
  :straight (odin-mode :type git :host github
                       :repo "mattt-b/odin-mode"))

(use-package typescript-mode)

(use-package markdown-mode)

(use-package haskell-mode)

(use-package web-mode)
