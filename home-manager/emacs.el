;; Declarative Emacs config

;; Leaf Config
;;--------------------------------------------------------------------|
;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))
;; </leaf-install-code>


;; Visual Enhancements
;;--------------------------------------------------------------------|

(dolist (font-name '("Atkinson Hyperlegible" "JetBrains Mono"))
  (unless (find-font (font-spec :family font-name))
      (message "ERROR: Font %s were not found" font-name)))

(leaf emacs
  :if
  (find-font (font-spec :family "Atkinson Hyperlegible"))
  (find-font (font-spec :family "JetBrains Mono"))
  :custom-face
  (default     . `((t (:font "JetBrains Mono" :height 120))))
  (fixed-pitch . `((t (:font "JetBrains Mono" :height 120))))
  (variable-pitch . `((t (:font "Atkinson Hyperlegible" :height 140))))
  :setq-default
  (cursor-type . 'bar)
  :setq
  (ring-bell-function . 'ignore)
  (inhibit-startup-screen . t)
  :hook
  (before-save . whitespace-cleanup)
  :config
  (scroll-bar-mode 0)
  (tooltip-mode  0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (delete-selection-mode)
  (electric-pair-mode)
  (global-display-fill-column-indicator-mode t))

(leaf custom-startup-screen
  :doc "Show *Welcome* buffer."
  :config
  (unless (file-exists-p "~/.emacs.d/emacs.png")
    (url-copy-file "https://raw.githubusercontent.com/TanbinIslam43/mydotfiles/main/.doom.d/emacs.png" "~/.emacs.d/emacs.png" t))
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/emacs.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(leaf wildcharm-theme
  :doc "A high contrast dark theme"
  :ensure t
  :config
  (load-theme `wildcharm))

(leaf smooth-scrolling
  :doc "Less jarring scrolling in windows"
  :ensure t
  :config (smooth-scrolling-mode t))

(leaf mood-line
  :doc "Minimal looking modeline"
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code))

(leaf ligature
  :doc "Enable font ligatures"
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
                          '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            (";" (rx (+ ";")))
                            ("&" (rx (+ "&")))
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ("%" (rx (+ "%")))
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ("\\" (rx (or "/" (+ "\\"))))
                            ("+" (rx (or ">" (+ "+"))))
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ("w" (rx (+ "w")))
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ("_" (rx (+ (or "_" "|"))))
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

(leaf vertico
  :doc "Vertical minibuffer completion"
  :ensure t
  :config
  (savehist-mode)
  (vertico-mode))

(leaf orderless
  :doc "Fuzzy(and more) completions"
  :ensure t
  :setq
  (completion-styles . '(orderless basic))
  (completion-category-defaults  . nil)
  (completion-category-overr . '((file (styles basic partial-completion)))))

(leaf embark
  :doc "Common at-point actions"
  :ensure t
  :setq (prefix-help-command . #'embark-prefix-help-command)
  :bind ("C-." . embark-act))

(leaf marginalia
  :ensure t
  :config
  (marginalia-mode))

(leaf corfu
  :doc "popup completion-at-point"
  :ensure t
  :setq (corfu-auto . t)
  :config (global-corfu-mode))

(leaf expand-region
  :doc "intelligent selection at point"
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(leaf magit
  :doc "Git client for emacs"
  :ensure t)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")))


;; Language specific
;;--------------------------------------------------------------------|

(leaf visual-fill-column :ensure t)
(leaf org
  :doc "A markdown like documentation format"
  :setq (org-ellipsis . " ▾")
  :custom-face
  ;; Heading size hierarchy
  (org-level-1 . '((t (:height 1.728 :weight bold :inherit variable-pitch))))
  (org-level-2 . '((t (:height 1.44  :weight bold :inherit variable-pitch))))
  (org-level-3 . '((t (:height 1.2   :weight bold :inherit variable-pitch))))
  (org-level-4 . '((t (:height 1.1   :weight bold :inherit variable-pitch))))
  (org-document-title . '((t (:height 1.728 :weight bold :inherit variable-pitch))))
  :hook (org-mode-hook . my-org-startup)
  :config
  (defun my-org-startup ()
    (org-indent-mode)
    (visual-fill-column-mode)
    (set-fill-column  78)
    (setq visual-fill-column-center-text    t)
    (visual-line-mode)
    (display-fill-column-indicator-mode -1)))
