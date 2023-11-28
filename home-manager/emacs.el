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
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>


;; Visual Enhancements
;;--------------------------------------------------------------------|
(set-face-attribute 'default nil
		    :font "FiraCode"
		    :height 120)
(set-face-attribute 'fixed-pitch nil
		    :font "FiraCode"
		    :height 120)
(set-face-attribute 'variable-pitch nil
		    :font "Atkinson Hyperlegible"
		    :height 150)

(setq inhibit-startup-screen t) ; Disable startup screen
(setq-default cursor-type 'bar) ; Set cursor to a bar: |
(set-fringe-mode 0)             ; disable padding at the sides of the frame
(scroll-bar-mode 0)             ; disable scrollbars
(tooltip-mode    0)             ; disable tooltips
(menu-bar-mode   0)             ; disable menubar
(tool-bar-mode   0)             ; disable toolbar
(line-number-mode   0)          ; disable line number in mode line
(column-number-mode 1)          ; enabled column number in mode line
(global-display-line-numbers-mode t)           ; enable line numbers
(global-display-fill-column-indicator-mode t) ; enable standard line width indicator
(setq visible-bell 1)

(leaf gruvbox-theme
  :doc "The gruvbox theme used across emacs"
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

(leaf smooth-scrolling
  :doc "Less jarring scrolling in windows"
  :ensure t
  :config (smooth-scrolling-mode t))

(leaf mood-line
  :doc "Minimal looking modeline"
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (set-face-attribute 'mode-line nil ; EXPLICIT! IMPURE!! PREPOSTEROUS!!!
		      :box '(:line-width 12 :color "#3c3836")
		      :background "#3c3836")) ; TODO: figure out a way to not define colors explicitly

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
  :init (vertico-mode)
  :config
  (leaf savehist
    :doc "Persist minibuffer history over restarts"
    :init (savehist-mode)))

(leaf expand-region
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(leaf magit
  :ensure t)

(leaf visual-fill-column)

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

(leaf nix-mode
  :ensure t)

(leaf elm-mode)

(leaf haskell-mode)

(leaf sass-mode
  :config (leaf haml-mode :ensure t))

(leaf org
  :setq ((org-ellipsis                   . " ▾")
	 (visual-fill-column-width       .  100)
	 (visual-fill-column-center-text .    t))
  :hook (org-mode-hook . config/org-init)
  :config
  (defun config/org-init ()
     (variable-pitch-mode 1)    ; Normal font mode

     (dolist (face '((org-level-1 . 2.3)    ; To increase font size of the headings based on
                     (org-level-2 . 1.9)    ; heading level
                     (org-level-3 . 1.5)
                     (org-level-4 . 1.3)
                     (org-level-5 . 1.1)
                     (org-level-6 . 1.1)
                     (org-level-7 . 1.1)
                     (org-level-8 . 1.1)
		     (org-document-title . 2.3)))
       (set-face-attribute (car face) nil
			   :font   "Atkinson HyperLegible"
			   :weight 'bold
			   :height (cdr face)))

     ;; Setting special text blocks' font face as fixed-pitch for better alignment
     (dolist (face '((org-code            . (shadow fixed-pitch))
		     (org-table           . (shadow fixed-pitch))
		     (org-verbatim        . (shadow fixed-pitch))
		     (org-special-keyword . (font-lock-comment-face fixed-pitch))
		     (org-meta-line       . (font-lock-comment-face fixed-pitch))
		     (org-checkbox        . fixed-pitch)
		     (org-block           . fixed-pitch)))
       (set-face-attribute (car face) nil
			   :inherit (cdr face)))

     (visual-fill-column-mode   1)  ; To center the window
     (auto-fill-mode    1)          ; Wrap text after 78 characters
     (set-fill-column  78)
     (display-line-numbers-mode 0)
     (org-indent-mode   1)))

