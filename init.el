;;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(bind-key "<f8>" 'bookmark-bmenu-list)
(bind-key "C-x &" 'delete-other-windows)
(bind-key "C-x à" 'delete-window)
(bind-key "C-x \"" 'split-window-right)
(bind-key "C-x é" 'split-window-below)
(bind-key "C-c c" 'erase-buffer)
(bind-key "M-à" 'mark-word)
(bind-key "C-M-à" 'mark-sexp)
(bind-key "C-x '" 'ctl-x-4-prefix)
(bind-key "C-x (" 'ctl-x-5-prefix)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Packages list    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq frame-title-format "%b")
  (setq read-process-output-max (* 1024 1024))

  (put 'narrow-to-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  (use-package ibuffer
	:defer t
	:init
	(defalias 'list-buffers 'ibuffer)
	:custom
	(ibuffer-formats
	 '((mark modified read-only locked " "
			 (name 18 -1 :left)
			 " "
			 (size 9 -1 :right)
			 " "
			 (mode 16 16 :left :elide)
			 " " filename-and-process)
	   (mark " "
			 (name 16 -1)
			 " " filename))))


  (use-package isearch
	:preface
	(defun my/isearch-copy-matches ()
	  (interactive)
	  (if (not isearch-success)
		  (message "%s" "No match found")
		(kill-new "")
		(save-excursion
		  (let ((count 0))
			(while isearch-success
			  (kill-append (buffer-substring (point) isearch-other-end) nil)
			  (isearch-repeat-forward)
			  (kill-append "\n" nil)
			  (cl-incf count))
			(message "%d matches copied to kill ring" count)))))

	:bind 
	(:map isearch-mode-map (("C-k" . 'my/isearch-copy-matches))))


  (use-package dired
	:bind
	("C-<return>" . 'dired-do-async-shell-command)
	:hook
	(dired-mode . dired-omit-mode)
	:custom
	(dired-auto-revert-buffer t)
	(dired-create-destination-dirs t)
	(dired-guess-shell-alist-user
	 '(("\\.pdf" "zathura")
	   ("\\.ods\\|.odt\\|.doc\\|.xls\\|.ppt\\|.docx\\|.pptx\\|.xlsx" "libreoffice")
	   ("\\.avi\\|.mp4\\|.mpg" "vlc")))
	(dired-listing-switches "-Alvh --group-directories-first")
	(dired-omit-verbose nil)
	(dired-use-ls-dired nil))


  (use-package tramp
	:defer t
	:custom
	(tramp-auto-save-directory "~/.cache/emacs/tramp")
	(tramp-backup-directory-alist '(("." . "~/.cache/emacs/backups")))
	(tramp-connection-properties nil)
	(tramp-default-method "ssh")
	(tramp-use-ssh-controlmaster-options nil)
	(tramp-remote-process-environment
	 '("ENV=" "TMOUT=0" "LC_CTYPE=" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=" "HISTFILE=/dev/null"))
	:config
	(with-eval-after-load 'vc
	  (customize-set-value 'vc-ignore-dir-regexp 
						   (format "\\(%s\\)\\|\\(%s\\)"
								   vc-ignore-dir-regexp
								   tramp-file-name-regexp))))


  (use-package eshell
	:defer t
	:preface
	(defun eshell-new ()
	  "Open a new instance of eshell."
	  (interactive)
	  (eshell 'N))
	:config
	(require 'em-tramp)
	:bind
	("C-c h" . 'eshell-new)
	:custom
	(eshell-prefer-lisp-variables t)
	(eshell-prefer-lisp-functions t)
	(eshell-prompt-function (lambda ()
							  (concat 
							   (propertize (eshell/pwd) 'face `(:foreground "blue"))
							   "\n"
							   (if (= (user-uid) 0)
								   " # "
								 (propertize " $" 'face `(:foreground "yellow")))
							   " "))))
  
  :hook
  (web-mode . subword-mode)  
  (prog-mode . show-paren-mode)
  (find-file . linum-mode)
  
  :custom
  (max-lisp-eval-depth 8000)
  (max-specpdl-size 16000)
  (inhibit-startup-screen t)
  (gc-cons-threshold 100000000)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (delete-by-moving-to-trash t)
  (tab-width 4)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (mouse-autoselect-window t)
  (password-cache-expiry 300)
  (minibuffer-electric-default-mode t)
  (global-prettify-symbols-mode t)
  (bookmark-save-flag 1)
  (display-buffer-alist '(("\\*Async Shell Command\\*.*" . '(#'display-buffer-no-window))))
  (savehist-mode t)
  (recentf-mode t)
  (read-extended-command-predicate 'command-completion-default-include-p)
  (set-mark-command-repeat-pop t)
  (async-shell-command-buffer 'new-buffer)
  (next-line-add-newlines t)
  (version-control t)
  (kept-new-versions 5)
  (delete-old-versions t)
  (backup-by-copying t)
  (backup-directory-alist
   '(("tramp-file-name-regexp" . "nil")
	 ("." . "~/.cache/emacs/backups")))
  (electric-indent-mode t)
  (electric-pair-mode t)
  (electric-pair-pairs
   '((?\" . ?\")
     (?\{ . ?\}))))


(use-package quelpa
  :ensure
  :custom
  (quelpa-checkout-melpa-p nil)
  (quelpa-self-upgrade-p nil))


(use-package quelpa-use-package
  :ensure
  :after quelpa
  :config
  (quelpa-use-package-activate-advice))


(use-package bookmark+
  :ensure
  :after bookmark
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus"))


(use-package dired+
  :ensure
  :after dired
  :quelpa 
  (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
  :config 
  (diredp-toggle-find-file-reuse-dir t))


(use-package exec-path-from-shell
  :config
  (when (daemonp)
	(exec-path-from-shell-initialize))  
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK")))


(use-package avy
  :ensure
  :bind
  ("M-g f" . avy-goto-line))


(use-package paredit
  :ensure
  :bind
  (:map paredit-mode-map
        ("C-M-l" . paredit-recentre-on-sexp)
        ("C-c ( n"   . paredit-add-to-next-list)
        ("C-c ( p"   . paredit-add-to-previous-list)
        ("C-c ( j"   . paredit-join-with-next-list)
        ("C-c ( J"   . paredit-join-with-previous-list))
  :hook 
  ((emacs-lisp-mode lisp-mode) . paredit-mode)
  :config
  (with-eval-after-load 'eldoc
	(eldoc-add-command
	 'paredit-backward-delete
	 'paredit-close-round)))


(use-package haskell-mode
  :ensure
  :custom
  (haskell-indentation-electric-flag t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-show-debug-tips nil)
  (haskell-process-suggest-remove-import-lines t)
  :hook
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . highlight-uses-mode)
  (haskell-mode . lsp-deferred)
  (haskell-mode . interactive-haskell-mode))


(use-package php-mode
  :ensure
  :config
  (setq lsp-enable-file-watchers nil)
  :hook
  (php-mode . lsp-deferred))


(use-package elm-mode
  :ensure
  :hook
  (elm-mode . lsp-deferred))


(use-package dap-mode
  :ensure
  :defer t
  :custom
  (dap-auto-configure-features '(locals expressions)))


(use-package iedit
  :ensure
  :bind ("C-;" . iedit-mode))


(use-package treemacs
  :ensure
  :bind 
  ("<f7>" . treemacs-add-display-current-project-exclusively)
  :config 
  (treemacs-indent-guide-mode))


(use-package docker
  :ensure 
  :bind ("C-c d" . docker)
  :custom
  (docker-container-default-sort-key '("Names" . nil)))


(use-package docker-tramp
  :ensure
  :after tramp)


(use-package magit
  :ensure
  :bind ("C-c g" . magit-status))


(use-package company
  :ensure
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 3)
  :hook 
  (after-init . global-company-mode)
  (emacs-lisp-mode . (lambda () 
					   (setq company-backends 
							 '(company-elisp company-capf company-files company-dabbrev)))))


(use-package lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-lens-enable nil)
  (lsp-server-install-dir "~/.cache/lsp")
  :hook 
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . yas-minor-mode))


(use-package lsp-ui
  :ensure
  :bind ("<f6>" . lsp-ui-imenu))


(use-package less-css-mode
  :hook
  (less-css-mode . lsp-deferred)
  (less-css-mode . electric-indent-local-mode))


(use-package projectile
  :ensure
  :custom
  (projectile-require-project-root 'prompt)
  (projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("s-p" . projectile-command-map))


(use-package vterm
  :ensure
  :defer t
  :config
  (defalias 'shell 'vterm)
  :hook
  (vterm-mode . (lambda () 
				  (linum-mode 0)
				  (setq-local nobreak-char-display nil))))


(use-package which-key
  :ensure
  :hook (prog-mode . which-key-mode))


(use-package helpful
  :ensure
  :defer t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h F" . 'helpful-function)
  ("C-h C" . 'helpful-command))


(use-package js2-mode
  :ensure
  :defer t
  :custom
  (js-indent-level 2)
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))


(use-package prettier-js
  :ensure
  :defer t
  :custom
  (prettier-js-show-errors 'echo))


(use-package rjsx-mode
  :ensure
  :defer t
  :mode "\\.js"
  :hook 
  (rjsx-mode . (lambda ()
				 (lsp-deferred)
				 (electric-indent-local-mode)
				 (prettier-js-mode)
				 (setq tab-width 2))))


(use-package vertico
  :ensure
  :hook 
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-mode t)
  :bind 
  (:map vertico-map
		("<return>" . vertico-directory-enter)
		("<backspace>" . vertico-directory-delete-char)
		("M-<backspace>" . vertico-directory-delete-word)
		("<next>" . vertico-scroll-down)
		("<prior>" . vertico-scroll-up)))


(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package embark
  :ensure
  :custom 
  (embark-quit-after-action nil)
  :bind 
  ("C-," . embark-act)
  (:map minibuffer-local-map 
		("M-;" . embark-dwim)))


(use-package ace-window
  :ensure
  :bind ("C-x o" . ace-window))


(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :custom
  (marginalia-mode t))


(use-package consult
  :defer t
  :config
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)
  (setq completion-in-region-function #'consult-completion-in-region)
  :bind
  ("C-c s" . 'consult-line)
  ("C-c m" . 'consult-mark)
  ("C-c i" . 'consult-imenu)
  ("C-c l" . 'consult-goto-line)
  ("C-c r" . 'consult-ripgrep)
  ("C-c f" . 'consult-find)
  ("C-c e" . 'consult-register)
  ("C-x b" . 'consult-buffer))


(use-package consult-tramp
  :ensure
  :bind
  ("C-c o" . 'consult-tramp)
  :custom
  (consult-tramp-enable-shosts nil)
  (consult-tramp-method "ssh")
  :quelpa 
  (consult-tramp :fetcher github :repo "Ladicle/consult-tramp"))


(use-package embark-consult
  :ensure
  :defer t
  :after (embark consult)
  :bind (:map minibuffer-local-map
			  ("M-o" . embark-export)))


(use-package esh-autosuggest
  :ensure
  :hook (eshell-mode . esh-autosuggest-mode))


(use-package doom-modeline
  :ensure
  :init
  (doom-modeline-mode))


(use-package modus-themes
  :ensure
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))
