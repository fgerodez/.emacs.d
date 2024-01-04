;;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(require 'package)

;; ELPA (Emacs Lisp Package Archive) is the official emacs packages repository managed via package.el.
;; MELPA is a compatible repository containing a lot of user created packages (not curated by emacs).
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Installs a small use-package helper to download packages from source code repositories.
;; Will be included with emacs from version 30.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

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

  (global-display-line-numbers-mode 1)
  
  ;; Key bindings
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
  
  ;; Builtin packages
  (use-package modus-themes
	:init
	(modus-themes-load-themes)
	:custom
	(modus-themes-syntax '(yellow-comments))
	:config
	(modus-themes-load-operandi))
  
  (use-package project
	:config
	(defun project-disable-vc-tramp (orig-fun dir)
	  (if (tramp-tramp-file-p dir)
		  nil
		(funcall orig-fun dir)))

	(advice-add 'project-try-vc :around #'project-disable-vc-tramp))

  
  (use-package ibuffer
	:defer t
	:init
	(defalias 'list-buffers 'ibuffer)
	:custom
	(ibuffer-show-empty-filter-groups nil)
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
	(dired-dwim-target t)
	(dired-auto-revert-buffer t)
	(dired-create-destination-dirs t)
	(dired-guess-shell-alist-user
	 '(("\\.pdf" "zathura")
	   ("\\.ods\\|.odt\\|.doc\\|.xls\\|.ppt\\|.docx\\|.pptx\\|.xlsx" "libreoffice")
	   ("\\.avi\\|.mp4\\|.mpg" "vlc")))
	(dired-listing-switches "-Alvh --group-directories-first")
	(dired-omit-verbose nil))

  ;; Activates autorevert for remote files.
  ;; Autorevert checks the modified timestamp of files every x seconds
  (use-package autorevert
	:custom
	(auto-revert-remote-files t))

  ;; Customizations for the tramp core emacs package.
  ;; Tramp is used to connect to remote or local machine via various methods (ssh, docker, su...)
  (use-package tramp
	:defer t
	:custom

	;; Puts autosave files in a local directory (faster and does not clutter remote hosts)
	(tramp-auto-save-directory "~/.cache/emacs/tramp")

	;; Puts backups in a local directory as well
	(tramp-backup-directory-alist '(("." . "~/.cache/emacs/backups")))

	;; Modifies default tramp pattern to recognize the values added by vterm directory tracking.
	;; Without this tramp is confused and can not recognize where the prompt ends.
	;; The added part is \\(?:\x1b][0-9;A]*.*\x1b\\\\\\)*
	(tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]\n#-%>]*#?[]#-%>][[:blank:]]*\\(?:\\[[;[:digit:]]*[[:alpha:]][[:blank:]]*\\)*\\(?:\x1b][0-9;A]*.*\x1b\\\\\\)*")

	(tramp-remote-process-environment
	 '("ENV=" "TMOUT=0" "LC_ALL=fr_FR.UTF-8" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=" "HISTFILE=/dev/null"))

	:config

	;; Profile definition to use bash as remote shell
	(connection-local-set-profile-variables
	 'remote-bash
	 '((shell-file-name . "/bin/bash")))

	;; Applies remote bash to pi connections
	(connection-local-set-profiles
	 '(:application tramp :machine "pi")
	 'remote-bash)

	;; Proxies su and sudo via ssh on pi
	(add-to-list 'tramp-default-proxies-alist
				 '("pi" "root" "/ssh:pi:"))

	;; Prevents vc refresh on tramp remote directories
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
	:hook
	(eshell-mode . (lambda ()
					 (company-mode -1)))
	:custom
	(eshell-prefer-lisp-variables t)
	(eshell-prefer-lisp-functions t)
	(eshell-prompt-function (lambda ()
							  (concat 
							   (propertize (eshell/pwd) 'face 'eshell-prompt)
							   "\n"
							   (if (= (user-uid) 0)
								   " # "
								 (propertize " $" 'face 'eshell-prompt))
							   " "))))
  
  :hook
  (web-mode . subword-mode)  
  (prog-mode . show-paren-mode)
  (prog-mode . hs-minor-mode)
  
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

(use-package bookmark+
  :after bookmark
  :config
  (defun bmkp-root-or-sudo-logged-p ()
	"The detection of root is broken because you can be logged in as root on a remote host
     and not logged in as root on your localhost."
	nil)
  :vc (:fetcher github :repo "emacsmirror/bookmark-plus"))

(use-package dired+
  :after dired
  :vc (:fetcher github :repo "emacsmirror/dired-plus")
  :config 
  (diredp-toggle-find-file-reuse-dir t))

(use-package exec-path-from-shell
  :config
  (when (daemonp)
	(exec-path-from-shell-initialize))  
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK")))

(use-package avy
  :bind
  ("M-g a" . avy-goto-line))

(use-package paredit
  :bind
  (:map paredit-mode-map
		("M-s" . nil)
		("M-s s" . paredit-split-sexp)
		("M-s r" . paredit-raise-sexp)
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
  :custom
  (haskell-indentation-electric-flag t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-show-debug-tips nil)
  (haskell-process-suggest-remove-import-lines t)
  :hook
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . highlight-uses-mode)
  (haskell-mode . interactive-haskell-mode))

(use-package php-mode)

(use-package dap-mode
  :defer t
  :custom
  (dap-auto-configure-features '(locals expressions)))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package treemacs
  :preface
  (defun my/treemacs-close ()
	(interactive)
   	(save-excursion
	  (treemacs-select-window)
	  (treemacs-quit)))
  :bind
  ("<f7>" . treemacs-select-window)
  ("C-x t &" . treemacs-delete-other-windows)
  ("C-x t t" . treemacs)
  ("C-x t d" . treemacs-select-directory)
  ("C-x t B" . treemacs-bookmark)
  ("C-x t C-f" . treemacs-find-file)
  ("C-x t q" . my/treemacs-close)
  :config
  (treemacs-define-RET-action 'file-node-open   #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-indent-guide-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package docker
  :bind ("C-c d" . docker)
  :custom
  (docker-container-default-sort-key '("Names" . nil)))

(use-package magit
  :bind ("C-c g" . magit-status))

;;
;; Company text and code completion package.
;;
(use-package company
  :custom
  ;; Does not wait before showing the completion popin.
  (company-idle-delay 0.0)

  ;; Shows completion popin after a minimum input of 3 characters.
  (company-minimum-prefix-length 3)

  ;; Activates dabbrev-code for all modes.
  ;; It handles words with underscores correctly as one word.
  (company-dabbrev-code-modes t)

  ;; Uses dabbrev-code completion for comments and non code texts.
  (company-dabbrev-code-everywhere t)
  
  :hook
  ;; Activates company-mode globally
  (after-init . global-company-mode))

(use-package less-css-mode
  :hook
  (less-css-mode . electric-indent-local-mode))

(use-package vterm
;  :config
;  (defalias 'shell 'vterm)
  :custom
  (vterm-buffer-name-string "*[v] %s*")
  (vterm-eval-cmds '(("find-file" find-file)
					 ("view-file" view-file)
					 ("message" message)
					 ("vterm-clear-scrollback" vterm-clear-scrollback)))
  (vterm-tramp-shells '(("docker" "/bin/sh")
						("ssh" "/bin/bash")))
  :hook
  (vterm-mode . (lambda () 
				  (display-line-numbers-mode -1)
				  (setq-local nobreak-char-display nil))))

(use-package which-key
  :custom
  (which-key-add-column-padding 2)
  (which-key-max-description-length nil)
  :hook (prog-mode . which-key-mode))

(use-package helpful
  :defer t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h F" . 'helpful-function)
  ("C-h C" . 'helpful-command))

(use-package js2-mode
  :defer t
  :custom
  (js-indent-level 2)
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package prettier-js
  :defer t
  :custom
  (prettier-js-show-errors 'echo))

(use-package rjsx-mode
  :defer t
  :mode "\\.js"
  :hook 
  (rjsx-mode . (lambda ()
				 (lsp-deferred)
				 (electric-indent-local-mode)
				 (prettier-js-mode)
				 (setq tab-width 2))))

(use-package vertico
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :custom 
  (embark-quit-after-action nil)
  :bind 
  ("C-," . embark-act)
  (:map minibuffer-local-map 
		("M-;" . embark-dwim)))

(use-package ace-window
  :custom
  (aw-keys  '(?q ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-ignore-current t)
  :bind
  ("M-o" . ace-window))

(use-package marginalia
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :custom
  (marginalia-mode t))

(use-package consult
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  (consult-customize consult-bookmark consult-recent-file consult-xref
					 consult--source-bookmark consult--source-file-register
					 consult--source-recent-file consult--source-project-recent-file :preview-key '(:debounce 0.4 any))

  (setq completion-in-region-function #'consult-completion-in-region)

  (defun consult-buffer-state-no-tramp ()
	"Buffer state function that doesn't preview Tramp buffers."
	(let ((orig-state (consult--buffer-state))
          (filter (lambda (action cand)
					(if (and cand
							 (or (eq action 'return)
								 (let ((buffer (get-buffer cand)))
                                   (and buffer
										(not (file-remote-p (buffer-local-value 'default-directory buffer)))))))
						cand
                      nil))))
      (lambda (action cand)
		(funcall orig-state action (funcall filter action cand)))))

  (setq consult--source-buffer
		(plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp))
  
  :bind
  (;; Custom bindings
   ("C-c s" . consult-line)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-mode-command)
   ("C-c r" . consult-ripgrep)
   ("C-c f" . consult-find)
   ("C-c e" . consult-register)
   ;; C-x bindings overrides 
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark) 
   ("C-x b" . 'consult-buffer)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)
   ;; M-g bindings
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g M-g" . consult-goto-line)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g o" . consult-outline)
   ("M-g k" . consult-global-mark)
   ("M-g m" . consult-mark)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)        
   ("M-s e" . consult-isearch-history)      
   ("M-s l" . consult-line)                 
   ("M-s L" . consult-line-multi)           
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                
   ("M-r" . consult-history)))

(use-package consult-tramp
  :bind
  ("C-c o" . 'consult-tramp)
  :custom
  (consult-tramp-enable-shosts nil)
  (consult-tramp-method "ssh")
  :vc
  (:fetcher github :repo "Ladicle/consult-tramp"))

(use-package embark-consult
  :defer t
  :after (embark consult)
  :bind (:map minibuffer-local-map
			  ("M-o" . embark-export)))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode))

(use-package vcl-mode)
