 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/francois/.emacs.d/bookmarks/all-bookmarks")
 '(consult-tramp-enable-shosts nil nil nil "Customized with use-package consult-tramp")
 '(csv-separators '(";" "	"))
 '(custom-enabled-themes '(tango-dark) nil nil "Customized with use-package custom")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("dired"
	  (used-mode . dired-mode))
	 ("terms"
	  (mode . vterm-mode))
	 ("xmonad"
	  (visiting-file)
	  (projectile-files . "~/.xmonad/"))
	 ("programming"
	  (or
	   (derived-mode . prog-mode)
	   (mode . ess-mode)
	   (mode . compilation-mode)))
	 ("text document"
	  (and
	   (derived-mode . text-mode)
	   (not
		(starred-name))))
	 ("TeX"
	  (or
	   (derived-mode . tex-mode)
	   (mode . latex-mode)
	   (mode . context-mode)
	   (mode . ams-tex-mode)
	   (mode . bibtex-mode)))
	 ("web"
	  (or
	   (derived-mode . sgml-mode)
	   (derived-mode . css-mode)
	   (mode . javascript-mode)
	   (mode . js2-mode)
	   (mode . scss-mode)
	   (derived-mode . haml-mode)
	   (mode . sass-mode)))
	 ("gnus"
	  (or
	   (mode . message-mode)
	   (mode . mail-mode)
	   (mode . gnus-group-mode)
	   (mode . gnus-summary-mode)
	   (mode . gnus-article-mode)))))
 '(kept-new-versions 5)
 '(lsp-auto-guess-root t)
 '(lsp-lens-enable nil)
 '(lsp-server-install-dir "/home/francois/.cache/lsp")
 '(max-lisp-eval-depth 8000)
 '(max-specpdl-size 16000)
 '(menu-bar-mode nil)
 '(minibuffer-electric-default-mode t)
 '(mouse-autoselect-window t)
 '(next-line-add-newlines t)
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(js2 dap-php dap-mode php-mode ace-jump-mode lispy esh-autosuggest rainbow-delimiters paredit embark-consult embark lsp dired+ dired diredp consult-tramp marginalia consult orderless vertico bookmark+ quelpa-use-package quelpa systemd trashed yasnippet treemacs-projectile add-node-modules-path prettier-js rjsx-mode csv-mode ibuffer-projectile helpful ag ## disk-usage vterm projectile iedit which-key lsp-ui flycheck lsp-haskell lsp-mode exec-path-from-shell use-package company docker dockerfile-mode magit haskell-mode))
 '(password-cache-expiry 300)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(prettier-js-show-errors 'echo)
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-self-upgrade-p nil)
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(set-mark-command-repeat-pop t)
 '(shr-cookie-policy nil)
 '(speedbar-supported-extension-expressions
   '(".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".hs"))
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(use-package-always-ensure nil)
 '(version-control t)
 '(vterm-tramp-shells '(("docker" "/bin/sh") ("ssh" "/bin/bash")))
 '(which-function-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(Info-quoted ((t (:inherit default))))
 '(diredp-compressed-file-name ((t (:foreground "DodgerBlue1"))))
 '(diredp-compressed-file-suffix ((t (:foreground "DodgerBlue1"))))
 '(highlight ((t (:background "#057f9c" :foreground "#2e3436"))))
 '(js2-function-call ((t (:inherit default :foreground "DeepSkyBlue"))))
 '(js2-object-property ((t (:foreground "gold"))))
 '(term-color-black ((t (:background "#4a4f54" :foreground "#1d1f21"))))
 '(term-color-blue ((t (:background "#8bb4f7" :foreground "#427ede"))))
 '(term-color-cyan ((t (:background "#10e8e8" :foreground "#5cd6d6"))))
 '(term-color-green ((t (:background "#98cb35" :foreground "#00ad08"))))
 '(term-color-magenta ((t (:background "#c6bbfd" :foreground "#a36ac7"))))
 '(term-color-red ((t (:background "#fa6161" :foreground "#d60b0b"))))
 '(term-color-white ((t (:background "#ffffff" :foreground "#f2f2f2"))))
 '(term-color-yellow ((t (:background "#ebe050" :foreground "#ffd839")))))
