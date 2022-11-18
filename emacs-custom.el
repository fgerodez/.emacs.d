(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/francois/.emacs.d/bookmarks/all-bookmarks")
 '(custom-enabled-themes '(modus-operandi) nil nil "Customized with use-package custom")
 '(custom-safe-themes
   '("a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
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
 '(package-selected-packages
   '(burly modus-themesq doom-modeline modus-themes elm-mode docker-tramp js2 dap-php dap-mode php-mode ace-jump-mode esh-autosuggest paredit embark-consult embark lsp dired+ dired consult-tramp marginalia consult orderless vertico bookmark+ quelpa-use-package quelpa systemd trashed yasnippet treemacs-projectile prettier-js rjsx-mode csv-mode ibuffer-projectile helpful ## disk-usage vterm projectile iedit which-key lsp-ui flycheck lsp-haskell lsp-mode exec-path-from-shell use-package company docker dockerfile-mode magit haskell-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :slant normal :weight normal :height 112 :width normal :foundry "SRC" :family "Hack"))))
 '(Info-quoted ((t (:inherit default))))
 '(diredp-compressed-file-name ((t (:foreground "DodgerBlue1"))))
 '(diredp-compressed-file-suffix ((t (:foreground "DodgerBlue1"))))
 '(highlight ((t (:background "#057f9c" :foreground "#2e3436"))))
 '(js2-function-call ((t (:inherit default :foreground "DeepSkyBlue"))))
 '(js2-object-property ((t (:foreground "gold")))))

