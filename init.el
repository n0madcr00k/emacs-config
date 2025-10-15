;; -*- coding: utf-8; lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Disable the circus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-buffer-choice t)      ;; open *scratch* on start
(setq ring-bell-function 'ignore)   ;; never ding at me, ever
(setq use-short-answers t)          ;; I am not typing the whole yes/no
(setq use-dialog-box nil)           ;; don't GUI me
(setq-default indent-tabs-mode nil) ;; tabs are banned
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

;; Don’t litter my filesystem
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Enable the goodness
(global-auto-revert-mode t)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (string= (buffer-name) "*scratch*")
              (display-line-numbers-mode))))

;; UTF-8 everywhere
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; Treat whitespaces properly
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Font (larger, easy to read)
(set-face-attribute 'default nil
                    :family "JetBrains Mono NL"
                    :height 180)  ;; 100 = 10pt, adjust to taste

;; Bootstrap use-package
(setq package-install-upgrade-built-in t)
(require 'package)
(setq package-enable-at-startup nil)  ;; living on the edge
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Colors – Solarized Dark
;; (you can change to 'solarized-light if you hate darkness)
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; Nicer line spacing and subtle line highlight
(setq-default line-spacing 2)
(global-hl-line-mode 1)

;; No startup screen, no message buffer stealing focus
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; No custom garbage in config
(setq custom-file null-device)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Make dired behave like I want

;; Single-buffer Dired, but files open in their own buffer
(use-package dired
  :ensure nil ;; built-in
  :commands (dired)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:map dired-mode-map
        ("RET" . n0mad/dired-open)
        ("^"   . (lambda () (interactive) (find-alternate-file "..")))))

(defun n0mad/dired-open ()
  "Open file or directory in Dired. Directories reuse buffer, files open normally."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;; magit
(use-package magit :ensure t)

;; Eglot servers setup
(use-package eglot
  :ensure nil
  :config
  ;; Python
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; Scala
  (add-to-list 'eglot-server-programs
               '((scala-mode) . ("metals-emacs"))))

;; Completion UI: Corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                      ;; auto popup
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)    ;; don't kill popup too eagerly
  (corfu-preselect 'prompt)           ;; keep predictable
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 2)
  :bind
  (:map corfu-map
        ("M-j" . corfu-next)
        ("M-k" . corfu-previous)
        ("C-j" . corfu-insert)
        ("RET" . corfu-insert)))

;; Make sure we don't force TAB to complete
(setq tab-always-indent t)

;; Orderless matching (way nicer than basic/flex)
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Files often work better without orderless.
        completion-category-overrides '((file (styles basic)))
        ;; Make sure Eglot completions use orderless
        ;; (Eglot's CAPF uses the 'eglot category)
        )
  (add-to-list 'completion-category-overrides '(eglot (styles orderless))))

;; Show docs/signatures in the popup
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :init (corfu-popupinfo-mode 1)
  :custom
  (corfu-popupinfo-delay 0.1))

;; magit
(setq magit-view-git-manual-method 'woman)

;; BBDB
(use-package bbdb :ensure t)

;; org-mode shenanigans
(setq org-directory "~/org")

;; Markup circus I can't avoid
(use-package markdown-mode :ensure t)
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)

;; Containers hell
(use-package dockerfile-mode :ensure t)

;; Python support is a must
(use-package python
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

;; Scala support
(use-package scala-mode :ensure t
  :mode "\\.s\\(cala\\|bt\\)\\'"
  :hook ((scala-mode . eglot-ensure)))
