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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; UTF-8 everywhere
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; Treat whitespaces properly
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Font (larger, easy to read)
(set-face-attribute 'default nil
                    :family "JetBrains Mono NL"
                    :height 160)  ;; 100 = 10pt, adjust to taste

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

;; Nerd icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; dirvish - modern dired
(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode))
(setq dirvish-attributes
      (append
       '(vc-state subtree-state all-the-icons collapse)
       '(file-modes file-time file-size)))
(setq dirvish-header-line-format
      '(:left (path) :right (free-space))
      dirvish-mode-line-format
      '(:left (sort symlink) :right (omit yank index)))
;; Enable extensions
(with-eval-after-load 'dirvish
  (let* ((dirvish-dir (file-name-directory (locate-library "dirvish")))
         (ext-dir     (expand-file-name "extensions" dirvish-dir)))
    (when (file-directory-p ext-dir)
      (add-to-list 'load-path ext-dir))))
;; Enable subtree expalsion
(use-package dirvish-subtree
  :after dirvish
  :bind (:map dirvish-mode-map
              ("<tab>" . dirvish-subtree-toggle)))  ;; expand/collapse node

;; magit
(use-package magit :ensure t)
(setq magit-view-git-manual-method 'woman)

;; org-mode shenanigans
(setq org-directory "~/org")

;; Markup circus I can't avoid
(use-package markdown-mode :ensure t)
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)

;; scala-mode for .scala/.sbt
(use-package scala-mode :ensure t
  :mode "\\.s\\(cala\\|bt\\)\\'")
