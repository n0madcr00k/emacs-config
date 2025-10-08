;; -*- mode: emacs-lisp -*-

;; Disable the circus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-buffer-choice t) ;; open *scratch* on start

;; Don’t litter your filesystem
(setq make-backup-files nil)
(setq auto-save-default nil)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)

;; Font (larger, easy to read)
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 160)  ;; 100 = 10pt, adjust to taste

;; Bootstrap use-package
(setq package-install-upgrade-built-in t)
(require 'package)
(setq package-enable-at-startup nil)  ;; living on the edge
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
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

;; Optional: nicer line spacing and subtle line highlight
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

;; Circe IRC client
(unless (package-installed-p 'circe)
  (package-refresh-contents)
  (package-install 'circe))

;; magit
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))
