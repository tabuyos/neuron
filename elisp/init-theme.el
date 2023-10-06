;;; elisp/init-theme.el --- Theme Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Theme config file

;;; Code:

(use-package doom-themes
  ;; :custom-face
  ;; (cursor ((t (:background "BlanchedAlmond"))))
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (load-theme 'doom-vibrant t))

(use-package doom-modeline
  :custom
  ;; Dom't compact font caches during GC.  Windows Laggy Issue
  (inhibit-compacting-cont-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))

(provide 'init-theme)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-theme.el ends here
