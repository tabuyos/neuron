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
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (load-theme 'doom-vibrant t))

(provide 'init-theme)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-theme.el ends here
