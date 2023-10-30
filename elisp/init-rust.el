;;; elisp/init-rust.el --- Rust Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/30

;;; Commentary:

;; Rust config file

;;; Code:

(use-package rust-mode
  :hook
  (
   (rust-mode . prettif-symbols-mode)
   (rust-mode . eglot-ensure)
   (rust-mode . (lambda () (setq indent-tabs-mode nil)))
   )
  :config
  (setq rust-format-on-save t)
  )

(use-package tree-sitter
  :ensure tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'init-rust)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-rust.el ends here
