;;; elisp/init-haskell.el --- Haskell Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Haskell config file

;;; Code:

(use-package haskell-mode
  :ensure t
  :init
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
    (add-hook 'haskell-mode-hook 'eglot-ensure)
    (add-hook 'haskell-mode-hook 'subword-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-cabal-mode 'subword-mode)
    (setq haskell-process-args-cabal-new-repl
	        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
    (setq haskell-process-type 'cabal-new-repl)
    (setq haskell-stylish-on-save 't)
    (setq haskell-tags-on-save 't)
    ))

(provide 'init-haskell)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-haskell.el ends here
