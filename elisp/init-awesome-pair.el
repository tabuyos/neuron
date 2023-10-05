;;; elisp/init-awesome-pair.el --- Awesome Pair Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Awesome Pair config file

;;; Code:

;; AwesomePair
(use-package awesome-pair
  :load-path (lambda () (expand-file-name "site-elisp/awesome-pair" user-emacs-directory))
  :bind
  (("TAB" . up-list))
  (:map prog-mode-map
        (("(" . awesome-pair-open-round)
	       ("[" . awesome-pair-open-bracket)
	       ("{" . awesome-pair-open-curly)
	       (")" . awesome-pair-close-round)
	       ("]" . awesome-pair-close-bracket)
	       ("}" . awesome-pair-close-curly)
	       ("=" . awesome-pair-equal)
	       ("%" . awesome-pair-match-paren)
	       ("\"" . awesome-pair-double-quote)
	       ("SPC" . awesome-pair-space)
	       ("M-o" . awesome-pair-backward-delete)
	       ;; ("C-d" . awesome-pair-forward-delete)
	       ("C-k" . awesome-pair-kill)
	       ("M-\"" . awesome-pair-wrap-double-quote)
	       ("M-[" . awesome-pair-wrap-bracket)
	       ("M-{" . awesome-pair-wrap-curly)
	       ("M-(" . awesome-pair-wrap-round)
	       ("M-)" . awesome-pair-unwrap)
	       ("M-p" . awesome-pair-jump-right)
	       ("M-n" . awesome-pair-jump-left)
	       ("M-:" . awesome-pair-jump-out-pair-and-newline)
	       ))
  :config
  (electric-pair-mode)
  (show-paren-mode)
  :hook
  ((prog-mode
    c-mode-common
    c-mode c++-mode
    java-mode
    haskell-mode
    emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    maxima-mode
    ielm-mode
    sh-mode
    makefile-gmake-mode
    php-mode
    python-mode
    js-mode
    go-mode
    haskell-mode
    qml-mode
    jade-mode
    css-mode
    ruby-mode
    coffee-mode
    rust-mode
    qmake-mode
    lua-mode
    swift-mode
    minibuffer-inactive-mode) . awesome-pair-mode))


;; RainbowDelimiter
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-awesome-pair)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-awesome-pair.el ends here
