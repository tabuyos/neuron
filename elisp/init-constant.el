;;; elisp/init-constant.el --- Constant Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Constant config file

;;; Code:

;; UserInfo
(defvar neuron/user-nick-name nil
  "User's nickname for current user.")

(setq user-full-name "Aaron Liew")
(setq neuron/user-nick-name "Tabuyos")
(setq user-mail-address "tabuyos@outlook.com")

;; Constants
(defconst *sys/windows*
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/python*
  (executable-find "python")
  "Do we have python?")

(defconst *sys/pip*
  (executable-find "pip")
  "Do we hava pip?")

(defconst *sys/clangd*
  (executable-find "clangd")
  "Do we have clangd?")

(defconst *sys/eaf-env*
  (and *sys/linux* (display-graphic-p) *sys/python* *sys/pip*
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")

(provide 'init-constant)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-constant.el ends here
