;;; elisp/init-font.el --- Font Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Font config file

;;; Code:

;; FontsList
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar neuron/default-font-size 11.0
  "Default size of font.")

(defvar neuron/available-chinese-font-list nil
  "List of available font for chinese.")

(defvar neuron/available-english-font-list nil
  "List of available font for english.")

(defvar neuron/chinese-font-list '("FandolKai" "Noto Serif CJK SC" "Noto Sans CJK SC")
  "List of Chinese font, size isn't set by defualt(default size: `neuron/default-font-size').")

(defvar neuron/default-font-list '("Noto Mono" "Consolas" "Input" "Consolas" "Love LetterTW" "Source Code Variable" "DejaVu Sans Code" "JetBrains Mono")
  "List of fonts and sizes.  The first one available will be used.")

;; FontFun
(defun neuron/check-available-font (pending-check-font-list)
  (let (available-font-list)
    (dolist (font-name pending-check-font-list)
      (when (member font-name (font-family-list))
				(push font-name available-font-list)))
    (setq available-font-list (nreverse available-font-list))))

(defun neuron/change-font ()
  "Documentation."
  (interactive)
  (let* (english-font-name
				 chinese-font-name
				 font-size ignore-case
				 efn
				 cfn)
    (setq ignore-case completion-ignore-case
					completion-ignore-case t)
    ;; check available font of english
    (when (not neuron/available-english-font-list)
      (setq neuron/available-english-font-list (neuron/check-available-font neuron/default-font-list)))
    ;; check available font fo chinese
    (when (not neuron/available-chinese-font-list)
      (setq neuron/available-chinese-font-list (neuron/check-available-font neuron/chinese-font-list)))

		(setq efn (car neuron/available-english-font-list))
		(setq cfn (car neuron/available-chinese-font-list))

    (if (not neuron/available-english-font-list)
				(message "No english fonts available, check your font was add into fonts' folder and try again.")
      (if (called-interactively-p 'interactive)
					(setq english-font-name (assoc-string
																	 (completing-read
																		(concat "What english font to use(default " efn ")? ")
																		neuron/available-english-font-list
																		(lambda (x) x)
																		nil
																		nil
																		nil
																		efn)
																	 neuron/available-english-font-list)
								chinese-font-name (assoc-string
																	 (completing-read
																		(concat "What chinese font to use(default " cfn ")? ")
																		neuron/available-chinese-font-list
																		(lambda (x) x)
																		nil
																		nil
																		nil
																		cfn)
																	 neuron/available-chinese-font-list)
								font-size (read-number "Font size: " neuron/default-font-size))
				(setq english-font-name (car neuron/available-english-font-list)
							chinese-font-name (car neuron/available-chinese-font-list)
							font-size neuron/default-font-size)))
    (setq completion-ignore-case ignore-case)
    (if (integerp font-size)
				(setq font-size (/ (* 3 font-size) 4.0)))
    (set-frame-font (font-spec :family english-font-name :size font-size) nil t)
    (set-fontset-font "fontset-default" 'han (font-spec :family chinese-font-name))))

(when (or (display-graphic-p))
  (neuron/change-font))

;; ATIPac
(use-package all-the-icons :if (display-graphic-p))

(provide 'init-font)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-font.el ends here
