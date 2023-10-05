;;; elisp/init-edit.el --- Edit Config -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; Edit config file

;;; Code:

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default make-backup-files nil)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)

;; open global display line number mode
(global-display-line-numbers-mode)
;; open line number mode.
(line-number-mode)
;; open column number mode.
(column-number-mode)
;; Replace selection on insert
(delete-selection-mode 1)

;; History
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
		                 ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)

;; SmallConfigs
;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;Turn off cursor alarms
(setq ring-bell-function 'ignore)

;; Show keystrokes in progress instantly
(setq echo-keystrokes 0.1)

;; Don't lock files
(setq-default create-lockfiles nil)

;; Better compilation
;; kill compilation process before starting another
(setq-default compilation-always-kill t)

;; save all buffers on `compile'
(setq-default compilation-ask-about-save t)

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defavice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move custom-set-variables to defferent file
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So long mitigates slowness due to extrmely long lines
;; Currently available in emacs master branch *only*
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))

;; Move lines
(defun neuron/move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t)
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun neuron/move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (neuron/move-lines (- (or n 1))))

(defun neuron/move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (neuron/move-lines (or n 1)))

;; CommentBlock
(defun neuron/comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; CutAndCopy
(defun neuron/kill-ring-save@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (current-line-non-whitespace-begin-position) (current-line-non-whitespace-end-position))))
  (apply fn args))

(defun neuron/kill-region@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (apply fn args))


;; OpLine
(defun neuron/is-normal-string (str)
  "Determine whether the current character is a normal string."
  (not (or (null str) (equal "" str) (equal " " str) (equal "\n" str) (equal "\t" str))))

(defun neuron/get-one-char (position &optional is-negative)
  "Get one char by position."
  (let ((forward (1+ position)) (backward (1- position)))
    (if is-negative
	(if (> position 0)
	    (buffer-substring-no-properties backward position)
	  nil)
      (if (> position 0)
	  (buffer-substring-no-properties position forward)
	nil))))

(defun neuron/current-line-non-whitespace-position (&optional reverse)
  "Get begin or end position of current line with non-whitespace."
  (let ((begin (line-beginning-position)) (end (line-end-position)) (loop t) po)
    (while (and loop (< begin end))
      (if reverse
	  (if (neuron/is-normal-string (neuron/get-one-char end reverse))
	      (setq loop nil)
	    (decf end))
	(if (neuron/is-normal-string (neuron/get-one-char begin reverse))
	    (setq loop nil)
	  (incf begin))
	(setq postion begin)))
    (if loop
	(if reverse
	    (line-beginning-position)
	  (line-end-position))
      (if reverse
	  end
	begin))))

(defun neuron/current-line-non-whitespace-begin-position ()
  "Get begin position of current line with non-whitespace."
  (neuron/current-line-non-whitespace-position nil))

(defun neuron/current-line-non-whitespace-end-position ()
  "Get end position of current line with non-whitespace."
  (neuron/current-line-non-whitespace-position t))

(defun neuron/gen-new-line-in-below ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun neuron/des-current-line ()
  (interactive)
  (kill-whole-line))

;; OpFile
(defun neuron/refresh-current-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun neuron/sudo-edit-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (find-file (concat "/sudo:" user-login-name "@localhost:" (buffer-file-name)))
      (goto-char old-point))))

(defvar *neuron/unshifted-special-chars-layout*
  '(("1" "!")
    ("2" "@")
    ("3" "#")
    ("4" "$")
    ("5" "%")
    ("6" "^")
    ("7" "&")
    ("8" "*")
    ("9" "(")
    ("0" ")")
    ("!" "1")
    ("@" "2")
    ("#" "3")
    ("$" "4")
    ("%" "5")
    ("^" "6")
    ("&" "7")
    ("*" "8")
    ("(" "9")
    (")" "0")))

(defun neuron/mb-str-to-unibyte-char (str)
  "Translate first multibyte char in s to internal unibyte representation."
  (multibyte-char-to-unibyte (string-to-char str)))

(defun neuron/remap-keyboard (mapping)
  "Setup keyboard translate table using a list of pairwise key-mappings."
  (mapcar
   (lambda (mb-string-pair)
     (apply #'keyboard-translate
            (mapcar #'neuron/mb-str-to-unibyte-char mb-string-pair)))
   mapping))

(defun neuron/change-keyboard-map ()
  "Change my keyboard map."
  (interactive)
  (remap-keyboard *neuron/unshifted-special-chars-layout*))

;; AceJump
(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-mode)))

;; MoveText
(use-package move-text
  :ensure t
  :bind
  (("C-c p" . move-text-up)
   ("C-c n" . move-text-down)))

(keymap-global-set "C-j" #'neuron/gen-new-line-in-below)
(keymap-global-set "C-<f5>" #'neuron/refresh-current-file)
(keymap-global-set "M-;" #'neuron/comment-dwim-line)
(keymap-global-set "M-p" #'neuron/move-lines-up)
(keymap-global-set "M-n" #'neuron/move-lines-down)

;; Add around advice for copy or cut
(advice-add 'kill-ring-save :around #'neuron/kill-ring-save@around)
(advice-add 'kill-region :around #'neuron/kill-region@around)

(provide 'init-edit)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; elisp/init-edit.el ends here
