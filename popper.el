;;; popper.el --- Summon and dismiss buffers as popups -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.20
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://github.com/karthink/popper

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; This package provides a minor-mode to designate buffers as "popups" and
;; summon and dismiss them with a key. Useful for many things, including
;; toggling REPLS, documentation, compilation or shell output, etc. This package
;; will place buffers on your screen, but it works best in conjunction with some
;; system to handle window creation and placement, like shackle.el. Under the
;; hood popper summons windows defined by the user as "popups" by simply
;; calling `display-buffer'.
;;
;; COMMANDS:
;;
;; popper-toggle-latest : Toggle latest popup
;; popper-cycle         : Cycle through all popups, or close all open popups
;; popper-toggle-type   : Turn a regular window into a popup or vice-versa
;;
;; CUSTOMIZATION:
;;
;; `popper-reference-buffers': A list of major modes or regexps whose
;; corresponding buffer major-modes or regexps (respectively) should be treated
;; as popups.
;;
;; `popper-mode-line': String or sexp to show in the mode-line of
;; popper. Setting this to NIL removes the mode-line entirely from
;; popper.
;;
;; TODO: Add popup list maintenance to `make-frame-finish-functions',
;; (add-hook 'after-make-frame-functions 'popper-update-popups)
;;
;; by Karthik Chikmagalur <karthik.chikmagalur@gmail.com>

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar popper-mode)

(defgroup popper nil
  "Provide functions for easy access to popup windows"
  :group 'convenience)

(defcustom popper-reference-buffers '("\\*Messages\\*$")
  "List of buffers to treat as popups.
Each entry in the list can be a regexp (string) to match buffer
names against, or a `major-mode' (symbol) to match buffer
major-modes against.

Example:

'(\"\\*Messages\\*\"
  \"Output\\*$\"
  help-mode
  compilation-mode)

Will match against the Messages buffer, any buffer ending in
Output*, and all help and compilation buffers."
  :type '(restricted-sexp :match-alternatives (stringp symbolp))
  :group 'popper)

(defcustom popper-mode-line '(:eval (propertize " POP" 'face 'mode-line-emphasis))
  "String or sexp to show in the mode-line of popper.

 Can be a quoted list or function. Setting this to NIL removes
the mode-line entirely from popper."
  :group 'popper
  :type '(choice (const :tag "Off" nil)
                 (string :tag "Literal text")
                 (sexp :tag "General `mode-line-format' entry")))

(defcustom popper-mode-line-position 0
  "Position in mode-line to place `popper-mode-line'."
  :type 'integer
  :group 'popper)

(defcustom popper-display-control t
  "Whether popper should control the placement of popup windows.
Choices are:
'user: The default. Only control placement of explicitly marked popups.
 nil : Do not control popup placement.
 t   : Control placement of all popups."
  :group 'popper
  :type '(choice (const :tag "Explicitly set popups only" 'user)
                 (const :tag "All popups" t)
                 (const :tag "Never" nil)))

(defcustom popper-display-function #'popper-select-popup-at-bottom
  "Function to use to display popper.

 Note that this is only invoked when
`popper-display-control' is non-nil.

This function accepts two arguments, a buffer and (optional) an
action alist and displays the buffer. See (info \"(elisp) Buffer
Display Action Alists\") for details on the alist."
  :group 'popper
  :type 'function)

(defvar popper-reference-names nil
  "List of buffer names whose windows are treated as popups.")

(defvar popper-reference-modes nil
 "List of buffer major-modes whose buffers are treated as popups.")

(defvar popper-open-popup-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups.")

(defvar popper-buried-popup-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups.")

(defvar-local popper-popup-status nil
  "Identifies a buffer as a popup by its buffer-local value.
  Valid values are 'popup, 'raised, 'user-popup or nil.

'popup     : This is a popup buffer specified in `popper-reference-buffers'.
'raised    : This is a POPUP buffer raised to regular status by the user.
'user-popup: This is a regular buffer lowered to popup status by the user.")

(defun popper-select-popup-at-bottom (buffer &optional _alist)
  "Display and switch to popup-buffer BUFFER at the bottom of the screen."
  (let ((window (display-buffer-in-side-window
                 buffer
                 '((window-height . (lambda (win)
                                      (fit-window-to-buffer
                                       win
                                       (floor (frame-height) 3))))
                   (side . bottom)
                   (slot . 1)))))
    (select-window window)))

(defun popper-popup-p (buf)
  "Predicate to test if buffer BUF meets the criteria listed in `popper-reference-buffers'."
  (or (seq-some (lambda (buf-regexp)
               (string-match-p buf-regexp (buffer-name buf)))
             popper-reference-names)
      (member (buffer-local-value 'major-mode buf) popper-reference-modes)))

(defun popper-display-control-p (buf &optional _act)
  "Predicate to test if display of buffer BUF needs to be handled by popper.

This is intended to be used in `display-buffer-alist'."
  (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
    (pcase popper-display-control
      ('user
       (with-current-buffer buffer
         (eq popper-popup-status 'user-popup)))
      ('t (with-current-buffer buffer
            (memq popper-popup-status '(popup user-popup)))))))

(defun popper-find-popups (test-buffer-list)
  "Return an alist corresponding to popups in TEST-BUFFER-LIST.

Each element of the alist is a cons cell of the form (window . buffer)."
  (let* (open-popups)
    (dolist (b test-buffer-list open-popups)
      (let ((popup-status (buffer-local-value 'popper-popup-status b)))
        (when (and (not (minibufferp b))
                   (not (eq popup-status 'raised))
                   (or (member popup-status '(popup user-popup))
                       (popper-popup-p b)))
          (with-current-buffer b
            (setq popper-popup-status (or popup-status
                                          'popup)))
          (push (cons (get-buffer-window b) b)
                open-popups))))))

(defun popper-update-popups ()
  "Update the list of currently open popups.

 Meant to be added to `window-configuration-change-hook'."
  (let* ((open-buffers (mapcar #'window-buffer (window-list)))
         (open-popups (popper-find-popups open-buffers))
         (closed-popups (cl-remove-if-not
                         (lambda (arg)
                           (memq (buffer-local-value 'popper-popup-status (cdr arg))
                                 '(popup user-popup)))
                         (cl-set-difference popper-open-popup-alist
                                            open-popups
                                            :key #'cdr))))
         (setq popper-open-popup-alist (nreverse open-popups))
         (setq popper-buried-popup-alist
               (append closed-popups
                       (cl-set-difference popper-buried-popup-alist
                                          closed-popups
                                          :key #'cdr))))
  ;; Mode line update
  (cl-loop for (_ . buf) in popper-open-popup-alist do
             (with-current-buffer buf
               (setq mode-line-format (popper-modified-mode-line)))))

(defun popper-find-buried-popups ()
  "Update the list of currently buried popups.

 Meant to be run when starting command `popper-mode'."
  (setq popper-buried-popup-alist
        (popper-find-popups
            (cl-set-difference (buffer-list)
                               (mapcar #'window-buffer
                                       (window-list))))))

(defun popper-close-latest ()
  "Close the last opened popup."
  (if (null popper-open-popup-alist)
      (message (if popper-mode
                   "No open popups!"
                 "popper-mode not active!"))
    (cl-destructuring-bind ((win . buf) . rest) popper-open-popup-alist
      (when (and (window-valid-p win) (window-parent win))
        ;;only close window when window has a parent:
        (unless (seq-some
                 (lambda (item) (eq buf (cdr item)))
                 popper-buried-popup-alist)
          ;; buffer doesn't already exist in the buried popup list
          (push (cons nil buf) popper-buried-popup-alist)
          (pop popper-open-popup-alist))
        (with-selected-window win
          (bury-buffer buf)
          (delete-window win))))))

(defun popper-open-latest ()
  "Open the last closed popup."
  (if (null popper-buried-popup-alist)
      (message (if popper-mode
                   "No buried popups!"
                 "popper-mode not active!"))
    (let* ((new-popup (pop popper-buried-popup-alist))
           (buf (cdr new-popup)))
      (if (buffer-live-p buf)
          (progn (display-buffer buf))
        (popper-open-latest)))))

(defun popper-modified-mode-line ()
  "Return modified mode-line string."
  (when popper-mode-line
    (if (member popper-mode-line mode-line-format)
        mode-line-format
      (append (cl-subseq (default-value 'mode-line-format) 0 popper-mode-line-position)
              (cons popper-mode-line (nthcdr popper-mode-line-position
                                             (default-value 'mode-line-format)))))))

(defun popper-bury-all ()
  "Bury all open popper."
  (while popper-open-popup-alist
    (popper-close-latest)))

(defun popper-open-all ()
  "Open all popups.

Note that buffers that are displayed in the same 'position' on
the screen by `display-buffer' will not all be displayed."
  (while popper-buried-popup-alist
    (popper-open-latest)))

(defun popper-toggle-latest (&optional arg)
  "Toggle visibility of the last opened popup window.

With prefix ARG \\[universal-argument], toggle visibility of the next popup windows
while keeping the current one (FIXME: This behavior can be
inconsistent.)

With a double prefix ARG \\[universal-argument]
\\[universal-argument], toggle all popup-windows. Note that only
one buffer can be show in one 'slot', so it will display as many
windows as it can."
  (interactive "p")
  (if popper-open-popup-alist
      (pcase arg
        (4 (popper-open-latest))
        (16 (popper-bury-all))
        (_ (popper-close-latest)))
    (if (equal arg 16)
        (popper-open-all)
      (popper-open-latest))))

(defun popper-cycle (&optional _arg)
  "Cycle visibility of popup windows one at a time.

TODO: With a prefix argument ARG, cycle in the opposite
direction."
  (interactive "p")
  (if (null popper-open-popup-alist)
      (popper-open-latest)
    (if (null popper-buried-popup-alist)
        (popper-bury-all) ; starting new cycle, so bury everything first.
      ;; cycle through buffers
      (popper-close-latest)
      (let ((bufs popper-buried-popup-alist))
        (setq popper-buried-popup-alist
              (append (cdr bufs) (cons (car bufs) nil))))
      (popper-open-latest))))

(defun popper-raise-popup (&optional buffer)
  "Raise a popup to regular status.
If BUFFER is not specified,raise the current buffer."
  (when-let* ((buf (get-buffer (or buffer (current-buffer))))
              (popup-status (buffer-local-value 'popper-popup-status buf)))
    (with-current-buffer buf
      (setq popper-popup-status (and (popper-popup-p buf) 'raised))
      (setq mode-line-format (default-value 'mode-line-format)))
    (delete-window (get-buffer-window buf))
    (pop-to-buffer buf)))

(defun popper-lower-to-popup (&optional buffer)
  "Turn a regular buffer BUFFER into a popup.

If BUFFER is not specified act on the current buffer instead."
  (let ((buf (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer buf
      (setq popper-popup-status (if (popper-popup-p buf)
                                           'popup
                                         'user-popup))
      (delete-window (get-buffer-window buf t))
      (pop-to-buffer buf))
    (popper-update-popups)))

(defun popper-toggle-type (&optional buffer)
  "Turn a popup buffer BUFFER into a regular window or vice-versa.

If BUFFER is not specified act on the current buffer instead."
  (interactive)
  (let* ((buf (get-buffer (or buffer (current-buffer))))
         (popup-status (buffer-local-value 'popper-popup-status buf)))
    (pcase popup-status
      ((or 'popup 'user-popup) (popper-raise-popup buf))
      (_ (popper-lower-to-popup buf)))))

;;;###autoload
(define-minor-mode popper-mode
  "Toggle Popper mode. When enabled, treat certain buffer
windows as popups, a class of window that can be summoned or
dismissed with a command. See the customization options for
details on how to designate buffer types as popups."
  :global t
  :version "0.20"
  :lighter ""
  :group 'popper
  :keymap (let ((map (make-sparse-keymap))) map)
  (if popper-mode
      ;; Turning the mode ON
      (progn
        (setq popper-reference-names
              (cl-remove-if-not #'stringp popper-reference-buffers)
              popper-reference-modes
              (cl-remove-if-not #'symbolp popper-reference-buffers))
        (popper-find-buried-popups)
        (popper-update-popups)
        (add-hook 'window-configuration-change-hook #'popper-update-popups)
        (add-to-list 'display-buffer-alist
                     `(popper-display-control-p
                       (,popper-display-function))))
    ;; Turning the mode OFF
    (setq popper-buried-popup-alist nil
          popper-open-popup-alist nil)
    (remove-hook 'window-configuration-change-hook #'popper-update-popups)
    (setq display-buffer-alist
          (delete `(popper-display-control-p
                    (,popper-display-function))
                display-buffer-alist))))

(provide 'popper)
;;; popper.el ends here
