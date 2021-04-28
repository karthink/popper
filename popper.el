;;; popper.el --- Summon and dismiss buffers as popups -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.30
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

;; Popper is a minor-mode to tame the flood of ephemeral windows Emacs
;; produces, while still keeping them within arm's reach. Designate any
;; buffer to "popup" status, and it will stay out of your way. Disimss
;; or summon it easily with one key. Cycle through all your "popups" or
;; just the ones relevant to your current buffer. Useful for many
;; things, including toggling display of REPLs, documentation,
;; compilation or shell output, etc.
;;
;; For a demo describing usage and customization see
;; https://www.youtube.com/watch?v=E-xUNlZi3rI
;;
;; COMMANDS:
;;
;; popper-toggle-latest : Toggle latest popup
;; popper-cycle         : Cycle through all popups, or close all open popups
;; popper-toggle-type   : Turn a regular window into a popup or vice-versa
;; popper-kill-latest-popup : Kill latest open popup
;;
;; CUSTOMIZATION:
;;
;; `popper-reference-buffers': A list of major modes or regexps whose
;; corresponding buffer major-modes or regexps (respectively) should be
;; treated as popups.
;;
;; `popper-mode-line': String or sexp to show in the mode-line of
;; popper. Setting this to nil removes the mode-line entirely from
;; popper.
;;
;; `popper-group-function': Function that returns the context a popup
;; should be shown in. The context is a string or symbol used to group
;; together a set of buffers and their associated popups, such as the
;; project root. Customize for available options.
;;
;; `popper-display-control': This package summons windows defined by the
;; user as popups by simply calling `display-buffer'. By default,
;; it will display your popups in a non-obtrusive way. If you want
;; Popper to display popups according to window rules you specify in
;; `display-buffer-alist' (or through a package like Shackle), set this
;; variable to nil.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function project-root "project")
(declare-function project-current "project")
(declare-function projectile-project-root "projectile")
(declare-function persp-current-name "perspective")

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

 Can be a quoted list or function. Setting this to nil removes
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

(defcustom popper-group-function nil
  "Function that returns a popup context.

When set to nil popups are not grouped by context.

This function is called with no arguments and should return a
string or symbol identifying a popup buffer's group. This
identifier is used to associate popups with regular buffers (such
as by project, directory, or `major-mode') so that popup-cycling
from a regular buffer is restricted to its associated group.

Built-in choices include

`popper-group-by-directory': Return project root or default directory.
`popper-group-by-project': Return project root using project.el.
`popper-group-by-projectile': Return project root using projectile.
`popper-group-by-perspective': Return perspective name."
  :group 'popper
  :type '(choice
          (const :tag "Don't group popups" nil)
          (const :tag "Group by project (project.el)" popper-group-by-project)
          (const :tag "Group by project (projectile)" popper-group-by-projectile)
          (const :tag "Group by perspective" popper-group-by-perspective)
          (const :tag "Group by directory" popper-group-by-directory)
          (function :tag "Custom function")))

(defvar popper-reference-names nil
  "List of buffer names whose windows are treated as popups.")

(defvar popper-reference-modes nil
 "List of buffer major-modes whose buffers are treated as popups.")

(defvar popper-open-popup-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups.")

(defvar popper-buried-popup-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups.

If `popper-group-function' is non-nil, these are
grouped by the predicate `popper-group-function'.")

(defvar popper-lighter ""
  "Lighter for `popper-mode'.")

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
            (or (memq popper-popup-status '(popup user-popup))
                (unless (eq popper-popup-status 'raised)
                  (popper-popup-p buffer))))))))

(defun popper-group-by-directory ()
  "Return an identifier (default directory) to group popups.

The project root is used if found by project, with the default
directory as a fall back."
  (or (and (fboundp 'project-root)
           (project-root (project-current)))
      (expand-file-name default-directory)))

(defun popper-group-by-project ()
  "Return an identifier (project root) to group popups."
  (unless (fboundp 'project-root)
    (user-error "Cannot find project directory to group popups.
  Please install `project' or customize
  `popper-group-function'"))
  (project-root (project-current)))

(defun popper-group-by-projectile ()
  "Return an identifier to group popups.

This returns the project root found using the projectile package."
  (unless (fboundp 'projectile-project-root)
    (user-error "Cannot find project directory to group popups.
  Please install `projectile' or customize
  `popper-group-function'"))
  (projectile-project-root))

(defun popper-group-by-perspective ()
  "Return an identifier to group popups.

This returns the name of the perspective."
  (unless (fboundp 'persp-current-name)
    (user-error "Cannot find perspective name to group popups.
  Please install `perspective' or customize
  `popper-group-function'"))
  (persp-current-name))

(defun popper-find-popups (test-buffer-list)
  "Return an alist corresponding to popups in TEST-BUFFER-LIST.

Each element of the alist is a cons cell of the form (window . buffer)."
  (let* (popups)
    (dolist (b test-buffer-list popups)
      (when (and (not (minibufferp b))
                 (popper-popup-p b))
        (push (cons (get-buffer-window b) b) popups)))))

(defun popper-update-popups ()
  "Update the list of currently open popups.

 Intended to be added to `buffer-list-update-hook' and
`window-configuration-change-hook'."
  (let* ((popups (popper-find-popups (buffer-list)))
         (open-popups (cl-remove-if-not
                       (lambda (win-buf) (windowp (car win-buf)))
                       popups))
         (closed-popups (cl-set-difference popups open-popups :key #'cdr))
         buried-alist)
    (setq popper-open-popup-alist (nreverse open-popups))
    (if popper-group-function
        (cl-loop for (win . buf) in closed-popups do
                 (let ((group (with-current-buffer buf
                                (funcall popper-group-function)))
                       (newelt (cons win buf)))
                   (if (alist-get group buried-alist)
                       (push newelt (alist-get group buried-alist))
                     (push (cons group (list newelt)) buried-alist)))
                 finally
                 (setq popper-buried-popup-alist buried-alist))
      (setq popper-buried-popup-alist closed-popups)))
  ;; Mode line update
  (cl-loop for (_ . buf) in popper-open-popup-alist do
           (with-current-buffer buf
             (setq mode-line-format (popper-modified-mode-line)))))

(defun popper-find-buried-popups ()
  "Update the list of currently buried popups.

 Meant to be run when starting command `popper-mode'."
  (let ((buried-popups (popper-find-popups
                        (cl-set-difference
                         (buffer-list)
                         (mapcar #'window-buffer
                                 (window-list))))))
    (if popper-group-function
        (cl-loop for (win . buf) in buried-popups do
                 (push (cons win buf)
                       (alist-get
                        (with-current-buffer buf
                          (funcall popper-group-function))
                        popper-buried-popup-alist
                        nil nil 'equal)))
      (setq popper-buried-popup-alist
            (list (cons nil buried-popups))))))

(defun popper-close-latest ()
  "Close the last opened popup."
  (unless popper-mode (user-error "Popper-mode not active!"))
  (if (null popper-open-popup-alist)
      (message "No open popups!")
    (cl-destructuring-bind ((win . buf) . rest) popper-open-popup-alist
      (let ((group (when popper-group-function
                     (with-current-buffer buf
                       (funcall popper-group-function)))))
        (unless (cl-member buf
                           (cdr (assoc group popper-buried-popup-alist))
                           :key 'cdr)
          ;; buffer doesn't already exist in the buried popup list
          (push (cons nil buf) (alist-get group
                                          popper-buried-popup-alist
                                          nil nil 'equal))))
      (pop popper-open-popup-alist)
      (with-selected-window win
        (bury-buffer)
        ;;only close window when window has a parent or in a child frame:
        (if (and (window-valid-p win)
                 (window-parent win))
            (delete-window win)
          (if (frame-parent)
              (delete-frame)))))))

(defun popper-open-latest (&optional group)
  "Open the last closed popup.

Optional argument GROUP is called with no arguments to select
a popup buffer to open."
  (unless popper-mode (user-error "Popper-mode not active!"))
  (let* ((identifier (when popper-group-function group))
        (no-popup-msg (format "No buried popups for group %s"
                                 (if (symbolp identifier)
                                     (symbol-name identifier)
                                   identifier))))
    (if (null (alist-get identifier popper-buried-popup-alist
                         nil 'remove 'equal))
        (message (if identifier no-popup-msg "No buried popups"))
      (if-let* ((new-popup (pop (alist-get identifier popper-buried-popup-alist
                                           nil 'remove 'equal)))
                (buf (cdr new-popup)))
          (if (buffer-live-p buf)
              (progn (display-buffer buf))
            (popper-open-latest))
        (message no-popup-msg)))))

(defun popper-modified-mode-line ()
  "Return modified mode-line string."
  (when popper-mode-line
    (if (member popper-mode-line mode-line-format)
        mode-line-format
      (append (cl-subseq (default-value 'mode-line-format) 0 popper-mode-line-position)
              (cons popper-mode-line (nthcdr popper-mode-line-position
                                             (default-value 'mode-line-format)))))))

(defun popper-restore-mode-lines (win-buf-alist)
  "Restore the default value of `mode-line-format'.

This applies to popup-buffers in the list WIN-BUF-ALIST."
  (dolist (buf (mapcar 'cdr win-buf-alist))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq mode-line-format (default-value 'mode-line-format))
        (force-mode-line-update)))))

(defun popper-bury-all ()
  "Bury all open popups."
  (while popper-open-popup-alist
    (popper-close-latest)))

(defun popper-open-all ()
  "Open all popups.

Note that buffers that are displayed in the same 'position' on
the screen by `display-buffer' will not all be displayed."
  (let ((group (when popper-group-function
                 (funcall popper-group-function))))
    (while popper-buried-popup-alist
      (popper-open-latest group))))

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
  (let ((group (when popper-group-function
                 (funcall popper-group-function))))
    (if popper-open-popup-alist
        (pcase arg
          (4 (popper-open-latest group))
          (16 (popper-bury-all))
          (_ (popper-close-latest)))
      (if (equal arg 16)
          (popper-open-all)
        (popper-open-latest group)))))

(defun popper-cycle (&optional default-group)
  "Cycle visibility of popup windows one at a time.

With a prefix argument DEFAULT-GROUP, cycle through popups
belonging to the default group."
  (interactive "P")
  (let* ((group (when (and popper-group-function
                           (not default-group))
                  (funcall popper-group-function))))
    (if (null popper-open-popup-alist)
        (popper-open-latest group)
      (if (null (alist-get group popper-buried-popup-alist nil nil 'equal))
          (popper-bury-all) ; starting new cycle, so bury everything first.
        ;; cycle through buffers
        (popper-close-latest)
        (let ((bufs (cdr (assoc group popper-buried-popup-alist))))
          (setf (alist-get group popper-buried-popup-alist nil nil 'equal)
                (append (cdr bufs) (cons (car bufs) nil))))
        (popper-open-latest group)))))

(defun popper-raise-popup (&optional buffer)
  "Raise a popup to regular status.
If BUFFER is not specified,raise the current buffer."
  (when-let ((buf (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer buf
      (if (popper-popup-p buf)
          (setq popper-popup-status 'raised)
        (setq popper-popup-status nil))
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

(defun popper-kill-latest-popup ()
  "Kill the latest popup-buffer and delete its window."
  (interactive)
  (cl-destructuring-bind ((win . buf) . rest) popper-open-popup-alist
    (pop popper-open-popup-alist)
    (if (and (window-valid-p win)
             (window-parent win))
            (delete-window win)
          (if (frame-parent)
              (delete-frame)))
    (kill-buffer buf)))

;;;###autoload
(define-minor-mode popper-mode
  "Toggle Popper mode. When enabled, treat certain buffer
windows as popups, a class of window that can be summoned or
dismissed with a command. See the customization options for
details on how to designate buffer types as popups."
  :global t
  :version "0.30"
  :lighter popper-lighter
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
        (add-hook 'buffer-list-update-hook #'popper-update-popups)
        (add-to-list 'display-buffer-alist
                     `(popper-display-control-p
                       (,popper-display-function))))
    ;; Turning the mode OFF
    (remove-hook 'window-configuration-change-hook #'popper-update-popups)
    (remove-hook 'buffer-list-update-hook #'popper-update-popups)
    (cl-loop for (_ . win-buf-alist) in popper-buried-popup-alist do
             (popper-restore-mode-lines win-buf-alist))
    (popper-restore-mode-lines popper-open-popup-alist)
    (setq popper-buried-popup-alist nil
          popper-open-popup-alist nil)
    (setq display-buffer-alist
          (cl-remove 'popper-display-control-p
                     display-buffer-alist
                     :key 'car))))

(provide 'popper)
;;; popper.el ends here
