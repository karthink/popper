;;; popper-echo.el --- Show a popup list in the echo area when cycling them  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.45
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

;;; Commentary:

;; This library provides two minor-modes to preview the list of relevant popup
;; names when toggling or cycling popups.  These popups can be accessed directly
;; using dispatch keybinds.  See Popper for how to classify a buffer as a popup.
;;
;; `popper-echo-mode' displays the list of popups in the echo area when toggling
;; or cycling popups.
;;
;; `popper-tab-line-mode' displays the list of popups in the tab-line of the
;; active popup when toggling or cycling them.  NOTE: This feature is
;; experimental.

;; CUSTOMIZATION:

;; `popper-echo-lines': The number of echo area/minibuffer lines to use when
;; showing a list of popups
;;
;; `popper-echo-dispatch-keys': A list of strings or characters representing the
;; keybindings to access popups shown in the echo area or tab-line.
;;
;; `popper-echo-dispatch-persist': A boolean to control whether the dispatch
;; keymap stays active after using a dispatch key.
;;
;; `popper-echo-transform-function': A function to transform the display of
;; these popups, such as by truncating buffer names, etc.

;;; Code:

(require 'popper)

(defcustom popper-echo-transform-function nil
  "Function to transform buffer names.

This is called on buffer-names displayed by `popper-echo'.

This function should accept a string (the buffer name) and return
a transformed string."
  :type '(choice (const :tag "Don't transform buffer-names" nil)
          function)
  :group 'popper)

 (defcustom popper-echo-lines 2
  "Number of minibuffer lines used to show popup buffer names by `popper-echo'.

This has no effect when popper-echo-mode is turned off."
  :type 'integer
  :group 'popper)

(defcustom popper-echo-dispatch-persist t
  "Controls whether the `popper-echo' dispatch menu is persistent."
  :type 'boolean
  :group 'popper)

(defcustom popper-echo-dispatch-actions nil
  "Controls whether `popper-echo' dispatch actions are bound.

When true, you can
- Kill popup buffers with k
- Raise popup buffers with ^

when using the dispatch menu by prefixing the dispatch keys with them.

NOTE: This feature is experimental."
  :type 'boolean
  :group 'popper)

(defcustom popper-echo-dispatch-keys '("M-0" "M-1" "M-2" "M-3" "M-4"
                                       "M-5" "M-6" "M-7" "M-8" "M-9")
  "List of keys used for dispatching to popup buffers.

The first element is bound to the currently open popup.

Each entry in the list can be a character or a string suitable
for the kbd macro.  These keys are available when using
popper-echo-mode.

Examples:
\\='(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p)
\\='(\"M-1\" \"M-2\" \"M-3\" \"M-4\" \"M-5\")

This variable has no effect when popper-echo-mode is turned
off."
  :type '(repeat (choice character string))
  :group 'popper)


(defface popper-echo-area-buried
  '((t :inherit shadow))
  "Echo area face for buried popups."
  :group 'popper)

(defface popper-echo-area
  '((t :inverse-video t
       :weight bold))
  "Echo area face for opened popup."
  :group 'popper)

(defface popper-echo-dispatch-hint
  '((t :inherit bold))
  "Echo area face for popper dispatch key hints."
  :group 'popper)

;;; Utility functions

(defun popper-echo--dispatch-toggle (i buf-list repeat)
  "Return a function to switch to buffer I in list BUF-LIST.

This is used to create functions for switching between popups
quickly."
  (lambda (&optional arg)
    (interactive "P")
    (when-let ((buf (nth i buf-list)))
      (unless arg (popper-close-latest))
      (display-buffer buf)
      (popper--update-popups)
      (when popper-echo-dispatch-persist
        (with-current-buffer buf (funcall repeat))))))

(defun popper-echo--dispatch-kill (i buf-list repeat)
  "Return a function to Kill buffer I in list BUF-LIST."
  (lambda ()
    (interactive)
    (let* ((buf (nth i buf-list))
           (win (get-buffer-window buf)))
      (kill-buffer buf)
      (popper--delete-popup win))
    (popper--update-popups)
    (when (and popper-echo-dispatch-persist
               popper-open-popup-alist)
      (with-current-buffer (cdar popper-open-popup-alist)
        (funcall repeat)))))

(defun popper-echo--dispatch-raise (i buf-list repeat)
  "Return a function to raise buffer I in list BUF-LIST.

Raising converts if from a popup to a regular buffer."
  (lambda ()
    (interactive)
    (let* ((buf (nth i buf-list)))
      (popper-toggle-type buf))
    (popper--update-popups)
    (when (and popper-echo-dispatch-persist
               popper-open-popup-alist)
      (with-current-buffer (cdar popper-open-popup-alist)
        (funcall repeat)))))

(defun popper-echo--popup-info ()
  "Return the popper group and list of buried popup buffers."
  (let ((grp-symb (when popper-group-function
                    (funcall popper-group-function))))
    (cons grp-symb
          (thread-last (alist-get grp-symb popper-buried-popup-alist nil nil 'equal)
                       (mapcar #'cdr)
                       (cl-remove-if-not #'buffer-live-p)
                       (delete-dups)))))

(defun popper-echo--activate-keymap (buffers repeat)
  "Activate a transient keymap to switch to or manipulate BUFFERS.

Each command in the keymap calls the function REPEAT afterwards."
  (set-transient-map
   (cl-loop with map = (make-sparse-keymap)
            for i upto 9
            for keybind in popper-echo-dispatch-keys
            for rawkey = (cond ((characterp keybind) (char-to-string keybind))
                               (t keybind))
            do
            (define-key map (kbd rawkey) (popper-echo--dispatch-toggle i buffers repeat))
            (define-key map (kbd (concat "k " rawkey))
                        (popper-echo--dispatch-kill i buffers repeat))
            (define-key map (kbd (concat "^ " rawkey))
                        (popper-echo--dispatch-raise i buffers repeat))
            finally return map)))

;;; Notify in echo area:
(defun popper-echo ()
  "Show popup list in the echo area when cycling popups."
  (pcase-let*
      ((message-log-max nil)
       (`(,grp-symb . ,buried-popups) (popper-echo--popup-info))
       (buried-popups (mapcar #'buffer-name buried-popups))
       (group (and grp-symb
                   (concat "Group (" (truncate-string-to-width (format "%S" grp-symb) 20 nil nil t) "): ")))
       (open-popup (buffer-name))
       (dispatch-keys-extended
        (append (cdr popper-echo-dispatch-keys)
                (make-list (max 0 (- (length buried-popups)
                                     (1- (length popper-echo-dispatch-keys))))
                           nil)))
       (popup-strings
        (apply #'concat
               (cons
                (if-let ((transform popper-echo-transform-function))
                    (funcall transform open-popup)
                  (propertize open-popup 'face 'popper-echo-area))
                (cl-mapcar (lambda (key buf)
                             (concat
                              (propertize ", " 'face 'popper-echo-area-buried)
                              (propertize "[" 'face 'popper-echo-area-buried)
                              (and key
                                   (concat
                                    (propertize (if (characterp key)
                                                    (char-to-string key)
                                                  key)
                                                'face 'popper-echo-dispatch-hint)
                                    (propertize ":" 'face 'popper-echo-area-buried)))
                              (if-let ((transform popper-echo-transform-function))
                                  (funcall transform buf)
                                (concat
                                 (propertize buf 'face 'popper-echo-area-buried)))
                              (propertize "]" 'face 'popper-echo-area-buried)))
                           dispatch-keys-extended
                           buried-popups)))))
    (let* ((max-width (- (* popper-echo-lines (frame-width)) (if group (length group) 11)))
           (plen (length popup-strings))
           (space-p (> max-width plen)))
      (message "%s"
               (concat
                (or group "Popups: ")
                (substring popup-strings 0 (if space-p plen max-width))
                (unless space-p
                  (propertize "..." 'face 'popper-echo-area-buried)))))
    (popper-echo--activate-keymap (cons open-popup buried-popups) #'popper-echo)))

(defvar popper-tab-line-mode "popper-echo")

;;;###autoload
(define-minor-mode popper-echo-mode
  "Toggle Popper Echo mode.
Show popup names in cycling order in the echo area when
performing an action that involves showing a popup.  These popups
can be accessed directly or acted upon by using quick keys (see
`popper-echo-dispatch-keys').

To define buffers as popups and customize popup display, see
`popper-mode'."
  :global t
  :lighter ""
  :group 'popper
  (if popper-echo-mode
      (progn
        (when popper-tab-line-mode
          (message "`popper-echo-mode'. is incompatible with `popper-tab-line-mode'  Disabling `popper-tab-line-mode'.")
          (popper-tab-line-mode -1))
        (add-hook 'popper-open-popup-hook 'popper-echo)
        (unless popper-mode (popper-mode 1)))
    (remove-hook 'popper-open-popup-hook 'popper-echo)))

;;; Notify using tab-line
(declare-function tab-line-mode "tab-line")
(declare-function tab-line-tab-name-format-default "tab-line")
(defvar tab-line-tab-name-format-function)
(defvar tab-line-tabs-function)
(defvar tab-line-mode)

(defun popper-tab-line--format (tab tabs)
  (let ((name (tab-line-tab-name-format-default tab tabs))
        (idx (cl-position tab tabs)))
    (concat
     (propertize
      (char-to-string (+ idx #x2460)) ;; #x2776
      'face (if (eq tab (current-buffer))
                (if (mode-line-window-selected-p)
                    'tab-line-tab-current 'tab-line-tab)
              'tab-line-tab-inactive))
     name)))

(defun popper-tab-line--ensure ()
  (pcase-let ((`(_ . ,buried-popups) (popper-echo--popup-info)))
    (if (not buried-popups)
        (tab-line-mode -1)
      (unless tab-line-mode
        (setq-local
         tab-line-tabs-function
         (lambda () (cl-sort (cons (current-buffer) (cdr (popper-echo--popup-info)))
                        #'string< :key #'buffer-name))
         tab-line-tab-name-format-function #'popper-tab-line--format)
        (when popper-echo-transform-function
          (setq-local tab-line-tab-name-function
                      (lambda (buf _) (funcall popper-echo-transform-function
                                          (buffer-name buf)))))
        (tab-line-mode 1)))
    (popper-echo--activate-keymap
     (cl-sort (cons (current-buffer) buried-popups) #'string< :key #'buffer-name)
     #'popper-tab-line--ensure)))

;;;###autoload
(define-minor-mode popper-tab-line-mode
  "Toggle Popper Tab Line Mode.
Show popup names in cycling order in the tab-line of the popup
window when performing an action that involves showing a popup.
These popups can be accessed directly or acted upon by using
quick keys (see `popper-echo-dispatch-keys').

To define buffers as popups and customize popup display, see
`popper-mode'."
  :global t
  :lighter ""
  :group 'popper
  (if popper-tab-line-mode
      (progn
       (require 'tab-line)
       (when popper-echo-mode
         (message "`popper-tab-line-mode' is incompatible with `popper-echo-mode'.  Disabling `popper-echo-mode'.")
         (popper-echo-mode -1))
       (add-hook 'popper-open-popup-hook #'popper-tab-line--ensure)
       (unless popper-mode (popper-mode 1)))
    (remove-hook 'popper-open-popup-hook #'popper-tab-line--ensure)
    ;; Clear tab-lines
    (mapc
     (pcase-lambda (`(_ . ,buf))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (kill-local-variable 'tab-line-tabs-function)
           (kill-local-variable 'tab-line-tab-name-format-function)
           (unless global-tab-line-mode (tab-line-mode -1)))))
     (mapcan #'cdr (cons (cons nil popper-open-popup-alist)
                         popper-buried-popup-alist)))
    (force-mode-line-update)))

(provide 'popper-echo)
;;; popper-echo.el ends here
