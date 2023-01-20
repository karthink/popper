;;; popper-echo.el --- Show a popup list in the echo area when cycling them  -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur

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

;; Popper-echo is a minor-mode to display a list of popup names in the echo area
;; when toggling or cycling popups. These popups can be accessed directly using
;; dispatch keybinds. See Popper for how to classify a buffer as a popup.

;; COMMANDS:

;; popper-echo-mode : Turn on the echo area display

;; CUSTOMIZATION:

;; `popper-echo-lines': The number of echo area/minibuffer lines to use when
;; showing a list of popups
;;
;; `popper-echo-dispatch-keys': A list of strings or characters representing the
;; keybindings to access popups shown in the echo area.
;;
;; `popper-echo-dispatch-persist': A boolean to control whether the dispatch
;; menu stays open after choosing a popup.
;;
;; `popper-echo-transform-function': A function to transform the display of
;; these popups, such as by truncating buffer names, etc.

;;; Code:

(require 'popper)

(defcustom popper-echo-transform-function nil
  "Function to transform buffer names.

This is called on buffer-names displayed by `popper-echo'.

This function should accept a
  string (the buffer name) and return a transformed string."
  :type 'function
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
for the kbd macro. These keys are available when using
popper-echo-mode.

Examples:
'(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p)
'(\"M-1\" \"M-2\" \"M-3\" \"M-4\" \"M-5\")

This variable has no effect when popper-echo-mode is turned
off."
  :type '(group character string)
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

;; Notify in echo area:
(defun popper-echo ()
  "Show popup list in the echo area when cycling popups."
  (let* ((message-log-max nil)
         (grp-symb (when popper-group-function
                       (funcall popper-group-function)))
         (buried-popups (thread-last (alist-get grp-symb popper-buried-popup-alist nil nil 'equal)
                          (mapcar #'cdr)
                          (cl-remove-if-not #'buffer-live-p)
                          (mapcar #'buffer-name)
                          (delete-dups)))
         (group (if (and grp-symb (symbolp grp-symb))
                         (symbol-name grp-symb)
                       grp-symb))
         (open-popup (buffer-name))
         (dispatch-keys-extended (append (cdr popper-echo-dispatch-keys)
                                     (make-list (max 0 (- (length buried-popups)
                                                          (1- (length popper-echo-dispatch-keys))))
                                                nil)))
         (popup-strings
          (cl-reduce #'concat
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
    (let* ((max-width (- (* popper-echo-lines (frame-width))
                         (if group (+ 13 (length group)) 11)))
           (plen (length popup-strings))
           (space-p (> max-width plen)))
      (message "%s"
               (concat
                (if group (format "Group (%s): " group) "Popups: ")
                (substring popup-strings 0 (if space-p plen max-width))
                (unless space-p
                  (propertize "..." 'face 'popper-echo-area-buried)))))
    (set-transient-map (let ((map (make-sparse-keymap))
                             (i 0))
                         (dolist (keybind popper-echo-dispatch-keys map)
                           (define-key map (cond
                                            ((characterp keybind)
                                             (make-vector 1 keybind))
                                            ((stringp keybind)
                                             (kbd keybind)))
                             (popper-echo--dispatch-toggle i (cons open-popup
                                                                   buried-popups)))
                           (when popper-echo-dispatch-actions
                             (define-key map
                               (kbd
                                (concat "k " (cond
                                              ((characterp keybind)
                                               (char-to-string keybind))
                                              ((stringp keybind)
                                               keybind))))
                               (popper-echo--dispatch-kill i (cons open-popup
                                                                   buried-popups)))
                             
                             (define-key map
                               (kbd
                                (concat "^ " (cond
                                              ((characterp keybind)
                                               (char-to-string keybind))
                                              ((stringp keybind)
                                               keybind))))
                               (popper-echo--dispatch-raise i (cons open-popup
                                                                    buried-popups))))
                           (setq i (1+ i)))))))


(defun popper-echo--dispatch-toggle (i buf-list)
  "Return a function to switch to buffer I in list BUF-LIST.

This is used to create functions for switching between popups
quickly."
  (lambda (&optional arg)
    (interactive "P")
    (when-let ((buf (nth i buf-list)))
      (unless arg (popper-close-latest))
      (display-buffer buf)
      (popper--update-popups))
    (when popper-echo-dispatch-persist (popper-echo))))

(defun popper-echo--dispatch-kill (i buf-list)
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
      (popper-echo))))

(defun popper-echo--dispatch-raise (i buf-list)
  "Return a function to Kill buffer I in list BUF-LIST."
  (lambda ()
    (interactive)
    (let* ((buf (nth i buf-list)))
      (popper-toggle-type buf))
    (popper--update-popups)
    (when (and popper-echo-dispatch-persist
               popper-open-popup-alist)
      (popper-echo))))

;;;###autoload
(define-minor-mode popper-echo-mode
  "Show popup names in cycling order in the echo area when
  performing an action that involves showing a popup. These
  popups can be accessed directly or acted upon by using quick
  keys (see `popper-echo-dispatch-keys').

To define buffers as popups and customize popup display, see
`popper-mode'."
  :global t
  :lighter ""
  :group 'popper
  (if popper-echo-mode
      (progn
        (add-hook 'popper-open-popup-hook 'popper-echo)
        (unless popper-mode (popper-mode 1)))
    (remove-hook 'popper-open-popup-hook 'popper-echo)))

(provide 'popper-echo)
;;; popper-echo.el ends here
