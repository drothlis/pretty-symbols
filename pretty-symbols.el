;;; pretty-symbols.el --- l a m b d a -> λ.  -*- coding: utf-8; -*-

;; Copyright (C) 2012 David Röthlisberger

;; Author: David Röthlisberger <david@rothlis.net>
;; Version: 2.0
;; Keywords: faces

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for drawing multi-character tokens as Unicode glyphs.
;; http://github.com/drothlis/pretty-symbols
;;
;; This mode is heavily inspired by Trent Buck's pretty-symbols-mode[1]
;; and Arthur Danskin's pretty-mode[2]; but aims to replace those modes, and
;; the many others scattered on emacswiki.org, with:
;;
;; * A simple framework that others can use to define their own symbol
;;   replacements,
;; * that doesn't turn on all sorts of crazy mathematical symbols by default,
;; * is a self-contained project under source control, open to contributions,
;; * available in a well-known package repository like Marmalade (TODO).
;;
;; You probably won't want to use this with haskell-mode which has its own much
;; fancier fontification[3]. Eventually it would be nice if this package grew
;; in power and became part of Emacs, so other packages could use it instead of
;; rolling their own.
;;
;; Add your own custom symbol replacements to the list
;; `pretty-symbol-patterns'.
;;
;; Only tested with GNU Emacs 24.
;;
;; [1] http://paste.lisp.org/display/42335/raw
;; [2] http://www.emacswiki.org/emacs/pretty-mode.el
;; [3] https://github.com/haskell/haskell-mode/blob/master/haskell-font-lock.el

;;; Code:

;;;###autoload
(define-minor-mode pretty-symbols-mode
  "Draw multi-character tokens as Unicode glyphs.
For example, in lisp modes draw λ instead of the characters
l a m b d a. The on-disk file keeps the original characters.

This may sound like a neat trick, but be extra careful: it
changes the line length and can thus lead to surprises with
respect to alignment and layout.

To enable, add to the hooks of the major modes you want pretty
symbols in: (add-hook 'emacs-lisp-mode 'pretty-symbols-mode)."
  nil " λ" nil
  (if pretty-symbols-mode
      (font-lock-add-keywords nil (pretty-symbol-keywords) t)
    (font-lock-remove-keywords nil (pretty-symbol-keywords))
    ;; TODO: Disabling the mode doesn't decompose existing symbols.
    ;; Is the following safe to do -- what else uses composition?
    ;; (remove-text-properties (point-min) (point-max) '(composition nil))
    ))


;; User options

(defgroup pretty-symbols nil
  "Draw multi-character tokens as Unicode glyphs."
  :group 'font-lock)

(defcustom pretty-symbol-patterns
  (let ((lispen '(emacs-lisp-mode inferior-lisp-mode lisp-mode)))
    `((?λ "\\<lambda\\>" (,@lispen python-mode))
      (?ƒ "\\<function\\>" (js-mode))))
  "A list of (character pattern major-modes).
For each entry in the list, if the buffer's major mode (or one of
its parent modes) is listed in MAJOR-MODES, occurrences of
PATTERN will be shown as CHARACTER instead.

Note that a major mode's presence in this list doesn't turn on
pretty-symbols-mode; you have to do so in the major mode's hook."
  :group 'pretty-symbols)


;; Internal functions

(defun pretty-symbol-keywords ()
  "Return the pretty font-lock keywords for the current major mode."
  (delq nil (mapcar (lambda (x) (apply 'pretty-symbol-pattern-to-keyword x))
                    pretty-symbol-patterns)))

(defun pretty-symbol-pattern-to-keyword (char pattern modes)
  "For a single entry in `pretty-symbol-patterns' return a list
suitable as a single entry in `font-lock-keywords'."
  (if (apply 'derived-mode-p modes)
      `(,pattern (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           ,char 'decompose-region)
                           nil)))))


(provide 'pretty-symbols)

;;; pretty-symbols.el ends here
