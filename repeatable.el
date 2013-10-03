;;; repeatable.el --- make repeatable commands       -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: extensions, convenience
;; Created: 2013-10-02

;; This program is free software; you can redistribute it and/or modify
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

;; A few functions to make repeatable commands.

;;; Code:

(require 'help-fns)

(eval-and-compile
  (or (fboundp 'autoloadp)              ; new in 24.3
      (defun autoloadp (object)
        (eq 'autoload (car-safe object)))))

(eval-and-compile
  (or (fboundp 'set-temporary-overlay-map) ; new in 24.3
      (defun set-temporary-overlay-map (map &optional keep-pred)
        "Set MAP as a temporary keymap taking precedence over most other keymaps.
Note that this does NOT take precedence over the \"overriding\" maps
`overriding-terminal-local-map' and `overriding-local-map' (or the
`keymap' text property).  Unlike those maps, if no match for a key is
found in MAP, the normal key lookup sequence then continues.

Normally, MAP is used only once.  If the optional argument
KEEP-PRED is t, MAP stays active if a key from MAP is used.
KEEP-PRED can also be a function of no arguments: if it returns
non-nil then MAP stays active."
        (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
               (overlaysym (make-symbol "t"))
               (alist (list (cons overlaysym map)))
               (clearfun
                `(lambda ()
                   (unless ,(cond ((null keep-pred) nil)
                                  ((eq t keep-pred)
                                   `(eq this-command
                                        (lookup-key ',map
                                                    (this-command-keys-vector))))
                                  (t `(funcall ',keep-pred)))
                     (set ',overlaysym nil) ;Just in case.
                     (remove-hook 'pre-command-hook ',clearfunsym)
                     (setq emulation-mode-map-alists
                           (delq ',alist emulation-mode-map-alists))))))
          (set overlaysym overlaysym)
          (fset clearfunsym clearfun)
          (add-hook 'pre-command-hook clearfunsym)
          (push alist emulation-mode-map-alists)))))

(defvar repeatable-pending nil)

;;;###autoload
(defun make-command-repeatable (cmd)
  "Make CMD a repeatable command and return its function definition."
  (cond
   ((and (functionp cmd)
         (not (autoloadp (indirect-function cmd))))
    (let ((iform (or (interactive-form cmd)
                     (error "`%s' not a command" cmd)))
          (doc (or (cdr (help-split-fundoc (documentation cmd 'raw) nil))
                   (documentation cmd 'raw))))
      (let ((rcmd `(lambda (&rest args)
                     ,(help-add-fundoc-usage
                       (concat doc (and doc "\n\n")
                               "This is a repeatable command.")
                       (help-function-arglist cmd))
                     ,iform
                     (let ((fobj ,(indirect-function cmd)))
                       (prog1 (apply fobj args)
                         (when (called-interactively-p 'any)
                           (set-temporary-overlay-map
                            (let ((map (make-sparse-keymap)))
                              (define-key map (vector last-input-event) fobj)
                              map)
                            t)))))))
        (if (symbolp cmd)
            ;; Don't use defalias which also change CMD's load file.
            (fset cmd rcmd)
          rcmd))))
   (t (ignore (push cmd repeatable-pending)))))

;;;###autoload
(defmacro define-repeat-command (name args &rest body)
  "Define NAME as a repeatable command.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug defun))
  `(defalias ',name ,(make-command-repeatable `(lambda ,args ,@body))))

(defun repeatable-after-load-hook (_file)
  "A function for `after-load-functions'."
  (mapc (lambda (cmd)
          (with-demoted-errors
            (make-command-repeatable cmd)))
        (copy-sequence (prog1 repeatable-pending
                         (setq repeatable-pending nil)))))

(add-hook 'after-load-functions #'repeatable-after-load-hook)

(provide 'repeatable)
;;; repeatable.el ends here
