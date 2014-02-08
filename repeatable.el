;;; repeatable.el --- make repeatable commands       -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: extensions, convenience
;; Created: 2013-10-02
;; URL: https://github.com/leoliu/repeatable.el

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

;; A repeatable command is one that can be invoked again by the last
;; character of its key binding.
;;
;; Use `make-command-repeatable' to make existing commands repeatable,
;; for example: (make-command-repeatable 'enlarge-window), after which
;; one can use just ^ immediately following C-x ^ to enlarge window.
;;
;; Use `define-repeat-command' to define new ones.

;;; Code:

(require 'help-fns)

(eval-and-compile
  (or (fboundp 'autoloadp)              ; new in 24.3
      (defun autoloadp (object)
        (eq 'autoload (car-safe object)))))

(eval-and-compile
  (cond
   ((fboundp 'set-transient-map) nil)
   ((fboundp 'set-temporary-overlay-map) ; new in 24.3
    (defalias 'set-transient-map 'set-temporary-overlay-map))
   (t
    (defun set-transient-map (map &optional keep-pred)
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
        (push alist emulation-mode-map-alists))))))

(defvar repeatable-pending nil)

(defun repeatable-p (symbol)
  (and (symbolp symbol) (get symbol 'repeatable)))

;;;###autoload
(defun make-command-repeatable (cmd)
  "Make CMD a repeatable command and return its function definition."
  (cond
   ((and (functionp cmd)
         (not (autoloadp (indirect-function cmd)))
         (not (repeatable-p cmd)))
    (let ((iform (or (interactive-form cmd)
                     (error "`%s' not a command" cmd)))
          (doc (or (cdr (help-split-fundoc (documentation cmd 'raw) nil))
                   (documentation cmd 'raw))))
      (let ((rcmd `(lambda (&rest args)
                     ,(help-add-fundoc-usage
                       (concat
                        (when doc
                          (concat (if (string-match "[ \t\n]+\\'" doc)
                                      (substring doc 0 (match-beginning 0))
                                    doc)
                                  "\n\n"))
                        "This command is repeatable.")
                       (help-function-arglist cmd))
                     ,iform
                     (let ((f ,(indirect-function cmd)))
                       (prog1 (apply f args)
                         (when (called-interactively-p 'any)
                           (set-transient-map
                            (let ((map (make-sparse-keymap)))
                              (define-key map (vector last-command-event)
                                `(lambda ()
                                   (interactive)
                                   (let ((current-prefix-arg ',current-prefix-arg))
                                     (call-interactively #',f))))
                              map)
                            t)))))))
        (when (symbolp cmd)
          ;; Don't use defalias which also change CMD's load file.
          (fset cmd rcmd)
          (put cmd 'repeatable t)
          ;; (info "(elisp)Documentation Basics")
          (when (get cmd 'function-documentation)
            (put cmd 'function-documentation nil)))
        rcmd)))
   ((repeatable-p cmd) (symbol-function cmd))
   (t (ignore (push cmd repeatable-pending)))))

;;;###autoload
(defmacro define-repeat-command (name args &rest body)
  "Define NAME as a repeatable command.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug defun))
  `(defalias ',name ,(make-command-repeatable `(lambda ,args ,@body))))

(defun repeatable-pre-command-hook ()
  ;; This hook removes itself from `pre-command-hook' after running.
  (mapc (lambda (cmd)
          (with-demoted-errors
            (make-command-repeatable cmd)))
        (copy-sequence (prog1 repeatable-pending
                         (setq repeatable-pending nil))))
  (remove-hook 'pre-command-hook 'repeatable-pre-command-hook))

(defun repeatable-after-load-hook (_file)
  "A function for `after-load-functions'."
  (add-hook 'pre-command-hook #'repeatable-pre-command-hook))

(add-hook 'after-load-functions #'repeatable-after-load-hook)

(provide 'repeatable)
;;; repeatable.el ends here
