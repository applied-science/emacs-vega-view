;;; vega-view.el --- Vega visualization viewer      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jack Rusher

;; Author: Jack Rusher <jack@appliedscience.studio>
;; URL: http://www.github.com/applied-science/emacs-vega-view
;; Created: 20200330
;; Keywords: multimedia
;; Package-Requires: ((emacs "25") (parseedn "0.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; vega-view is an Emacs Lisp library for visualizing Vega
;; spcifications.

(require 'cl-lib)
(require 'parseedn)

;;; Code:

(defvar vega-view--vega-svg-command "vl2svg"
  "The local command to be invoked to convert a Vega JSON spec to SVG.")

(defvar vega-view--vega-png-command "vl2png"
  "The local command to be invoked to convert a Vega-lite JSON spec to SVG.")

(defvar vega-view-base-directory nil
  "If this value is set non-nil, vega view will use it as the base directory for resources loaded by the Vega command line tools.  Otherwise, the default directory of the buffer in which view-vega is invoked will be used.  This is useful if your Vega specification makes reference by relative path to external resources, like external JSON datasets or geojson map data.")

(defvar vega-view-prefer-png nil
  "If this value is set non-nil, vega view will always try to render to PNG.  Otherwise, it will use SVG whenever possible.  This is useful in situations where one's Emacs can't properly display SVG, but thinks it can, or for performance when one is sure that the drawings will be smaller as PNG than SVG.")

(defun vega-view--json (json-string vega-buffer)
  "Passes `JSON-STRING` through the Vega command line tools, displaying the resulting SVG in `VEGA-BUFFER` using `image-mode' ."
  (cl-assert (or (image-type-available-p 'svg) (image-type-available-p 'png))
             nil
             "vega-view requires an emacs that supports either SVG or PNG!")
  (with-current-buffer vega-buffer
    (fundamental-mode)
    (setq buffer-read-only nil) ; cider likes to set results buffers read-only
    (erase-buffer)
    (insert json-string)
    ;; Only switch the output buffer to image-mode if the command was
    ;; successful so any error text will be visible in the buffer.
    (let ((coding-system-for-read 'raw-text) ; in case it's a PNG
          (vega-view-command (if (and (image-type-available-p 'svg)
                                      (not vega-view-prefer-png))
                                 `(,vega-view--vega-svg-command
                                   ,(string-join `("-h -b "
                                                   ,(or vega-view-base-directory default-directory))))
                               `(,vega-view--vega-png-command ""))))
      (when (= 0 (call-process-region (point-min)
                                      (point-max)
                                      (car vega-view-command)
                                      t t nil
                                      (cadr vega-view-command)))
        (image-mode)))
    (display-buffer vega-buffer)))

;; debug version that just shows the output in the *vega* buffer
;; (defun vega-view--json (json-string vega-buffer)
;;   "Passes `json-string` through the Vega command line tools, displaying the
;; resulting SVG in `vega-buffer` using image-mode ."
;;   (with-current-buffer vega-buffer
;;     (fundamental-mode)
;;     (setq buffer-read-only nil) ; cider likes to set results buffers read-only
;;     (erase-buffer)
;;     (insert json-string)))

(defun vega-view--elisp (elisp-form-string vega-buffer)
  "Parse `ELISP-FORM-STRING`, evaluate it, then convert the resulting form to JSON and pass it on to `vega-view--json' to display in `VEGA-BUFFER`."
  (vega-view--json (json-encode (eval (read elisp-form-string))) vega-buffer))

(defun vega-view--clojure (clojure-form-string vega-buffer)
  "Evaluate `CLOJURE-FORM-STRING` in the cider context of the buffer from which it is called, convert the result to JSON, then pass it to `vega-view--json' to display in `VEGA-BUFFER`."
  (cl-assert (member 'cider-mode minor-mode-list)
             nil
             "view-view requires an active cider connection for use with clojure forms!")
  (with-current-buffer vega-buffer
    (setq cider-popup-output-marker (point-marker)))
  (cider-interactive-eval
   ;; in case local printer settings would truncate the output
   (format "(do (set! *print-length* nil) %s)" clojure-form-string)
   (nrepl-make-response-handler vega-buffer
                                (lambda (buffer value)
                                  (vega-view--json (json-encode (parseedn-read-str value)) buffer))
                                (lambda (_buffer out)
                                  (cider-emit-interactive-eval-output out))
                                (lambda (_buffer err)
                                  (cider-emit-interactive-eval-err-output err))
                                nil
                                nil
                                nil
                                (lambda (buffer warning)
                                  (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t)))))

;; TODO should come in "view-preceding-sexp" and "view-defun" versions
(defun vega-view ()
  "Convert the preceding sexp (in supported languages) to JSON and pass it through the Vega command line tools, then display the resulting SVG in the `*vega*` buffer."
  (interactive)
  (let* ((supported-modes '((cider-repl-mode vega-view--clojure)
                            (clojure-mode vega-view--clojure)
                            (emacs-lisp-mode vega-view--elisp)
                            (lisp-interaction-mode vega-view--elisp)
                            (json-mode vega-view--json)
                            (js-mode vega-view--json)
                            (js2-mode vega-view--json)))
         (mode-fn (or (assoc major-mode supported-modes)
                      (error (format "vega-view currently supports buffers with these major modes: %s"
                                     (mapcar #'car supported-modes))))))
    (let ((sexp-string (if (or (eq major-mode 'clojure-mode)
                               (eq major-mode 'cider-repl-mode))
                           (cider-sexp-at-point)
                         (thing-at-point 'sexp 'no-props))))
      (cl-assert (and (stringp sexp-string) (> (length sexp-string) 0))
                 nil
                 "vega-view was unable to parse the preceding sexp!")
      (funcall (symbol-function (cadr mode-fn))
               sexp-string
               (get-buffer-create "*vega*")))))

(provide 'vega-view)

;;; vega-view.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An example visualization

;; (require 'seq)

;; `(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
;;   (description . "A simple bar chart with embedded data.")
;;   (data
;;    (values . ,(seq-map-indexed (lambda (x i) `((a . ,i) (b . ,(sin x))))
;;                                '(0 1 2 3 4 5 6 7 8 9))))
;;   (mark . "line")
;;   (width . 800)
;;   (height . 600)
;;   (encoding
;;    (x (field . "a")
;;       (type . "ordinal") (axis (labelAngle . 0)))
;;    (y (field . "b")
;;       (type . "quantitative"))))

;; If you have some vega JSON around, try visualizing:
;;(json-read-file ...your file...)

