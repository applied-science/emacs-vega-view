;;; vega-view.el --- Vega visualization viewer      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jack Rusher

;; Author: Jack RUsher <jack@appliedscience.studio>
;; Keywords: vega visualization clojure json
;; Package-Version: 20200331.1
;; Package-Requires: ((emacs "25") (parseedn "20191113.831"))
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

(unless (package-installed-p 'parseedn)
  (package-install 'parseedn))

(require 'cl-lib)
(require 'parseedn)

(setq vega-view--vega-svg-command "vl2svg -h -b ")
(setq vega-view--vega-png-command "vl2png")

(defvar vega-view-base-directory nil
  "If this value is set non-nil, vega-view will use it as the
  base directory for resources loaded by the Vega command line
  tools. Otherwise, the default directory of the buffer in which
  view-vega is invoked will be used. This is useful if your Vega
  specification makes reference by relative path to external
  resources, like external JSON datasets or geojson map data.")

(defun vega-view--json (json-string vega-buffer)
  "Passes `json-string` through the Vega command line tools, displaying the
resulting SVG in `vega-buffer` using image-mode ."
  (cl-assert (or (image-type-available-p 'svg) (image-type-available-p 'png))
             nil
             "vega-view requires an emacs that supports either SVG or PNG in image-mode!")
  (with-current-buffer vega-buffer
    (fundamental-mode)
    (setq buffer-read-only nil) ; cider likes to set results buffers read-only
    (erase-buffer)
    (insert json-string)
    ;; Only switch the output buffer to image-mode if the command was
    ;; successful so any error text will be visible in the buffer.
    (let ((coding-system-for-read 'raw-text) ; in case it's a PNG
          (vega-view-command (if (image-type-available-p 'svg)
                                 (string-join `(,vega-view--vega-svg-command
                                                ,(or vega-view-base-directory default-directory)))
                               vega-view--vega-png-command)))
      (when (= 0 (shell-command-on-region (buffer-end -1)
                                          (buffer-end 1)
                                          vega-view-command
                                          vega-buffer))
        (image-mode)))
    (display-buffer vega-buffer)))

(defun vega-view--elisp (elisp-form-string vega-buffer)
  "Parses `elisp-form-string`, evaluates it, then converts the
resulting form to JSON and passes it on to vega-view--json to
display in `vega-buffer`."
  (vega-view--json (json-encode (eval (read elisp-form-string))) vega-buffer))

(defun vega-view--clojure (clojure-form-string vega-buffer)
  "Evaluate `clojure-form-string` in the cider context of the
buffer from which it is called, convert the result to JSON, then
pass it to vega-view--json to display in `vega-buffer`."
  (cl-assert (member 'cider-mode minor-mode-list)
          nil
          "view-view requires an active cider connection for use with clojure forms!")
  (with-current-buffer vega-buffer
    (setq cider-popup-output-marker (point-marker)))
  (cider-interactive-eval
   ;; in case local printer settings would truncate the output
   (format "(binding [*print-level* nil *print-length* nil] %s)" clojure-form-string)
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
  "Converts the preceding sexp (in supported languages) to JSON
and passes it through the Vega command line tools, displaying the
resulting SVG in the `*vega*` buffer."
  (interactive)
  (let* ((supported-modes '((clojure-mode vega-view--clojure)
                            (emacs-lisp-mode vega-view--elisp)
                            (lisp-interaction-mode vega-view--elisp)
                            (json-mode vega-view--json)
                            (js-mode vega-view--json)
                            (js2-mode vega-view--json)))
         (mode-fn (or (assoc major-mode supported-modes)
                      (error (format "vega-view currently supports buffers with these major modes: %s"
                                     (mapcar #'car supported-modes))))))
    (let ((sexp-string (if (eq major-mode 'clojure-mode)
                           (cider-defun-at-point)
                         (thing-at-point 'defun 'no-props))))
      (cl-assert (and (stringp sexp-string) (> (length sexp-string) 0))
                 nil
                 "vega-view was unable to parse the surrounding sexp!")
      (funcall (symbol-function (cadr mode-fn))
               sexp-string
               (get-buffer-create "*vega*")))))

(provide 'vega-view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A test visualization

;; (require 'seq)

;; `(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
;;  (description . "A simple bar chart with embedded data.")
;;  (data
;;   (values . ,(seq-map-indexed (lambda (x i) `((a . ,i) (b . ,(sin x))))
;;                            '(0 1 2 3 4 5 6 7 8 9))))
;;  (mark . "line")
;;  (width . 800)
;;  (height . 600)
;;  (encoding
;;   (x (field . "a")
;;      (type . "ordinal") (axis (labelAngle . 0)))
;;   (y (field . "b")
;;      (type . "quantitative"))))

;; If you have some vega JSON around, try visualizing:
;;(json-read-file ...your file...)
