;;; real-time-translation.el --- real time translation.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/jin-emacs/
;; ```
;; git clone git@github.com:ginqi7/jin-emacs.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/jin-emacs/")
;; (require 'real-time-translation)
;; ```
;;

;;; Code:


(require 'websocket-bridge)

(defvar real-time-translation-overlays (make-hash-table))

(defgroup
  real-time-translation ()
  "Check grammar in buffers by real-time-translation."
  :group 'applications)

(defvar real-time-translation-py-path
  (concat
   (if load-file-name
       (file-name-directory load-file-name)
     (file-name-directory (buffer-file-name)))
   "real-time-translation.py"))


(defun real-time-translation-start ()
  "Start websocket bridge real-time-translation."
  (interactive)
  (websocket-bridge-app-start "real-time-translation" "/opt/homebrew/bin/python3" real-time-translation-py-path))

(defun real-time-translation-remove-overlays ()
  "Remove overlays."
  (maphash
   (lambda (key value) (delete-overlay value))
   real-time-translation-overlays)
  (setq real-time-translation-overlays (make-hash-table)))

(defun real-time-translation-render (translations)
  "Render TRANSLATIONS."

  (loop for
        (key value)
        on translations by #'cddr
        when (not (string-empty-p value))
        do
        (let* ((line-number
                (string-to-number (substring (symbol-name key) 1)))
               (begin
                (save-excursion
                  (goto-line line-number)
                  (line-beginning-position)))
               (end (+ begin 1))
               (ov (make-overlay begin end))
               (old-ov
                (gethash line-number real-time-translation-overlays)))
          (when old-ov (delete-overlay old-ov))
          (puthash line-number ov real-time-translation-overlays)
          (overlay-put
           ov 'before-string
           (propertize
            (format "%s\n" value)
            'face 'dictionary-overlay-translation)))))

;;;###autoload
(define-minor-mode real-time-translation-mode
  "A minor mode that real-time-translation."
  :init-value nil
  :global nil
  (if (not real-time-translation-mode)
      (progn
        (remove-hook 'after-save-hook 'real-time-translation-translate-current-file t)
        (remove-hook 'post-command-hook 'real-time-translation-translate-current-line t)
        (real-time-translation-remove-overlays))
    (progn
      (add-hook 'after-save-hook 'real-time-translation-translate-current-file nil t)
      (add-hook 'post-command-hook 'real-time-translation-translate-current-line nil t))))

(defun real-time-translation-translate-current-file ()
  "Real time translate the current file."
  (interactive)
  ( real-time-translation-translate
    (buffer-file-name)))

(defun real-time-translation-translate-current-line ()
  "Real time translate the current file."
  (interactive)
  (websocket-bridge-call "real-time-translation" "translate-line"
                         (+ (current-line) 1)
                         (thing-at-point 'line t)))

(defun real-time-translation-translate (file)
  "Real time translate the FILE."
  (websocket-bridge-call "real-time-translation" "translate" file))

(defun real-time-translation-restart ()
  "Restart websocket bridge real-time-translation and show process."
  (interactive)
  (websocket-bridge-app-exit "real-time-translation")
  (real-time-translation-start)
  (websocket-bridge-app-open-buffer "real-time-translation"))

(provide 'real-time-translation)
;;; real-time-translation.el ends here
