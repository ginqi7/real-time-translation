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
(require 'cl-macs)

(defvar real-time-translation-overlays (make-hash-table))

(defvar real-time-translation-buffer-ids (make-hash-table))

(defvar real-time-translation-high-quality-timer nil)

(defgroup
  real-time-translation ()
  "Real-time translation."
  :group 'applications)

(defcustom real-time-translation-python-command (executable-find "python3")
  "Python Command Path for Real-time Translation"
  :type 'string
  :group 'real-time-translation)

(defcustom real-time-translation-target-languages '("en" "zh")
  "The target languages of Real-time translation"
  :type 'cons
  :group 'real-time-translation)

(defcustom real-time-translation-refine-p nil
  "Do you allow `real-time-translation` to refine your sentences in the original language?"
  :type 'boolean
  :group 'real-time-translation)

(defcustom real-time-translation-high-speed-engine "argos"
  "The translation engine."
  :group 'real-time-translation
  :type '(choice
          (const :tag "argos" "argos")
          (const :tag "mtranserver" "mtranserver")))

(defcustom real-time-translation-high-quality-engine "deepl"
  "The translation engine."
  :group 'real-time-translation
  :type '(choice
          (const :tag "deepl" "deepl")))

(defcustom real-time-translation-deepl-key ""
  "The translation engine."
  :group 'real-time-translation
  :type 'string)

(defcustom real-time-translation-mtranserver-url "http://localhost:8989/translate"
  "The url of mtranserver engine."
  :group 'real-time-translation
  :type 'string)

(defcustom real-time-translation-oneline-mode nil
  "Dose it just show a single line translation in the buffer?"
  :group 'real-time-translation
  :type 'boolean)

(defcustom real-time-translation-cache-directory
  (file-name-concat user-emacs-directory ".real-time-translation")
  "Cache directory."
  :group 'real-time-translation
  :type 'string)

(defcustom real-time-translation-high-quality-idle 1
  "high-quality translation idle."
  :group 'real-time-translation
  :type 'number)


(defface real-time-translation-default
  '((t (:foreground "#81a1c1")))
  "The face for show real-time-translation"
  :group 'real-time-translation)

(defface real-time-translation-refine
  '((t (:foreground "#b48ead")))
  "The face for show real-time-translation"
  :group 'real-time-translation)


(defvar real-time-translation-py-path
  (file-name-concat
   (if load-file-name
       (file-name-directory load-file-name)
     (file-name-directory (buffer-file-name)))
   "real-time-translation.py"))


(defun real-time-translation-start ()
  "Start websocket bridge real-time-translation."
  (interactive)
  (websocket-bridge-app-start "real-time-translation"
                              real-time-translation-python-command
                              real-time-translation-py-path))

(defun real-time-translation-remove-overlays ()
  "Remove overlays."
  (maphash
   (lambda (key value) (delete-overlay value))
   real-time-translation-overlays)
  (setq real-time-translation-overlays (make-hash-table)))

(defun real-time-translation-render-list (translations)
  "Render TRANSLATIONS."
  (mapc #'real-time-translation-render translations))

(defun real-time-translation-render (translation)
  "Render TRANSLATION."
  (let* ((buffer-id (plist-get translation :buffer-id))
         (buffer (real-time-translation-get-buffer-by-id buffer-id))
         (line (plist-get translation :line))
         (beginning (plist-get translation :beginning))
         (end (plist-get translation :end))
         (id (intern (md5 (format "%s:%s" buffer-id line))))
         (trans (plist-get translation :trans))
         (refine (plist-get translation :refine))
         (default-background-color (face-attribute 'default :background))
         (old-ov (gethash id real-time-translation-overlays))
         (ov)
         (placeholder)
         (before-string))
    ;; Delete the old ov from the current line.
    (when old-ov (delete-overlay old-ov))
    (when real-time-translation-oneline-mode
      (real-time-translation-remove-overlays))
    (with-current-buffer buffer
      (save-excursion
        (goto-line (1+ line))
        (beginning-of-line 1)
        ;; Create an invisible placeholder to align the text.
        (setq placeholder (buffer-substring (point) beginning))
        ;; Create a new ov.
        (setq ov (make-overlay (point) (1+ (point))))))
    (puthash id ov real-time-translation-overlays)
    (overlay-put ov 'evaporate t)
    ;; Rendering the translation.
    (when trans
      (setq before-string
            (concat
             (propertize placeholder
                         'face `(:foreground ,default-background-color))
             (propertize (decode-coding-string (base64-decode-string trans) 'utf-8)
                         'face 'real-time-translation-default)
             "\n")))
    ;; Rendering the refined sentence.
    (when refine
      (setq before-string
            (concat
             before-string
             (propertize placeholder
                         'face `(:foreground ,default-background-color))
             (propertize (decode-coding-string (base64-decode-string refine) 'utf-8)
                         'face 'real-time-translation-refine)
             "\n")))

    (overlay-put ov 'before-string
                 before-string)))

;;;###autoload
(define-minor-mode real-time-translation-mode
  "A minor mode that real-time-translation."
  :init-value nil
  :global nil
  (if (not real-time-translation-mode)
      (progn
        ;; (remove-hook 'after-save-hook 'real-time-translation-translate-current-file t)
        (real-time-translation-cancel-high-quality-timer)
        (remove-hook 'post-command-hook 'real-time-translation-translate-text t)
        (real-time-translation-remove-overlays))
    (progn
      ;; (add-hook 'after-save-hook 'real-time-translation-translate-current-file nil t)
      (real-time-translation-create-high-quality-timer)
      (add-hook 'post-command-hook 'real-time-translation-translate-text nil t))))


(defun real-time-translation-translate-current-file ()
  "Real time translate the current file."
  (interactive)
  ( real-time-translation-translate
    (buffer-file-name)))

(defun real-time-translation-translate-text (&optional high-quality-p)
  "Real time translate the current file."
  (interactive)
  (let ((buffer-id (real-time-translation-get-buffer-id (current-buffer)))
        (line (current-line))
        (beginning (line-beginning-position 1))
        (end (line-end-position 1))
        (text))
    (if (region-active-p)
        (progn
          (setq beginning (region-beginning))
          (setq end (region-end))))
    (setq text (buffer-substring-no-properties beginning end))
    (websocket-bridge-call "real-time-translation" "translate-text"
                           (list :buffer-id buffer-id
                                 :line line
                                 :beginning beginning
                                 :end end
                                 :text text
                                 :high-quality high-quality-p))))


(defun real-time-translation-translate (file)
  "Real time translate the FILE."
  (websocket-bridge-call "real-time-translation" "translate" file))

(defun real-time-translation-restart ()
  "Restart websocket bridge real-time-translation and show process."
  (interactive)
  (websocket-bridge-app-exit "real-time-translation")
  (real-time-translation-start)
  (websocket-bridge-app-open-buffer "real-time-translation"))

(defun real-time-translation-get-buffer-id (buffer)
  (let ((id (gethash buffer real-time-translation-buffer-ids)))
    (unless id
      (setq id (org-id-uuid))
      (puthash buffer id real-time-translation-buffer-ids))
    id))

(defun real-time-translation-get-buffer-by-id (id)
  (cl-find-if
   (lambda(key) (equal id (gethash key real-time-translation-buffer-ids)))
   (hash-table-keys real-time-translation-buffer-ids)))

(defun real-time-translation-create-high-quality-timer ()
  (interactive)
  (unless real-time-translation-high-quality-timer
    (setq real-time-translation-high-quality-timer
          (run-with-idle-timer real-time-translation-high-quality-idle t #'real-time-translation-translate-text t))))


(defun real-time-translation-cancel-high-quality-timer ()
  (interactive)
  (when real-time-translation-high-quality-timer
    (cancel-timer real-time-translation-high-quality-timer)
    (setq real-time-translation-high-quality-timer nil)))


(provide 'real-time-translation)
;;; real-time-translation.el ends here
