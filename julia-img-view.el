;;; julia-img-view.el --- Display julia plots and images -*- lexical-binding:t -*-

;; Copyright (c) 2019 Dennis Ogbe
;; Author: Dennis Ogbe <do@ogbe.net>
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((emacs "25") julia-repl)

;;; Usage:
;; The set-up function hooks into all places where it should and together with
;; the julia code given above, inline images should just work.
;;
;; For images in dedicated plot buffers, the package defines
;; julia-img-minor-mode, which gives a key map for controlling the plot
;; windows. The idea behind this package is that at any point in time, zero or
;; more image-mode (in this package aliased to julia-img-view-mode) buffers are
;; considered /active/. In the case that the REPL detects an image and a
;; julia-img-view-mode buffer is active, it displays the image in that
;; buffer. There can be multiple julia-img-view-mode-buffers if you want to
;; open multiple images, but the only buffer updated by the REPL is the
;; currently active one.
;;
;; For more detailed information, please consult the accompanying README.org
;; file.

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:
;; This package contains a small set of tools meant to enhance the image
;; viewing and plotting experience when using `julia-repl' in Emacs.  It
;; contains two main tools:
;; 1. Inline display of images/plots
;; 2. Display of images/plots in dedicated plot buffers
;; See the provided README.org for additional discussion.

;;; Code:

(require 'iimage)
(require 'button)
(require 'julia-repl)

;; customizations, constants, and variables

(defgroup julia-img-view nil
  "Display julia plots and images"
  :group 'julia)

(defcustom julia-img-view-buffer-prefix "julia-img"
  "The default prefix for all image buffers.")

(defvar julia-img-view--active-buffer nil
  "Holds the current active image buffer.")

(defconst julia-img-view--img-uri-regexp "juliaimg:\\([-+./_0-9a-zA-Z]+\\)"
  "The regular expression that matches julia's image URI output.")

;; interface

(defun julia-img-view-create-buffer (arg)
  "Make a new image buffer and mark it as active.

When called with prefix argument ARG, prompt for a custom name
  prefix."
  (interactive "P")
  (let* ((uniquify-buffer-name-style nil)
         (prefix (when arg (read-from-minibuffer "Image buffer name prefix: ")))
         (buf (generate-new-buffer (or prefix julia-img-view-buffer-prefix))))
    (with-current-buffer buf (read-only-mode))
    (setq julia-img-view--active-buffer buf)
    (display-buffer buf)
    (message "Created new julia-img-view buffer %s." buf)))

(defun julia-img-view-active-buffer ()
  "Get the active image buffer.

When called interactively, pop to it instead."
  (interactive)
  (when (buffer-live-p julia-img-view--active-buffer)
    (if (called-interactively-p)
        (pop-to-buffer julia-img-view--active-buffer)
      julia-img-view--active-buffer)))

(defun julia-img-view-disable-buffers ()
  "Disable any active image buffers and fall back to inline plotting."
  (interactive)
  (setq julia-img-view--active-buffer nil)
  (message "Disabled julia-img-view buffers."))

(defun julia-img-view-mark-buffer-active ()
  "Mark the selected buffer as active."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (eq major-mode 'julia-img-view-mode))
        (message "Buffer %s is not a julia-img-view buffer." buf)
      (setq julia-img-view--active-buffer buf)
      (message "Set %s as active julia-img-view buffer." buf))))

;; plumbing

(defun julia-img-view--show-buffer (imgdata repl-buf beg end)
  "Display the image in IMGDATA in the active plot buffer.

The additional arguments REPL-BUF, BEG, and END give the
  originating `julia-repl' buffer and the region of the URI,
  respectively."
  (unless (julia-img-view-active-buffer)
    (julia-img-view-create-buffer))
  (save-window-excursion
    (let ((inhibit-read-only t)
          (inhibit-message t))
      ;; display the image in the active plot buffer
      (with-current-buffer (julia-img-view-active-buffer)
        (erase-buffer)
        (insert imgdata)
        (julia-img-view-mode)
        (set-buffer-modified-p nil))
      ;; replace the text in the REPL buffer with a link to the plot buffer
      (with-current-buffer repl-buf
        (delete-region beg end)
        (goto-char beg)
        (insert (format "↪ buffer: %s" (buffer-name (julia-img-view-active-buffer))))
        (make-button beg (point)
                     'action `(lambda (b) (pop-to-buffer ,(julia-img-view-active-buffer)))
                     'follow-link t
                     'help-echo "Go to image buffer")))))

(defun julia-img-view--show-inline (imgdata repl-buf beg end)
  "Insert the image in IMGDATA into the buffer REPL-BUF as text property between BEG and END."
  (with-current-buffer repl-buf
    (let* ((inhibit-read-only t)
           ;; if emacs is compiled with imagemagick, we resize the image to fit
           ;; comfortably into the smallest window that is visiting the
           ;; `julia-repl' buffer.
           (winlist (get-buffer-window-list (julia-repl-inferior-buffer) nil 'active))
           (wd (sort (mapcar (lambda (w) (window-body-width w t)) winlist) #'<))
           (ht (sort (mapcar (lambda (w) (window-body-height w t)) winlist) #'<))
           (img (when (and wd ht (image-type-available-p 'imagemagick))
                  (cons 'image `(:type imagemagick
                                       :data ,imgdata
                                       :max-width ,(round (* 0.75 (seq-min wd)))
                                       :max-height ,(round (* 0.8 (seq-min ht))))))))
      (add-text-properties beg end `(display ,(if img img (create-image imgdata nil t))
                                             modification-hooks (iimage-modification-hook))))))

(defun julia-img-view--process-output (proc str)
  "Addition to julia's process filter that extracts image URIs.

PROC is the sending process, STR is the raw string and is ignored."
  (let ((repl-buf (julia-repl--live-buffer)) (file))
    (when (and repl-buf (eq (get-buffer-process repl-buf) proc))
      (with-current-buffer repl-buf
        ;; search for image uri, load it, and put image where it should go.
        (save-excursion
          (goto-char julia-img-view--last-point)
          (while (re-search-forward julia-img-view--img-uri-regexp nil t)
            (when (and (setq file (match-string 1)) (file-exists-p file))
              (let ((imgdata (with-temp-buffer (insert-file-contents file)
                                               (string-to-unibyte (buffer-string))))
                    (beg (match-beginning 0))
                    (end (match-end 0)))
                (delete-file file) ; done reading image, can delete
                (funcall (if (julia-img-view-active-buffer)
                             #'julia-img-view--show-buffer #'julia-img-view--show-inline)
                         imgdata repl-buf beg end)))))
        ;; save point for the next time.
        (setq julia-img-view--last-point (point))))))

(defun julia-img-view--enable-advice ()
  "Advise the process filter of the `julia-repl' process."
  (defvar-local julia-img-view--last-point 0)
  (add-function :after (process-filter (get-buffer-process (julia-repl--live-buffer)))
                #'julia-img-view--process-output))

;;;###autoload
(defun julia-img-view-setup ()
  "Set up the `julia-img-view' tools."
  (if (not (display-graphic-p))
      (message "No graphical display, not enabling `julia-img-view'")
    (add-hook 'julia-repl-hook #'julia-img-view--enable-advice)
    (add-hook 'julia-repl-hook #'julia-img-view-minor-mode)))

;; mode definitions

;;;###autoload
(define-minor-mode julia-img-view-minor-mode
  "Minor mode for showing images and plots inline or in plot buffers." nil nil
  `((,(kbd "C-x j i c") . julia-img-view-create-buffer)
    (,(kbd "C-x j i a") . julia-img-view-active-buffer)
    (,(kbd "C-x j i d") . julia-img-view-disable-buffers)
    (,(kbd "C-x j i m") . julia-img-view-mark-buffer-active)))

;;;###autoload
(define-derived-mode julia-img-view-mode image-mode "Julia Image"
  "A mode for displaying Julia images. This just aliases `image-mode'"
  (julia-img-view-minor-mode))

(provide 'julia-img-view)
;;; julia-img-view.el ends here
