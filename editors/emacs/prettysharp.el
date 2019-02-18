;;; prettysharp.el --- Support for formatting C# with prettysharp

;;; Commentary:

;; Copyright 2019 John Doty.  All rights reserved.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: John Doty
;; Version: 0.7.0
;; Keywords: convenience wp edit csharp
;; URL: https://github.com/decarabas/prettysharp
;;
;; This file is not part of GNU Emacs.

;;; Code:
(defgroup prettysharp nil
  "Minor mode to format C# code on save."
  :group 'languages
  :prefix "prettysharp"
  :link '(url-link "https://github.com/DeCarabas/PrettySharp"))

(defcustom prettysharp-command "prettysharp"
  "The 'prettysharp' command."
  :type 'string
  :group 'prettysharp)

(defcustom prettysharp-show-errors 'buffer
  "Where to display prettysharp error output.

It can either be displayed in its own buffer, in the echo area,
or not at all."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'prettysharp)

(defun prettysharp/show-errors (outfile)
  "Display the errors in OUTFILE.

Errors are displayed according to the value of prettysharp-show-errors."
  )

(defun prettysharp/make-patch (outfile patch-buffer)
  "Diff the contents of the current buffer with OUTFILE, generate an RCS-style patch, and put the results into PATCH-BUFFER."
  (call-process-region (point-min) (point-max) "diff" nil
                       patch-buffer nil "-n" "--strip-trailing-cr"
                       "-" outfile))

(defun prettysharp/apply-diff (patch-buffer)
  "Apply the contents of PATCH-BUFFER as an RCS diff against the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Tracks the "current" line number in the target buffer. This is the
        ;; logical line number in the file before any changes were
        ;; made. Keeping this number accurate is subtle. Just remember:
        ;; (point) is always at tne end of the text we just manipulated, and
        ;; of course we're always at the line the script just told us to go
        ;; to. Keep that in mind and you'll be OK.
        (current-line 0))
    (save-excursion
      (goto-char (point-min))
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (cond

           ((looking-at "^$")
            ;; blank line is a valid command, meaning do nothing.
            (forward-line))

           ((looking-at "^a\\([0-9]+\\) \\([0-9]+\\)$")
            ;; add lines.
            (forward-line)
            (let ((text-start (point))
                  (insert-at (string-to-number (match-string 1)))
                  (line-count (string-to-number (match-string 2))))
              (forward-line (1+ line-count))
              (let ((to-insert (buffer-substring text-start (point))))
                (with-current-buffer target-buffer
                  (forward-line (- insert-at current-line))
                  ;; It can happen that forward-line moves us to the end of
                  ;; the buffer but not a blank line; in this case we need to
                  ;; insert a newline.
                  (if (and (eobp) (looking-at "$") (not (looking-at "^")))
                      (insert "\n"))
                  (insert to-insert))
                (setq current-line insert-at))))

           ((looking-at "^d\\([0-9]+\\) \\([0-9]+\\)$")
            ;; delete lines.
            (forward-line)
            (let ((delete-at (string-to-number (match-string 1)))
                  (line-count (string-to-number (match-string 2))))
              (with-current-buffer target-buffer
                (forward-line (1- (- delete-at current-line)))
                (let ((delete-start (point)))
                  (forward-line line-count)
                  (delete-region delete-start (point))))
              (setq current-line delete-at)))

           (t
            (error "Unrecognized RCS command in prettysharp/apply-diff"))
           ))))))

(defun prettysharp ()
  "Format the current buffer according to prettysharp."
  (interactive)
  (let ((outfile (make-temp-file "prettysharp" nil "cs"))
        (patch-buffer (get-buffer-create "*prettysharp patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patch-buffer
            (erase-buffer))

          (if (zerop (call-process-region (point-min) (point-max) prettysharp-command
                                          nil (list :file outfile) nil))
              (progn
                (prettysharp/make-patch outfile patch-buffer)
                (prettysharp/apply-diff patch-buffer))
            (prettysharp/show-errors outfile)))

      (delete-file outfile))))

(provide 'prettysharp)
;;; prettysharp.el ends here
