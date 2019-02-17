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
(defcustom prettysharp-command "prettysharp"
  "The 'prettysharp' command."
  :type 'string
  :group 'prettysharp)

(defcustom prettysharp-args nil
  "Additional arguments to pass to prettysharp."
  :type '(repeat string)
  :group 'prettysharp)

(defcustom prettysharp-show-errors 'buffer
  "Where to display prettysharp error output.

It can either be displayed in its own buffer, in the echo area,
or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite prettysharp's echo output if used from
inside a `before-save-hook'."
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
                       patch-buffer nil "-e" "--strip-trailing-cr"
                       "-" outfile))

(defun prettysharp/apply-patch (patch-buffer)
  "Apply the contents of PATCH-BUFFER as an ed-script against the current buffer."
  (let ((target-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "^\\([0-9]+\\)a")
            ;; add command: Insert the following text
            (forward-line)
            (let ((start-point (point))
                  (target-line (string-to-number (match-string 1))))
              (while (not (or (looking-at "^.$") (eobp)))
                (forward-line))
              (let ((to-insert (buffer-substring start-point (point))))
                (with-current-buffer target-buffer
                  (goto-char (point-min))
                  (forward-line target-line)
                  (insert to-insert)))
              (forward-line))

           ((looking-at "^\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?c")
            ;; change command
            (forward-line)
            (letrec ((start-point (point))
                     (target-start (string-to-number (match-string 1)))
                     (target-end (if (match-string 2)
                                     (string-to-number (match-string 2))
                                   target-start)))

              (while (not (or (looking-at "^.$") (eobp)))
                (forward-line))
              (let ((to-insert (buffer-substring start-point (point))))
                (with-current-buffer target-buffer
                  (goto-char (point-min))
                  (forward-line target-line)
                  (let ((del-start (point)))
                    (forward-line (1+ (- target-end target-start)))
                    (delete-region del-start (point)))
                  (insert to-insert)))
              (forward-line))

           ((looking-at "^\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?d")
            ;; delete command
            (forward-line)
            (letrec ((target-start (string-to-number (match-string 1)))
                     (target-end (if (match-string 2)
                                     (string-to-number (match-string 2))
                                   target-start)))
              (with-current-buffer target-buffer
                  (goto-char (point-min))
                  (forward-line target-line)
                  (let ((del-start (point)))
                    (forward-line (1+ (- target-end target-start)))
                    (delete-region del-start (point))))))

           (t
            (error "Internal error in prettysharp/apply-patch: Unrecognized ed command"))

          )))))))


(defun prettysharp ()
  "Format the current buffer according to prettysharp."
  (interactive)
  (let ((outfile (make-temp-file "prettysharp" nil "cs"))
        (patch-buffer (get-buffer-create "*prettysharp patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-prettysharp-args)

    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patch-buffer
            (erase-buffer))

          (if (zerop (call-process-region (point-min) (point-max) prettysharp-command
                                          nil (list :file outfile) nil))
              (progn
                (prettysharp/make-patch outfile patch-buffer)
                (prettysharp/apply-patch patch-buffer))
            (prettysharp/show-errors outfile)))

      (delete-file outfile))))

(provide 'prettysharp)
;;; prettysharp.el ends here
