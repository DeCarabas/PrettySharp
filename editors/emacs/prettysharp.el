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

(defconst prettysharp/error-buffer-name " *PrettySharp Errors*"
  "The title of the buffer we show errors in.")

(defconst prettysharp/patch-buffer-name " *PrettySharp Patch*"
  "The title of the buffer we format the patch in.")

(defun prettysharp/show-errors (outfile)
  "Display the errors in OUTFILE.

Errors are displayed according to the value of prettysharp-show-errors."
  (when prettysharp-show-errors
    (let ((error-buffer (get-buffer-create prettysharp/error-buffer-name)))
      (with-current-buffer error-buffer
        (setq buffer-read-only nil)
        (insert-file-contents outfile nil nil nil 'replace)
        (cond
         ((eq prettysharp-show-errors 'buffer)
          (goto-char (point-min))
          (insert "prettysharp errors:\n")
          (compilation-mode)
          (display-buffer error-buffer))

         ((eq prettysharp-show-errors 'echo)
          (message "%s" (buffer-string)))
         )))))

(defun prettysharp/clear-errors ()
  "Clear any errors from PrettySharp."
  (let ((error-buffer (get-buffer prettysharp/error-buffer-name)))
    (when error-buffer
      (message "Clear errors?")
      (with-current-buffer error-buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      (kill-buffer error-buffer))))

(defun prettysharp/make-patch (outfile patch-buffer)
  "Diff the contents of the current buffer with OUTFILE, generate an RCS-style patch, and put the results into PATCH-BUFFER."
  (call-process-region (point-min) (point-max) "diff" nil
                       patch-buffer nil "-n" "--strip-trailing-cr"
                       "-" outfile))

(defun prettysharp/apply-diff (patch-buffer)
  "Apply the contents of PATCH-BUFFER as an RCS diff against the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
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
              (forward-line line-count)
              (let ((to-insert (buffer-substring text-start (point))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (+ insert-at line-offset))
                    ;; It can happen that forward-line moves us to the end of
                    ;; the buffer but not a blank line; in this case we need
                    ;; to insert a newline.
                    (if (and (eobp) (looking-at "$") (not (looking-at "^")))
                        (insert "\n"))
                    (insert to-insert))
                  (setq line-offset (+ line-offset line-count))))))

           ((looking-at "^d\\([0-9]+\\) \\([0-9]+\\)$")
            ;; delete lines.
            (forward-line)
            (let ((delete-at (string-to-number (match-string 1)))
                  (line-count (string-to-number (match-string 2))))
              (with-current-buffer target-buffer
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- (+ delete-at line-offset)))
                  (let ((delete-start (point)))
                    (forward-line line-count)
                    (delete-region delete-start (point))))
                (setq line-offset (- line-offset line-count)))))

           (t
            (error "Unrecognized RCS command in prettysharp/apply-diff"))
           ))))))

(defun prettysharp ()
  "Format the current buffer according to prettysharp."
  (interactive)
  (let ((outfile (make-temp-file "prettysharp" nil "cs"))
        (patch-buffer (get-buffer-create prettysharp/patch-buffer-name))
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
                (prettysharp/apply-diff patch-buffer)
                (prettysharp/clear-errors))
            (prettysharp/show-errors outfile)))

      (delete-file outfile))))

;;;###autoload
(define-minor-mode prettysharp-mode
  "Minor mode to run prettysharp on file save."
  :lighter " pretty#"
  (if prettysharp-mode
      (add-hook 'before-save-hook 'prettysharp nil 'local)
    (remove-hook 'before-save-hook 'prettysharp 'local)))

(provide 'prettysharp)
;;; prettysharp.el ends here
