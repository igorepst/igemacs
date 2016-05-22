(defun describe-symbols (pattern)
  "Describe the Emacs Lisp symbols matching PATTERN."
  (interactive "sDescribe symbols matching: ")
  (with-temp-buffer
  (let (
	 (standard-output (current-buffer))
	(describe-func
         (function
          (lambda (s)

            ;; Print description of symbol.
            (if (fboundp s)             ; It is a function.
                (princ
                 (format "%s\t%s\n%s\n\n" s
                   (if (commandp s)
                       (let ((keys (where-is-internal s)))
                         (if keys
                             (concat
                              "Keys: "
                              (mapconcat 'key-description
                                         keys " "))
                           "Keys: none"))
                     "Function")

                   (or (documentation s)
                       "not documented"))))

            (if (boundp s)              ; It is a variable.

                (princ
                 (format "%s\t%s\n%s\n\n" s
                   (if (custom-variable-p s)
                       "Option " "Variable")

                   (or (documentation-property
                         s 'variable-documentation)
                       "not documented")))))))
        sym-list)

    ;; Build a list of symbols that match pattern.
    (mapatoms (function
               (lambda (sym)
                 (if (string-match pattern (symbol-name sym))
                     (setq sym-list (cons sym sym-list))))))

    ;; Display the data.
      (mapcar describe-func (sort sym-list 'string<))
      (buffer-string))))

(provide 'desc-sym)
