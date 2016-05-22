;; timestamps in *Messages*
;; http://www.reddit.com/r/emacs/comments/1auqgm/speeding_up_your_emacs_startup
;; TODO: breaks exiting second instance with 'buffer is read-only *Messages*'
;; TODO: eldoc results in empty timestamped msgs (see adarg0, not working)
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (let ((adarg0 (ad-get-arg 0)))
  (if (and (not (or (null adarg0) (string= "" adarg0))) (not (string-equal adarg0 "%s%s")))
      (let ((inhibit-read-only t)
            (deactivate-mark nil))
        (with-current-buffer (messages-buffer)
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (current-time-microseconds)))))))

(provide 'msg-timestamps)
