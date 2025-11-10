(require 'jsonrpc)

(defvar process nil "The `teamtype client` process")
(defvar connection nil "The jsonrpc connection to that process")

(defun line-col-to-pos (line col)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (+ (line-beginning-position) col)))

(defun replace-region (line1 col1 line2 col2 text)
  (let ((beg (line-col-to-pos line1 col1))
        (end (line-col-to-pos line2 col2)))
    (let ((inhibit-modification-hooks t))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(defun find-root (file)
  (locate-dominating-file file ".teamtype"))

(defun file-opened ()
  (message "File %s" buffer-file-name)
  (message "Root: %s" (find-root buffer-file-name))

  ; Initialize the connection if there is none yet.
  (when (not connection)
    (setq process (make-process
                    :name "teamtype"
                    :buffer nil
                    :command '("teamtype" "client")
                    :connection-type 'pipe))
    (set-process-query-on-exit-flag process nil)
    (setq connection (make-instance 'jsonrpc-process-connection
                                    :process process
                                    ; TODO: Use the find-root function here.
                                    :request-dispatcher (lambda (_conn method params)
                                                          (message "request %s %S" method params))
                                    :notification-dispatcher (lambda (_conn method params)
                                                               (message "notification %s %S" method params))
                                    :on-shutdown (lambda (_conn) (message "shutdown")))))

  (jsonrpc-async-request connection "open" `(:uri ,(browse-url-file-url buffer-file-name) :content "blabb")
                         :success-fn (lambda ()
                                       ; TODO: We don't seem to get here yet?
                                       (track-changes (lambda (from to replacement)
                                                        (message "Replaced from (%s) to (%s) with %s" from to replacement)))
                                       )))

(add-hook 'find-file-hook #'file-opened)

;;;; CHANGETRACKER

(defvar-local my-last-change-old-text nil)

(defvar-local change-callback nil
              "Callback to run on buffer changes in the current buffer.")

(defun pos-to-line-col (pos)
  (save-excursion
    (goto-char pos)
    (cons (- (line-number-at-pos) 1) (- pos (line-beginning-position)))))

; Note: beg and end are 1-indexed "character positions"! Not sure yet what exactly that means.
(defun my-before-change (beg end)
  (setq my-last-change-old-text (buffer-substring-no-properties beg end)))

(defun my-after-change (beg end len)
  (message "after")
  (funcall change-callback
           (pos-to-line-col beg)
           (pos-to-line-col end)
           (buffer-substring-no-properties beg end))
  )

(defun track-changes (callback)
  (message "tracking")
  (setq-local change-callback callback)
  (add-hook 'before-change-functions #'my-before-change nil t)
  (add-hook 'after-change-functions #'my-after-change nil t))

;;;;

(provide 'teamtype)
