;;; teamtype.el --- Collaborative editing with Teamtype -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2025 Teamtype Contributors
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; This package provides integration with the Teamtype collaborative
;; editing daemon. It implements the daemon-editor protocol to synchronize
;; buffer changes with other editors in real-time.

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)

;;; Client Management

;; Registry of active clients. Each client is a plist with:
;; :root-dir - the root directory containing .teamtype
;; :process - the teamtype client process
;; :connection - the JSON-RPC connection
;; :files - hash table mapping file paths to file state
(defvar teamtype--clients nil
  "List of active teamtype clients.")

;;; File State (buffer-local variables)

(defvar-local teamtype--editor-revision 0
  "Number of operations this editor has made to the current buffer.")

(defvar-local teamtype--daemon-revision 0
  "Number of operations received from the daemon for the current buffer.")

(defvar-local teamtype--client nil
  "The teamtype client associated with this buffer.")

(defvar-local teamtype--ignore-changes nil
  "When non-nil, buffer changes are ignored and not sent to daemon.")

;;; Utility Functions

(defun teamtype--find-root (file)
  "Find the root directory containing .teamtype for FILE."
  (locate-dominating-file file ".teamtype"))

(defun teamtype--line-col-to-pos (line col)
  "Convert zero-based LINE and character COL to buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (+ (line-beginning-position) col)))

(defun teamtype--pos-to-line-col (pos)
  "Convert buffer POS to zero-based line and character position.
Returns a cons (LINE . CHARACTER)."
  (save-excursion
    (goto-char pos)
    (cons (- (line-number-at-pos) 1)
          (- pos (line-beginning-position)))))

(defun teamtype--buffer-content ()
  "Get the entire buffer content as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

;;; Client Management

(defun teamtype--find-or-create-client (root-dir)
  "Find or create a teamtype client for ROOT-DIR."
  ;; Look for existing client for this root directory
  (let ((client (cl-find-if (lambda (c) (string= (plist-get c :root-dir) root-dir))
                            teamtype--clients)))
    (or client
        (teamtype--create-client root-dir))))

(defun teamtype--create-client (root-dir)
  "Create a new teamtype client for ROOT-DIR."
  (message "Creating Teamtype client for %s" root-dir)
  (let* ((default-directory root-dir) ; Set working directory for the process.
         (process-name (format "teamtype-%s" root-dir))
         (process (make-process
                   :name process-name
                   :buffer nil ; Do not append output of this process to any buffer.
                   :command '("teamtype" "client")
                   :connection-type 'pipe
                   :noquery t))
         (connection (make-instance 'jsonrpc-process-connection
                                    :process process
                                    :request-dispatcher #'teamtype--handle-request
                                    :notification-dispatcher #'teamtype--handle-notification
                                    :on-shutdown (lambda (_conn)
                                                   (message "Teamtype connection closed"))))
         (client (list :root-dir root-dir
                       :process process
                       :connection connection
                       :files (make-hash-table :test 'equal))))
    (push client teamtype--clients)
    (message "Connected to Teamtype daemon in %s" root-dir)
    client))

;;; File Operations

(defun teamtype--file-opened ()
  "Handle opening a file in a teamtype-enabled directory."
  (when buffer-file-name
    (let ((root-dir (teamtype--find-root buffer-file-name)))
      (when root-dir
        (let ((client (teamtype--find-or-create-client root-dir)))
          (teamtype--open-file client buffer-file-name))))))

(defun teamtype--open-file (client filepath)
  "Open FILEPATH with CLIENT."
  (setq-local teamtype--client client)
  (setq-local teamtype--editor-revision 0)
  (setq-local teamtype--daemon-revision 0)

  (let* ((uri (concat "file://" filepath))
         (content (teamtype--buffer-content))
         (connection (plist-get client :connection))
         (process (plist-get client :process)))

    (condition-case err
        (progn
          (let ((result (jsonrpc-request connection "open"
                                         (list :uri uri :content content)
                                         :timeout 10)))
            (message "Open request succeeded: %S" result))
          (teamtype--track-changes)
          (message "Teamtype: tracking %s" filepath))
      (jsonrpc-error
       (message "Teamtype error opening file: %s" (error-message-string err))
       (message "Error details: %S" err)))))

(defun teamtype--close-file ()
  "Close the current file in teamtype."
  (when (and teamtype--client buffer-file-name)
    (let* ((uri (concat "file://" buffer-file-name))
           (connection (plist-get teamtype--client :connection)))
      (jsonrpc-notify connection "close" (list :uri uri))
      (setq-local teamtype--client nil))))

;;; Change Tracking

(defvar-local teamtype--last-change-beg nil)
(defvar-local teamtype--last-change-end nil)
(defvar-local teamtype--last-change-old-text nil)

(defun teamtype--before-change (beg end)
  "Track the region and text before a change."
  (unless teamtype--ignore-changes
    (setq-local teamtype--last-change-beg beg)
    (setq-local teamtype--last-change-end end)
    (setq-local teamtype--last-change-old-text
                (buffer-substring-no-properties beg end))))

(defun teamtype--after-change (beg end _len)
  "Send the change to the daemon after a buffer modification."
  (unless teamtype--ignore-changes
    (when teamtype--client
      (let* ((start-pos (teamtype--pos-to-line-col teamtype--last-change-beg))
             (end-pos (teamtype--pos-to-line-col teamtype--last-change-end))
             (new-text (buffer-substring-no-properties beg end))
             (uri (concat "file://" buffer-file-name))
             (delta (vector (list :range (list :start (list :line (car start-pos)
                                                            :character (cdr start-pos))
                                               :end (list :line (car end-pos)
                                                          :character (cdr end-pos)))
                                  :replacement new-text)))
             (connection (plist-get teamtype--client :connection)))

        (setq-local teamtype--editor-revision (1+ teamtype--editor-revision))

        (jsonrpc-request connection "edit"
                         (list :uri uri
                               :revision teamtype--daemon-revision
                               :delta delta))))))

(defun teamtype--track-changes ()
  "Start tracking changes for the current buffer."
  (add-hook 'before-change-functions #'teamtype--before-change nil t)
  (add-hook 'after-change-functions #'teamtype--after-change nil t))

;;; Protocol Message Handlers

(defun teamtype--handle-request (_conn method params)
  "Handle JSON-RPC requests from daemon."
  (message "Teamtype request: %s %S" method params))

(defun teamtype--handle-notification (_conn method params)
  "Handle JSON-RPC notifications from daemon."
  (cond
   ((string= method "edit")
    (teamtype--handle-edit-notification params))
   (t
    (message "Unknown teamtype notification: %s" method))))

(defun teamtype--handle-edit-notification (params)
  "Handle an 'edit' notification from the daemon."
  (let* ((uri (plist-get params :uri))
         (revision (plist-get params :revision))
         (delta (plist-get params :delta))
         ; Remove the 'file://' from the URI
         ; TODO: We probably need to remove HTTP encodings here?
         (filepath (substring uri 7))
         (buffer (find-buffer-visiting filepath)))

    (when buffer
      (with-current-buffer buffer
        ;; Only apply if revision matches our editor revision
        (when (= revision teamtype--editor-revision)
          (teamtype--apply-delta delta)
          (setq-local teamtype--daemon-revision (1+ teamtype--daemon-revision)))))))

(defun teamtype--apply-delta (delta)
  "Apply DELTA to the current buffer."
  (setq-local teamtype--ignore-changes t)
  (save-excursion
    (dolist (change (append delta nil)) ; Convert vector to list
      (let* ((range (plist-get change :range))
             (replacement (plist-get change :replacement))
             (start-info (plist-get range :start))
             (end-info (plist-get range :end))
             (start-line (plist-get start-info :line))
             (start-char (plist-get start-info :character))
             (end-line (plist-get end-info :line))
             (end-char (plist-get end-info :character))
             (beg (teamtype--line-col-to-pos start-line start-char))
             (end (teamtype--line-col-to-pos end-line end-char)))
        (delete-region beg end)
        (goto-char beg)
        (insert replacement))))
  (setq-local teamtype--ignore-changes nil))

;;; Setup

(defun teamtype--setup ()
  "Set up teamtype for the current Emacs session."
  (add-hook 'find-file-hook #'teamtype--file-opened)
  (add-hook 'kill-buffer-hook #'teamtype--close-file))

(teamtype--setup)

(provide 'teamtype)

;;; teamtype.el ends here
