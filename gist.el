;;; gist.el --- interface to the gist.github.com pastebin service

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Licence: Whatever Works
;; Version: 1.0b
;; Created: 2011-10-02 17:10 +0200 Sunday
;; Keywords: gist, git, github, pastebin

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; http://developer.github.com/v3/gists/

;; This started as a rewrite of <https://github.com/defunkt/gist.el>, but
;; nothing from the original remains other than some of the command names.

;; The functionality currently depends on the `curl' command line utility, as
;; I haven't managed to get the Emacs URL package to authenticate with GitHub.
;; On the plus side, I think curl is maintained, standards-compliant and
;; reliable, neither of which can be said about the URL package.

;; (If URL on my system ever comes to its senses, providing a way to use it in
;; this package will be trivial and I'll happily do it. Also, maybe I'm just
;; an idiot and it'd work for other people. It even works for me with other
;; websites, in any case.)

;; Corrections and constructive feedback appreciated.

;;; Todo:

;; - Do something about the paging maybe? I don't have enough gists to care at
;;   this point.

;;; Code:

(require 'dotelib)
(require 'json)

(defvar gist-view-gist nil
  "*If non-nil, automatically view new gists after posting, using `browse-url'.")

(put 'gist-http-error 'error-conditions '(error gist-http-error))
(put 'gist-http-error 'error-message "Gist HTTP error")
(defun gist-curl (path &optional data method)
  "Make a synchronous HTTPS request with METHOD for PATH to the GitHub API.
METHOD defaults to GET when DATA is nil, or POST with DATA as
payload. Returns the JSON response as parsed by
`json-read-from-string' when successful, otherwise signals a
`gist-http-error' with error data consisting of a list of the
return status and the JSON payload (if any)."
  (let ((auth (auth-source-user-or-password
               '("login" "password") "api.github.com" "https" t)))
    (with-current-buffer (get-buffer-create "*gist-http*")
      (erase-buffer)
      (apply 'call-process "curl" nil t nil
             `("-sD-"
               ,@(when (cdr auth) `("-u" ,(mapconcat 'identity auth ":")))
               ,@(when data `("--data-binary" ,data))
               ,@(when method `("-X" ,method))
               ,(concat "https://api.github.com" path)))
      (goto-char (point-min))
      (while (re-search-forward "^HTTP/1.1 100 Continue\r\n\r\n" nil t))
      (let ((status (and (looking-at "HTTP/1.. +\\([0-9]+.*\\)\r\n")
                         (match-string 1)))
            (json (let ((json-object-type 'plist)
                        (json-false nil))
                    (search-forward "\r\n\r\n")
                    (ignore-errors (json-read)))))
        (cond ((string-match "200\\|201" status) json)
              ((string-match "204" status) status)
              (t (signal 'gist-http-error (list status json))))))))

(defun gist-encode (filename content &optional public description newname)
  (json-encode
   `(,@(when description `((description . ,description)))
     ,@(when public `((public . ,public)))
     ,@(when content `((files (,filename
                               ,@(when newname `((filename . ,newname)))
                               (content . ,content))))))))

(defun gist--file (gist) (cadr (plist-get gist :files)))

;;;###autoload
(defun gist-region (begin end &optional arg callback)
  "Post the current region as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, prompts for description, privacy and file name."
  (interactive "r\nP")
  (let* ((deffile (file-name-nondirectory
                   (or (buffer-file-name) (buffer-name))))
         (description (and arg (read-string "Description: ")))
         (name (if arg (.read-string-with-default "File name" nil deffile)))
         (private (and arg (y-or-n-p "Private? ")))
         (gist (gist-curl
                "/gists"
                (gist-encode name (buffer-substring-no-properties begin end)
                             (or (not private) json-false) description)))
         (url (plist-get gist :html_url)))
    (kill-new url)
    (message "Posted at %s" url)
    (when gist-view-gist (browse-url url))))

;;;###autoload
(defun gist-buffer (&optional arg)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, prompts for privacy and file name."
  (interactive "P")
  (gist-region (point-min) (point-max) arg))

;;;###autoload
(defun gist-file (file &optional arg)
  "Post the contents of FILE as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, prompts for privacy and file name."
  (interactive "fFile: \nP")
  (with-current-buffer (find-file-noselect file)
    (gist-region (point-min) (point-max) arg)
    (kill-buffer nil)))

;;;###autoload
(defun gist-region-or-buffer (&optional arg)
  "Post the current region or buffer (when region is not active) as a new gist.
Copies the URL into the kill ring.

With a prefix argument, prompts for privacy and file name."
  (interactive "P")
  (if (use-region-p) (gist-region (region-beginning) (region-end) arg)
    (gist-buffer arg)))

;;;###autoload
(defun gist-update (&optional id data)
  "Update the (single-file) gist ID with DATA.
DATA should be in the form returned by `gist-encode'.
Interactively, prompts for ID and and DATA (which can come from a
region, buffer, file, or X selection) in the minibuffer."
  (interactive)
  (let* ((gists (gist-curl "/gists"))
         (id (.complete-with-default
              "ID" (mapcar (& (.flip 'plist-get) :id) gists)
              nil (gist-fetch--default))))
    (gist-curl (concat "/gists/" id)
               (or data
                   (let* ((gist (find-if (λ (g) (equal (plist-get g :id) id))
                                         gists))
                          (file (gist--file gist))
                          (oldname (plist-get file :filename))
                          (olddesc (plist-get gist :description))
                          (newdesc (.read-string-with-default
                                    "Description" nil olddesc))
                          (newname (.read-string-with-default
                                    "File name" nil oldname))
                          (content (cond ((region-active-p)
                                          (buffer-substring-no-properties
                                           (region-beginning) (region-end)))
                                         ((y-or-n-p "Buffer? ")
                                          (with-current-buffer (read-buffer "Buffer name: " (current-buffer) t)
                                            (buffer-substring-no-properties (point-min) (point-max))))
                                         ((y-or-n-p "Selection? ")
                                          (x-get-selection))
                                         ((y-or-n-p "File? ")
                                          (.file-string (read-file-name "File name: ")))
                                         (t
                                          (message "There's no pleasing some people")
                                          nil))))
                     (gist-encode oldname content nil newdesc
                                  (unless (equal newname oldname) newname))))
               "PATCH")))

(defvar gist-list-time-format "%m/%d %R"
  "*`format-time-string'-compatible format for gist time stamps.")

(defun gist-list--display (gists)
  (with-current-buffer (get-buffer-create "*gists*")
    (let ((inhibit-read-only t)
          (line-format
           (concat "%-20s %-"
                   (number-to-string
                    (length (format-time-string gist-list-time-format)))
                   "s %s\n")))
      (erase-buffer)
      (save-excursion
        (insert (format line-format "ID" "Created" "Description (or file name)"))
        (overlay-put (make-overlay (point-min) (point)) 'face 'header-line)
        (mapc (lambda (g)
                (destructuring-bind (id time desc)
                    (mapcar (& 'plist-get g) '(:id :created_at :description))
                  (insert
                   (propertize
                    ;; `timezone-parse-date' or something might be useful
                    (format line-format
                            id
                            (format-time-string
                             gist-list-time-format
                             (apply
                              'encode-time
                              (parse-time-string
                               (replace-regexp-in-string "T\\|Z" " " time))))
                            (or (.non-empty-string desc)
                                (concat "file:"
                                        (plist-get (gist--file g) :filename))))
                    'gist-metadata g))))
              gists)))
    (unless (derived-mode-p 'gist-list-mode) (gist-list-mode))
    (switch-to-buffer-other-window (current-buffer))))

(define-derived-mode gist-list-mode special-mode "Gist List")
(.define-keys gist-list-mode-map '(("\C-m" gist-list-fetch-gist)
                                   ("b" gist-list-browse-gist)
                                   ("d" gist-list-delete-gist)
                                   ("e" gist-list-edit-description)
                                   ("n" next-line)
                                   ("p" previous-line)))

(defun gist-list-fetch-gist ()
  "Fetch and display the gist on the current line."
  (interactive)
  (gist-fetch (gist-list--get :id)))

(defun gist-list-browse-gist ()
  "Go to the URL of the gist on the current line using `browse-url'."
  (interactive)
  (browse-url (gist-list--get :html_url)))

(defun gist-list-delete-gist ()
  "Delete the gist on the current line."
  (interactive)
  (let ((id (gist-list--get :id))
        (inhibit-read-only t))
    (when (y-or-n-p (format "Delete gist %s? " id))
      (message "%s" (gist-curl (concat "/gists/" id) nil "DELETE"))
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun gist-list-edit-description ()
  "Edit description of the gist on the current line."
  (interactive)
  (let* ((id (gist-list--get :id))
         (old (gist-list--get :description))
         (new (.read-string-with-default "New description" nil old)))
    (message "Updating description of gist %s..." id)
    (when (gist-curl (concat "/gists/" id)
                     (json-encode `((description . ,new)))
                     "PATCH")
      (message "Updating description of gist %s...done" id))))

(defun gist-list--get (prop)
  (plist-get (get-text-property (point) 'gist-metadata) prop))

;;;###autoload
(defun gist-list ()
  "Display a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (gist-list--display (gist-curl "/gists")))

(defun gist-fetch--default ()
  (or (.match-nearest-point "https://gist\\.github\\.com/\\([0-9a-f]+\\)")
      (.match-nearest-point "\\b\\(?:[0-9]\\{7\\}\\|[0-9a-f]\\{20\\}\\)\\b")
      (.match-nearest-point "\\(?:[0-9]\\{1,7\\}\\|[0-9a-f]\\{20\\}\\)")))
(defvar gist-fetch-history nil)
;;;###autoload
(defun gist-fetch (id)
  "Fetch a gist ID and display it in a new buffer.
Assumes a single-file gist (just use Git for working with
multi-file gist repos)."
  (interactive (list (.read-string-with-default
                      "Gist ID" 'gist-fetch-history (gist-fetch--default))))
  (message "Fetching Gist %s..." id)
  (let* ((gist (gist-curl (concat "/gists/" id)))
         (file (gist--file gist))
         (buffer (generate-new-buffer
                  (concat "gist:" id ":" (plist-get file :filename)))))
    (with-current-buffer buffer
      (prin1 gist (current-buffer))
      (undo-boundary)
      (erase-buffer)
      (insert (plist-get file :content))
      (normal-mode)
      (switch-to-buffer-other-window (current-buffer))))
  (message "Fetching Gist %s...done" id))

(provide 'gist)
;;; gist.el ends here.
