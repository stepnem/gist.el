;;; gist.el --- interface to the gist.github.com pastebin service

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Licence: Whatever Works
;; Version: 1.0b
;; Created: 2011-10-02 17:10 +0200 Sunday
;; Keywords: gist, git, github, pastebin, comm, tools, vc

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is only useful with single-file gists, and will most probably remain
;; that way, as the complexity of various ways to work with multi-file gists
;; is in my opinion better handled by other Emacs Git tools (notably Magit and
;; Magithub). This library is primarily for "traditional" quick pastes and
;; viewing/editing thereof. You _can_ clone or fork multi-file Gist repos
;; using this package, though, so if you have Magit installed the integration
;; should be silky smooth.

;; API reference: http://developer.github.com/v3/gists/

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

(require 'cl)                           ; `find-if'
(require 'dotelib)
(require 'json)

(defvar gist-view-gist nil
  "*If non-nil, automatically `browse-url' new gists after posting.")

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
               ;; GitHub returns 411 without this
               ,@(when (string-equal method "PUT") '("-H" "Content-Length: 0"))
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

(defun gist--json-encode (object)
  "`json-encode' with a workaround for the broken GitHub API."
  (replace-regexp-in-string "%" "\\\\u0025" (json-encode object)))

(defun gist-encode (&optional filename content public description newname)
  (gist--json-encode
   `(,@(when description `((description . ,description)))
     ,@(when public `((public . ,public)))
     ,@(when content `((files (,filename
                               ,@(when newname `((filename . ,newname)))
                               (content . ,content))))))))

(defun gist--file (gist) (cadr (plist-get gist :files)))

(defun gist--read-content-interactively (&optional noerror)
  (cond
   ((region-active-p) (buffer-substring-no-properties
                       (region-beginning) (region-end)))
   ((y-or-n-p "Buffer? ")
    (with-current-buffer (read-buffer "Buffer name: " (current-buffer) t)
      (buffer-substring-no-properties (point-min) (point-max))))
   ((y-or-n-p "Selection? ") (x-get-selection))
   ((y-or-n-p "File? ")
    (.file-string (read-file-name "File name: " nil nil t)))
   (t (funcall (if noerror 'message 'error)
               "There's no pleasing some people"))))

(defun gist-create (filename content &optional description public)
  (gist-curl "/gists" (gist-encode filename content
                                   (or public json-false) description)))

;;;###autoload
(defun gist-paste (content filename &optional description public)
  "Post CONTENT as FILENAME with DESCRIPTION as a PUBLIC gist."
  (interactive
   (list (gist--read-content-interactively)
         (read-file-name "File name: " nil (car file-name-history))
         (read-string "Description: ")
         (not (y-or-n-p "Private? "))))
  (let ((url (plist-get (gist-create filename content description public)
                        :html_url)))
    (kill-new url)
    (message "Posted at %s" url)
    (when gist-view-gist (browse-url url))))

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
         (private (and arg (y-or-n-p "Private? "))))
    (gist-paste (buffer-substring-no-properties begin end)
                name description (not private))))

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
(defun gist-update (id data)
  "Update the (single-file) gist ID with DATA.
DATA should be a valid gist update JSON payload. Interactively,
prompts for ID and information necessary for building DATA in the
minibuffer. The gist content can come from a region, buffer,
file, or X selection."
  (interactive
   (let*
     ((gists (gist-curl "/gists"))
      (id (.complete-with-default
           "ID" (mapcar (& (.flip 'plist-get) :id) gists)
           'gist-id-history (gist--guess-id)))
      (gist (find-if (λ (g) (equal (plist-get g :id) id)) gists))
      (file (gist--file gist))
      (oldname (plist-get file :filename))
      (newname (.read-string-with-default "File name" nil oldname))
      (olddesc (plist-get gist :description))
      (newdesc (.read-string-with-default "Description" nil olddesc))
      (content (gist--read-content-interactively t)))
     (list id (gist-encode oldname content nil newdesc
                           (unless (equal newname oldname) newname)))))
  (gist-curl (concat "/gists/" id) data "PATCH"))

(autoload 'magit-run-git "magit")
(autoload 'magit-status "magit")
;;;###autoload
(defun gist-clone (&optional id dir)
  "Clone gist ID into DIR and run `magit-status' on it.
DIR should be a directory name suitable as the second argument to
git-clone(1)."
  (interactive)
  (let* ((id (or id (.read-string-with-default
                     "ID" 'gist-id-history (gist--guess-id))))
         (name (unless dir (.read-string-with-default "Repo dirname" nil id)))
         (dir (or dir
                  (expand-file-name
                   name (read-directory-name "Parent directory: " nil nil t)))))
    (magit-run-git "clone" (concat "git@gist.github.com:" id ".git") dir)
    (magit-status dir)))

;;;###autoload
(defun gist-fork (id &optional clone)
  "Fork gist ID.
With a prefix argument or if CLONE is non-nil, also clone the new
fork and run `magit-status' on it."
  (interactive (list (.read-string-with-default
                      "ID" gist-id-history (gist--guess-id))
                     current-prefix-arg))
  (let ((gist (gist-curl (concat "/gists/" id "/fork") nil "POST")))
    (when clone
      (gist-clone (plist-get gist :id)))))

;;;###autoload
(defun gist-star (id &optional unstar)
  "Star (or unstar if UNSTAR is non-nil) gist ID."
  (interactive (list (.read-string-with-default
                      "ID" gist-id-history (gist--guess-id))
                     current-prefix-arg))
  (gist-curl (concat "/gists/" id "/star") nil (if unstar "DELETE" "PUT")))

(defvar gist-list-time-format "%m/%d %R"
  "*`format-time-string'-compatible format for gist time stamps.")
(defvar gist-list-line-format
  (concat "%-20s %-"
          (number-to-string
           (length (format-time-string gist-list-time-format)))
          "s %s\n"))

(defun gist-list--refresh (&rest ignore)
  (let ((inhibit-read-only t))
    (save-excursion
      (erase-buffer)
      (insert (format gist-list-line-format
                      "ID" "Created" "Description[file name]"))
      (overlay-put (make-overlay (point-min) (point)) 'face 'header-line)
      (mapc 'gist-list--insert-line
            (gist-curl (if gist-list-user
                           (concat "/users/" gist-list-user "/gists")
                         "/gists/starred"))))))

(defun gist-list--insert-line (data)
  (destructuring-bind (id time desc)
      (mapcar (& 'plist-get data) '(:id :created_at :description))
    (insert
     (propertize
      ;; `timezone-parse-date' or something might be useful
      (format gist-list-line-format
              id
              (format-time-string
               gist-list-time-format
               (apply 'encode-time
                      (parse-time-string
                       (replace-regexp-in-string "T\\|Z" " " time))))
              (concat desc "[" (plist-get (gist--file data) :filename) "]"))
      'gist-metadata data))))

(defun gist-list--get (prop)
  (plist-get (funcall (if (eq prop :filename) 'gist--file 'identity)
                      (get-text-property (point) 'gist-metadata))
             prop))

(define-derived-mode gist-list-mode special-mode "Gist List" nil
  (.setq-local revert-buffer-function 'gist-list--refresh))
(.define-keys gist-list-mode-map '(("\C-m" gist-list-fetch-gist)
                                   ("b" gist-list-browse-gist)
                                   ("c" gist-list-clone-gist)
                                   ("d" gist-list-delete-gist)
                                   ("e" gist-list-edit-description)
                                   ("f" gist-list-fork-gist)
                                   ("n" next-line)
                                   ("p" previous-line)
                                   ("s" gist-list-star-gist)
                                   ("u" gist-list-kill-url)))

(defun gist-list-fetch-gist ()
  "Fetch and display the gist on the current line."
  (interactive)
  (gist-fetch (gist-list--get :id)))

(defun gist-list-clone-gist ()
  "Clone the gist on the current line."
  (interactive)
  (gist-clone (gist-list--get :id)))

(defun gist-list-fork-gist (&optional clone)
  "Fork the gist on the current line.
With a prefix argument, also clone the new gist and open its
Magit status buffer."
  (interactive "P")
  (gist-fork (gist-list--get :id) clone))

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
         (fname (gist-list--get :filename))
         (time (gist-list--get :created_at))
         (old (gist-list--get :description))
         (new (.read-string-with-default "New description" nil old))
         (inhibit-read-only t))
    (message "Updating description of gist %s..." id)
    (when (gist-update id (gist--json-encode `((description . ,new))))
      (save-excursion
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (gist-list--insert-line
         `(:id ,id :created_at ,time :description ,new :filename ,fname)))
      (message "Updating description of gist %s...done" id))))

(defun gist-list-kill-url ()
  "Put the url of the gist on the current line onto the `kill-ring'."
  (interactive)
  (let ((url (gist-list--get :html_url)))
    (kill-new url)
    (message "%s copied into the kill ring" url)))

(defun gist-list-star-gist (&optional unstar)
  "Star (with a prefix argument, unstar) the gist on the current line."
  (interactive "P")
  (message "%s" (gist-star (gist-list--get :id) unstar)))

(make-variable-buffer-local
 (defvar gist-list-user nil
   "*Name of the listed gists' owner.
You can `setq-default' this to your Gist (GitHub) user name."))
(defvar gist-user-history nil "List of Gist users read.")
;;;###autoload
(defun gist-list (&optional user)
  "Display a list of all USER's (`gist-list-user''s by default) gists.
With a prefix argument, prompts for USER in the minibuffer."
  (interactive)
  (gist--list
   (or user
       (let ((def (default-value 'gist-list-user)))
         (if current-prefix-arg
             (read-string "User: " nil 'gist-user-history def)
           (or def
               (setq-default gist-list-user
                             (read-string "Default `gist-list-user': "
                                          nil 'gist-user-history))))))))

(defun gist--list (&optional user)
  (let ((s (concat (or user "starred") (when user "'s") " gists")))
    (message (concat "Retrieving list of " s))
    (with-current-buffer (get-buffer-create (format "*%s*" s))
      (unless (derived-mode-p 'gist-list-mode) (gist-list-mode))
      (setq gist-list-user user)
      (gist-list--refresh)
      (switch-to-buffer-other-window (current-buffer)))))

;;;###autoload
(defun gist-list-starred ()
  "Display a list of all your starred gists."
  (interactive)
  (gist--list))

(defun gist--guess-id ()
  (or (.match-nearest-point "https://gist\\.github\\.com/\\([0-9a-f]+\\)" "w")
      (.match-nearest-point "\\b\\(?:[0-9]\\{7\\}\\|[0-9a-f]\\{20\\}\\)\\b" "w")
      (.match-nearest-point "\\(?:[0-9]\\{1,7\\}\\|[0-9a-f]\\{20\\}\\)" "w")))
(defvar gist-id-history nil "List of Gist IDs read.")
;;;###autoload
(defun gist-fetch (id)
  "Fetch a gist ID and display it in a new buffer.
Assumes a single-file gist (just use Git for working with
multi-file gist repos)."
  (interactive (list (.read-string-with-default
                      "Gist ID" 'gist-id-history (gist--guess-id))))
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
