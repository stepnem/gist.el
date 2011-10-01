;; gist.el --- Emacs integration for the gist.github.com pastebin service

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Contributors: Farrington <wcfarrington@gmail.com>
;;               Michael Ivey
;;               Phil Hagelberg
;;               Dan McKinley
;; Version: 0.6
;; Created: 21 Jul 2008
;; Keywords: gist, git, github, pastebin

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO
;; http://developer.github.com/v3/gists/

;; Uses your local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config

;;; Code:

(require 'dotelib)
(require 'xml)

(defvar github-user nil
  "*If non-nil, used as your GitHub username without checking git-config(1).")

(defvar github-token nil
  "*If non-nil, used as your GitHub token without checking git-config(1).")

(defvar gist-view-gist nil
  "*If non-nil, automatically view new gists after posting, using `browse-url'.")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
                                     (clojure-mode . "clj")
                                     (common-lisp-mode . "lisp")
                                     (css-mode . "css")
                                     (diff-mode . "diff")
                                     (emacs-lisp-mode . "el")
                                     (erlang-mode . "erl")
                                     (haskell-mode . "hs")
                                     (html-mode . "html")
                                     (io-mode . "io")
                                     (java-mode . "java")
                                     (javascript-mode . "js")
                                     (jde-mode . "java")
                                     (js2-mode . "js")
                                     (lua-mode . "lua")
                                     (ocaml-mode . "ml")
                                     (objective-c-mode . "m")
                                     (perl-mode . "pl")
                                     (php-mode . "php")
                                     (python-mode . "py")
                                     (ruby-mode . "rb")
                                     (text-mode . "txt")
                                     (scala-mode . "scala")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))

(defun github-config (key)
  "Return a GitHub-specific value from the global Git config."
  (let ((value
         (shell-command-to-string
          (concat (executable-find "git") " config --global github." key))))
    (when (> (length value) 0) (substring value 0 -1))))

(defun github-set-config (key value)
  "Set a GitHub-specific value in the global Git config."
  (shell-command-to-string
   (format "%s config --global github.%s %s"
           (executable-find "git") key value)))

(defun github-auth-info ()
  "Return the user's GitHub authorization information.
Searches for a GitHub username and token in the global Git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)
  ;; If we've been called within a scope that already has this
  ;; defined, don't take the time to get it again.
  (if (boundp '*github-auth-info*)
      *github-auth-info*
    (cons (or github-user (github-config "user")
              (let ((user (read-string "GitHub username: ")))
                (github-set-config "user" user)
                user))
          (or github-token (github-config "token")
              (let ((token (read-string "GitHub API token: ")))
                (github-set-config "token" token)
                token)))))

(defmacro github-with-auth-info (login token &rest body)
  "Evaluate BODY with LOGIN and TOKEN bound to the GitHub authentication credentials.
The credentials are retrieved at most once within the body of this macro."
  (declare (indent 2))
  `(let ((*github-auth-info* (github-auth-info)))
     (destructuring-bind (,login . ,token) *github-auth-info*
       ,@body)))

(defun gist-request (url callback &optional params)
  "Make an asynchronous request to URL, calling CALLBACK when complete.
The GitHub parameters are included in the request. Optionally
accepts additional POST PARAMS as a list of (KEY . VALUE) pairs."
  (github-with-auth-info login token
    (let ((url-request-data (gist-make-query-string
                             `(("login" . ,login)
                               ("token" . ,token) ,@params)))
          (url-max-redirecton -1)
          (url-request-method "POST"))
      (url-retrieve url callback))))

(defun gist-make-query-string (params)
  "Return a query string constructed from PARAMS.
PARAMS should be a list with elements of the form (KEY . VALUE).
KEY and VALUE should both be strings."
  (mapconcat (lambda (param) (concat (url-hexify-string (car param)) "="
                                     (url-hexify-string (cdr param))))
             params "&"))

(defun gist-created-callback (status)
  (let ((location (cadr status)))
    (message "Paste created: %s" location)
    (when gist-view-gist (browse-url location))
    (kill-new location)
    (kill-buffer nil)))

;;;###autoload
(defun gist-region (begin end &optional arg callback)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, prompts for privacy and file name."
  (interactive "r\nP")
  (let* ((deffile (file-name-nondirectory
                   (or (buffer-file-name) (buffer-name))))
         (name (if arg (.read-string-with-default "File name" nil deffile)))
         ;; (defext (or (cdr (assoc major-mode gist-supported-modes-alist))
         ;;             (file-name-extension name)
         ;;             "txt"))
         ;; (ext (if arg (.complete-with-default
         ;;               "File type" (mapcar 'cdr gist-supported-modes-alist)
         ;;               nil defext)
         ;;        defext))
         (private (and arg (y-or-n-p "Private? "))))
    (gist-request
     "https://gist.github.com/gists"
     (or callback 'gist-created-callback)
     `(,@(if private '(("action_button" . "private")))
       ;; FIXME
       ;; ("file_ext[gistfile1]" . ,(concat "." ext))
       ("file_name[gistfile1]" . ,name)
       ("file_contents[gistfile1]" . ,(buffer-substring begin end))))))

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
  "Post either the current region, or if mark is not set, the current buffer as a new gist.
Copies the URL into the kill ring.

With a prefix argument, prompts for privacy and file name."
  (interactive "P")
  (if (use-region-p) (gist-region (region-beginning) (region-end) arg)
    (gist-buffer arg)))

;; borrowed from rss.el
(defun gist-xml-cleanup (xml-list)
  "Remove empty strings or whitespace nodes from XML-LIST."
  (mapcar 'gist-xml-cleanup-node xml-list))

(defun gist-xml-cleanup-node (node)
  "Recursively remove whitespace and empty strings from the given XML NODE."
  (apply 'list
         (xml-node-name node)
         (xml-node-attributes node)
         (let (new)
           (dolist (child (xml-node-children node))
             (if (stringp child)
                 (or (string-match "\\`[ \t\n]+\\'" child)
                     (push child new))
               (push (gist-xml-cleanup-node child) new)))
           (nreverse new))))

(defun gist-lists-retrieved-callback (status)
  "Called when the list of gists has been retrieved.
Parses the result and displays the list."
  (goto-char (point-min))
  (search-forward "<?xml")
  (let ((gists (gist-xml-cleanup
                (xml-parse-region (match-beginning 0) (point-max)))))
    (kill-buffer nil)
    (with-current-buffer (get-buffer-create "*gists*")
      (toggle-read-only -1)
      (goto-char (point-min))
      (save-excursion
        (kill-region (point-min) (point-max))
        (gist-insert-list-header)
        (mapc 'gist-insert-gist-link (xml-node-children (car gists)))
        ;; remove the extra newline at the end
        (delete-char -1))
      ;; skip header
      (forward-line)
      (toggle-read-only t)
      (set-window-buffer nil (current-buffer)))))

(defun gist-insert-list-header ()
  "Create the header line in the gist list buffer."
  (save-excursion
    (insert (format "%-8s %-25s %s %s\n"
                    "ID" "Created" "Public" "Description")))
  (overlay-put (make-overlay (line-beginning-position) (line-end-position))
               'face 'header-line)
  (forward-line))

(defun gist-insert-gist-link (gist)
  "Insert a button that will open the given gist when pressed."
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (insert (apply 'format "%-8s %-25s %-6s %s" data))
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'gist-fetch-button
                      'face 'default))
  (insert "\n"))

(defun gist-fetch-button (button)
  "Called when a gist button has been pressed. Fetches and displays the gist."
  (gist-fetch (button-get button 'repo)))

(defun gist-parse-gist (gist)
  "Return a list of GIST's attributes for display.
GIST is the parsed XML <gist> element."
  (mapcar (lambda (s) (gist-child-text s gist))
          '(repo created-at public description)))

(defun gist-child-text (sym node)
  "Retrieve the text content of a child of a <gist> element."
  (car (xml-node-children (assq sym (xml-node-children node)))))

;;;###autoload
(defun gist-list ()
  "Display a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (github-with-auth-info login token
    (gist-request
     (format "https://gist.github.com/api/v1/xml/gists/%s" login)
     'gist-lists-retrieved-callback)))

(defvar gist-fetch-url "https://raw.github.com/gist/"
  "Raw gist content base URL.")

(defun gist-fetch--default ()
  (or (.non-empty-string (number-at-point))
      (.match-nearest-point "https://gist\\.github\\.com/\\([0-9]+\\)")))
(defvar gist-fetch-history nil)
;;;###autoload
(defun gist-fetch (id)
  "Fetch a gist ID and display it in a new buffer.
If the gist already exists in a buffer, switches to it."
  (interactive (list (.read-string-with-default
                      "Gist ID" 'gist-fetch-history (gist-fetch--default))))
  (let ((bname (format "*gist %s*" id)))
    (if (get-buffer bname) (switch-to-buffer-other-window bname)
      (message "Fetching Gist %s..." id)
      (with-current-buffer (url-retrieve-synchronously
                            (concat gist-fetch-url id))
        (rename-buffer bname t)
        (goto-char (point-min))
        (search-forward-regexp "\n\n")
        (buffer-enable-undo)
        (delete-region (point-min) (point))
        (fundamental-mode)
        (set-buffer-modified-p nil)
        (switch-to-buffer-other-window (current-buffer))))))

(provide 'gist)
;;; gist.el ends here.
