;;; org2jekyll.el --- Minor mode to publish org-mode post to jekyll without specific yaml

;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Maintainer: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Version: 0.2.2
;; Package-Requires: ((dash-functional "2.11.0") (s "1.9.0") (deferred "0.3.1") (kv "0.0.19"))
;; Keywords: org-mode jekyll blog publish
;; URL: https://github.com/ardumont/org2jekyll

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Functions to ease publishing jekyll posts from org-mode file

;; Providing you have a working `'jekyll`' and `'org-publish`'
;; This will permit you to simply export an org-mode file with the right jekyll
;; format to the right folder
;;
;; M-x org2jekyll-create-draft create a draft with the necessary metadata
;;
;; M-x org2jekyll-publish publish the current post (or page) to the jekyll folder
;;
;; M-x org2jekyll-publish-pages to publish all pages (layout 'default')
;;
;; M-x org2jekyll-publish-posts to publish all post pages (layout 'post')
;;
;; M-x org2jekyll-mode to activate org2jekyll's minor mode
;;
;; You can customize using M-x customize-group RET org2jekyll RET
;;
;; More information on https://github.com/ardumont/org2jekyll

;;; Code:

(require 'org)
(require (if (version< emacs-version "24.4") 'org-publish 'ox-publish))

(require 'dash-functional)
(require 's)
(require 'deferred)
(require 'ido)
(require 'kv)
(require 'cl-lib) ;; lexical-let

(defconst org2jekyll--version "0.2.2" "Current org2jekyll version installed.")

(defgroup org2jekyll nil "Publish org-mode posts to jekyll"
  :tag "org2jekyll"
  :version "0.0.3"
  :group 'org)

(defcustom org2jekyll-blog-author nil
  "Blog entry author."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-source-directory nil
  "Path to the source directory."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-directory nil
  "Path to Jekyll blog."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-drafts-dir nil
  "Path to drafts directory relative to `org2jekyll-jekyll-directory`."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-posts-dir nil
  "Path to posts directory relative to `org2jekyll-jekyll-directory`."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layouts '("post" "default")
  "Possible layouts, by default either a post or a page"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layout-post "post"
  "Article blog post layout"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layout-page "default"
  "Article page layout, mostly intended as static pages (e.g about, contacts, etc...)"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defvar org2jekyll-jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defvar org2jekyll-jekyll-org-post-template nil
  "Default template for org2jekyll draft posts.
The `'%s`' will be replaced respectively by name, author, generated date, title,
 description and categories.")

(setq org2jekyll-jekyll-org-post-template
      "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: %s
#+AUTHOR: %s
#+DATE: %s
#+TITLE: %s
#+DESCRIPTION: %s
#+TAGS: %s
#+CATEGORIES: %s
\n")

(defun org2jekyll--optional-folder (folder-source &optional folder-name)
  "Compute the folder name from a FOLDER-SOURCE and an optional FOLDER-NAME."
  (format "%s/%s" folder-source (if folder-name folder-name "")))

;;;###autoload
(defun org2jekyll-input-directory (&optional folder-name)
  "Compute the input folder from the FOLDER-NAME."
  (org2jekyll--optional-folder org2jekyll-source-directory folder-name))

;;;###autoload
(defun org2jekyll-output-directory (&optional folder-name)
  "Compute the output folder from the optional FOLDER-NAME."
  (org2jekyll--optional-folder org2jekyll-jekyll-directory folder-name))

(defun org2jekyll--make-slug (s)
  "Turn a string S into a slug."
  (->> s
       (replace-regexp-in-string "[\]\[(){}!#$~^\\]" "")
       downcase
       (replace-regexp-in-string " " "-")))

(defun org2jekyll--yaml-escape (s)
  "Escape a string S for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun org2jekyll-now ()
  "Generate a formatted now date."
  (format-time-string "%Y-%m-%d %a %H:%M"))

(defun org2jekyll-default-headers-template (blog-layout
                                            blog-author
                                            post-date
                                            post-title
                                            post-description
                                            post-tags
                                            post-categories)
  "Compute default headers.
BLOG-LAYOUT is the layout of the post.
BLOG-AUTHOR is the author.
POST-DATE is the date of the post.
POST-TITLE is the title.
POST-DESCRIPTION is the description.
POST-TAGS is the tags
POST-CATEGORIES is the categories."
  (format org2jekyll-jekyll-org-post-template
          blog-layout
          blog-author
          post-date
          (org2jekyll--yaml-escape post-title)
          post-description
          post-tags
          post-categories))

(defun org2jekyll--draft-filename (draft-dir title)
  "Compute the draft's filename from the DRAFT-DIR and TITLE."
  (concat draft-dir (org2jekyll--make-slug title) org2jekyll-jekyll-post-ext))

(defun org2jekyll--read-title ()
  "Read the title."
  (read-string "Title: "))

(defun org2jekyll--read-description ()
  "Read the description."
  (read-string "Description: "))

(defun org2jekyll--read-tags ()
  "Read the tags."
  (read-string "Tags (space separated values): "))

(defun org2jekyll--read-categories ()
  "Read the categories."
  (read-string "Categories (space separated values): "))

(defun org2jekyll--input-read (prompt collection)
  "Input PROMPT with possibilities limited to COLLECTION."
  (ido-completing-read prompt
                       collection
                       nil
                       'require-match))

(defun org2jekyll--init-buffer-metadata ()
  "Initialize a DRAFT-FILE or current buffer routine.
:: () -> [(Symbol, String)]"
  (list :author      org2jekyll-blog-author
        :date        (org2jekyll-now)
        :layout      (org2jekyll--input-read "Layout: " org2jekyll-jekyll-layouts)
        :title       (org2jekyll--read-title)
        :description (org2jekyll--read-description)
        :tags        (org2jekyll--read-tags)
        :categories  (org2jekyll--read-categories)))

;;;###autoload
(defun org2jekyll-init-current-buffer ()
  "Given an existing buffer, add the needed metadata to make it a post or page."
  (interactive)
  (let* ((metadata (org2jekyll--init-buffer-metadata))
         (author      (plist-get metadata :author))
         (date        (plist-get metadata :date))
         (layout      (plist-get metadata :layout))
         (title       (plist-get metadata :title))
         (description (plist-get metadata :description))
         (tags        (plist-get metadata :tags))
         (categories  (plist-get metadata :categories)))
    (save-excursion
      (with-current-buffer (buffer-name)
        (goto-char (point-min))
        (insert (org2jekyll-default-headers-template layout
                                                     author
                                                     date
                                                     title
                                                     description
                                                     tags
                                                     categories))))))

;;;###autoload
(defun org2jekyll-create-draft ()
  "Create a new Jekyll blog post with TITLE.
The `'%s`' will be replaced respectively by the blog entry name, the author, the
 generated date, the title, the description, the tags and the categories."
  (interactive)
  (let* ((metadata (org2jekyll--init-buffer-metadata))
         (author      (plist-get metadata :author))
         (date        (plist-get metadata :date))
         (layout      (plist-get metadata :layout))
         (title       (plist-get metadata :title))
         (description (plist-get metadata :description))
         (tags        (plist-get metadata :tags))
         (categories  (plist-get metadata :categories))
         (draft-file  (org2jekyll--draft-filename
                       (org2jekyll-input-directory org2jekyll-jekyll-drafts-dir)
                       title)))
    (unless (file-exists-p draft-file)
      (with-temp-file draft-file
        (insert (org2jekyll-default-headers-template layout
                                                     author
                                                     date
                                                     title
                                                     description
                                                     tags
                                                     categories))
        (insert "* ")))
    (find-file draft-file)))

(defun org2jekyll--list-dir (dir)
  "List the content of DIR."
  (find-file dir))

;;;###autoload
(defun org2jekyll-list-posts ()
  "Lists the posts folder."
  (interactive)
  (org2jekyll--list-dir
   (org2jekyll-output-directory org2jekyll-jekyll-posts-dir)))

;;;###autoload
(defun org2jekyll-list-drafts ()
  "List the drafts folder."
  (interactive)
  (org2jekyll--list-dir
   (org2jekyll-input-directory org2jekyll-jekyll-drafts-dir)))

(defun org2jekyll-get-options-from-buffer ()
  "Return special lines at the beginning of current buffer."
  (let ((special-line-regex "^#\\+\\(.+\\):[ \t]+\\(.+\\)$")
        (get-current-line (lambda ()
                            (buffer-substring-no-properties (line-beginning-position)
                                                            (line-end-position))))
        (options-plist))
    (save-excursion
      (goto-char (point-min))
      (catch 'break
        (while (string-match special-line-regex (funcall get-current-line))
          (setq options-plist (plist-put options-plist
                                         (->> (funcall get-current-line)
                                              (match-string 1)
                                              downcase
                                              (concat ":")
                                              intern)
                                         (match-string 2 (funcall get-current-line))))
          (unless (= 0 (forward-line))
            (throw 'break nil))))
      options-plist)))

(defun org2jekyll--without-option-p (option &optional options)
  "Determine if OPTION needs to be deactivated amongst options."
  ;; FIXME: Find the proper org implementation call ¯\_(ツ)_/¯
  (let ((properties (-> (if options options (org2jekyll-get-options-from-buffer))
                        (plist-get :options))))
    (when properties
      (let ((off-option (format "%s:nil" option)))
        (->> properties
             (s-split " ")
             (--filter (string= off-option it)))))))

(defun org2jekyll--with-tags-p (options)
  "Determine, from OPTIONS if we need to export in yaml the tags options"
  (-> "tags"
      (org2jekyll--without-option-p options)
      not))

(defun org2jekyll-get-options-from-file (orgfile)
  "Return special lines at the beginning of ORGFILE."
  (with-temp-buffer
    (when (file-exists-p orgfile)
      (insert-file-contents orgfile)
      (org2jekyll-get-options-from-buffer))))

(defun org2jekyll-article-p (org-file)
  "Determine if the current ORG-FILE's layout.
Depends on the metadata header #+LAYOUT."
  (plist-get (org2jekyll-get-options-from-file org-file) :layout))

(defun org2jekyll--org-to-jekyll-metadata (org-metadata)
  "Given an ORG-METADATA map, translate Org keywords to Jekyll keywords."
  (let ((org2jekyll-map-keys '(("description" . "excerpt"))))
    (--map (-if-let (jekyll-car (assoc-default (car it) org2jekyll-map-keys))
               (cons jekyll-car (cdr it))
             it)
           org-metadata)))

(defun org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh (timestamp)
  "Convert org TIMESTAMP to YYYY-MM-DD HH:MM. For yaml header purposes."
  (format-time-string "%Y-%m-%d %H:%M"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun org2jekyll--convert-timestamp-to-yyyy-dd-mm (timestamp)
  "Convert org TIMESTAMP to to YYYY-MM-DD. For filename renaming purposes."
  (format-time-string "%Y-%m-%d"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun org2jekyll--to-yaml-header (org-metadata)
  "Given a list of ORG-METADATA, compute the yaml header string."
  (--> org-metadata
       org2jekyll--org-to-jekyll-metadata
       (--map (format "%s: %s" (car it) (cdr it)) it)
       (cons "---" it)
       (-snoc it "---\n")
       (s-join "\n" it)))

(defun org2jekyll--space-separated-values-to-yaml (str)
  "Transform a STR of space separated values entries into yaml entries."
  (->> (if str str "")
       (s-split " ")
       (--filter (unless (equal it "") it))
       (--map (format  "- %s" it))
       (cons "")
       (s-join "\n")))

(defun org2jekyll--compute-ready-jekyll-file-name (date org-file)
  "Given a DATE and an ORG-FILE, compute a ready jekyll file name.
If the current path contains the `'org2jekyll-jekyll-drafts-dir`', removes it."
  (let* ((temp-org-jekyll-filename (format "%s-%s" date
                                           (file-name-nondirectory org-file))))
    (->> temp-org-jekyll-filename
         (format "%s/%s" org2jekyll-source-directory)
         (replace-regexp-in-string (format "%s" org2jekyll-jekyll-drafts-dir) "")
         (replace-regexp-in-string "//" "/"))))

(defun org2jekyll-assoc-default (key org-data default-value)
  "Given KEY, ORG-DATA and DEFAULT-VALUE, return the value associated with key.
Return DEFAULT-VALUE if not found."
  (-if-let (data (assoc-default key org-data nil default-value))
      data
    default-value))

(defconst org2jekyll-required-org-header-alist '((:title       . 'required)
                                                 (:date)
                                                 (:categories  . 'required)
                                                 (:tags)
                                                 (:description . 'required)
                                                 (:author)
                                                 (:layout      . 'required))
  "Map of required org headers for jekyll to accept rendering.")

(defun org2jekyll-check-metadata (org-metadata)
  "Check that all required headers in ORG-METADATA are provided.
Return error messages for any required headers that are missing,
and nil if no problems are found."
  (let ((required-options (funcall (-compose (lambda (l) (mapcar #'car l))
                                             (lambda (l) (-filter #'cdr l)))
                                   org2jekyll-required-org-header-alist)))
    (-when-let (error-messages
                (->> required-options
                     (--map (unless (plist-member org-metadata it)
                              (format (concat "- The %s is required, please add "
                                              "'#+%s' at the top of your org buffer.")
                                      (substring (symbol-name it) 1 nil)
                                      (upcase (substring (symbol-name it) 1 nil)))))
                     (s-join "\n")
                     s-trim))
      (if (string= "" error-messages) nil error-messages))))

(defun org2jekyll-remove-org-only-options (yaml-alist)
  "Filter out org options with no Jekyll meaning from YAML-ALIST."
  (let* ((required-options (--map (substring (symbol-name (car it)) 1 nil)
                                  org2jekyll-required-org-header-alist))
         (org-options (--map (downcase (substring it 0 -1))
                             org-options-keywords))
         (org-only-options (--filter (not (member it required-options))
                                     org-options))
         (jekyll-options (--filter (not (member (car it) org-only-options))
                                   yaml-alist)))
    jekyll-options))

(defun org2jekyll-read-metadata (org-file)
  "Given an ORG-FILE, return its org metadata.
It can display an error message about missing required values."
  (let* ((buffer-metadata (org2jekyll-get-options-from-file org-file))
         (org-defaults `(:date ,(org2jekyll-now) :author ""))
         (merged-metadata (kvplist-merge org-defaults buffer-metadata))
         (categories (org2jekyll--space-separated-values-to-yaml
                      (plist-get merged-metadata :categories)))
         (tags (if (org2jekyll--with-tags-p buffer-metadata)
                   (org2jekyll--space-separated-values-to-yaml
                    (plist-get merged-metadata :tags))
                 ""))
         (date (org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh
                (plist-get merged-metadata :date)))
         (yaml-metadata (-> merged-metadata
                            (plist-put :categories categories)
                            (plist-put :tags tags)
                            (plist-put :date date)))
         (yaml-alist (--map (cons (symbol-name (car it))
                                  (cdr it))
                            (kvplist->alist yaml-metadata))))
    (-if-let (error-messages (org2jekyll-check-metadata buffer-metadata))
        (format "This org-mode file is missing required header(s):
%s
Publication skipped" error-messages)
      (org2jekyll-remove-org-only-options yaml-alist))))


(defun org2jekyll-read-metadata-and-execute (action-fn org-file)
  "Execute ACTION-FN function after checking metadata from the ORG-FILE."
  (let ((filename-non-dir (file-name-nondirectory org-file)))
    (if (org2jekyll-article-p org-file)
        (let ((org-metadata (org2jekyll-read-metadata org-file)))
          (if (stringp org-metadata)
              (org2jekyll-message org-metadata)
            (let ((page-or-post (if (org2jekyll-post-p
                                     (assoc-default "layout" org-metadata))
                                    "Post"
                                  "Page")))
              (funcall action-fn org-metadata org-file)
              (format "%s '%s' published!" page-or-post filename-non-dir))))
      (format "'%s' is not an article, publication skipped!" filename-non-dir))))

(defun org2jekyll-message (&rest args)
  "Log formatted ARGS."
  (apply 'message (format "org2jekyll - %s" (car args)) (cdr args)))

(defun org2jekyll--publish-post-org-file-with-metadata (org-metadata org-file)
  "Publish as post with ORG-METADATA the ORG-FILE."
  (let* ((blog-project (assoc-default "layout" org-metadata))
         (file-date    (->  (assoc-default "date" org-metadata) org2jekyll--convert-timestamp-to-yyyy-dd-mm))
         (temp-file    (org2jekyll--compute-ready-jekyll-file-name file-date org-file)))
    (copy-file org-file temp-file 'overwrite 'keep-time 'preserve-ids 'preserve-perms)
    (org-publish-file temp-file
                      (assoc blog-project org-publish-project-alist)
                      'no-cache)))

(defun org2jekyll-publish-post (org-file)
  "Publish ORG-FILE as a post."
  (org2jekyll-read-metadata-and-execute
   'org2jekyll--publish-post-org-file-with-metadata
   org-file))

(defun org2jekyll-install-yaml-headers (original-file published-file)
  "Read ORIGINAL-FILE metadata and install yaml header to PUBLISHED-FILE.
Then delete the original-file which is intended as a temporary file.
This function is intended to be used as org-publish hook function."
  (let ((yaml-headers (-> original-file
                          org2jekyll-read-metadata
                          org2jekyll--to-yaml-header)))
    (with-temp-file published-file
      (insert-file-contents published-file)
      (goto-char (point-min))
      (insert yaml-headers))
    (when (file-exists-p original-file) (delete-file original-file))))

(defun org2jekyll--publish-page-org-file-with-metadata (org-metadata org-file)
  "Publish as page with ORG-METADATA the ORG-FILE."
  (let* ((blog-project (assoc-default "layout" org-metadata))
         (filename     (file-name-nondirectory org-file))
         (ext          (file-name-extension filename))
         (temp-file    (format "%s/%sorg2jekyll"
                               (s-chop-suffix "/" org2jekyll-source-directory)
                               (s-chop-suffix ext filename))))
    (copy-file org-file temp-file 'overwrite 'keep-time 'preserve-ids 'preserve-perms)
    (org-publish-file temp-file
                      (assoc blog-project org-publish-project-alist)
                      'no-cache)))

(defun org2jekyll-publish-page (org-file)
  "Publish ORG-FILE as a page."
  (org2jekyll-read-metadata-and-execute
   'org2jekyll--publish-page-org-file-with-metadata
   org-file))

(defun org2jekyll-post-p (layout)
  "Determine if the LAYOUT corresponds to a post."
  (string= org2jekyll-jekyll-layout-post layout))

(defun org2jekyll-page-p (layout)
  "Determine if the LAYOUT corresponds to a page."
  (string= org2jekyll-jekyll-layout-page layout))

(defun org2jekyll-publish-web-project ()
  "Publish the 'web' project."
  (org2jekyll-message "Publish `'web`' project (images, css, js, etc...).")
  (org-publish-project "web"))

;;;###autoload
(defun org2jekyll-publish ()
  "Publish the current org file as post or page depending on the chosen layout.
Layout `'post`' is a jekyll post.
Layout `'default`' is a page (depending on the user customs)."
  (interactive)
  (lexical-let* ((org-file (buffer-file-name (current-buffer))))
    (let* ((publish-fn (-> (plist-get (org2jekyll-get-options-from-buffer) :layout)
                           org2jekyll-post-p
                           (if 'org2jekyll-publish-post
                               'org2jekyll-publish-page)))
           (final-message (funcall publish-fn org-file)))
      (progn
        (org2jekyll-publish-web-project)
        (org2jekyll-message final-message)))))

(defvar org2jekyll-mode-map nil "Default Bindings map for org2jekyll mode.")

(setq org2jekyll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c . n") 'org2jekyll-create-draft)
        (define-key map (kbd "C-c . p") 'org2jekyll-publish)
        (define-key map (kbd "C-c . P") 'org2jekyll-publish-posts)
        (define-key map (kbd "C-c . l") 'org2jekyll-list-posts)
        (define-key map (kbd "C-c . d") 'org2jekyll-list-drafts)
        map))

;;;###autoload
(defun org2jekyll-publish-posts ()
  "Publish all the posts."
  (interactive)
  (deferred:$
    (deferred:next
      (lambda () (->> (assoc org2jekyll-jekyll-layout-post org-publish-project-alist)
                 org-publish-get-base-files
                 (--filter (org2jekyll-post-p (org2jekyll-article-p it))))))
    (deferred:nextc it
      (lambda (posts)
        (mapc #'org2jekyll-publish-post posts)))))

;;;###autoload
(defun org2jekyll-publish-pages ()
  "Publish all the pages."
  (interactive)
  (deferred:$
    (deferred:next
      (lambda () (->> (assoc org2jekyll-jekyll-layout-page org-publish-project-alist)
                 org-publish-get-base-files
                 (--filter (org2jekyll-page-p (org2jekyll-article-p it))))))
    (deferred:nextc it
      (lambda (pages)
        (mapc #'org2jekyll-publish-page pages)))))

(defun org2jekyll--bug-report ()
  "Compute the bug report for the user to include."
  (->> `("Please:"
         "- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)"
         "- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...)."
         "- A sample of your configuration."
         "- Report the following message trace inside your issue."
         ""
         "System information:"
         ,(format "- system-type: %s" system-type)
         ,(format "- locale-coding-system: %s" locale-coding-system)
         ,(format "- emacs-version: %s" (emacs-version))
         ,(format "- org version: %s" (org-version))
         ,(format "- org2jekyll version: %s" org2jekyll--version)
         ,(format "- org2jekyll path: %s" (find-library-name "org2jekyll")))
       (s-join "\n")))

(defun org2jekyll-bug-report (&optional open-url)
  "Display a bug report message.
When OPEN-URL is filled, with universal argument (`C-u') is used,
opens new issue in org-trello's github tracker."
  (interactive "P")
  (when open-url
    (browse-url "https://github.com/ardumont/org2jekyll/issues/new"))
  (message (org2jekyll--bug-report)))


;;;###autoload
(define-minor-mode org2jekyll-mode
  "Functionality for publishing the current org-mode post to jekyll.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2jekyll-mode-map}"

  :init-value nil
  :lighter " o2j"
  :group 'org2jekyll
  :keymap org2jekyll-mode-map)

(defvar org2jekyll-mode-on-hook nil "org2jekyll starting hook")
(setq org2jekyll-mode-off-hook nil) ;; for dev
;; install org2jekyll hook in org-publish when activating org2jekyll-mode
(add-hook 'org2jekyll-mode-on-hook
          (lambda ()
            (add-hook 'org-publish-after-publishing-hook 'org2jekyll-install-yaml-headers)))

(defvar org2jekyll-mode-off-hook '() "org2jekyll stoping hook")
(setq org2jekyll-mode-off-hook nil) ;; for dev
;; uninstall hook on org-publish
(add-hook 'org2jekyll-mode-off-hook
          (lambda () (remove-hook 'org-publish-after-publishing-hook 'org2jekyll-install-yaml-headers)))

;; install/uninstall hook when activating/deactivating org2jekyll-mode
;; org2jekyll inserts the yaml when the publishing step is done, so now org
;; does all the publishing in concordance to what the user expects and we do
;; our update after it so Jekyll is happy.

(provide 'org2jekyll)
;;; org2jekyll.el ends here
