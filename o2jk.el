;;; o2jk.el --- Minor mode to publish org-mode post to jekyll without specific yaml

;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Maintainer: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Version: 0.2.7
;; Package-Requires: ((dash "2.18.0") (s "1.9.0"))
;; Keywords: org-mode jekyll blog publish
;; URL: https://github.com/kimim/o2jk

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
;; M-x o2jk-create-draft create a draft with the necessary metadata
;;
;; M-x o2jk-publish publish the current post (or page) to the jekyll folder
;;
;; M-x o2jk-publish-pages to publish all pages (layout 'default')
;;
;; M-x o2jk-publish-posts to publish all post pages (layout 'post')
;;
;; M-x o2jk-mode to activate o2jk's minor mode
;;
;; You can customize using M-x customize-group RET o2jk RET
;;
;; More information on https://github.com/kimim/o2jk

;;; Code:

(require 'org)
(require (if (version< emacs-version "24.4") 'org-publish 'ox-publish))

(require 'dash)
(require 's)
(require 'ido)
(require 'consult)

(defconst o2jk--version "0.2.7" "Current o2jk version installed.")

(defgroup o2jk nil "Publish org-mode posts to jekyll"
  :tag "o2jk"
  :version "0.0.3"
  :group 'org)

(defcustom o2jk-blog-author nil
  "Blog entry author."
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-source-directory nil
  "Path to the source directory."
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-directory nil
  "Path to Jekyll blog."
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-drafts-dir nil
  "Path to drafts directory relative to `o2jk-jekyll-directory`."
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-posts-dir nil
  "Path to posts directory relative to `o2jk-jekyll-directory`."
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-layouts '("post" "default")
  "Possible layouts, by default either a post or a page"
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-layout-post "post"
  "Article blog post layout"
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defcustom o2jk-jekyll-layout-page "default"
  "Article page layout, mostly intended as static pages (e.g about, contacts, etc...)"
  :type 'string
  :require 'o2jk
  :group 'o2jk)

(defvar o2jk-default-template-entries
  '(("layout")
    ("title")
    ("tags")
    ("categories"))
  "Default list of tuple '(<header-name> <default-value>).
Optionally a tuple could be in the form '(<header-name>).
Its values are initialized according to the values defined in version <= 0.2.2.")

(defun o2jk--header-entry (header-entry)
  "Given a HEADER-ENTRY, format string as org-mode header."
  (let ((header-name  (-> header-entry car s-upcase))
        (header-value (-if-let (val (cadr header-entry)) val "%s")))
    (format "#+%s: %s" header-name header-value)))

(defun o2jk--inline-headers (tuple-entries)
  "Given TUPLE-ENTRIES, format as org-mode headers."
  (->> tuple-entries
       (mapcar 'o2jk--header-entry)
       (s-join "\n")
       (format "%s\n\n")))

(defcustom o2jk-default-template-entries-extra nil
  "Allow users to define extra template headers entries."
  :type 'alist)

(defvar o2jk-jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defun o2jk--optional-folder (folder-source &optional folder-name)
  "Compute the folder name from a FOLDER-SOURCE and an optional FOLDER-NAME."
  (format "%s/%s" folder-source (if folder-name folder-name "")))

;;;###autoload
(defun o2jk-input-directory (&optional folder-name)
  "Compute the input folder from the FOLDER-NAME."
  (o2jk--optional-folder o2jk-source-directory folder-name))

;;;###autoload
(defun o2jk-output-directory (&optional folder-name)
  "Compute the output folder from the optional FOLDER-NAME."
  (o2jk--optional-folder o2jk-jekyll-directory folder-name))

(defun o2jk--make-slug (s)
  "Turn a string S into a slug."
  (->> s
       (replace-regexp-in-string "[\]\[(){}!#$~^\\]" "")
       downcase
       (replace-regexp-in-string " " "-")))

(defun o2jk--yaml-escape (s)
  "Escape a string S for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun o2jk-now ()
  "Generate a formatted now date."
  (format-time-string "%Y-%m-%d %a %H:%M"))

(defun o2jk-default-headers-template (headers)
  "Return an org metadata string with entries in HEADERS."
  (mapconcat #'o2jk--header-entry headers "\n"))

(defun o2jk--draft-filename (draft-dir filename)
  "Compute the draft's filename from the DRAFT-DIR and TITLE."
  (concat draft-dir "/" (o2jk--make-slug filename) o2jk-jekyll-post-ext))

(defun o2jk--read-title ()
  "Read the title."
  (read-string "Title: "))

(defun o2jk--read-description ()
  "Read the description."
  (read-string "Description: "))

(defun o2jk--read-tags ()
  "Read the tags."
  (let* ((tag-file "~/.emacs.d/o2jk.tags.el")
         ;; if tag file not exists, create tag file
         (_ (unless (file-exists-p tag-file)
              (with-temp-buffer
                (insert "(())")
                (append-to-file nil nil tag-file))))
         (history-tags (org-persist--read-elisp-file tag-file))
         (new-tags (completing-read-multiple "Tags (, separated values): " history-tags))
         (updated-tags (cl-union history-tags (mapcar #'list new-tags))))
    (org-persist--write-elisp-file tag-file updated-tags)
    (mapconcat #'identity new-tags " ")))

(defun o2jk--read-categories ()
  "Read the categories."
  (consult--read (--filter
                  (not (string-prefix-p "." it))
                  (directory-files o2jk-jekyll-directory))))

(defun o2jk--read-filename ()
  "Read the file name."
  (read-string "Filename: "))

(defun o2jk--input-read (prompt collection)
  "Input PROMPT with possibilities limited to COLLECTION."
  (ido-completing-read prompt
                       collection
                       nil
                       'require-match))

(defun o2jk--init-buffer-metadata (&optional ignore-plist)
  "Return an plist holding buffer metadata information collected from the user.
Any non-nil property in IGNORE-PLIST will not be collected from the user, and
will instead have its value omitted in the returned plist."
    (-concat
   ;; (unless (plist-get ignore-plist :author)
   ;;   (list :author o2jk-blog-author))
   ;; (unless (plist-get ignore-plist :date)
   ;;   (list :date (o2jk-now)))
   (unless (plist-get ignore-plist :layout)
	 (list :layout o2jk-jekyll-layout-post))
   (unless (plist-get ignore-plist :filename)
     (list :filename (o2jk--read-filename)))
   (unless (plist-get ignore-plist :title)
	 (list :title (o2jk--read-title)))
   ;; (unless (plist-get ignore-plist :description)
   ;;   (list :description (o2jk--read-description)))
   (unless (plist-get ignore-plist :tags)
	 (list :tags (o2jk--read-tags)))
   (unless (plist-get ignore-plist :categories)
	 (list :categories (o2jk--read-categories)))))

(defun o2jk--get-template-entries (&optional decided-options-alist)
  "Return the contents of o2jk-default-template-entries and
o2jk-default-template-entries-extra replaced by entries is
DECIDED-OPTIONS-ALIST."
  (let ((-compare-fn #'string=)
	(decided-options (-map #'car decided-options-alist)))
(--map-when (and (not (cdr it)) (-contains? decided-options (car it)))
	    (-first (lambda (option) (string= (car it) (car option)))
		    decided-options-alist)
	    (-concat o2jk-default-template-entries
		     o2jk-default-template-entries-extra))))

(defun o2jk--get-filtered-entries (entries ignore-list)
  "Return a list from ENTRIES whose car is not present in IGNORE-LIST."
  (let ((-compare-fn #'string=))
    (--filter (not (-contains? ignore-list (car it))) entries)))

;;;###autoload
(defun o2jk-init-current-buffer ()
  "Given an existing buffer, add the needed metadata to make it a post or page."
  (interactive)
  (let* ((existing-options-plist (o2jk-get-options-from-buffer))
	 (existing-options-alist (o2jk--plist-to-alist existing-options-plist))
	 (metadata (o2jk--plist-to-alist
		    (o2jk--init-buffer-metadata existing-options-plist)))
	 ;; Drop options known to already be in the buffer
	 (ignore-list (-map #'car existing-options-alist))
	 (add-to-file-options (o2jk--get-filtered-entries
			       (o2jk--get-template-entries metadata) ignore-list))
	 (add-to-file-tuples (o2jk--alist-to-tuples add-to-file-options)))
    (save-excursion
      (with-current-buffer (buffer-name)
        (goto-char (point-min))
	(let* ((no-options (length existing-options-alist))
	       (non-metadata-present-p (> (count-lines (point-min) (point-max))
					  no-options)))
	  ;; Ensure we insert after any existing options
	  (forward-line (length existing-options-alist))
	  ;; If the buffer ends at the end of existing options, insert a new line
	  (when (and (> no-options 0) (not (char-equal ?\n (char-before))))
	    (insert "\n"))
	  (insert (o2jk-default-headers-template add-to-file-tuples) "\n")
	  ;; Create line between metadata and non-metadata if non-metadata exists
	  (when (and (= no-options 0) non-metadata-present-p) (insert "\n")))))))

;;;###autoload
(defun o2jk-create-draft ()
  "Prompt the user for org metadata and then create a new Jekyll blog post.
The specified title will be used as the name of the file."
  (interactive)
  (let* ((metadata (o2jk--init-buffer-metadata))
	     (metadata-alist (o2jk--plist-to-alist metadata))
         (category (plist-get metadata :categories))
	     (filename (plist-get metadata :filename))
         (draft-file  (o2jk--draft-filename
                       o2jk-jekyll-drafts-dir
                       filename))
	     (add-to-file-options (o2jk--get-template-entries metadata-alist))
	     (add-to-file-tuples (o2jk--alist-to-tuples add-to-file-options)))
    (unless (file-exists-p draft-file)
      (with-temp-file draft-file
        (insert (o2jk-default-headers-template add-to-file-tuples) "\n\n")))
    (find-file draft-file)
    (end-of-buffer)))

(defun o2jk--list-dir (dir)
  "List the content of DIR."
  (find-file dir))

;;;###autoload
(defun o2jk-list-posts ()
  "Lists the posts folder."
  (interactive)
  (o2jk--list-dir
   (o2jk-output-directory o2jk-jekyll-posts-dir)))

;;;###autoload
(defun o2jk-list-source ()
  "Lists the posts source folder."
  (interactive)
  (o2jk--list-dir
   o2jk-source-directory))


;;;###autoload
(defun o2jk-list-drafts ()
  "List the drafts folder."
  (interactive)
  (o2jk--list-dir
   o2jk-jekyll-drafts-dir))

(defun o2jk-get-options-from-buffer ()
  "Return special lines at the beginning of current buffer."
  (let ((special-line-regex "^#\\+\\(.+\\):[ \t]+\\(.*\\)$")
        (get-current-line (lambda ()
                            (buffer-substring-no-properties (line-beginning-position)
                                                            (line-end-position))))
        (options-plist))
    (save-excursion
      (goto-char (point-min))
      ;; skip drawer
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0))
        (forward-line))
      (catch 'break
        (while (string-match special-line-regex (funcall get-current-line))
          (let ((current-line (funcall get-current-line)))
            (unless (s-blank-str-p (match-string 2 current-line))
              (setq options-plist (plist-put options-plist
                                             (->> current-line
                                                  (match-string 1)
                                                  downcase
                                                  (concat ":")
                                                  intern)
                                             (match-string 2 current-line)))))
          (unless (= 0 (forward-line))
            (throw 'break nil))))
      options-plist)))

(defun o2jk--without-option-p (option &optional options)
  "Determine if OPTION needs to be deactivated amongst options."
  ;; FIXME: Find the proper org implementation call ¯\_(ツ)_/¯
  (let ((properties (-> (if options options (o2jk-get-options-from-buffer))
                        (plist-get :options))))
    (when properties
      (let ((off-option (format "%s:nil" option)))
        (->> properties
             (s-split " ")
             (--filter (string= off-option it)))))))

(defun o2jk--with-tags-p (options)
  "Determine, from OPTIONS if we need to export in yaml the tags options"
  (-> "tags"
      (o2jk--without-option-p options)
      not))

(defun o2jk-get-options-from-file (orgfile)
  "Return special lines at the beginning of ORGFILE."
  (with-temp-buffer
    (when (file-exists-p orgfile)
      (insert-file-contents orgfile)
      (o2jk-get-options-from-buffer))))

(defun o2jk-article-p (org-file)
  "Determine if the current ORG-FILE's layout.
Depends on the metadata header #+LAYOUT."
  (plist-get (o2jk-get-options-from-file org-file) :layout))

(defun o2jk--org-to-jekyll-metadata (org-metadata)
  "Given an ORG-METADATA map, translate Org keywords to Jekyll keywords."
  (let ((o2jk-map-keys '(("description" . "excerpt"))))
    (--map (-if-let (jekyll-car (assoc-default (car it) o2jk-map-keys))
               (cons jekyll-car (cdr it))
             it)
           org-metadata)))

(defun o2jk--convert-timestamp-to-yyyy-dd-mm-hh (timestamp)
  "Convert org TIMESTAMP to YYYY-MM-DD HH:MM. For yaml header purposes."
  (format-time-string "%Y-%m-%d %H:%M"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun o2jk--convert-timestamp-to-yyyy-dd-mm (timestamp)
  "Convert org TIMESTAMP to to YYYY-MM-DD. For filename renaming purposes."
  (format-time-string "%Y-%m-%d"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun o2jk--to-yaml-header (org-metadata)
  "Given a list of ORG-METADATA, compute the yaml header string."
  (--> (o2jk--org-to-jekyll-metadata org-metadata)
       (--map (format "%s: %s" (car it) (cdr it)) it)
       (cons "---" it)
       (-snoc it "---\n")
       (s-join "\n" it)))

(defun o2jk--space-separated-values-to-yaml (str)
  "Transform a STR of space separated values entries into yaml entries."
  (concat "["
          (->> (if str str "")
               (s-split " ")
               (--filter (unless (equal it "") it))
               (--map (format  "%s" it))
               (s-join ","))
          "]"))

(defun o2jk--compute-ready-jekyll-file-name (date org-file)
  "Given a DATE and an ORG-FILE, compute a ready jekyll file name.
If the current path contains the `'o2jk-jekyll-drafts-dir`', removes it."
  (let* ((temp-org-jekyll-filename (format "%s-%s" date
                                           (file-name-nondirectory org-file))))
    (->> temp-org-jekyll-filename
         (format "%s/%s" o2jk-source-directory)
         (replace-regexp-in-string (format "%s" o2jk-jekyll-drafts-dir) "")
         (replace-regexp-in-string "//" "/"))))

(defconst o2jk-required-org-header-alist '((:title       . 'required)
                                           (:categories  . 'required)
                                           (:tags)
                                           (:layout      . 'required)
                                           (:date        . 'required))
  "Map of required org headers for jekyll to accept rendering.")

(defun o2jk-check-metadata (org-metadata)
  "Check that all required headers in ORG-METADATA are provided.
Return error messages for any required headers that are missing,
and nil if no problems are found."
  (let ((required-options (funcall (-compose (lambda (l) (mapcar #'car l))
                                             (lambda (l) (-filter #'cdr l)))
                                   o2jk-required-org-header-alist)))
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

(defun o2jk--symbol-to-string (symbol)
  "Make a string out of a SYMBOL.
symbol is of the form ':<name>'"
  (let ((s (if (stringp symbol) symbol (symbol-name symbol))))
    (substring s 1 nil)))

(defun o2jk--plist-to-alist (plist)
  "Make an alist out of a PLIST."
  (--map
   (let ((key (-> it
                  car
                  o2jk--symbol-to-string))
         (value (cadr it)))
     `(,key . ,value))
   (-partition 2 plist)))

(defun o2jk--alist-to-tuples (alist)
  "Return a list of tuples with values from ALIST.
Any values for which -cons-pair? returns nil are left unchanged."
  (--map (cond
	  ((and (-cons-pair? it) (cdr it)) (list (car it) (cdr it)))
	  (t it))
	 alist))

(defun o2jk-remove-org-only-options (yaml-alist)
  "Filter out org options with no Jekyll meaning from YAML-ALIST."
  (let* ((required-options (--map (-> it car o2jk--symbol-to-string)
                                  o2jk-required-org-header-alist))
         (org-options (--map (downcase (substring it 0 -1))
                             org-options-keywords))
         (org-only-options (--filter (not (member it required-options))
                                     org-options))
         (jekyll-options (--filter (not (member (car it) org-only-options))
                                   yaml-alist)))
    jekyll-options))

(defun o2jk-read-metadata (org-file)
  "Given an ORG-FILE, return its org metadata.
It can display an error message about missing required values."
  (let* ((buffer-metadata (o2jk-get-options-from-file org-file)))
    (-if-let (error-messages (o2jk-check-metadata buffer-metadata))
        (format "This org-mode file is missing required header(s):
%s
Publication skipped" error-messages)
      (let* ((org-defaults `(:date ,(o2jk-now) :author ""))
             (merged-metadata (org-combine-plists org-defaults buffer-metadata))
             (categories (o2jk--space-separated-values-to-yaml
                          (plist-get merged-metadata :categories)))
             (tags (if (o2jk--with-tags-p buffer-metadata)
                       (o2jk--space-separated-values-to-yaml
                        (plist-get merged-metadata :tags))
                     ""))
             (date (o2jk--convert-timestamp-to-yyyy-dd-mm
                    (plist-get merged-metadata :date)))
             (yaml-metadata (-> merged-metadata
                                (plist-put :categories categories)
                                (plist-put :tags tags)
                                (plist-put :date date)))
             (yaml-alist (o2jk--plist-to-alist yaml-metadata)))
        (o2jk-remove-org-only-options yaml-alist)))))

(defun o2jk-read-metadata-and-execute (action-fn org-file)
  "Execute ACTION-FN function after checking metadata from the ORG-FILE."
  (let ((filename-non-dir (file-name-nondirectory org-file)))
    (if (o2jk-article-p org-file)
        (let ((org-metadata (o2jk-read-metadata org-file)))
          (if (stringp org-metadata)
              (o2jk-message org-metadata)
            (let ((page-or-post (if (o2jk-post-p
                                     (assoc-default "layout" org-metadata))
                                    "Post"
                                  "Page")))
              (funcall action-fn org-metadata org-file)
              (format "%s '%s' published!" page-or-post filename-non-dir))))
      (format "'%s' is not an article, publication skipped!" filename-non-dir))))

(defun o2jk-message (&rest args)
  "Log formatted ARGS."
  (apply 'message (format "o2jk - %s" (car args)) (cdr args)))

(defun o2jk--publish-temp-file-then-cleanup (org-file temp-file project)
  "Publish ORG-FILE using TEMP-FILE (with yaml header) using PROJECT metadata."
  (copy-file org-file temp-file 'overwrite 'keep-time 'preserve-ids 'preserve-perms)
  (with-temp-file temp-file
    ;; activate o2jk-mode to rely on its hook to work properly
    (o2jk-mode)
    (org-publish-file temp-file project))
  ;; the o2jk installed hook should have kicked-in already, if it remains
  ;; dangling temporary file, just delete it
  (when (file-exists-p temp-file)
    (delete-file temp-file)))

(defun o2jk--publish-post-org-file-with-metadata (org-metadata org-file)
  "Publish as post with ORG-METADATA the ORG-FILE."
  (let* ((project      (-> "layout"
                           (assoc-default org-metadata)  ;; layout is the blog-project
                           (assoc org-publish-project-alist)))
         (published (->> org-file
                         (string-replace o2jk-source-directory
                                         o2jk-jekyll-directory)
                         (string-replace (expand-file-name o2jk-source-directory)
                                         (expand-file-name o2jk-jekyll-directory))
                         (replace-regexp-in-string ".org$" ".html"))))
    (org-publish-file org-file project)
    (let ((yaml-headers (-> org-file
                            o2jk-read-metadata
                            o2jk--to-yaml-header)))
      (with-temp-file published
        (insert-file-contents published)
        (goto-char (point-min))
        (insert yaml-headers)))))

(defun o2jk-publish-post (org-file)
  "Publish ORG-FILE as a post."
  (o2jk-read-metadata-and-execute
   'o2jk--publish-post-org-file-with-metadata
   org-file))

;; (defun o2jk-install-yaml-headers (original-file published-file)
;;   "Read ORIGINAL-FILE metadata and install yaml header to PUBLISHED-FILE.
;; Then delete the original-file which is intended as a temporary file.
;; Only for org-mode file, for other files, it's a noop.
;; This function is intended to be used as org-publish hook function."
;;   (let ((original-file-ext (file-name-extension original-file))
;;         (published-file-ext (file-name-extension published-file)))
;;     ;; original-file is the temporary file generated which will be edited with
;;     ;; jekyll's yaml headers

;;     ;; careful about extensions: "post" -> org ; page -> o2jk
;;     ;; other stuff are considered neither, so it's a noop
;;     (when (and (or (string= "txt" original-file-ext) (string= "o2jk" original-file-ext))
;;                (string= "html" published-file-ext))
;;       (let ((yaml-headers (-> original-file
;;                               o2jk-read-metadata
;;                               o2jk--to-yaml-header)))
;;         (with-temp-file published-file
;;           (insert-file-contents published-file)
;;           (goto-char (point-min))
;;           (insert yaml-headers))))))

(defun o2jk--publish-page-org-file-with-metadata (org-metadata org-file)
  "Publish as page with ORG-METADATA the ORG-FILE."
  (let* ((project      (-> "layout"
                           (assoc-default org-metadata)  ;; layout is the blog-project
                           (assoc org-publish-project-alist)))
         (filename     (file-name-nondirectory org-file))
         (ext          (file-name-extension filename))
         (temp-file    (format "%s/%so2jk"
                               (s-chop-suffix "/" o2jk-source-directory)
                               (s-chop-suffix ext filename))))
    (o2jk--publish-temp-file-then-cleanup org-file temp-file project)))

(defun o2jk-publish-page (org-file)
  "Publish ORG-FILE as a page."
  (o2jk-read-metadata-and-execute
   'o2jk--publish-page-org-file-with-metadata
   org-file))

(defun o2jk-post-p (layout)
  "Determine if the LAYOUT corresponds to a post."
  (string= o2jk-jekyll-layout-post layout))

(defun o2jk-page-p (layout)
  "Determine if the LAYOUT corresponds to a page."
  (string= o2jk-jekyll-layout-page layout))

(defun o2jk-publish-web-project ()
  "Publish the 'web' project."
  (let ((result (org-publish-project "web")))
    (o2jk-message "Publish `'web`' project (images, css, js, etc...) done.")
    result))

(defun o2jk-publish-from-jekyll (org-file)
  (let* ((org-options (with-current-buffer (current-buffer) (o2jk-get-options-from-buffer)))
         (publish-fn (-> (plist-get org-options :layout)
                         o2jk-post-p
                         (if 'o2jk-publish-post
                             'o2jk-publish-page)))
         (final-message (funcall publish-fn org-file)))
    (progn
      (o2jk-publish-web-project)
      (o2jk-message final-message)
      (magit-status-setup-buffer o2jk-source-directory))))

;;;###autoload
(defun o2jk-publish ()
  "Publish the current org file as post or page depending on the chosen layout.
Layout `'post`' is a jekyll post.
Layout `'default`' is a page (depending on the user customs)."
  (interactive)
  (let* ((buffer (current-buffer))
         (org-file (buffer-file-name (current-buffer)))
         (filepath (file-name-directory org-file))
         (date-property (plist-get (o2jk-get-options-from-buffer) :date)))
    ;; add date property if missing
    (when (not date-property)
      (let ((date-string (o2jk--convert-timestamp-to-yyyy-dd-mm
                          (format-time-string "%Y-%m-%d %H:%M"))))
        (save-excursion
          (with-current-buffer buffer
            (goto-char (point-min))
            (insert (format "#+DATE: %s\n" date-string))
            (save-buffer)))))
    (if (string-prefix-p (expand-file-name o2jk-source-directory)
                         filepath)
        (o2jk-publish-from-jekyll org-file)
      (let* ((filename (file-name-nondirectory org-file))
             (movefile (concat
                        o2jk-source-directory "/"
                        (plist-get (o2jk-get-options-from-buffer) :categories) "/"
                        (format-time-string "%Y-%m-%d-") filename)))
        (rename-file buffer-file-name movefile)
        (switch-to-buffer (find-file-noselect movefile))
        (o2jk-publish-from-jekyll movefile)))))

(defvar o2jk-mode-map nil "Default Bindings map for o2jk mode.")

(setq o2jk-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c . n") 'o2jk-create-draft)
        (define-key map (kbd "C-c . p") 'o2jk-publish)
        (define-key map (kbd "C-c . P") 'o2jk-publish-posts)
        (define-key map (kbd "C-c . l") 'o2jk-list-posts)
        (define-key map (kbd "C-c . d") 'o2jk-list-drafts)
        map))

;;;###autoload
(defun o2jk-publish-posts ()
  "Publish all posts."
  (interactive)
  (->> (assoc o2jk-jekyll-layout-post org-publish-project-alist)
       org-publish-get-base-files
       (--filter (-> it o2jk-article-p o2jk-post-p))
       (mapc #'o2jk-publish-post)))

;;;###autoload
(defun o2jk-publish-pages ()
  "Publish all pages."
  (interactive)
  (->> (assoc o2jk-jekyll-layout-page org-publish-project-alist)
       org-publish-get-base-files
       (--filter (-> it o2jk-article-p o2jk-page-p))
       (mapc #'o2jk-publish-page)))

(defun o2jk-version ()
  "Little version helper"
  (interactive)
  (let ((version o2jk--version))
    (message "o2jk-version: %s" version)
    version))

(defun o2jk--bug-report ()
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
         ,(format "- o2jk version: %s" o2jk--version)
         ,(format "- o2jk path: %s" (find-library-name "o2jk")))
       (s-join "\n")))

(defun o2jk-bug-report (&optional open-url)
  "Display a bug report message.
When OPEN-URL is filled, with universal argument (`C-u') is used,
opens new issue in org-trello's github tracker."
  (interactive "P")
  (when open-url
    (browse-url "https://github.com/ardumont/o2jk/issues/new"))
  (message (o2jk--bug-report)))


;;;###autoload
(define-minor-mode o2jk-mode
  "Functionality for publishing the current org-mode post to jekyll.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{o2jk-mode-map}"

  :init-value nil
  :lighter " o2j"
  :group 'o2jk
  :keymap o2jk-mode-map)

;; install/uninstall hook when activating/deactivating o2jk-mode
;; o2jk inserts the yaml when the publishing step is done, so now org
;; does all the publishing in concordance to what the user expects and we do
;; our update after it so Jekyll is happy.

(defvar o2jk-mode-on-hook nil "o2jk starting hook")
(setq o2jk-mode-off-hook nil) ;; for dev
;; install o2jk hook in org-publish when activating o2jk-mode
;; (add-hook 'o2jk-mode-on-hook
;;           (lambda ()
;;             (add-hook 'org-publish-after-publishing-hook 'o2jk-install-yaml-headers)))

(defvar o2jk-mode-off-hook '() "o2jk stoping hook")
(setq o2jk-mode-off-hook nil) ;; for dev
;; uninstall hook on org-publish
;; (add-hook 'o2jk-mode-off-hook
;;           (lambda () (remove-hook 'org-publish-after-publishing-hook 'o2jk-install-yaml-headers)))

;; Extend org-mode hyperlinks policy with a "local" link so we can publish
;; internal links which are then browsable when published
;; https://orgmode.org/manual/Adding-Hyperlink-Types.html#Adding-Hyperlink-Types

;; register new local links so org-mode exports them as is
(org-link-set-parameters "local" :export #'o2jk-local-link-export)

(defun o2jk-local-link-export (link description format)
  "Export a man page link from Org files."
  (let ((desc (or description link)))
    (if (string= format "html")
        (format "<a href=\"%s\">%s</a>" link desc)
      (o2jk-message "Unknown format %s, only dealing with html" format))))

(provide 'o2jk)
;;; o2jk.el ends here
