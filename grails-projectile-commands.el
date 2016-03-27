;;; grails-projectile-commands.el --- Functions and commands for Grails Projectile.

;; Copyright (C) 2013-2016 Rimero Solutions and contributors.

;; Author: Yves Zoundi <rimerosolutions@gmail.com>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'grails-projectile-config)

(defun grails-projectile-version ()
  (interactive)
  grails-projectile-version-info)

;;;_. Utilities
(defun grails-projectile--join-lines (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(defun grails-projectile--read-grails-options-projectile-file (file-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (mark-whole-buffer)
    (grails-projectile--join-lines (point-min) (point-max))
    (buffer-string)))

;;;_. Wizard functions
(defun grails-projectile-wizard-new-app ()
  "Create a new application project."
  (interactive)
  (grails-projectile--wizard-new-app-or-plugin "create-app"))

(defun grails-projectile-integrate-with (grails-tool-or-editor)
  "Integrate Grails with an IDE or Build System `grails-tool-or-editor'."
  (interactive
   (let ((grails-tools-or-editors '("Eclipse" "Intellij" "Textmate" "Ant" "Git")))
     (list (completing-read "Select IDE or Build System: " grails-tools-or-editors))))
  (let ((grails-projectile-jvm-opts ""))
    (grails-projectile--command (concat "integrate-with --" (downcase grails-tool-or-editor)))))

(defun grails-projectile-wizard-new-plugin ()
  "Create a new plugin project."
  (interactive)
  (grails-projectile--wizard-new-app-or-plugin "create-plugin"))

(defun grails-projectile--wizard-new-app-or-plugin (cmd)
  "Create a new application or plugin.
`cmd' is either create-app or create-plugin."
  (let ((insert-default-directory  t))
    ;; Ask the user for the project folder
    (let* ((grails-project-folder (read-directory-name "Application Directory: " default-directory))
           (app-name              (read-from-minibuffer "Application Name: "))
           (default-directory     (file-name-as-directory grails-project-folder))
           (grails-command        grails-projectile-executable)
           (grails-arguments      (concat cmd " --inplace " app-name)))

      ;; Create the project folder.
      (unless (file-exists-p default-directory)
        (make-directory default-directory t))

      ;; Create the .projectile file in the new project folder.
      (grails-projectile--create-grails-projectile-file default-directory)

      ;; Generate the Grails app or plugin in-place inside the new project folder.
      (let ((grails-command-line (concat grails-command " " grails-arguments)))
        (compilation-start grails-command-line
                           'compilation-mode
                           'grails-projectile--get-compilation-buffer-name)))))

(defun grails-projectile--create-grails-projectile-file (dir)
  "Add the default .projectile file after creating a new app or plugin."
  (with-temp-file (concat dir ".projectile")
    (insert "-/target")))

;;;; File locator functions
(defun grails-projectile--find-grails-file (grails-proj-folder pred-fn-sym file-basename &optional no-auto-open)
  "Find a Grails file in a project folder.
`grails-proj-folder' is the base search folder.
`pred-fn-sym' is the function to filter project files.
`file-basename' is the filename to search without extension.
`no-auto-open' Do not open the file automatically for a single result."
  (let ((result-list (grails-projectile--find-grails-files grails-proj-folder file-basename pred-fn-sym)))
    (if result-list
        (if (= (length result-list) 1)
            (if (not no-auto-open)
                (find-file (concat (projectile-project-root) (car result-list)))
              (let* ((file-list     (mapcar #'(lambda(p) (concat (projectile-project-root) p)) result-list))
                     (selected-file (completing-read "Select a file:" file-list)))
                (find-file selected-file)))
          (let* ((file-list     (mapcar #'(lambda(p) (concat (projectile-project-root) p)) result-list))
                 (selected-file (completing-read "Select a file:" file-list)))
            (find-file selected-file)))
      (message "No artefact found for %s in '%s'" file-basename grails-proj-folder))))

(defun grails-projectile--find-grails-files (dirname file-basename pred-fn)
  "Jump to a filename from a given base folder."
  (let ((folder-files (projectile-files-in-project-directory dirname)))
    (cl-loop for project-file in folder-files
             when (funcall pred-fn (file-name-base project-file) file-basename)
             collect project-file)))

(defun grails-projectile--base-name-matches-p (value expected)
  "Match two strings."
  (string= expected value))

(defun grails-projectile--test-matches-p (value expected)
  "Test whether a file basename matches a test class."
  (or (string= (concat expected "Tests") value)
      (string= (concat expected "Spec") value)))

(defun grails-projectile--all-files (value expected) "Test whether a file should be opened." t)

(defun grails-projectile--artefact-name-no-suffix (file-basename)
  "Return the Grails artefact name without its suffix
`file-basename' is the full basename of the file such as TestController.
The transformation of TestControllerSpec would remove both Spec and Controller
from the basename and return only Test."
  (let ((artefact-name file-basename)
        (artifact-suffixes '("Spec" "Tests" "Service" "Controller" "TagLib" "Command")))

    (dolist (elt artifact-suffixes)
      (when (string-match (concat elt "$") artefact-name)
        (setq artefact-name (substring artefact-name 0 (- (length artefact-name) (length elt))))))

    artefact-name))

(defun grails-projectile--find-artefact (artefact-folder artefact-suffix &optional artefact-full-name)
  "Finds a Grails artefact in a given folder by suffix.
`artefact-folder' is the Grails sub-folder to look at usually inside grails-app.
`artefact-suffix' is a suffix convention such as Controller, Service when applicable.
`artefact-full-name' refers to the full basename of the file to search or the current buffer filename."
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" artefact-folder)
                                       'grails-projectile--base-name-matches-p
                                       (or artefact-full-name
                                           (concat (grails-projectile--artefact-name-no-suffix (file-name-base (buffer-file-name)))
                                                   artefact-suffix))))

;;;_. Artefact location commands
(defun grails-projectile-locate-test ()
  "Locate a test class in the project."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "test")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-find-test-for-file ()
  "Find a test class associated with the current file."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "test")
                                       'grails-projectile--test-matches-p
                                       (file-name-base (buffer-file-name))))

(defun grails-projectile-locate-view ()
  "Locate a view in the project."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" "views")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-locate-service ()
  "Locate a service class in the project."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" "services")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-find-service-for-file ()
  "Find a service class associated with the current file."
  (interactive)
  (grails-projectile--find-artefact "services" "Service"))

(defun grails-projectile-locate-controller ()
  "Locate a controller class in the project."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" "controllers")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-find-controller-for-file ()
  "Find a controller class associated with the current file."
  (interactive)
  (grails-projectile--find-artefact "controllers" "Controller"))

(defun grails-projectile-locate-domain ()
  "Locate a domain class in the project."
  (interactive)

  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" "domain")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-find-domain-for-file ()
  "Find a domain class associated with the current file."
  (interactive)
  (grails-projectile--find-artefact "domain" ""))


(defun grails-projectile-locate-taglib ()
  "Locate a taglib class in the project."
  (interactive)
  (grails-projectile--find-grails-file (grails-projectile--path-from-project-root "grails-app" "taglib")
                                       'grails-projectile--all-files
                                       ""
                                       t))

(defun grails-projectile-find-taglib-for-file ()
  "Find a taglib class associated to the current file."
  (interactive)
  (grails-projectile--find-artefact "taglib" "TagLib"))

;; --------------------------------
;; Folder helper functions
;; --------------------------------
(defun grails-projectile--path-from-project-root(&rest path-elements)
  (cl-reduce '(lambda (x &optional y)
                (concat (file-name-as-directory x) y))
             path-elements :initial-value (projectile-project-root)))


(defun grails-projectile--wrapper-exists-p (folder-name)
  "Check whether the Grails wrapper exist in a given folder."
  (file-exists-p (concat folder-name grails-projectile-wrapper-filename grails-projectile-executable-suffix)))

(defun grails-projectile--get-cmd (grails-command)
  "Generate the grails command line string."
  (let ((default-directory     (expand-file-name (projectile-project-root)))
        (grails-args           (concat grails-projectile-jvm-opts " " grails-projectile-cmd-opts))
        (grails-cmd-line       (if (and grails-projectile-use-grails-wrapper
                                        (grails-projectile--wrapper-exists-p default-directory))
                                   (concat default-directory
                                           grails-projectile-wrapper-filename
                                           grails-projectile-executable-suffix)
                                 grails-projectile-executable))
        (grails-cmd-extra-args (if (file-exists-p (concat default-directory grails-projectile-filename))
                                   (let ((grails-projectile-file (concat default-directory
                                                                         grails-projectile-filename)))
                                     (grails-projectile--read-grails-options-projectile-file grails-projectile-file))
                                 " ")))
    (mapconcat #'identity
               (list grails-cmd-line
                     grails-projectile-output-opts
                     grails-args
                     grails-cmd-extra-args
                     grails-command)
               " ")))

;;;_. Main functions
(defun grails-projectile--command (cmd)
  "Run a Grails command."
  (let ((grails-command-line (grails-projectile--get-cmd cmd))
        (default-directory   (expand-file-name (projectile-project-root))))
    (compilation-start grails-command-line
                       'compilation-mode
                       'grails-projectile--get-compilation-buffer-name)))

(defun grails-projectile--get-compilation-buffer-name (mode)
  "The buffer name to use for Grails Commands."
  grails-projectile-compilation-buffer-name)

(defun grails-projectile--read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command."
  (interactive)
  (let ((grails-command-argument (read-from-minibuffer input-hint)))
    (grails-projectile--command (concat grails-command " " grails-command-argument))))

(defun grails-projectile-generate-all ()
  "Create all artifacts for a Grails Domain Class."
  (interactive)
  (grails-projectile--read-param-and-run "Generate all for Domain class:" "generate-all"))

(defun grails-projectile-generate-views ()
  "Create views for a Grails Domain Class."
  (interactive)
  (grails-projectile--read-param-and-run "Generate views for Domain class:" "generate-views"))

(defun grails-projectile-generate-controller ()
  "Create a controller for a Grails Domain Class."
  (interactive)
  (grails-projectile--read-param-and-run "Generate controller for Domain class:" "generate-controller"))

;;;_. General commands
(defun grails-projectile-icommand ()
  "Enter a Grails command."
  (interactive)
  (grails-projectile--read-param-and-run "Goal:" ""))

(defun grails-projectile-create-domain ()
  "Create a Grails Domain Class."
  (interactive)
  (grails-projectile--read-param-and-run "Domain class:" "create-domain-class"))

(defun grails-projectile-create-controller ()
  "Create a Grails Controller."
  (interactive)
  (grails-projectile--read-param-and-run "Controller for class:" "create-controller"))

(defun grails-projectile-create-service ()
  "Create a Grails Service."
  (interactive)
  (grails-projectile--read-param-and-run "Service for class:" "create-service"))

(defun grails-projectile-create-taglib ()
  "Create a Grails Taglib."
  (interactive)
  (grails-projectile--read-param-and-run "TagLib Name:" "create-tag-lib"))

(defun grails-projectile-create-unit-test ()
  "Create a Grails Taglib."
  (interactive)
  (grails-projectile--read-param-and-run "Unit test for class:" "create-unit-test"))

;;;_. Plugin commands
(defun grails-projectile-plugins-list-installed ()
  "List Grails installed plugins."
  (interactive)
  (grails-projectile--command "list-plugins -installed"))

(defun grails-projectile-plugins-package-plugin ()
  "Package a Grails plugin."
  (interactive)
  (grails-projectile--command "package-plugin"))

;;;_. Other grails commands
(defun grails-projectile-compile ()
  "Compile."
  (interactive)
  (grails-projectile--command "compile"))

(defun grails-projectile-run-app ()
  "Run the application."
  (interactive)
  (grails-projectile--command "run-app"))

(defun grails-projectile-clean ()
  "Clean."
  (interactive)
  (grails-projectile--command "clean"))

(defun grails-projectile-refresh-dependencies ()
  "Refresh Grails Dependencies."
  (interactive)
  (grails-projectile--command "refresh-dependencies"))

;;;_. Documentation browsing commands
(defun grails-projectile-browse-wiki-docs ()
  "Browse the Wiki Documentation."
  (interactive)
  (if (boundp 'grails-projectile-url-wikidocs)
      (browse-url grails-projectile-url-wikidocs)
    (message "No Grails Wikidocs set. Customize the 'grails-projectile' group")))

(defun grails-projectile-browse-api-docs ()
  "Browse the API Documentation."
  (interactive)
  (if (boundp 'grails-projectile-url-apidocs)
      (browse-url grails-projectile-url-apidocs)
    (message "No Grails API URL set. Customize the 'grails-projectile' group")))

(defun grails-projectile--search-plugin (base-url query-string)
  "Search Grails plugins."
  (browse-url (url-encode-url (concat base-url query-string))))

(defun grails-projectile-search-plugin-query (query-string)
  "Search Grails plugins by query string."
  (interactive "sPlugin name or query: \n")
  (if (boundp 'grails-projectile-plugins-base-url)
      (grails-projectile--search-plugin grails-projectile-plugins-base-url query-string)
    (message "No Grails plugins base URL set. Customize the 'grails-projectile' group")))

(defun grails-projectile-search-plugin-tag (query-string)
  "Search Grails plugins."
  (interactive "sPlugin tag: \n")
  (if (boundp 'grails-projectile-plugins-base-url)
      (grails-projectile--search-plugin (concat grails-projectile-plugins-base-url "tag/") query-string)
    (message "No Grails plugins base URL set. Customize the 'grails-projectile' group")))

(defun grails-projectile-browse-latest-guide ()
  "Browse the official Grails Guide."
  (interactive)
  (if (boundp 'grails-projectile-url-guide)
      (browse-url grails-projectile-url-guide)
    (message "No Grails URL guide set. Customize the 'grails-projectile' group")))


(provide 'grails-projectile-commands)

;;; grails-projectile-commands.el ends here
