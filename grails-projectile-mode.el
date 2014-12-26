;;; grails-projectile-mode.el --- Grails mode with Projectile for projects management.

;; Copyright (C) 2013,2014 Rimero Solutions

;; Version: 1.1.0
;; Keywords: grails, projectile
;; Homepage: https://github.com/yveszoundi/grails-projectile-mode
;; Author: Yves Zoundi <rimerosolutions@gmail.com>
;; Maintainer: Yves Zoundi
;; Package-Requires: ((projectile "0.10.0") (emacs "24") (cl-lib "0.5"))
;; Contributors: The internet and people who surf it.
;; Last updated: 2014-12-26

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

;;; Commentary:

;;  Emacs Grails mode with Projectile for project-management.
;;    - You can run pre-defined or arbitrary Grails commans for a project.
;;    - You can also search service, domain or controller files against the current file or project.
;;    - You can browse documentation (wiki, guide, apidocs).
;;    - You can search plugins by tag or query string.
;;    - Menubar contributions if you make use of the menubar.
;;    - The default keymap prefix is `C-c ;` (see `grails-projectile-keymap-prefix`)
;;
;; You can customize the mode using `M-x customize-group` [RET] grails-projectile.
;;
;; Add the folder containing grails-projectile-mode.el in your load-path
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;;
;; (require 'grails-projectile-mode)
;; (grails-projectile-global-mode t)
;;
;; All the commands start with 'grails-'
;; From a projectile managed buffer run `M-x grails-projectile-compile [RET]`
;; to compile your Grails application.
;;
;; To list keybindings press `C-h b` or type `M-x describe-mode`
;; Then search for grails-projectile-mode.

;;; Code:

(mapc #'require '(projectile cl-lib))

(defgroup grails-projectile nil
  "Projectile utilities for Grails projects."
  :prefix "grails-projectile"
  :group 'projectile)

;;;_. Customizations
(defcustom grails-projectile-keymap-prefix (kbd "C-c ;")
  "Grails Projectile keymap prefix."
  :group 'grails-projectile
  :type 'string)

(defcustom grails-projectile-mode-line " Grails"
  "Grails projectile modeline."
  :type 'string
  :group 'grails-projectile)

(defconst grails-projectile-executable-suffix
  (if (eq system-type 'windows-nt)
      ".bat" "")
  "Suffix for the Grails executable file.")

(defcustom grails-projectile-compilation-buffer-name "*Grails*"
  "Buffer name for Grails commands."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-use-grails-wrapper t
  "Use the Grails wrapper whenever available."
  :type 'boolean
  :group 'grails-projectile)

(defcustom grails-projectile-output-opts ""
  "Output options such as --plain-output."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-cmd-opts "--non-interactive --stacktrace"
  "Grails command line options."
  :type 'string
  :group 'grails-projectile)

(defconst grails-projectile-wrapper-filename "grailsw" "Grails Wrapper file name.")

(defcustom grails-projectile-filename ".grails-projectile"
  "Project file to define custom grails command and JVM options.
   The contents of this file override both grails-projectile-cmd-opts and grails-jvm-opts.
   Everything must hold within a single line, no newline at the end of the file."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-jvm-opts "-DXmx1g"
  "Grails command line options"
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-executable "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-url-wikidocs "http://grails.org/Documentation"
  "Grails Wiki documentation URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-url-apidocs "http://grails.org/doc/latest/api/"
  "Grails documentation URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-plugins-base-url "http://grails.org/plugins/"
  "Grails plugins base URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-projectile-url-guide "http://grails.org/doc/latest/guide/single.html"
  "Grails Latest Guide URL."
  :type 'string
  :group 'grails-projectile)

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
  (grails-projectile--read-param-and-run "Controller Domain class:" "create-controller"))

(defun grails-projectile-create-service ()
  "Create a Grails Service."
  (interactive)
  (grails-projectile--read-param-and-run "Service Domain class:" "create-service"))

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

;;;_. Discover integration
(when (featurep 'discover)
  (defun grails-projectile-turn-on-discover-support ()
    (interactive)
    (defalias 'discover-grails-projectile-main          'makey-key-mode-popup-grails-projectile-discover-main)
    (defalias 'discover-grails-projectile-create        'makey-key-mode-popup-grails-projectile-discover-create)
    (defalias 'discover-grails-projectile-browse        'makey-key-mode-popup-grails-projectile-discover-browse)
    (defalias 'discover-grails-projectile-plugins       'makey-key-mode-popup-grails-projectile-discover-plugins)
    (defalias 'discover-grails-projectile-find          'makey-key-mode-popup-grails-projectile-discover-find)
    (defalias 'discover-grails-projectile-run-or-create 'makey-key-mode-popup-grails-projectile-discover-run-or-create)
    (defalias 'discover-grails-projectile-generate      'makey-key-mode-popup-grails-projectile-discover-generate)

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover
                     (description "Grails Projectile commands")
                     (actions
                      ("Execute Grails Projectile command.\n"
                       ("s" "Main commands such as compile"           discover-grails-projectile-main)
                       ("c" "Create artifact"                         discover-grails-projectile-create)
                       ("f" "Find resource"                           discover-grails-projectile-find)
                       ("g" "Generate related artefacts for domain"   discover-grails-projectile-generate)
                       ("r" "Run or create application/plugin"        discover-grails-projectile-run-or-create)
                       ("p" "Plugins operations"                      discover-grails-projectile-plugins)
                       ("b" "Browse documentation"                    discover-grails-projectile-browse))))
     :bind (concat (key-description grails-projectile-keymap-prefix) " d")
     :mode-hook 'grails-projectile-mode-hook)

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-main
                     (description "Main commands")
                     (actions
                      ("Run command"
                       ("c" "Compile"                   grails-projectile-compile)
                       ("C" "Clean"                     grails-projectile-clean)
                       ("r" "Refresh dependencies"      grails-projectile-refresh-dependencies)
		       ("i" "Integrate with"            grails-projectile-integrate-with)
		       ("A" "Arbitrary command"         grails-projectile-icommand))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-plugins
                     (description "Plugins commands")
                     (actions
                      ("Plugins actions"
                       ("l" "List installed"   grails-projectile-plugins-list-installed)
                       ("p" "Package"          grails-projectile-plugins-package-plugin))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-generate
                     (description "Generate commands")
                     (actions
                      ("Generate"
                       ("c" "Controller"       grails-projectile-generate-controller)
                       ("v" "Views"            grails-projectile-generate-views)
                       ("a" "All"              grails-projectile-generate-all))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-find
                     (description "Find resource")
                     (actions
                      ("Find artefact"
                       ("l d" "Domain"       grails-projectile-locate-domain)
                       ("l c" "Controller"   grails-projectile-locate-controller)
                       ("l s" "Service"      grails-projectile-locate-service)
                       ("l t" "Taglib"       grails-projectile-locate-taglib)
                       ("l T" "Test"         grails-projectile-locate-test))

                      ("Associated artefact"
                       ("f d" "Domain"       grails-projectile-find-domain-for-file)
                       ("f c" "Controller"   grails-projectile-find-controller-for-file)
                       ("f s" "Service"      grails-projectile-find-service-for-file)
                       ("f t" "Taglib"       grails-projectile-find-taglib-for-file)
                       ("f T" "Test"         grails-projectile-find-test-for-file))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-browse
                     (description "Jump to documentation")
                     (actions
                      ("Jump to"
                       ("a" "API"          grails-projectile-browse-api-docs)
                       ("g" "Guide"        grails-projectile-browse-latest-guide)
                       ("w" "Wiki"         grails-projectile-browse-wiki-docs))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-create
                     (description "Create Grails artefacts")
                     (actions
                      ("Create"
                       ("d" "Domain class"        grails-projectile-create-domain)
                       ("c" "Controller"          grails-projectile-create-controller)
                       ("s" "Service"             grails-projectile-create-service)
                       ("t" "TagLib"              grails-projectile-create-taglib))))
     :bind ""))

  (add-hook 'discover-mode-hook 'grails-projectile-turn-on-discover-support))

;;;_. Minor mode
(defvar grails-projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map   (kbd "r d") 'grails-projectile-refresh-dependencies)
      (define-key prefix-map   (kbd "c p") 'grails-projectile-compile)
      (define-key prefix-map   (kbd "c l") 'grails-projectile-clean)
      (define-key prefix-map   (kbd "e")   'grails-projectile-icommand)

      (define-key prefix-map   (kbd "i w") 'grails-projectile-integrate-with)

      (define-key prefix-map   (kbd "g a") 'grails-projectile-generate-all)
      (define-key prefix-map   (kbd "g c") 'grails-projectile-generate-controller)
      (define-key prefix-map   (kbd "g v") 'grails-projectile-generate-views)

      (define-key prefix-map   (kbd "b a") 'grails-projectile-browse-api-docs)
      (define-key prefix-map   (kbd "b g") 'grails-projectile-browse-latest-guide)
      (define-key prefix-map   (kbd "b w") 'grails-projectile-browse-wiki-docs)

      (define-key prefix-map   (kbd "c d") 'grails-projectile-create-domain)
      (define-key prefix-map   (kbd "c t") 'grails-projectile-create-taglib)
      (define-key prefix-map   (kbd "c T") 'grails-projectile-create-unit-test)
      (define-key prefix-map   (kbd "c s") 'grails-projectile-create-service)
      (define-key prefix-map   (kbd "c c") 'grails-projectile-create-controller)

      (define-key prefix-map   (kbd "f d") 'grails-projectile-find-domain-for-file)
      (define-key prefix-map   (kbd "f t") 'grails-projectile-find-taglib-for-file)
      (define-key prefix-map   (kbd "f T") 'grails-projectile-find-test-for-file)
      (define-key prefix-map   (kbd "f s") 'grails-projectile-find-service-for-file)
      (define-key prefix-map   (kbd "f c") 'grails-projectile-find-controller-for-file)

      (define-key prefix-map   (kbd "l d") 'grails-projectile-locate-domain)
      (define-key prefix-map   (kbd "l t") 'grails-projectile-locate-test)
      (define-key prefix-map   (kbd "l s") 'grails-projectile-locate-service)
      (define-key prefix-map   (kbd "l v") 'grails-projectile-locate-view)
      (define-key prefix-map   (kbd "l c") 'grails-projectile-locate-controller)

      (define-key prefix-map   (kbd "r a") 'grails-projectile-run-app)

      (define-key prefix-map   (kbd "n a") 'grails-projectile-wizard-new-app)
      (define-key prefix-map   (kbd "n p") 'grails-projectile-wizard-new-plugin)

      (define-key prefix-map   (kbd "p l") 'grails-projectile-plugins-list-installed)
      (define-key prefix-map   (kbd "p p") 'grails-projectile-plugins-package-plugin)

      (define-key map grails-projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Grails Projectile mode.")

(easy-menu-define grails-projectile-mode-menu grails-projectile-mode-map
  "Grails Projectile Mode Menu."
  '("Grails"
    ["Execute Command"           grails-projectile-icommand                 t]
    ["Compile"                   grails-projectile-compile                  t]
    ["Run app"                   grails-projectile-run-app                  t]
    ["Clean"                     grails-projectile-clean                    t]

    ["--"                        'ignore                          ]

    ["Create Domain Class"       grails-projectile-create-domain            t]
    ["Create Controller"         grails-projectile-create-controller        t]
    ["Create Service"            grails-projectile-create-service           t]
    ["Create Unit Test"          grails-projectile-create-unit-test         t]
    ["Create TagLib"             grails-projectile-create-taglib            t]

    ["--"                        'ignore                          ]

    ["Find domain for file"      grails-projectile-find-domain-for-file     t]
    ["Find controller for file"  grails-projectile-find-controller-for-file t]
    ["Find service for file"     grails-projectile-find-service-for-file    t]
    ["Find taglib for file"      grails-projectile-find-taglib-for-file     t]
    ["Find test for file"        grails-projectile-find-test-for-file       t]

    ["--"                        'ignore                          ]

    ["Locate domain"             grails-projectile-locate-domain            t]
    ["Locate controller"         grails-projectile-locate-controller        t]
    ["Locate service"            grails-projectile-locate-service           t]
    ["Locate test"               grails-projectile-locate-test              t]

    ["--"                        'ignore                          ]

    ["Browse API"                grails-projectile-browse-api-docs          t]
    ["Browse guide"              grails-projectile-browse-latest-guide      t]
    ["Browse wiki"               grails-projectile-browse-wiki-docs         t]

    ["--"                        'ignore                          ]

    ["Installed Plugins"         grails-projectile-plugins-list-installed   t]
    ["Package Plugin"            grails-projectile-plugins-package-plugin   t]))

;;;###autoload
(define-minor-mode grails-projectile-mode
  "Grails Projectile Mode.

  \\{grails-projectile-mode-map}"
  :lighter grails-projectile-mode-line
  :keymap  'grails-projectile-mode-map
  :group   'grails-projectile
  :require 'grails-projectile-mode

  (progn
    (easy-menu-add grails-projectile-mode-menu)))

;;;###autoload
(define-globalized-minor-mode grails-projectile-global-mode
  grails-projectile-mode
  grails-projectile-on)

(defun grails-projectile-on ()
  "Enable Grails Projectile minor mode."
  (grails-projectile-mode 1))

(defun grails-projectile-off ()
  "Disable Grails Projectile minor mode."
  (grails-projectile-mode -1))

(defun grails-projectile-global-on ()
  "Enable Grails Projectile global minor mode."
  (grails-projectile-global-mode +1))

(defun grails-projectile-global-off ()
  "Disable Grails Projectile global minor mode."
  (grails-projectile-global-mode -1))

(provide 'grails-projectile-mode)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   eval: (progn (allout-hide-bodies) (beginning-of-buffer) (allout-hide-current-subtree))
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; grails-projectile-mode.el ends here
