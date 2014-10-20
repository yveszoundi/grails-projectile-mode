;;; grails-projectile-mode.el --- Grails mode with Projectile for projects management.

;; Copyright (C) 2013,2014 Rimero Solutions

;; Version: 1.0.0
;; Keywords: grails, projectile
;; Author: Yves Zoundi <rimerosolutions@gmail.com>
;; Maintainer: Yves Zoundi
;; Package-Requires: ((projectile "0.10.0") (emacs "24") (cl-lib "0.5"))
;; Contributors: The internet and people who surf it.
;; Last updated: 2014-10-15

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
;; You can customize the mode using `M-x customize-group` [RET] grails.
;;
;; Add the folder containing grails-projectile-mode.el in your load-path
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;;
;; (require 'grails-projectile-mode)
;; (grails-projectile-global-mode t)
;;
;; All the commands start with 'grails-'
;; From a projectile managed buffer run `M-x grails-compile [RET]`
;; to compile your Grails application.
;;
;; To list keybindings press `C-h b` or type `M-x describe-mode`
;; Then search for grails-projectile-mode.

;;; Code:

(require 'projectile)

(require 'cl-lib)

;;;_. Customizations
(defcustom grails-projectile-keymap-prefix (kbd "C-c ;")
  "Grails Projectile keymap prefix."
  :group 'grails-projectile
  :type 'string
  :link '(url-link :tag "Github" "https://github.com/rimerosolutions/emacs-grails-mode-ext"))

(defcustom grails-projectile-mode-line " Grails"
  "Grails projectile modeline."
  :type 'string
  :group 'grails-projectile)

(defconst grails-executable-suffix
  (if (eq system-type 'windows-nt)
      ".bat" "")
  "Suffix for the Grails executable file.")

(defcustom grails-compilation-buffer-name "*Grails*"
  "Buffer name for Grails commands."
  :type 'string
  :group 'grails-projectile)

(defcustom use-grails-wrapper-when-possible t
  "Use the Grails wrapper whenever available."
  :type 'boolean
  :group 'grails-projectile)

(defcustom grails-output-opts ""
  "Output options such as --plain-output."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-cmd-opts "--non-interactive --stacktrace"
  "Grails command line options."
  :type 'string
  :group 'grails-projectile)

(defconst grails-wrapper-filename "grailsw" "Grails Wrapper file name.")

(defcustom grails-projectile-filename ".grails-projectile"
  "Project file to define custom grails command and JVM options.
   The contents of this file override both grails-cmd-opts and grails-jvm-opts.
   Everything must hold within a single line, no newline at the end of the file."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-jvm-opts "-DXmx1g"
  "Grails command line options"
  :type 'string
  :group 'grails-projectile)

(defcustom grails-executable "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-url-wikidocs "http://grails.org/Documentation"
  "Grails Wiki documentation URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-url-apidocs "http://grails.org/doc/latest/api/"
  "Grails documentation URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-plugins-base-url "http://grails.org/plugins/"
  "Grails plugins base URL."
  :type 'string
  :group 'grails-projectile)

(defcustom grails-url-guide "http://grails.org/doc/latest/guide/single.html"
  "Grails Latest Guide URL."
  :type 'string
  :group 'grails-projectile)

;;;_. Utilities
(defun grails--join-lines (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(defun grails--read-grails-options-projectile-file (file-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (mark-whole-buffer)
    (grails--join-lines (point-min) (point-max))
    (buffer-string)))

;;;_. Wizard functions
(defun grails-wizard-new-app ()
  "Create a new application project."
  (interactive)
  (grails--wizard-new-app-or-plugin "create-app"))

(defun grails-integrate-with (grails-tool-or-editor)
  "Integrate Grails with an IDE or Build System `grails-tool-or-editor'."
  (interactive
   (let ((grails-tools-or-editors
          '("Eclipse" "Intellij" "Textmate" "Ant" "Git")))
     (list (completing-read "Select IDE or Build System: " grails-tools-or-editors))))
  (progn
    (let ((grails-jvm-opts ""))
      (grails--command (concat "integrate-with --" (downcase grails-tool-or-editor))))))

(defun grails-wizard-new-plugin ()
  "Create a new plugin project."
  (interactive)
  (grails--wizard-new-app-or-plugin "create-plugin"))

(defun grails--wizard-new-app-or-plugin (cmd)
  "Create a new application or plugin.
`cmd' is either create-app or create-plugin."
  (let ((insert-default-directory  t))
    ;; Ask the user for the project folder
    (let* ((grails-project-folder (read-directory-name "Application Directory: " default-directory))
           (app-name (read-from-minibuffer "Application Name: "))
           (default-directory (file-name-as-directory grails-project-folder))
           (grails-command grails-executable)
           (grails-arguments (concat cmd " --inplace " app-name)))

      ;; Create the project folder.
      (unless (file-exists-p default-directory)
        (make-directory default-directory t))

      ;; Create the .projectile file in the new project folder.
      (grails--create-grails-projectile-file default-directory)

      ;; Generate the Grails app or plugin in-place inside the new project folder.
      (let ((grails-command-line (concat grails-command " " grails-arguments)))
        (compilation-start grails-command-line
                           'compilation-mode
                           'grails--get-compilation-buffer-name)))))

(defun grails--create-grails-projectile-file (dir)
  "Add the default .projectile file after creating a new app or plugin."
  (with-temp-file (concat dir ".projectile")
    (insert "-/target")))

;;;; File locator functions
(defun grails--find-grails-file (grails-proj-folder pred-fn-sym file-basename &optional no-auto-open)
  "Find a Grails file in a project folder.

   grails-proj-folder is the base search folder.
   pred-fn-sym is the function to filter project files.
   file-basename is the filename to search without extension.
   no-auto-open Do not open the file automatically for a single result.
"
  (let ( (result-list (grails--find-grails-files grails-proj-folder
                                                 file-basename
                                                 pred-fn-sym)))
    (if result-list
        (if (= (length result-list) 1)
            (if (not no-auto-open)
                (find-file (concat (projectile-project-root) (car result-list)))
              (progn
                (let ((file-list (mapcar #'(lambda(p) (concat (projectile-project-root) p)) result-list)))
                  (let ((selected-file (completing-read "Select a file:" file-list)))
                    (find-file selected-file)))))
          (progn
            (let ((file-list (mapcar #'(lambda(p) (concat (projectile-project-root) p)) result-list)))
              (let ((selected-file (completing-read "Select a file:" file-list)))
                (find-file selected-file)))))
      (message "No artefact found for %s in '%s'" file-basename grails-proj-folder))))

(defun grails--find-grails-files (dirname file-basename pred-fn)
  "Jump to a filename from a given base folder."
  (let ((folder-files (projectile-files-in-project-directory dirname))
        (filtered-folder-files '()))
    (dolist (elt folder-files)
      (when (funcall pred-fn (file-name-base elt) file-basename)
        (add-to-list 'filtered-folder-files elt)))
    filtered-folder-files))

(defun grails--base-name-matches-p (value expected)
  "Match two strings."
  (string= expected value))

(defun grails--test-matches-p (value expected)
  "Test whether a file basename matches a test class."
  (or (string= (concat expected "Tests") value)
      (string= (concat expected "Spec") value)))

(defun grails--all-files (value expected) "Test whether a file should be opened." t)

(defun grails--artefact-name-no-suffix (file-basename)
  "Return the Grails artefact name without its suffix

  file-basename is the full basename of the file such as TestController.

  The transformation of TestControllerSpec would remove both Spec and Controller
  from the basename and return only Test.
  "
  (let ((artefact-name file-basename)
        (artifact-suffixes '("Spec" "Tests" "Service" "Controller" "TagLib" "Command")))

    (dolist (elt artifact-suffixes)
      (when (string-match (concat elt "$") artefact-name)
        (setq artefact-name (substring artefact-name 0 (- (length artefact-name) (length elt))))))

    artefact-name))

(defun grails--find-artefact (artefact-folder artefact-suffix &optional artefact-full-name)
  "Finds a Grails artefact in a given folder by suffix.

  artefact-folder is the Grails sub-folder to look at usually inside grails-app.
  artefact-suffix is a suffix convention such as Controller, Service when applicable.
  artefact-full-name refers to the full basename of the file to search or the current buffer filename.
  "
  (grails--find-grails-file (grails--path-from-project-root "grails-app" artefact-folder)
                            'grails--base-name-matches-p
                            (or artefact-full-name
                                (concat (grails--artefact-name-no-suffix (file-name-base (buffer-file-name)))
                                        artefact-suffix))))

;;;_. Artefact location commands
(defun grails-locate-test ()
  "Locate a test class in the project."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "test")
                            'grails--all-files
                            ""
                            t))

(defun grails-find-test-for-file ()
  "Find a test class associated with the current file."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "test")
                            'grails--test-matches-p
                            (file-name-base (buffer-file-name))))

(defun grails-locate-view ()
  "Locate a view in the project."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "grails-app" "views")
                            'grails--all-files
                            ""
                            t))

(defun grails-locate-service ()
  "Locate a service class in the project."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "grails-app" "services")
                            'grails--all-files
                            ""
                            t))

(defun grails-find-service-for-file ()
  "Find a service class associated with the current file."
  (interactive)
  (grails--find-artefact "services" "Service"))

(defun grails-locate-controller ()
  "Locate a controller class in the project."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "grails-app" "controllers")
                            'grails--all-files
                            ""
                            t))

(defun grails-find-controller-for-file ()
  "Find a controller class associated with the current file."
  (interactive)
  (grails--find-artefact "controllers" "Controller"))

(defun grails-locate-domain ()
  "Locate a domain class in the project."
  (interactive)

  (grails--find-grails-file (grails--path-from-project-root "grails-app" "domain")
                            'grails--all-files
                            ""
                            t))

(defun grails-find-domain-for-file ()
  "Find a domain class associated with the current file."
  (interactive)
  (grails--find-artefact "domain" ""))

(defun grails-locate-taglib ()
  "Locate a taglib class in the project."
  (interactive)
  (grails--find-grails-file (grails--path-from-project-root "grails-app" "taglib")
                            'grails--all-files
                            ""
                            t))

(defun grails-find-taglib-for-file ()
  "Find a taglib class associated to the current file."
  (interactive)
  (grails--find-artefact "taglib" "TagLib"))

;; --------------------------------
;; Folder helper functions
;; --------------------------------
(defun grails--path-from-project-root(&rest path-elements)
  (cl-reduce '(lambda (x &optional y)
                (concat (file-name-as-directory x) y))
             path-elements :initial-value (projectile-project-root)))


(defun grails--wrapper-exists-p (folder-name)
  "Check whether the Grails wrapper exist in a given folder."
  (file-exists-p (concat folder-name grails-wrapper-filename grails-executable-suffix)))

(defun grails--get-cmd (grails-command)
  "Generate the grails command line string."
  (let ((default-directory (expand-file-name (projectile-project-root)))
        (grails-args (concat grails-jvm-opts " " grails-cmd-opts))
        (grails-cmd-line grails-executable))

    (when (and use-grails-wrapper-when-possible
               (grails--wrapper-exists-p default-directory))
      (let ((grailsw-file (concat default-directory
                                  grails-wrapper-filename
                                  grails-executable-suffix)))
        (setq grails-cmd-line grailsw-file)))

    (when (file-exists-p (concat default-directory grails-projectile-filename))
      (let ((grails-projectile-file (concat default-directory grails-projectile-filename)))
        (setq grails-args (grails--read-grails-options-projectile-file grails-projectile-file))))

    (concat grails-cmd-line " " grails-output-opts " " grails-args " " grails-command)))

;;;_. Main functions
(defun grails--command (cmd)
  "Run a Grails command."
  (let ((grails-command-line (grails--get-cmd cmd))
        (default-directory (expand-file-name (projectile-project-root))))
    (compilation-start grails-command-line 'compilation-mode 'grails--get-compilation-buffer-name)))

(defun grails--get-compilation-buffer-name (mode)
  "The buffer name to use for Grails Commands."
  grails-compilation-buffer-name)

(defun grails--read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command."
  (let (grails-command-argument)
    (setq grails-command-argument (read-from-minibuffer input-hint))
    (grails--command (concat grails-command " " grails-command-argument))))

(defun grails-generate-all ()
  "Create all artifacts for a Grails Domain Class."
  (interactive)
  (grails--read-param-and-run "Generate all for Domain class:" "generate-all"))

(defun grails-generate-views ()
  "Create views for a Grails Domain Class."
  (interactive)
  (grails--read-param-and-run "Generate views for Domain class:" "generate-views"))

(defun grails-generate-controller ()
  "Create a controller for a Grails Domain Class."
  (interactive)
  (grails--read-param-and-run "Generate controller for Domain class:" "generate-controller"))

;;;_. General commands
(defun grails-icommand ()
  "Enter a Grails command."
  (interactive)
  (grails--read-param-and-run "Goal:" ""))

(defun grails-create-domain ()
  "Create a Grails Domain Class."
  (interactive)
  (grails--read-param-and-run "Domain class:" "create-domain-class"))

(defun grails-create-controller ()
  "Create a Grails Controller."
  (interactive)
  (grails--read-param-and-run "Controller Domain class:" "create-controller"))

(defun grails-create-service ()
  "Create a Grails Service."
  (interactive)
  (grails--read-param-and-run "Service Domain class:" "create-service"))

(defun grails-create-taglib ()
  "Create a Grails Taglib."
  (interactive)
  (grails--read-param-and-run "TagLib Name:" "create-tag-lib"))

;;;_. Plugin commands
(defun grails-plugins-list-installed ()
  "List Grails installed plugins."
  (interactive)
  (grails--command "list-plugins -installed"))

(defun grails-plugins-package-plugin ()
  "Package a Grails plugin."
  (interactive)
  (grails--command "package-plugin"))

;;;_. Other grails commands
(defun grails-compile ()
  "Compile."
  (interactive)
  (grails--command "compile"))

(defun grails-run-app ()
  "Run the application."
  (interactive)
  (grails--command "run-app"))

(defun grails-clean ()
  "Clean."
  (interactive)
  (grails--command "clean"))

(defun grails-refresh-dependencies ()
  "Refresh Grails Dependencies."
  (interactive)
  (grails--command "refresh-dependencies"))

;;;_. Documentation browsing commands
(defun grails-browse-wiki-docs ()
  "Browse the Wiki Documentation."
  (interactive)
  (if (boundp 'grails-url-wikidocs)
      (browse-url grails-url-wikidocs)
    (message "No Grails Wikidocs set. Customize the 'grails' group")))

(defun grails-browse-api-docs ()
  "Browse the API Documentation."
  (interactive)
  (if (boundp 'grails-url-apidocs)
      (browse-url grails-url-apidocs)
    (message "No Grails API URL set. Customize the 'grails' group")))

(defun grails--search-plugin (base-url query-string)
  "Search Grails plugins."
  (browse-url (url-encode-url (concat base-url query-string))))

(defun grails-search-plugin-query (query-string)
  "Search Grails plugins by query string."
  (interactive "sPlugin name or query: \n")
  (if (boundp 'grails-plugins-base-url)
      (grails--search-plugin grails-plugins-base-url query-string)
    (message "No Grails plugins base URL set. Customize the 'grails-projectile' group")))

(defun grails-search-plugin-tag (query-string)
  "Search Grails plugins."
  (interactive "sPlugin tag: \n")
  (if (boundp 'grails-plugins-base-url)
      (grails--search-plugin (concat grails-plugins-base-url "tag/") query-string)
    (message "No Grails plugins base URL set. Customize the 'grails-projectile' group")))

(defun grails-browse-latest-guide ()
  "Browse the official Grails Guide."
  (interactive)
  (if (boundp 'grails-url-guide)
      (browse-url grails-url-guide)
    (message "No Grails URL guide set. Customize the 'grails-projectile' group")))

;;;_. Minor mode
(defvar grails-projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map   (kbd "r d") 'grails-refresh-dependencies)
      (define-key prefix-map   (kbd "c p") 'grails-compile)
      (define-key prefix-map   (kbd "c l") 'grails-clean)
      (define-key prefix-map   (kbd "e")   'grails-icommand)

      (define-key prefix-map   (kbd "i w") 'grails-integrate-with)

      (define-key prefix-map   (kbd "g a") 'grails-generate-all)
      (define-key prefix-map   (kbd "g c") 'grails-generate-controller)
      (define-key prefix-map   (kbd "g v") 'grails-generate-views)

      (define-key prefix-map   (kbd "b a") 'grails-browse-api-docs)
      (define-key prefix-map   (kbd "b g") 'grails-browse-latest-guide)
      (define-key prefix-map   (kbd "b w") 'grails-browse-wiki-docs)

      (define-key prefix-map   (kbd "c d") 'grails-create-domain)
      (define-key prefix-map   (kbd "c t") 'grails-create-taglib)
      (define-key prefix-map   (kbd "c s") 'grails-create-service)
      (define-key prefix-map   (kbd "c c") 'grails-create-controller)

      (define-key prefix-map   (kbd "f d") 'grails-find-domain-for-file)
      (define-key prefix-map   (kbd "f t") 'grails-find-test-for-file)
      (define-key prefix-map   (kbd "f s") 'grails-find-service-for-file)
      (define-key prefix-map   (kbd "f c") 'grails-find-controller-for-file)

      (define-key prefix-map   (kbd "l d") 'grails-locate-domain)
      (define-key prefix-map   (kbd "l t") 'grails-locate-test)
      (define-key prefix-map   (kbd "l s") 'grails-locate-service)
      (define-key prefix-map   (kbd "l v") 'grails-locate-view)
      (define-key prefix-map   (kbd "l c") 'grails-locate-controller)

      (define-key prefix-map   (kbd "r a") 'grails-run-app)

      (define-key prefix-map   (kbd "n a") 'grails-wizard-new-app)
      (define-key prefix-map   (kbd "n p") 'grails-wizard-new-plugin)

      (define-key prefix-map   (kbd "p l") 'grails-plugins-list-installed)
      (define-key prefix-map   (kbd "p p") 'grails-plugins-package-plugin)

      (define-key map grails-projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Grails Projectile mode.")

(easy-menu-define grails-projectile-mode-menu grails-projectile-mode-map
  "Emacs Grails Project Mode Menu."
  '("Grails"
    ["Execute Command"           grails-icommand                 t]
    ["Compile"                   grails-compile                  t]
    ["Run app"                   grails-run-app                  t]
    ["Clean"                     grails-clean                    t]

    ["--"                        'ignore                          ]

    ["Create Domain Class"       grails-create-domain            t]
    ["Create Controller"         grails-create-controller        t]
    ["Create Service"            grails-create-service           t]
    ["Create TagLib"             grails-create-taglib            t]

    ["--"                        'ignore                          ]

    ["Find domain for file"      grails-find-domain-for-file     t]
    ["Find controller for file"  grails-find-controller-for-file t]
    ["Find service for file"     grails-find-service-for-file    t]
    ["Find test for file"        grails-find-test-for-file       t]

    ["--"                        'ignore                          ]

    ["Locate domain"             grails-locate-domain            t]
    ["Locate controller"         grails-locate-controller        t]
    ["Locate service"            grails-locate-service           t]
    ["Locate test"               grails-locate-test              t]

    ["--"                        'ignore                          ]
    
    ["Browse API"                grails-browse-api-docs          t]
    ["Browse guide"              grails-browse-latest-guide      t]
    ["Browse wiki"               grails-browse-wiki-docs         t]
    
    ["--"                        'ignore                          ]

    ["Installed Plugins"         grails-plugins-list-installed   t]
    ["Package Plugin"            grails-plugins-package-plugin   t]))

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
