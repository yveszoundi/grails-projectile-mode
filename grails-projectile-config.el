(defconst grails-projectile-version-info "1.1.0" "Grails Projectile version")

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

(provide 'grails-projectile-config)
