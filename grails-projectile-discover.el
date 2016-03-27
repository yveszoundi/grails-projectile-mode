;;; grails-projectile-discover.el --- discover.el support for Grails Projectile.

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

(eval-when-compile
  (declare-function discover-add-context-menu "discover"))

(when (featurep 'discover)
  (mapc #'require '(discover grails-projectile-commands))

  (defun grails-projectile-discover-setup-keybindings()
    (interactive)
    "Add the default keybindings to show discover popups.
The default key sequence is `grails-projectile-keymap-prefix' followed by 'd'."
    (define-key grails-projectile-mode-map
      (kbd (concat (key-description grails-projectile-keymap-prefix) "d"))
      #'discover-grails-projectile-discover))

  (defun grails-projectile-turn-on-discover-support ()
    (interactive)
    (defalias 'discover-grails-projectile-discover 'makey-key-mode-popup-grails-projectile-discover)

    (defalias 'discover-grails-projectile-main     'makey-key-mode-popup-grails-projectile-discover-main)
    (defalias 'discover-grails-projectile-create   'makey-key-mode-popup-grails-projectile-discover-create)
    (defalias 'discover-grails-projectile-browse   'makey-key-mode-popup-grails-projectile-discover-browse)
    (defalias 'discover-grails-projectile-plugins  'makey-key-mode-popup-grails-projectile-discover-plugins)
    (defalias 'discover-grails-projectile-find     'makey-key-mode-popup-grails-projectile-discover-find)
    (defalias 'discover-grails-projectile-runornew 'makey-key-mode-popup-grails-projectile-discover-runornew)
    (defalias 'discover-grails-projectile-generate 'makey-key-mode-popup-grails-projectile-discover-generate)

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover
                     (description "Grails Projectile commands")
                     (actions
                      ("Execute a Grails Projectile command.\n"
                       ("m" "Main commands(compile, clean, etc.)"     discover-grails-projectile-main)
                       ("c" "Create artifact"                         discover-grails-projectile-create)
                       ("f" "Find resource"                           discover-grails-projectile-find)
                       ("g" "Generate related artefacts for domain"   discover-grails-projectile-generate)
                       ("r" "Run or create new application"           discover-grails-projectile-runornew)
                       ("p" "Plugins operations"                      discover-grails-projectile-plugins)
                       ("b" "Browse documentation"                    discover-grails-projectile-browse))))
     :bind ""
     :mode-hook 'grails-projectile-mode-hook)

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-main
                     (description "Main commands")
                     (actions
                      ("Main commands"
                       ("c c" "Compile"   grails-projectile-compile)
                       ("c C" "Clean"     grails-projectile-clean)
                       ("c r" "Refresh"   grails-projectile-refresh-dependencies)
                       ("i w" "Integrate" grails-projectile-integrate-with)
                       ("e" "Execute"     grails-projectile-icommand))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-plugins
                     (description "Plugins Commands")
                     (actions
                      ("Plugins"
                       ("l" "List installed" grails-projectile-plugins-list-installed)
                       ("p" "Package"        grails-projectile-plugins-package-plugin))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-generate
                     (description "Generate Commands")
                     (actions
                      ("Generate"
                       ("c" "Controller"       grails-projectile-generate-controller)
                       ("v" "Views"            grails-projectile-generate-views)
                       ("a" "All"              grails-projectile-generate-all))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-find
                     (description "Find Resource")
                     (actions
                      ("Artefact"
                       ("l d" "Domain"       grails-projectile-locate-domain)
                       ("l c" "Controller"   grails-projectile-locate-controller)
                       ("l s" "Service"      grails-projectile-locate-service)
                       ("l t" "Taglib"       grails-projectile-locate-taglib)
                       ("l T" "Test"         grails-projectile-locate-test))

                      ("Associated Artefact"
                       ("f d" "Domain"       grails-projectile-find-domain-for-file)
                       ("f c" "Controller"   grails-projectile-find-controller-for-file)
                       ("f s" "Service"      grails-projectile-find-service-for-file)
                       ("f t" "Taglib"       grails-projectile-find-taglib-for-file)
                       ("f T" "Test"         grails-projectile-find-test-for-file))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-runornew
                     (description "Run or create application")
                     (actions
                      ("Run"
                       ("r" "Run application"    grails-projectile-run-app))
                      ("New project"
                       ("a" "Web application"    grails-projectile-wizard-new-app)
                       ("p" "Plugin application" grails-projectile-wizard-new-plugin))))

     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-browse
                     (description "Jump to documentation")
                     (actions
                      ("Browse documentation"
                       ("a" "API"   grails-projectile-browse-api-docs)
                       ("g" "Guide" grails-projectile-browse-latest-guide)
                       ("w" "Wiki"  grails-projectile-browse-wiki-docs))))
     :bind "")

    (discover-add-context-menu
     :context-menu '(grails-projectile-discover-create
                     (description "Create Grails artefacts")
                     (actions
                      ("Create"
                       ("d" "Domain class" grails-projectile-create-domain)
                       ("c" "Controller"   grails-projectile-create-controller)
                       ("s" "Service"      grails-projectile-create-service)
                       ("t" "TagLib"       grails-projectile-create-taglib)
                       ("T" "Unit test"    grails-projectile-create-unit-test))))
     :bind ""))

  (add-hook 'discover-mode-hook 'grails-projectile-turn-on-discover-support))

(provide 'grails-projectile-discover)

;;; grails-projectile-discover.el ends here
