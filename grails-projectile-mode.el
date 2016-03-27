;;; grails-projectile-mode.el --- Grails mode with Projectile for projects management.

;; Copyright (C) 2013-2016 Rimero Solutions and contributors.

;; Version: 1.1.2
;; Keywords: grails, projectile
;; Homepage: https://github.com/yveszoundi/grails-projectile-mode
;; Author: Yves Zoundi <rimerosolutions@gmail.com>
;; Maintainer: Yves Zoundi
;; Package-Requires: ((projectile "0.10.0") (emacs "24") (cl-lib "0.5"))
;; Contributors: The internet and people who surf it.
;; Last updated: 2016-03-27

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
;; All the commands start with 'grails-projectile'
;; From a projectile managed buffer run `M-x grails-projectile-compile [RET]`
;; to compile your Grails application.
;;
;; To list keybindings press `C-h b` or type `M-x describe-mode`
;; Then search for grails-projectile-mode.
;;
;; There is integration with discover.el when it's available for easier
;; navigation between commands without resorting to muscle memory for keybindings.

;;; Change log: Split package into separate files.

;;; Code:

(mapc #'require '(projectile cl-lib grails-projectile-commands))

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

    ["--"                        'ignore                                     ]

    ["Create Domain Class"       grails-projectile-create-domain            t]
    ["Create Controller"         grails-projectile-create-controller        t]
    ["Create Service"            grails-projectile-create-service           t]
    ["Create Unit Test"          grails-projectile-create-unit-test         t]
    ["Create TagLib"             grails-projectile-create-taglib            t]

    ["--"                        'ignore                                     ]

    ["Find domain for file"      grails-projectile-find-domain-for-file     t]
    ["Find controller for file"  grails-projectile-find-controller-for-file t]
    ["Find service for file"     grails-projectile-find-service-for-file    t]
    ["Find taglib for file"      grails-projectile-find-taglib-for-file     t]
    ["Find test for file"        grails-projectile-find-test-for-file       t]

    ["--"                        'ignore                                     ]

    ["Locate domain"             grails-projectile-locate-domain            t]
    ["Locate controller"         grails-projectile-locate-controller        t]
    ["Locate service"            grails-projectile-locate-service           t]
    ["Locate test"               grails-projectile-locate-test              t]

    ["--"                        'ignore                                     ]

    ["Browse API"                grails-projectile-browse-api-docs          t]
    ["Browse guide"              grails-projectile-browse-latest-guide      t]
    ["Browse wiki"               grails-projectile-browse-wiki-docs         t]

    ["--"                        'ignore                                     ]

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
