;; KEVIN ALBRECHT 2011-11-21
;;
;; 1. Find your emacs init file in one of the following locations:
;;    ~/.emacs
;;    ~/AppData/Roaming/.emacs.d/init.el
;;    ~/.emacs.d/init.el
;;
;; 2. Add the following lines to your emacs init file that you found
;;    in step 1, above:
;;
;;    (add-to-list 'load-path "~/code/personal-settings")
;;    (load-library "~/code/personal-settings/kevins-emacs-custom.el")

;;---------- Initialize marmalade package manager

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;---------- Auto-install marmelade packages at startup if not found

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;---------- VARIABLES

;; See if we're on MS Windows or some other OS
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))

;;---------- FONT SETUP

(if is-system-windows
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-10"))

(if is-system-mac
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-14"))

(setq-default indent-tabs-mode nil)

;;---------- FILE SYSTEM

(if is-system-windows
    (progn
      (cd "C:/Users/Kevin/code/")
      (setq default-directory "C:/Users/Kevin/code/")))

(if is-system-mac
    (progn
      (cd "~/code/")
      (setq default-directory "~/code/")))

