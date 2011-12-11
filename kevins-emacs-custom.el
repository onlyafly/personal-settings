;; KEVIN ALBRECHT 2011-12-11
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
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      color-theme-solarized)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;---------- VARIABLES

;; See if we're on MS Windows or some other OS
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))

;;---------- Color theme setup

(color-theme-solarized-light)

;;---------- FONT SETUP

(if is-system-windows
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-10"))

(if is-system-mac
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-14"))

(setq-default indent-tabs-mode nil)

;; Turn off wrapping
(setq-default truncate-lines 't)

;;---------- FILE SYSTEM

(if is-system-windows
    (progn
      (cd "C:/Users/Kevin/code/")
      (setq default-directory "C:/Users/Kevin/code/")))

(if is-system-mac
    (progn
      (cd "~/code/")
      (setq default-directory "~/code/")))

;;---------- ERLANG SUPPORT

(if is-system-mac
    (progn

      ;; The Erlang Emacs mode
      (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.6.5/emacs"
                            load-path))
      (setq erlang-root-dir "/usr/local/lib/erlang")
      (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
      (require 'erlang-start)

      ;; Automatic Erlang syntax checking
      (require 'erlang-flymake)))

;;---------- Mac Keyboard Support

;; This allows use of the Alt/Option key for international use on Mac:
;; ctrl = C
;; alt/option = not bound in emacs
;; cmd = M
;;
;; See:
;; http://stackoverflow.com/questions/3376863/unable-to-type-braces-and-square-braces-in-emacs
(if is-system-mac
    (progn
      (setq mac-option-modifier nil
            mac-command-modifier 'meta
            x-select-enable-clipboard t)))


