;; KEVIN ALBRECHT 2011-11-02
;;
;; .emacs should contain:
;;   (add-to-list 'load-path "~/code/personal-settings")
;;   (load-library "~/code/personal-settings/kevins-emacs-custom.el")

;;---------- VARIABLES

;; See if we're on MS Windows or some other OS
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))

;;---------- FONT SETUP

(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-10")

(setq-default indent-tabs-mode nil)

;;---------- FILE SYSTEM

(cd "C:/Users/Kevin/code/")
(setq default-directory "C:/Users/Kevin/code/")
