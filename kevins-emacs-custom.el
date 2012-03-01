;; KEVIN ALBRECHT 2012-02-03
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

;;---------- Setup

;; See if we're on MS Windows or some other OS
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))
(defvar is-system-linux (string-match "gnu/linux" (symbol-name system-type)))

(defvar home-dir
  (cond (is-system-mac "/Users/kevin.albrecht/")
        (is-system-linux "/home/kevin/")
        (is-system-windows "C:/Users/Kevin/")))

(defvar emacs-dir
  (cond (is-system-mac (concat home-dir ".emacs.d/"))
        (is-system-mac (concat home-dir ".emacs.d/"))
        (is-system-windows (concat home-dir "AppData/Roaming/.emacs.d/"))))

(defvar themes-dir
  (concat emacs-dir "themes/"))

(defvar code-dir
  (concat home-dir "code/"))

;;---------- Helper functions

(defun download-if-missing (file-path url-path)
  (if (not (file-exists-p file-path))
      (progn
        (url-copy-file url-path file-path))))

(defun create-directory-if-missing (path)
  (if (not (file-accessible-directory-p path))
      (progn
        (make-directory path))))

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
                      markdown-mode
                      go-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;---------- Color theme setup

(create-directory-if-missing themes-dir)

(download-if-missing (concat themes-dir "zenburn-theme.el")
                     "https://raw.github.com/bbatsov/zenburn-emacs/master/zenburn-theme.el")
(download-if-missing (concat themes-dir "solarized-theme.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized-theme.el")
(download-if-missing (concat themes-dir "solarized-dark-theme.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized-dark-theme.el")
(download-if-missing (concat themes-dir "solarized-light-theme.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized-light-theme.el")

(add-to-list 'custom-theme-load-path themes-dir)

(load-theme 'zenburn 't)
;;(load-theme 'solarized-light 't)
;;(load-theme 'solarized-dark 't)

;;---------- FONT SETUP

(if is-system-windows
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-10"))

(if is-system-mac
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-14"))

(setq-default indent-tabs-mode nil)

;; Turn off wrapping
(setq-default truncate-lines 't)

;;---------- FILE SYSTEM

;; Default directory
(cd (concat home-dir "code/"))
(setq default-directory (concat home-dir "code/"))

;;---------- ERLANG SUPPORT

(if is-system-mac
    (progn

      ;; The Erlang Emacs mode
      (add-to-list 'load-path (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
      (setq erlang-root-dir "/usr/local/lib/erlang")
      (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
      (require 'erlang-start)

      ;; Automatic Erlang syntax checking
      (require 'erlang-flymake)

      ;; Distel for Erlang; for this to work, first download from
      ;; https://github.com/massemanet/distel and then run make.
      (add-to-list 'load-path (concat code-dir
                                      "distel/elisp"))
      (require 'distel)
      (distel-setup)))

;;---------- Markdown support

(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

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

;;---------- Misc Settings

(setq column-number-mode t)
(setq default-tab-width 4)
