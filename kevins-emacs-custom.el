;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEVIN ALBRECHT, 2013-06-12
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Installing Emacs
;; ----------------
;; Mac OS X: http://emacsformacosx.com/builds
;; Windows:  http://code.google.com/p/emacs-for-windows/downloads/list
;; Debian:   http://emacs.naquadah.org/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setting up this file
;; --------------------
;; 
;; 1. Find your emacs init file in one of the following locations:
;;    ~/.emacs
;;    ~/AppData/Roaming/.emacs.d/init.el
;;    ~/.emacs.d/init.el
;;
;; 2. Add the following lines to your emacs init file that you found
;;    in step 1, above (change my-user-name to something else if the
;;    username on that computer is different than "kevin"):
;;
;;    (defvar my-user-name "kevin")
;;    (add-to-list 'load-path "~/code/personal-settings")
;;    (load-library "~/code/personal-settings/kevins-emacs-custom.el")
;;
;; 3. On Windows, install cygwin and then set the system PATH variable
;;    to have C:\cygwin\bin as the first thing on the PATH
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---------- Setup

;; Operating system
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))
(defvar is-system-linux (string-match "gnu/linux" (symbol-name system-type)))

;; Home directory
(defvar home-dir
  (cond (is-system-mac (concat "/Users/" my-user-name "/"))
        (is-system-linux (concat "/home/" my-user-name "/"))
        (is-system-windows (concat "C:/Users/" my-user-name "/"))))

;; Emacs directory
(defvar emacs-dir
  (cond (is-system-mac (concat home-dir ".emacs.d/"))
        (is-system-linux (concat home-dir ".emacs.d/"))
        (is-system-windows (concat home-dir "AppData/Roaming/.emacs.d/"))))

(defvar emacs-extras-dir
  (concat emacs-dir "extras/"))
(defvar emacs-vendor-dir
  (concat emacs-dir "vendor/"))
(defvar themes-dir
  (concat emacs-dir "themes/"))
(defvar theme-extras-dir
  (concat emacs-dir "theme-extras/"))
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

;; Add in your own as you wish. If one of these fails, try M-x
;; package-refresh-contents to update the list of packages from ELPA
;; and marmalade.
;;
;; To upgrade all installed packages, try M-x package-list-packages,
;; then press "U" followed by "x"
(defvar my-packages '(starter-kit      ;; the Emacs Starter Kit
                      starter-kit-lisp ;; for generic Lisp support
                      clojure-mode     ;; for Clojure
                      cider            ;; for Clojure (formerly nREPL)
                      markdown-mode    ;; for the Markdown markup language
                      go-mode          ;; for the Go programming lang
                      rainbow-delimiters
                      auto-complete
                      ac-nrepl         ;; for Clojure, https://github.com/purcell/ac-nrepl
                      projectile       ;; https://github.com/bbatsov/projectile
                      coffee-mode      ;; CoffeeScript,
                      ;; https://github.com/defunkt/coffee-mode
                      )       
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;---------- Projectile (project support)

(projectile-global-mode)

;;---------- Emacs Extras & Vendor Directories

(create-directory-if-missing emacs-extras-dir)
(add-to-list 'load-path emacs-extras-dir)

(create-directory-if-missing emacs-vendor-dir)
(add-to-list 'load-path emacs-vendor-dir)

;;---------- Rainbow Delimiters enabled in all languages

(global-rainbow-delimiters-mode)

;;---------- Color theme setup

;; Theme extra directory (support files that must be on the load path)
(create-directory-if-missing theme-extras-dir)
(add-to-list 'load-path theme-extras-dir)

;; Themes directory
(create-directory-if-missing themes-dir)
(add-to-list 'custom-theme-load-path themes-dir)

;; Zenburn
(download-if-missing (concat themes-dir "zenburn-theme.el")
                     "https://raw.github.com/bbatsov/zenburn-emacs/master/zenburn-theme.el")

;; Solarized #1: https://github.com/bbatsov/solarized-emacs
(download-if-missing (concat theme-extras-dir "solarized.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized.el")
(download-if-missing (concat themes-dir "solarized-dark-theme.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized-dark-theme.el")
(download-if-missing (concat themes-dir "solarized-light-theme.el")
                     "https://raw.github.com/bbatsov/solarized-emacs/master/solarized-light-theme.el")

;; Solarized #2: https://github.com/sellout/emacs-color-theme-solarized
;; (download-if-missing (concat themes-dir "color-theme-solarized-pkg.el")
;;                      "https://raw.github.com/sellout/emacs-color-theme-solarized/master/color-theme-solarized-pkg.el")
;; (download-if-missing (concat themes-dir "color-theme-solarized.el")
;;                      "https://raw.github.com/sellout/emacs-color-theme-solarized/master/color-theme-solarized.el")
;; (download-if-missing (concat themes-dir "solarized-dark-theme.el")
;;                      "https://raw.github.com/sellout/emacs-color-theme-solarized/master/solarized-dark-theme.el")
;; (download-if-missing (concat themes-dir "solarized-light-theme.el")
;;                      "https://raw.github.com/sellout/emacs-color-theme-solarized/master/solarized-light-theme.el")
;; (download-if-missing (concat themes-dir "solarized-definitions.el")
;;                      "https://raw.github.com/sellout/emacs-color-theme-solarized/master/solarized-definitions.el")

;;-- Load a theme

;;(load-theme 'zenburn 't)
;;(load-theme 'solarized-light 't)
(load-theme 'solarized-dark 't)

;;---------- FONT SETUP

(if is-system-windows
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-9"))

(if is-system-mac
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-14"))

(if is-system-linux
    (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-10"))

(setq-default indent-tabs-mode nil)

;; Turn off wrapping
(setq-default truncate-lines 't)

;;---------- FILE SYSTEM

;; Default directory
(cd code-dir)
(setq default-directory code-dir)

;;---------- GO SUPPORT

;; This requires go-autocomplete to be installed with "go get" to
;; correctly work. See: https://github.com/nsf/gocode
(if is-system-windows
    (progn
      (download-if-missing (concat emacs-extras-dir "go-autocomplete.el")
                           "https://raw.github.com/nsf/gocode/master/emacs/go-autocomplete.el")
      (require 'go-autocomplete)
      (require 'auto-complete-config)))

;;---------- Clojure support
;; 1. Install leiningen: http://leiningen.org/
;;
;; Using jump mode:
;;  1. Open a Clojure file
;;  2. Start nREPL: M-x nrepl-jack-in
;;  3. Load the namespace: C-c C-n
;;  4. Go to a form and use: M-.

;; Improve indentaion
(require 'clojure-mode)
(define-clojure-indent
  (describe 1) ; for speclj
  (it 1)       ; for speclj
  (dosync 0)
  (io! 0)
  (try+ 0)     ; for slingshot
  (in 1)       ; for waltz
  (out 1)      ; for waltz
  )

;; For nREPL mode
(add-hook 'clojure-mode-hook
          'nrepl-interaction-mode)

;; Enable eldoc in Clojure buffers
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;; Enable Paredit in nREPL buffers
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; Enable rainbow delimiters mode in nREPL buffers
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Autocomplete support for Clojure
(require 'auto-complete)
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'clojure-mode-hook
          'auto-complete-mode)

;; Trigger auto-complete using TAB in nrepl buffers
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;---------- ClojureScript support

(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

;;---------- JavaScript support

(add-hook 'js-mode-hook
          'auto-complete-mode)

;;---------- CoffeeScript support

;; Installation:
;;
;; 1. Install NPM
;;    ( see https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager )
;;
;;    sudo apt-get update
;;    sudo add-apt-repository ppa:chris-lea/node.js
;;    sudo apt-get update
;;    sudo apt-get install nodejs=0.8.18-1chl1~precise1
;;
;; 2. Install CoffeeScript
;;    
;;    sudo npm install -g coffee-script

(defun turn-on-whitespace-action-and-style ()
  (setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace
  )
(add-hook 'coffee-mode-hook 'turn-on-whitespace-action-and-style)

;;---------- Markdown support

(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;---------- Mac Keyboard Support

;; This allows use of the Alt/Option key for international use on Mac:
;; ctrl = C
;; alt/option = not bound in emacs, allows this to be used for other characters
;; cmd = M
;;
;; See:
;; http://stackoverflow.com/questions/3376863/unable-to-type-braces-and-square-braces-in-emacs
(if is-system-mac
    (progn
      (setq mac-option-modifier nil
            mac-command-modifier 'meta
            x-select-enable-clipboard t)))

;;---------- Find/Grep Support

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
;; From http://emacswiki.org/emacs/NTEmacsWithCygwin
(if is-system-windows
    (progn
      (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
        "Use cygwin's /dev/null as the null-device."
        (let ((null-device "/dev/null"))
          ad-do-it))
      (ad-activate 'grep-compute-defaults)))

;;---------- Misc Settings

(setq column-number-mode t)
(setq default-tab-width 4)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Automatically revert files that are updated outside of the editor
(global-auto-revert-mode)
