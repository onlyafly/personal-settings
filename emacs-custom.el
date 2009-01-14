;; KEVIN ALBRECHT 1/13/2009
;;
;; .emacs should contain:
;;   (add-to-list 'load-path "~/src/personal-settings")
;;   (load-library "~/src/personal-settings/emacs-custom.el")

;;-----------------------------------------------------------------------------
;;----- VARIABLES

;; See if we're on MS Windows or some other OS
(defvar is-system-windows (string-match "windows" (symbol-name system-type)))
(defvar is-system-mac (string-match "darwin" (symbol-name system-type)))

;;-----------------------------------------------------------------------------
;;----- FUTURE SUGGESTIONS

;(setq inhibit-startup-message t)
;(shell) ; run a shell

;;-----------------------------------------------------------------------------
;;----- PREINSTALLED

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;-----------------------------------------------------------------------------
;;----- FONT SETUP

(if is-system-windows
    (progn
      (set-default-font "-outline-Bitstream Vera Sans
Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")))

;;-----------------------------------------------------------------------------
;;----- CLOJURE

(add-to-list 'load-path "~/src/clojure/clojure-mode")
(add-to-list 'load-path "~/src/clojure/swank-clojure")
(setq swank-clojure-binary "clj")
(setq inferior-lisp-program "clj")

(add-hook 'clojure-mode-hook
    '(lambda ()
       (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)))

(require 'clojure-paredit)
(require 'clojure-auto)
(require 'swank-clojure-autoload)

;;-----------------------------------------------------------------------------
;;----- MACRO HELPERS

(defun save-macro (name)
 "save a macro. Take a name as argument
 and save the last defined macro under
 this name at the end of your .emacs"
 (interactive "SName of the macro :")  ; ask for the name of the macro
 (kmacro-name-last-macro name)         ; use this name for the macro
 (find-file "~/.emacs")                ; open the .emacs file
 (goto-char (point-max))               ; go to the end of the .emacs
 (newline)                             ; insert a newline
 (insert-kbd-macro name)               ; copy the macro
 (newline)                             ; insert a newline
 (switch-to-buffer nil))               ; return to the initial buffer

;;-----------------------------------------------------------------------------
;;----- SAVED MACROS


(fset 'five
  (lambda (&optional arg) "Keyboard macro." (interactive "p")
(kmacro-exec-ring-item (quote ("5" 0 "%d")) arg)))


(fset 'six
  (lambda (&optional arg) "Keyboard macro." (interactive "p")
(kmacro-exec-ring-item (quote ("6" 0 "%d")) arg)))
