;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Environment
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs path
(add-to-list 'load-path "~/emacs-lisp")

;; Shell path
(setenv "PATH" (concat (expand-file-name "~/") "bin" ":" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))

;; No backups becasuse VCS
(setq make-backup-files nil)

;; No blocking file access
(setq create-lockfiles nil)

;; Automatically refresh buffers from disk
(global-auto-revert-mode 1)

;; Launch the edit server to listen for edit commands 'ec'
(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; refresh package listings
;(when (not package-archive-contents)
  ;(package-refresh-contesnts))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swap some keys on OS X
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'control)
(setq mac-option-modifier 'control)

;; spaces, not tabs
(setq c-basic-offset 4)
(setq nxml-child-indent 4)

;; turn of the menu bars
(menu-bar-mode 0)
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))

;; Fix obscure bug with desktop mode file names
;(setq desktop-file-version (format "%s" desktop-file-version))

(ensure-package-installed 'color-theme-solarized)
(load-theme 'solarized-dark' t)
(set-frame-font "-*-Andale Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1 (#x03)")

;; Disable bell when scrolling to end of buffer
;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; Custom package for quickly moving lines/blocks vertically
;;(require 'move-lines)
;;(move-lines-binding)

(ensure-package-installed 'move-text)
(require 'move-text)
(move-text-default-bindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inline Linting with Flymake
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;(ensure-package-installed 'pylint)

;(ensure-package-installed
 ;'flymake-jslint
 ;'flymake-python-pyflakes
 ;'pymacs
;)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TODO language modes and miscellany
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(ensure-package-installed
 ;; Language modes
 ;'web-mode
 ;'markdown-mode
 ;'python-mode
 ;'yaml-mode
 ;'sass-mode
 ;; Git
 ;'ibuffer-git
 ;; Faces / colors
 ;'faces+
 ;'menu-bar+
 ;'color-theme
 ;'doremi
 ;'doremi-frm
 ;'doremi-cmd
 ;'frame-cmds
 ;'frame-fns
 ;'facemenu+
 ;; Other stuff
 ;'yasnippet-bundle
 ;'persistent-scratch
 ;'hexrgb
 ;'auto-complete
;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auto-Generated Custom Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-save-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
