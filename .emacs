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
(when (not package-archive-contents)
  (package-refresh-contesnts))

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

;; Language modes
(ensure-package-installed
 'gitconfig-mode
 'gitignore-mode
 'markdown-mode
 'scss-mode
 'web-mode
 'yaml-mode
 )
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Find files and references using Git
(ensure-package-installed 'find-things-fast)
(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)
(global-set-key '[f2] 'ftf-grepsource)

;; Sexier buffer list with git support
(ensure-package-installed 'ibuffer)
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(ensure-package-installed 'ibuffer-git)

(require 'ibuffer-git)
(setq ibuffer-git-column-length 8)
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only git-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      filename-and-process)))
(setq ibuffer-default-sorting-mode 'major-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inline Linting with Flycheck
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-css-csslint-executable "/usr/local/bin/csslint")
(setq flycheck-javascript-jshint-executable "/usr/local/bin/jshint")
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
(setq flycheck-json-jsonlint-executable "/usr/local/bin/jsonlint")
; Temporarily aiming python linter at project-specific file
;(setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
(setq flycheck-python-pylint-executable "~/code/10stories/run_pylint.sh")
(setq flycheck-sh-shellcheck-executable "/usr/local/bin/shellcheck")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Terminal Wizardry
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up starting terms with programs in them
(defun djcb-term-start-or-switch (prg &optional use-existing name cmd)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (if name (concat "*" name "*") (concat "*" prg "*"))))
    (when (not (and use-existing
                 (let ((buf (get-buffer bufname)))
                   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg (or name prg))
      (if cmd (process-send-string bufname (concat cmd "\n"))))))

(defmacro djcb-program-shortcut (key &optional fullname cmd)
  "* macro to create a key binding KEY to start some terminal program PRG;
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key
     '(lambda()
        (interactive)
        (djcb-term-start-or-switch "bash" t ,fullname ,cmd))))

(djcb-program-shortcut (kbd "<S-f3>") "shell" "cd ~/code/10stories")
(djcb-program-shortcut (kbd "<S-f4>") "web-shell" (concat "cd ~/code/10stories"
							  " && . .venv/bin/activate"
							  " && ./run shell"))
(djcb-program-shortcut (kbd "<S-f5>") "web-server" (concat "cd ~/code/10stories"
							  " && . .venv/bin/activate"
							  " && ./run runserver"))
(djcb-program-shortcut (kbd "<S-f6>") "postgres" "postgres -D /usr/local/var/postgres")

;; http://emacs-journey.blogspot.com/2011/02/proper-ansi-term-yankpaste.html
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))
(global-set-key "\C-cy" 'my-term-paste)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TODO  miscellany
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed
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
)


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
