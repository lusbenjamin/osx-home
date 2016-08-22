;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Environment
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs path
(add-to-list 'load-path "~/emacs-lisp")

;; Homebrew packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; No backups becasuse VCS
(setq make-backup-files nil)

;; No blocking file access
(setq create-lockfiles nil)

;; Automatically refresh buffers from disk
(global-auto-revert-mode 1)

;; stdout is bound to ASCII by default in Python 3
;; fix this. sigh
(setenv "LC_CTYPE" "UTF-8")

;; OSX 10.11 native fullscreen mode crashes when any frame gets a C-x C-c
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-11/msg00020.html
(setq ns-use-native-fullscreen nil)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Listen for emacsclient commands
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
  (package-refresh-contents))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (when (y-or-n-p (format "Package %s is missing. Install it? " package))
         (package-refresh-contents)
         (package-install package)
         (package-initialize)
         package)))
   packages))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fix our emacs shell path
(ensure-package-installed 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; swap some keys on OS X
(setq mac-command-modifier 'option)
(setq mac-control-modifier 'control)
(setq mac-option-modifier 'meta)

;; sane behavior for selected regions
(delete-selection-mode 1)

;; smoother scrolling
(setq mouse-wheel-scroll-amount '(0.005))

;; turn of the menu bars
(menu-bar-mode 0)
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))

;; launch first frame as fullscreen
(setq initial-frame-alist '((fullscreen . fullscreen)))
(global-set-key (kbd "M-`") 'toggle-frame-fullscreen)

(ensure-package-installed 'auto-complete)
(require 'auto-complete)

(show-paren-mode 1)
;; Show matching paren offscreen in the minibuffer
;; http://emacswiki.org/emacs/ShowParenMode
(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
      "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
      (interactive)
      (let* ((cb (char-before (point)))
             (matching-text (and cb
                                 (char-equal (char-syntax cb) ?\) )
                                 (blink-matching-open))))
        (when matching-text (message matching-text))))

;; Fix obscure bug with desktop mode file names
;(setq desktop-file-version (format "%s" desktop-file-version))

(ensure-package-installed 'solarized-theme)
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

(ensure-package-installed 'powerline)
(require 'powerline)
(powerline-default-theme)
(scroll-bar-mode -1)

(ensure-package-installed 'neotree)
(require 'neotree)
(global-set-key (kbd "C-x C-t") 'neotree)

(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Language Modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed
 'csv-mode
 'gitconfig-mode
 'gitignore-mode
 'json-mode
 'markdown-mode
 'scss-mode
 'web-mode
 'yaml-mode
 )
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))

(ensure-package-installed 'json-reformat)
(require 'json-reformat)


;; TODO explore jedi


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Helm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed 'helm)
;; http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;; projectile integration
(ensure-package-installed 'projectile
                          'helm-projectile
                          'ag
                          'helm-ag)
(require 'projectile)
(require 'helm-projectile)
(require 'ag)
(require 'helm-ag)
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "C-x C-d") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-projectile)
(global-set-key (kbd "C-x C-g") 'helm-projectile-ag)

;open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p           t
      ; move to end or beginning of source when reaching top or bottom of source.
      helm-move-to-line-cycle-in-source     t
      ; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-scroll-amount                    8
      helm-net-prefer-curl                  t
      helm-ag-fuzzy-match                 t
      helm-ag-use-agignore                t
      helm-completion-in-region-fuzzy-match t
;      helm-buffers-fuzzy-matching           t
;      helm-recentf-fuzzy-match              t
;      helm-M-x-fuzzy-match                  t
      )

(helm-autoresize-mode 1)

(helm-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inline Linting with Flycheck
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ensure-package-installed 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-mode 'html-tidy 'web-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'json-mode)
(add-to-list 'flycheck-disabled-checkers '(javascript-jshint))

;; HACK aim linter executables at project-specific scripts
(setq flycheck-html-tidy-executable "/usr/local/bin/tidy")
(setq flycheck-javascript-eslint-executable "~/code/10stories/run_eslint.sh")
(setq flycheck-python-pylint-executable "~/code/10stories/run_pylint.sh")


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

(djcb-program-shortcut (kbd "<S-f1>") "shell-1" (concat "cd ~/code/10stories"
                                                        " && . .venv/bin/activate"
                                                        " && git st"))
(djcb-program-shortcut (kbd "<S-f2>") "shell-2" (concat "cd ~/code/10stories"
                                                        " && . .venv/bin/activate"
                                                        " && git st"))
(djcb-program-shortcut (kbd "<S-f3>") "webserver" (concat "cd ~/code/10stories"
							  " && . .venv/bin/activate"
							  " && ./run runserver"))
(global-set-key (kbd "<S-f4>") '(lambda() (interactive)
                                  (browse-url "http://localhost:5000/tell-me-a-story")
                                  (message "Launched browser to localhost")))
(djcb-program-shortcut (kbd "<S-f5>") "dbserver" "postgres -D /usr/local/var/postgres")
(global-set-key (kbd "<S-f6>") '(lambda() (interactive)
                                  (djcb-term-start-or-switch "bash" t "psql_mon"
                                                             (concat "cd ~/code/10stories"
                                                                     " && . .venv/bin/activate"
                                                                     " && ./run psql_mon"))
                                  (browse-url "file:///tmp/queries.html")
                                  (message "Launched browser to psql_mon")))
(djcb-program-shortcut (kbd "<S-f7>") "python" (concat "cd ~/code/10stories"
                                                       " && . .venv/bin/activate"
                                                       " && ./run shell"))
(global-set-key (kbd "<S-f8>") '(lambda() (interactive)
                                  (djcb-term-start-or-switch "bash" nil "shell-spawn")))

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
 '(browse-url-browser-function (quote browse-url-chromium))
 '(browse-url-chromium-program "~/bin/chrome")
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-save-mode t)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(web-mode-attr-indent-offset 4)
 '(web-mode-attr-value-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
