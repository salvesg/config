;; This is my own emacs configuration file. It is mainly for learning :D

;; Inhibit the start-up message
(setq inhibit-statup-message t)

(scroll-bar-mode -1)               ;; No scroll bar
(tool-bar-mode -1)                 ;; No tool bar
(tooltip-mode -1)                  ;; Disable tooltips
(set-fringe-mode 10)               ;; Give some breathing room
(menu-bar-mode -1)                 ;; Disable the menu bar
(column-number-mode)               ;; Set column number
(global-display-line-numbers-mode) ;; Line numbers in all buffers

(dolist (mode '(shell-mode-hook)) ;; A hook is a feature that allows you to add functionalities to certain methods
  (add-hook mode (lambda () (display-line-number-mode 0)))) ;; The same hook can be added multiple times, be carefull

(setq visible-bell t)              ;; Enough beeping alreadyq

;; Things added by the custom feature
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load-theme 'wombat)  ;; Example theme

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package) ;; Brings into the scope all the package related functions

;; Set the package repositories to fetch from
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize) ;; Iniatialize the package system
(unless package-archive-contents
  (package-refresh-contents)) ;; See if there is a list of packages ready, if not, create it

;; Functions ending in -p are predicative functions which take an argument and return t or nil
(unless (package-installed-p 'use-package) ;; Check if use-package is installed
  (package-install 'use-package))          ;; install it if not

(require 'use-package)
(setq use-package-always-ensure t)  ;; It will make sure that packages are downloaded

(use-package command-log-mode) ;; This makes a window whith commands to prompt

(use-package counsel
  :diminish
  :init (counsel-mode 1))

(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ;; Every time we are in a programing mode, use the rainbow delimiters

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


