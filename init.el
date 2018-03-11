;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; better-defaults

     ;; Required for less-css-mode
     html
     ;; chrome ;; 2018-02-01 — Doesn't work here.
     (clojure :variables
              clojure-enable-fancify-symbols t)
     (colors :variables
             colors-enable-rainbow-mode t)
     elm
     emacs-lisp
     git
     github

     (ivy :variables
          ;; Wrap while traversing in minibuf
          ivy-wrap t
          ;; Show the full virtual file paths
		  		ivy-virtual-abbreviate 'full
          ;; Show current/all match count
		  		;; TODO: Does not work with `spacemacs/search-project-auto'
		  		;; ivy-count-format "%d/%d "
          ;; Hide "./" and "../" in the `counsel-find-file' completion list
		  		ivy-extra-directories nil)

     nlinum
     (markdown :variables
               ;; Install executable for github-flavored markdown via `npm -g i vmd'
               markdown-live-preview-engine 'vmd)
     ;; org
     osx

     ;; 2017-07-05 - Deactivated in favor of `rjsx-mode' from the `aj-javascript' layer below.
     ;; (javascript :variables
     ;;             js2-mode-show-strict-warnings nil
     ;;             js2-mode-show-parse-errors nil)
     ;; react

     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-enable-smart-eshell t
            shell-default-position 'bottom
            shell-default-full-span nil)

     (spell-checking :variables
                     ;; Enable manually with `SPC t S' - avoids noise due to
                     ;; false positives in programming modes.
                     spell-checking-enable-by-default nil
                     ;; Tries to detect the current language from the buffer
                     ;; content, and activate the corresponding dictionary.
                     spell-checking-enable-auto-dictionary t
                     ;; Enable auto-completion popup when the point is idle on a misspelled word
                     enable-flyspell-auto-completion t)

     search-engine
     spacemacs-layouts
     syntax-checking

     ;; https://github.com/syl20bnr/spacemacs/issues/4285#issuecomment-234579627
     (version-control :variables
                      version-control-global-margin t
                      ;; 2017-07-05 - The only gutter plugin I got working reliable
                      ;; with the custom bitmaps below.
                      version-control-diff-tool 'git-gutter+)
     yaml

     ;; In case of problems with `smartparens' look here:
     ;; http://milosophical.me/blog/2015/spacemacs-00.html
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"

                      ;; 2017-07-05 - Deactivated because the location of the tooltip
                      ;; is not reliable when working with multiple displays..?
                      auto-completion-enable-help-tooltip nil)

     aj-javascript)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; Example:
   ;; dotspacemacs-additional-packages
   ;; '((helm-spotify :location (recipe :fetcher github :repo "syl20bnr/helm-spotify")))
   dotspacemacs-additional-packages '(atomic-chrome
                                      solaire-mode
                                      define-word
                                      doom-themes
                                      evil-lion
                                      fancy-narrow
                                      fringe-helper
                                      google-this
                                      graphql-mode
                                      key-chord
                                      ivy-rich
                                      tldr
                                      all-the-icons-ivy
                                      ;; TODO 2017-07-05
                                      ;; https://github.com/domtronn/all-the-icons.el/issues/28
                                      ;; Somehow this noticeably leads to lags/slow rendering
                                      ;; I've tried using (setq inhibit-compacting-font-caches t)
                                      ;; Also the setup doesn't work; need to run it explicitely?
                                      ;; spaceline-all-the-icons
                                      pretty-mode
                                      magithub)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; lastest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 10
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "S-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
	 dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t
   )

  (defun define-keys (mode-map key-fn-pairs)
    (mapc (lambda (key-fn)
            (let ((key (kbd (car key-fn)))
                  (function (car (cdr key-fn))))
              (define-key mode-map key function)))
          key-fn-pairs)
    ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/FAQ.org#why-is-spacemacs-hanging-on-startup
  ;; Had this problem at the office; maybe just related to their network's setup?
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; Prevent Custom from dumping its local settings into this file.
  (add-to-load-path "~/.spacemacs.d/packages")
  (setq custom-file "~/.spacemacs.d/.custom")
  (load-file custom-file)
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq epa-pinentry-mode 'loopback)
  (setq epa-armor t)
  ;; (auth-source-search :host "api.github.com" :user "jazen")

  ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
  (setq auto-window-vscroll nil)

  (defun require-template (symbol)
    `(require (quote ,symbol)))

  (defmacro load-my-packages ()
    (let* ((filter-fn (lambda (x) (not (member x '("." "..")))))
           (strip-ext-fn (lambda (x) (s-left -3 x)))
           (all-files (directory-files "~/.spacemacs.d/packages/"))
           (package-files-ext (-filter filter-fn all-files))
           (package-files (-map strip-ext-fn package-files-ext))
           (package-symbols (-map 'read package-files))
           (package-requires (-map 'require-template package-symbols)))
      `(progn ,@package-requires)))

  (load-my-packages)

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; If you're going fuzzy all the way, you can do without the initial ^,
  ;; and simply let flx (did you install it?) sort the matches in a nice way:
  (setq-default
	 ivy-re-builders-alist
	 '((swiper . ivy--regex-plus)   ;; Restore old behaviour for swiper
		 (t      . ivy--regex-fuzzy))

	 ivy-initial-inputs-alist nil
	 )

  (when (spacemacs/system-is-mac)
    (setq-default mac-function-modifier 'hyper      ;; Use [fn] as Hyper (`H' prefix)
                  mac-control-modifier 'control
                  mac-command-modifier 'super       ;; Use `s' prefix
                  mac-right-command-modifier 'super

                  mac-option-modifier 'none         ;; Unbind left [alt] for accented input
                  mac-right-option-modifier 'meta
                  ;; mac-right-control-modifier 'control ;; No right control here
                  ))

  (define-key evil-motion-state-map (kbd "$") 'evil-last-non-blank)

  ;; Enables moving a selection in visual state up/down
  ;; http://vim.wikia.com/wiki/Moving_lines_up_or_down
  (define-key evil-visual-state-map "J"
    (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K"
    (concat ":m '<-2" (kbd "RET") "gv=gv"))

  ;; Use `v' in visual state to expand the region
  (define-key evil-visual-state-map "v" 'er/expand-region)

  ;; (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  ;; (define-key ivy-minibuffer-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Powerline: current date/time
  (setq-default display-time-string-forms
                '((substring year -4)
                  "-"
                  (format "%02d" (string-to-number month))
                  "-"
                  (format "%02d" (string-to-number day))
                  " "
                  24-hours
                  ":"
                  minutes))
	(display-time-mode t)

	;; Powerline: battery life
	;; (fancy-battery-mode t)

  ;; via http://cestlaz.github.io/posts/using-emacs-40-atomic-chrome
  (use-package atomic-chrome
    :ensure t
    :init
    (progn
      (atomic-chrome-start-server))
    :config
    (progn
      (setq atomic-chrome-buffer-open-style 'frame)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))
      (add-hook 'atomic-chrome-edit-done-hook
                (lambda () (shell-command "open -a \"Google Chrome\"")))))

  (use-package compile
    :defer t
    :init
    (use-package ansi-color
      :config
      (defun colorise-compilation-buffer ()
        (toggle-read-only)
        (ansi-color-apply-on-region (point-min) (point-max))
        (toggle-read-only))
      (add-hook 'compilation-filter-hook 'colorise-compilation-buffer)
      (add-hook 'compilation-mode-hook 'colorise-compilation-buffer)))

  (setq ivy-sort-matches-functions-alist
        '((t)
          (ivy-switch-buffer . ivy-sort-function-buffer)
          (counsel-find-file . ivy-sort-function-buffer)))

  (defun jazen/projectile-find-file-split (file)
    (spacemacs/find-file-split (expand-file-name file (projectile-project-root))))
  (defun jazen/projectile-find-file-vsplit (file)
    (spacemacs/find-file-vsplit (expand-file-name file (projectile-project-root))))
  (defun jazen/projectile-delete-file-confirm (file)
    (spacemacs/delete-file-confirm (expand-file-name file (projectile-project-root))))

  (ivy-set-actions
   'counsel-projectile-find-file
   '(("v" jazen/projectile-find-file-vsplit "in vertical split")
     ("s" jazen/projectile-find-file-split "in horizontal split")
     ("d" jazen/projectile-delete-file-confirm "delete file")))

  (defun jazen/pop-to-buffer-vsplit (buffer)
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . right))))

  (defun jazen/pop-to-buffer-split (buffer)
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . below))))

  (ivy-set-actions
   'ivy-switch-buffer
   '(("v" jazen/pop-to-buffer-vsplit "in vertical split")
     ("s" jazen/pop-to-buffer-split "in horizontal split")))

  (defun jazen/ivy-paste (x)
    (insert
     (if (stringp x)
         x
       (car x))))

  (defun jazen/ivy-yank (x)
    (kill-new
     (if (stringp x)
         x
       (car x))))

  ;; Add i and w to ivy actions to insert/copy the selection
  (ivy-set-actions
   t
   '(("p" jazen/ivy-paste "paste")
     ("y" jazen/ivy-yank "yank")))

  ;; (spaceline-toggle-hud-off)

  ;; Recenter after jump to definition
  (advice-add 'dumb-jump-go :after (lambda (&rest args) (recenter-top-bottom)))

  ;; Move point to the beginning of the line before opening a new line
  (advice-add 'open-line :before 'beginning-of-line)

  ;; Make sure vertical windows are split evenly
  (advice-add 'split-window-right :after #'balance-windows)

  ;; TODO How to make custom functions available in M-x?
  ;; (defun hf-paste-search ()
  ;;   (interactive)
  ;;   (let (line)
  ;;     (save-excursion
  ;;       (counsel-grep-or-swiper)
  ;;       (setq line (thing-at-point 'line)))
  ;;     (insert line)))

  ;; (use-package wand
  ;;   :ensure t
  ;;   :bind (("C-<return>" . wand:execute-current-line)
  ;;          ("<return>" . wand:execute))
  ;;   :config
  ;;   (wand:add-rule-by-pattern :match "https?://"
  ;;                             :capture :whole
  ;;                             :action browse-url-at-point)
  ;;   (wand:add-rule-by-pattern :match "\\$ "
  ;;                             :capture :after
  ;;                             :action shell-command))

  (defun jazen/shell-command (start end)
    (interactive "r")
    (shell-command (buffer-substring-no-properties start end)))

  (defhydra hydra-test (global-map "C-<return>")
    "test"
    ("p" ping)
    ("s" jazen/shell-command))

  (use-package define-word
    :ensure t
    :bind (("H-d" . define-word-at-point)
           ("H-D" . define-word)))

  (use-package tldr
    :ensure t
    :defer t)

  ;; Use `rg' as much as possible and avoid hanging emacs when searching
  ;; files with very long lines.
  ;; https://oremacs.com/2018/03/05/grep-exclude/
  (setq-default
   counsel-git-cmd "rg --files"
   counsel-rg-base-command "rg -S -M 120 --no-heading --line-number --color never %s .")

  (setq-default
   counsel-ag-base-command
   (concat "ag "
           "--noheading "
           "--nogroup "
           "--nocolor "
           "--skip-vcs-ignores "
           "--smart-case "
           "--follow " ; follow symlinks
           "%S"))

  (setq-default
   x-stretch-cursor t
   find-file-visit-truename t
   ping-program-options '("-c" "4")

   avy-timeout-seconds 0.35
   dumb-jump-prefer-searcher 'rg
   evil-escape-unordered-key-sequence t

   nlinum-format "%4d \u2502"
   nlinum-highlight-current-line t
   ;; git-gutter-fr+-side 'left-fringe
   ;; fringes-outside-margins t

   ;; word-wrap t ;; Natural reading; wrap at word
   ;; tab-always-indent 'complete
   google-translate-default-target-language "de"

   projectile-enable-caching t

   indent-tabs-mode nil ;; Indentation can't insert tabs
   tab-width 2
   js-indent-level 2

   history-delete-duplicates t
   scroll-margin 10 ;; Give your cursor some context

   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   web-mode-markup-indent-offset 2

   web-mode-enable-auto-pairing t
   web-mode-enable-auto-closing t
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-enable-css-colorization t

   clean-buffer-list-kill-regexps '("^.*$") ;; Set what to kill (everything)
   clean-buffer-list-kill-never-buffer-names ;; Set what not to kill
   '("*Messages*"
     "*cmd*"
     "*scratch*")

   prettify-symbols-unprettify-at-point 'right-edge)

  ;; (midnight-mode)

  ;; https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; https://github.com/Malabarba/aggressive-indent-mode#customization
  (global-aggressive-indent-mode 1)
  ;; (defun disable-aggressive-indent ()
  ;;   (interactive)
  ;;   (aggressive-indent-mode -1))
  ;; (add-hook 'evil-insert-state-entry-hook 'aggressive-indent-mode)
  ;; (add-hook 'evil-insert-state-exit-hook 'disable-aggressive-indent)

  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)))

  (defvar jazen/clojure--prettify-symbols-alist
    '(("not=" . ?≠)
      ("->" . ?→)
      ("identical?" . ?≡)))

  (defvar jazen/js--prettify-symbols-alist
    '(("function" . ?ƒ)
      ("!=" . ?≠)
      ("!==" . ?≢)
      ("===" . ?≡)))

  (defvar jazen/lisp--prettify-symbols-alist
    '(("compose" . ?∘)
      ("curry" . ?»)
      ("defun" . ?ƒ)
      ("rcurry" . ?«)
      ("."      . ?•)
      ("eq" . ?=)))

  (eval-after-load 'js2-mode
    '(setq js--prettify-symbols-alist
           (append jazen/js--prettify-symbols-alist
                   js--prettify-symbols-alist)))

  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append jazen/clojure--prettify-symbols-alist
                   clojure--prettify-symbols-alist)))

  (eval-after-load 'lisp-mode
    '(setq lisp--prettify-symbols-alist
           (append jazen/lisp--prettify-symbols-alist
                   lisp-prettify-symbols-alist)))

  (global-prettify-symbols-mode 1)

  ;; TODO https://github.com/ekaschalk/dotspacemacs/blob/master/.spacemacs#L780
  ;; (global-pretty-mode t)

  (defun jazen/cider-annotate-completion-function (type ns)
    "Get completion function based on TYPE and NS."
    (concat (when ns (format " (%s)" ns))
            (when type (format " %s" type))))

  (with-eval-after-load 'cider-mode
    (setq cider-pprint-fn 'puget)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-annotate-completion-function #'jazen/cider-annotate-completion-function)
    (setq nrepl-buffer-name-show-port t)
    (setq cider-repl-wrap-history t)
    (setq cider-repl-history-file "~/.cider-repl-history")
    (setq cider-repl-use-pretty-printing t)
    (setq cider-repl-result-prefix ";; => ")
    (setq cider-repl-pop-to-buffer-on-connect 'display-only))

  (with-eval-after-load 'clj-refactor
    (setq clojure-thread-all-but-last t))

  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (setq-default
   magit-repository-directories '("~/git")
   magit-diff-refine-hunk 'all
   magit-log-arguments (quote ("-n256" "--graph" "--decorate" "--color")))

  ;; Start commit in insert mode
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (advice-add 'magit-commit-diff :after (lambda ()
                                          "Disable magit-commit-show-diff."
                                          (setq magit-commit-show-diff nil)))
  (global-git-commit-mode t)

  (require 'gh)
  (setq-default
   paradox-execute-asynchronously t
   paradox-github-token (gh-auth-get-oauth-token))

  (setq powerline-default-separator 'utf-8)
  (setq powerline-gui-use-vcs-glyph t)
  ;; (setq ns-use-srgb-colorspace nil)
  ;; (spaceline-compile)

  (define-keys global-map
    '(("C-c C-." dumb-jump-go)
      ("C-c C-," dumb-jump-back)
      ("C-s" counsel-grep-or-swiper)
      ("M-g M-g" hydra-git-gutter+/body)))

  ;; No idea what goes wrong but I need to explicitly bind the keys.
  (define-keys yas-minor-mode-map
    '(("<tab>" yas-expand)))

  (define-keys evil-normal-state-map
    '(("j" evil-next-visual-line)
      ("k" evil-previous-visual-line)
      ("+" evil-numbers/inc-at-pt)
      ("-" evil-numbers/dec-at-pt)))

  ;; (define-key evil-normal-state-map (kbd "ö SPC") 'evil-unimpaired/insert-space-above)
  ;; (define-key evil-normal-state-map (kbd "Ö SPC") 'evil-unimpaired/insert-space-below)
  ;; (define-key evil-normal-state-map (kbd "ö P") 'evil-unimpaired/paste-above)
  ;; (define-key evil-normal-state-map (kbd "ö p") 'evil-unimpaired/paste-below)

  ;; Bury buffers instead of killing them by default
  (spacemacs/set-leader-keys "bd" 'bury-buffer)
  (spacemacs/set-leader-keys "bk" 'spacemacs/kill-this-buffer)
  (spacemacs/set-leader-keys "bK" 'spacemacs/kill-other-buffers)

  (defun jazen/esc-and-save ()
    "Run `<ESC>' and `save-buffer' in sequence."
    (interactive)
    (evil-normal-state)
    (save-buffer))

  (defun jazen/esc-and-jump ()
    "Run `<ESC>' and `evil-avy-goto-word-or-subword-1' in sequence."
    (interactive)
    (evil-normal-state)
    (evil-avy-goto-word-or-subword-1))

  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'jazen/esc-and-save)
  (key-chord-define evil-insert-state-map "jj" 'jazen/esc-and-jump)
  (key-chord-define evil-normal-state-map "jj" 'evil-avy-goto-word-or-subword-1)
  ;; (key-chord-define evil-normal-state-map "jj" 'evil-avy-goto-char-2)

  ;; https://github.com/ekaschalk/.spacemacs.d/blob/5ec0cdbbba8cd9bfd47d895bfcd04e549c881de2/layers/macros/funcs.el#L40
  (defun evil-global-set-keys (STATES &rest BINDINGS)
    "`evil-global-set-key' for all STATES with possibly many BINDINGS."
    (--each STATES
      (-each (-partition 2 BINDINGS)
        (-lambda ((key cmd))
          (evil-global-set-key it key cmd)))))

  (evil-global-set-keys
   '(normal visual motion)
   "H" 'evil-first-non-blank
   "L" (lambda () (interactive) (evil-end-of-line)))  ; Interactive fixes visual mode

  ;; https://github.com/syl20bnr/spacemacs/wiki/Keymaps-guide
  (define-keys evil-normal-state-map
    '(("SPC s o" google-this-word)
      ("s" avy-goto-char-timer)))

  ;; Open files and go places like we see from error messages, i e: path:line:col
  ;; (to-do "make `find-file-line-number' work for emacsclient as well")
  ;; (to-do "make `find-file-line-number' check if the file exists")
  (defadvice find-file (around find-file-line-number
                               (path &optional wildcards)
                               activate)
    "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
    (save-match-data
      (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path))
             (line-no (and match
                           (match-string 2 path)
                           (string-to-number (match-string 2 path))))
             (col-no (and match
                          (match-string 3 path)
                          (string-to-number (match-string 3 path))))
             (path (if match (match-string 1 path) path)))
        ad-do-it
        (when line-no
          ;; goto-line is for interactive use
          (goto-char (point-min))
          (forward-line (1- line-no))
          (when (> col-no 0)
            (forward-char (1- col-no)))))))

  ;; http://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close#11059012
  (defun jazen/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (if (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
        (run-with-timer 1 nil
                        (lambda (buf)
                          (bury-buffer buf)
                          (delete-windows-on "*compilation*"))
                        buffer)))

  (add-hook 'compilation-finish-functions #'jazen/bury-compile-buffer-if-successful)

  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun jazen/use-eslint-from-node-modules ()
    (interactive)
    (let ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 (lambda (dir)
                   (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                     (and eslint (file-executable-p eslint)))))))
      (when root
        (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
          (setq-local flycheck-javascript-eslint-executable eslint)))))

  ;; (add-hook 'flycheck-mode-hook #'jazen/use-eslint-from-node-modules)
  (add-hook 'prog-mode-hook 'rainbow-mode)

  ;; https://github.com/syl20bnr/spacemacs/issues/10315#issuecomment-365688761
  (spacemacs|use-package-add-hook company
    :post-config
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [return] nil))

  (global-company-mode t)

  (progn
    (require 'git-gutter-fringe+)
    ;; Git Gutter
    (set-face-attribute
     'git-gutter+-added nil :foreground "#b6ff00" :weight 'normal)
    (set-face-attribute
     'git-gutter+-deleted nil :foreground "red" :weight 'normal)
    (set-face-attribute
     'git-gutter+-modified nil :foreground "#ffbb00" :weight 'normal)

    (fringe-helper-define 'git-gutter-fr+-added nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......")
    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......")
    (fringe-helper-define 'git-gutter-fr+-modified nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......")
    )

  ;; No idea how he got it working: https://gist.github.com/hlissner/f9da197d40d1a0415a66b0bef49696fc
  ;;
  ;; (define-fringe-bitmap 'git-gutter-fr:added
  ;;   [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  ;;   nil nil 'center)
  ;; (define-fringe-bitmap 'git-gutter-fr:modified
  ;;   [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  ;;   nil nil 'center)
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted
  ;;   [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
  ;;   nil nil 'center)

  ;; Magit Popup extensions; taken from
  ;; https://github.com/magit/magit/wiki/Additional-proposed-infix-arguments-and-suffix-commands

  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")

  (autoload 'org-read-date "org")

  (defun magit-org-read-date (prompt &optional _default)
    (org-read-date 'with-time nil nil prompt))

  (magit-define-popup-option 'magit-log-popup
    ?s "Since date" "--since=" #'magit-org-read-date)

  (magit-define-popup-option 'magit-log-popup
    ?u "Until date" "--until=" #'magit-org-read-date)

  (defhydra hydra-git-gutter+
    (:body-pre (git-gutter+-mode 1) :hint nil)
    "
Git gutter:

  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter+
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk
"
    ("j" git-gutter+-next-hunk)
    ("k" git-gutter+-previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter+-next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter+-previous-hunk 1)))
    ("s" git-gutter+-stage-hunks)
    ("r" git-gutter+-revert-hunk)
    ("p" git-gutter+-popup-hunk)
    ;; ("R" git-gutter+-set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter+-mode -1)
                ;; git-gutter+-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter+-clear))
     :color blue))

  (use-package fancy-narrow
    :ensure t
    ;; :quelpa (fancy-narrow :repo Malabarba/fancy-narrow :fetcher github)
    :diminish
    :config
    (progn
      (fancy-narrow-mode)))

  (define-keys evil-normal-state-map
    '(("SPC n r" fancy-narrow-to-region)
      ("SPC n w" fancy-widen)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
