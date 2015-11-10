(setq default-directory "~/projects")

(text-scale-decrease 2)

(global-flycheck-mode t)

(require 'auto-complete)
(global-auto-complete-mode t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(scroll-bar-mode -1)

(setq x-select-enable-clipboard t)

(global-whitespace-mode -1)

;;; Jabber
(global-set-key (kbd "C-c C-j") jabber-global-keymap)
;;; Sets jabber account password for Gtalk to application password. Neat!
(setq jabber-account-list '(("lyterk@gmail.com" (:password . "xpanfwiwddntlcyd"))))                                
;;; This prevents the absurd kev@i-did-not-set--mail-host-address--so-tickle-me> error.
;;; Seriously, these Emacs guys must take some really interesting drugs.
(setq mail-host-address "lyterk@gmail.com")

;;; Should prevent emacs -nw from making window opacque.
(set-frame-parameter (selected-frame) 'alpha '(80 80))
(add-to-list 'default-frame-alist '(alpha 80 80))

;;; Should set face attributes col
(set-face-attribute 'default nil :background "black" :foreground "white" :height 82)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))
;;; I fucking *LOVE* Emacs.
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired"
                (mode . dired-mode)
                )
               ("Scala"
                (or
                 (mode . scala-mode)
                 (mode . sbt-mode)
                ))
               ("Front-End"
                (or
                 (mode . css-mode)
                 (mode . html-mode)
                 (mode . js-mode)
                 ))
               ("Git"
                (or
                 (name . "\*magit")
                 (mode . gitignore-mode)
                 ))
               ("Text-Editing"
                (or
                 (mode . latex-mode)
                 (mode . text-mode)
                 ))
               ("Shells"
                (or
                 (mode . eshell-mode)
                 (mode . term-mode)
                 ))
               ("Android"
                (or
                 (mode . nxml-mode)
                 (mode . java-mode)
                 (name . ".md$")
                 (name . ".gradle$")
                 ))
               ("EAP"
                (or
                 (name . "\*EAP\*")
                 (name . "\*EAP Playlist\*")                 
                 ))
               ("Emacs"
                (or
                 (name . "\*Help\*")
                 (name . "\*Apropos\*")
                 (name . "\*info\*")
                 (name . "\*Compile-Log\*")
                 (name . "\*Backtrace\*")
                 (name . "\*Messages\*")
                 (mode . emacs-lisp-mode)
                 ))
               ("Org"
                (mode . org-mode))
               ))))

;;; Configurations for Alsaplayer
(add-to-list 'load-path "~/.emacs.d/emacs-alsaplayer/")
(setq eap-music-library "~/Music"
      eap-playlist-library "~/.alsaplayer/playlists")
(require 'eap-autoloads)

;;; Python IDE settings (C-c i starts IPython window)
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

(load-file "~/dependencies/cedet/cedet-devel-load.el")
(add-hook 'after-init-hook (lambda ()
                             (message "activate-malabar-mode")
                             (activate-malabar-mode)))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "w3m")

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;quick access hacker news
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;;i need this often
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
