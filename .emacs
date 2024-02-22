;; require

(require 'w3m-load)
(require 'mime-w3m)
(require 'dired-x)

;; add-to-list

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; appearance

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(load-theme 'timu-rouge t)
(custom-set-variables
 '(inhibit-startup-screen t)
 '(package-selected-packages '(sudo-edit haskell-mode haskell-emacs))
 '(warning-suppress-types '((comp) (comp))))

;; bindings

(setq dired-guess-shell-alist-user
      '(
        ("\\.pdf$" "zathura")
	("\\.gif$" "sxiv -a")
        ("\\.png$" "feh -g 640x480 -d")
	("\\.jpeg$" "feh -g 640x480 -d")
        ("\\.jpg$" "feh -g 640x480 -d")
        ("\\.JPG$" "feh -g 640x480 -d")
	("\\.svg" "feh -g 640x480 -d")
	("\\.avi$" "mpv")
	("\\.mkv$" "mpv")
	("\\.mp4$" "mpv")
       )
      )

(global-set-key (kbd "C-x c") 'kill-ring-save)
(global-set-key (kbd "C-c C-r") 'sudo-edit)
(define-key dired-mode-map (kbd "C-<return>") 'dired-do-shell-command)
