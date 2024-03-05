;; add-to-list

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'load-path (expand-file-name "openwith-20120531.2136" "~/.emacs.d/custom"))

;; require

(require 'w3m-load)
(require 'mime-w3m)
(require 'openwith)

;; other requirements

(custom-set-variables
 '(inhibit-startup-screen t)
 '(package-selected-packages '(sudo-edit haskell-mode haskell-emacs))
 )
(openwith-mode t)

;; appearance

(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(load-theme 'timu-rouge t)

;; settings

(setq make-backup-files nil)
(setq openwith-associations
      '(
	("\\.pdf$" "zathura" (file))
	("\\.gif$" "sxiv -a" (file))
        ("\\.png$" "feh -g 640x480 -d" (file))
	("\\.jpeg$" "feh -g 640x480 -d" (file))
        ("\\.jpg$" "feh -g 640x480 -d" (file))
        ("\\.JPG$" "feh -g 640x480 -d" (file))
	("\\.svg$" "feh -g 640x480 -d" (file))
	("\\.avi$" "mpv" (file))
	("\\.mkv$" "mpv" (file))
	("\\.mp4$" "mpv" (file))
       )
)

(global-display-line-numbers-mode)
(global-set-key (kbd "C-x c") 'kill-ring-save)
(global-set-key (kbd "C-c C-r") 'sudo-edit)
