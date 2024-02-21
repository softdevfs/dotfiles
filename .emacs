(require 'w3m-load)
(require 'mime-w3m)
(require 'sudo-edit)
(require 'dired-x)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(load-theme 'timu-rouge t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages '(sudo-edit haskell-mode haskell-emacs))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-x c") 'kill-ring-save)
(global-set-key (kbd "C-c C-r") 'sudo-edit)

;; dired images

(setq dired-guess-shell-alist-user
      '(
        ("\\.pdf$" "devour zathura")
	("\\.gif$" "devour sxiv -a")
        ("\\.png$" "devour sxiv")
	("\\.jpeg$" "devour sxiv")
        ("\\.jpg$" "devour sxiv")
        ("\\.JPG$" "devour sxiv")
	("\\.svg" "devour feh -g 640x480 -d")
	("\\.avi$" "devour mpv")
	("\\.mkv$" "devour mpv")
	("\\.mp4$" "devour mpv")
       )
)

(define-key dired-mode-map (kbd "C-<return>") 'dired-do-shell-command)
