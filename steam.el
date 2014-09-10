;; steam.el --- Organize and launch Steam games with Emacs
;; Copyright (C) 2014 Erik Sjöstrand
;; MIT License

(require 'url)
(require 'xml)
(require 'cl)

(defvar steam-games nil)
(defvar steam-username nil)

(defun steam-get-xml ()
  (with-current-buffer
      (url-retrieve-synchronously (format "http://steamcommunity.com/id/%s/games?tab=all&xml=1"
                                          steam-username))
    (goto-char url-http-end-of-headers)
    (car (xml-get-children (car (xml-parse-region (point) (point-max)))
                           'games))))

(defun steam-game-attribute (game attribute)
  (caddar (xml-get-children game attribute)))

(defun steam-get-games ()
  "Retrieves a list of your games from the Steam server."
  (interactive)
  (setq steam-games
        (mapcar (lambda (game)
                  (cons (steam-game-attribute game 'name)
                        (steam-game-attribute game 'appID)))
                (xml-get-children (steam-get-xml)
                                  'game))))

(defun steam-launch-id (id)
  (case system-type
    ('windows-nt (shell-command (format "explorer steam://rungameid/%s" id)))
    ('gnu/linux (shell-command (format "steam steam://rungameid/%s" id)))
    ('darwin (shell-command (format "open steam://rungameid/%s" id)))))

(defun steam-insert-org ()
  "Inserts your game list in org format."
  (interactive)
  (unless steam-games (steam-get-games))
  (mapcar (lambda (game)
            (insert 
             (format "* [[elisp:(steam-launch-id %s)][%s]]\n  http://steamcommunity.com/app/%s\n"
                     (cdr game) (car game) (cdr game))))
  steam-games))

(defun steam-launch ()
  "Launch a game in your Steam library, using ido."
  (interactive)
  (unless steam-games (steam-get-games))
  (let ((game (cdr (assoc
                    (ido-completing-read
                     "Game: " steam-games)
                    steam-games))))
    (when game (steam-launch-id game))))

(provide 'steam)
