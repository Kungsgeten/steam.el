;; steam.el --- Organize and launch Steam games with Emacs
;; Copyright (C) 2015 Erik Sjöstrand
;; MIT License

(require 'url)
(require 'xml)
(require 'cl)

(defvar steam-games nil "An XML file of the user's games on Steam.")
(defvar steam-username nil "The Steam username")
(defvar steam-logo-dir "steamlogos" "The dir where logos will be downloaded, relative to the org-file.")

(defun steam-get-xml ()
  "Downloads the user's games as XML"
  (with-current-buffer
      (url-retrieve-synchronously (format "http://steamcommunity.com/id/%s/games?tab=all&xml=1"
                                          steam-username))
    (goto-char url-http-end-of-headers)
    (car (xml-get-children (car (xml-parse-region (point) (point-max)))
                           'games))))

(defun steam-game-attribute (game attribute)
  "Reads an XML attribute from a game"
  (caddar (xml-get-children game attribute)))

(defun steam-get-games ()
  "Download steam games as XML and update `steam-games'"
  (interactive)
  (setq steam-games (xml-get-children (steam-get-xml) 'game)))

(defun steam-launch-id (id)
  (case system-type
    ('windows-nt (shell-command (format "explorer steam://rungameid/%s" id)))
    ('gnu/linux (shell-command (format "steam steam://rungameid/%s" id)))
    ('darwin (shell-command (format "open steam://rungameid/%s" id)))))

(defun steam-launch ()
  "Launch a game in your Steam library."
  (interactive)
  (unless steam-games (steam-get-games))
  (let* ((games (mapcar
                 (lambda (game)
                   (cons (steam-game-attribute game 'name)
                         (steam-game-attribute game 'appID)))
                 steam-games))
         (game (cdr (assoc
                     (completing-read
                      "Game: " 
                      games)
                     games))))
    (when game (steam-launch-id game))))

(defun steam-insert-org-text ()
  "Insert each Steam game as an org heading, containing the
game's name and a link to execute the game. Entries already
existing in the buffer will not be duplicated."
  (interactive)
  (unless steam-games (steam-get-games))
  (let ((org-lvl (org-current-level)))
    (mapc (lambda (game)
            (unless (search
                     (format "elisp:(steam-launch-id %s)"
                             (steam-game-attribute game 'appID))
                     (buffer-string))
              (insert "*")
              (when org-lvl (dotimes (number org-lvl)
                              (insert "*")))
              (insert (format " [[elisp:(steam-launch-id %s)][%s]]\n"
                              (steam-game-attribute game 'appID)
                              (steam-game-attribute game 'name)))))
          steam-games)))

(defun steam-insert-org-images ()
  "Insert each Steam game as an org heading, containing an image
of the game's logo and a link to execute the game. Entries
already existing in the buffer will not be duplicated."
  (interactive)
  (unless steam-games (steam-get-games))
  (unless (file-exists-p steam-logo-dir)
    (make-directory steam-logo-dir))
  (let ((org-lvl (org-current-level)))
    (mapc (lambda (game)
            (unless  (search
                      (format "elisp:(steam-launch-id %s)"
                              (steam-game-attribute game 'appID))
                      (buffer-string))
              (insert "*")
              (when org-lvl (dotimes (number org-lvl)
                              (insert "*")))
              (insert (format " [[elisp:(steam-launch-id %s)][file:%s]] %s\n"
                              (steam-game-attribute game 'appID)
                              (steam-download-logo game)
                              (steam-game-attribute game 'name)))))
          steam-games)))

(defun steam-download-logo (game)
  "Downloads the logo image of a game into `steam-logo-dir' folder."
  (let ((link (steam-game-attribute game 'logo))
        (filename (concat steam-logo-dir "/img" (steam-game-attribute game 'appID) ".jpg")))
    (unless (file-exists-p filename)
      (url-retrieve
       link
       (lambda (status filename buffer)
         ;; Write current buffer to FILENAME
         ;; and update inline images in BUFFER
         (let ((err (plist-get status :error)))
           (if err (error
                    "\"%s\" %s" link
                    (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
         (delete-region
          (point-min)
          (progn
            (re-search-forward "\n\n" nil 'move)
            (point)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region nil nil filename nil nil nil nil)))
       (list
        (expand-file-name filename)
        (current-buffer))
       nil t)
      (sleep-for 0 100))
    filename))

(provide 'steam)
