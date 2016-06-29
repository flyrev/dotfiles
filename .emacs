(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-use-ls-dired nil)

(add-to-list 'load-path "~/.emacs.d/modes/")

(load "web-mode.el")
(require 'web-mode)

(load "ember-mode.el")
(require 'ember-mode)

(require 'cl)

(defun random-color-theme () (interactive)
       (let ((chosen-theme
	      (nth
	       (random
		(length (mapcar 'symbol-name (custom-available-themes))))
	       (custom-available-themes))))
	 (message "Theme: %s" chosen-theme)
	 (load-theme chosen-theme t nil)))

(random-color-theme)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(add-hook 'js-mode-hook (lambda () (ember-mode t)))
(add-hook 'web-mode-hook (lambda () (ember-mode t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
