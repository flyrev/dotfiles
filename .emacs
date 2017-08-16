(setq debug-on-error t)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs.d/modes/")
(require 'ember-mode)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq inhibit-startup-screen t)

(setq ensime-startup-notification nil)

(setq split-width-threshold nil)

;; the repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; mandatory modules installation

(defun package-conditional-install (package-name)
  "Installs a package if it is not present"
  (unless (package-installed-p package-name)
    (package-refresh-contents) (package-install package-name)))

(defun packages-conditional-install (packages)
  ""
  (when packages
    (package-conditional-install (car packages))
    (packages-conditional-install (cdr packages))))

(packages-conditional-install
 '(projectile web-mode ensime magit git-gutter neotree ace-window avy csv-mode
	      elmacro key-chord multiple-cursors smartparens
	      auto-package-update
	      ))

(projectile-global-mode)

(when (not package-archive-contents)
  (package-refresh-contents))

;; ensime hooked to scala-mode
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "c9e123d4ecd9ceb056806c6297336763e9e96eed6962bfc1d5252afcc2761610" default)))
 '(safe-local-variable-values (quote ((python-shell-interpreter . "python3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; missing tools
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (next-line))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun double-line (arg)
  "copy line and place it below the original"
  (interactive "p")
  (copy-line arg)
  (yank)
  (move-end-of-line))

;; additional shortkey
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x G") 'magit-status)
(global-set-key (kbd "C-x R") 'magit-ediff-resolve)
(global-set-key (kbd "C-c /") 'toggle-comment-on-line)
(global-set-key (kbd "C-c d") 'double-line)
(global-set-key (kbd "C-+") 'zoom-frm-in)
(global-set-key (kbd "C-_") 'zoom-frm-out)
(global-set-key (kbd "C-c C-v V") 'find-name-dired)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-c C-f g") 'find-grep)
(global-set-key (kbd "C-c j") 'avy-goto-subword-1)
(global-set-key (kbd "C-c \\") 'ace-window)
(global-set-key (kbd "C-c C-s s") 'ace-swap-window)
(global-set-key (kbd "C-c C-s m") 'ace-maximize-window)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

(key-chord-mode 1)

(key-chord-define-global "sw" 'ace-swap-window)

;; enable mandatory modes
(global-git-gutter-mode +1)

(custom-set-variables
 '(git-gutter:update-interval 2))

(show-paren-mode 1)
(column-number-mode 1)

;; disable gui-related nonsens
(tool-bar-mode 0)
(menu-bar-mode 0)

;; prompt only y or no
(fset `yes-or-no-p `y-or-n-p)

(setq haskell-mode-hook (quote (turn-on-haskell-indentation)))

(elmacro-mode 1)

;; ignore first test
(defun ignore-fst-test ()
  "Change me!"
  (interactive)
  (beginning-of-buffer nil)
  (isearch-forward nil 1)
  (isearch-printing-char 116 1)
  (isearch-printing-char 101 1)
  (isearch-printing-char 115 1)
  (isearch-printing-char 116 1)
  (isearch-printing-char 40 1)
  (isearch-printing-char 34 1)
  (isearch-exit)
  (forward-word 1)
  (backward-word 1)
  (backward-word 1)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (insert "ignore"))

;;(key-chord-define-global "it" 'ignore-fst-test)

;; Change font size globally:
;;(set-face-attribute 'default nil :height 115)

;; This is bound to f11 in Emacs 24.4
(toggle-frame-fullscreen) 
;; Who uses the bar to scroll?
(scroll-bar-mode 0)

(auto-package-update-maybe)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e9df267a1c808451735f2958730a30892d9a2ad6879fb2ae0b939a29ebf31b63" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'monokai t)

(setq ensime-startup-snapshot-notification nil)
(setq scala-prettify-symbols
      '(
	("=>" . ?⇒)
	("<-" . ?←)
	("->" . ?→)
	("undefined" . ?⊥)
	("&&" . ?∧)
	("||" . ?∨)
	("<<<" . ?⋘)
	(">>>" . ?⋙)
	("++" . ?⧺)
	("any" . ?∃)
	("all" . ?∀)
	("traverse" . ?↦)
	("map" . ?∘)
	("lambda" . ?λ)
	("alpha" . ?α)
	("beta" . ?β)
	("Unit" . ?∅)
	))

(add-hook 'scala-mode-hook
	  (lambda ()
	    (ensime-scala-mode-hook)
	    (setq prettify-symbols-alist scala-prettify-symbols)
	    (prettify-symbols-mode)
	    (define-key scala-mode-map (kbd "C-x M-e") 'ensime-fully-reload)
	    ))

(defun ensime-fully-reload ()
  "reload ensime"
  (interactive)
  (ensime-shutdown)
  (ensime))


(require 'editorconfig)
(editorconfig-mode 1)

(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

(add-hook 'js-mode-hook (lambda () (ember-mode t)))
(add-hook 'web-mode-hook (lambda () (ember-mode t)))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
)
(add-hook 'web-mode-hook  'web-mode-hook)

(setq-default indent-tabs-mode nil)

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
