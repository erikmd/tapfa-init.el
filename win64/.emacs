;;; .emacs --- Emacs conf file -*- coding: utf-8 -*-

;; Téléchargez et placez ce fichier à la racine de votre homedir (=> ~/.emacs)
;; puis lancez GNU Emacs en exécutant la commande "emacs &" dans un terminal.

;; L'installation des modes Emacs pour OCaml et Coq devrait se lancer
;; automatiquement et durer environ 2'30.

;; En cas de souci, faites "M-x package-install-selected-packages RET"
;; (M-x désignant Alt+X et RET la touche Entrée) et redémarrez emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de package.el, MELPA et use-package

(require 'package)

;; Si vous avez Emacs 26.1 (qui est la version fournie dans Debian 10)
;; vous pourriez avoir le message d'erreur "Failed to download 'melpa'
;; archive during the package refresh step". C'est un bug connu
;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341) qui a été
;; corrigé dans Emacs 26.3 et 27.1; un contournement simple est
;; implémenté ci-dessous.
(when (eval-when-compile (version= emacs-version "26.1"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; See https://www.reddit.com/r/emacs/comments/9rj5ou/comment/e8ibi8e/
(when (eval-when-compile (version< emacs-version "27"))
  ;; (load "~/.emacs.d/early-init.el")
  (package-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-switches "-u")
 '(package-selected-packages
   (quote (magit-gitflow magit learn-ocaml company-coq proof-general company merlin tuareg use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "#add8e6"))))
 '(region ((t (:background "gold1" :distant-foreground "dim gray")))))

;; Bootstrap use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de Tuareg, Merlin et Company

(use-package tuareg
  :ensure t
  :defer t
  :config
  (setq tuareg-interactive-program "ocaml"
        tuareg-opam "opam"))

;;;  Désactivé car le raccourci associé à <home> est malencontreux
;; (use-package bifocal
;;   :ensure t
;;   :hook
;;   (tuareg-interactive-mode . bifocal-mode))

(use-package merlin
  :ensure t
  :hook
  ((tuareg-mode caml-mode) . merlin-mode)
  :config
  (setq merlin-command "ocamlmerlin"))

;;;  Désactivé car induit un ralentissement sous Windows+WSL
;; (use-package merlin-eldoc
;;   :ensure t
;;   :hook
;;   ((tuareg-mode caml-mode) . merlin-eldoc-setup)
;;   :bind (:map merlin-mode-map
;;               ("C-c <C-left>" . merlin-eldoc-jump-to-prev-occurrence)
;;               ("C-c <C-right>" . merlin-eldoc-jump-to-next-occurrence)))
;;
;; (use-package merlin-iedit
;;   :ensure t
;;   :after merlin-eldoc
;;   :bind (:map merlin-mode-map
;;               ("C-c C-o" . merlin-iedit-occurrences)))

(use-package company
  :ensure t
  :hook
  ((tuareg-mode caml-mode) . company-mode)
  :config
  (bind-key "<backtab>" 'company-complete))

(use-package learn-ocaml
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de ProofGeneral et company-coq

(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :init                                 ; (:config doesn't work here)
  (setq coq-prog-name "coqtop")         ; or "C:/Coq/bin/coqtop.exe"…
  :config
  (setq overlay-arrow-string ""
        coq-double-hit-enable t))

(use-package company-coq
  :ensure t
  :hook
  (coq-mode . company-coq-mode)
  :init
  (setq company-coq-disabled-features '(hello prettify-symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de Magit
;; Copied-from: https://gist.github.com/erikmd/82c4b2a50a77c98e8fe6318530c531b7

;;; Pour plus d'infos :
;; https://github.com/magit/magit et https://magit.vc (doc officielle)
;; https://youtu.be/mtliRYQd0j4 (tuto vidéo sur git-rebase avec Magit)

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-diff-refine-hunk 'all)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package magit-gitflow
  :ensure t
  :after magit
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; Protect against accident pushes to upstream/pushremote
;; compatible with https://github.com/magit/magit/pull/3813
;; tested with magit-20200927.1644
(defadvice magit-push-current-to-upstream
    (around my-protect-accidental-magit-push-current-to-upstream)
  "Protect against accidental push to upstream.
Causes `magit-run-git-async' to ask the user for confirmation first."
  (let ((my-magit-ask-before-push t))
    ad-do-it))

(defadvice magit-push-current-to-pushremote
    (around my-protect-accidental-magit-push-current-to-pushremote)
  "Protect against accidental push to upstream.
Causes `magit-run-git-async' to ask the user for confirmation first."
  (let ((my-magit-ask-before-push t))
    ad-do-it))

(defun magit-git-to-string (args)
  "Pretty-print the `magit-run-git-async' arguments.
Quote the substrings if need be."
  (cond ((not args)
         "")
        ((stringp args)
         (shell-quote-argument args))
        ((listp args)
         (mapconcat #'magit-git-to-string args " "))
        (t (error "Unrecognized: %s" (pp-to-string args)))))
;(magit-git-to-string '("push" "-v" ("--force-with-lease") "origin" "master:refs/heads/master"))

(defadvice magit-run-git-async (around my-protect-accidental-magit-run-git-async)
  "Maybe ask the user for confirmation before pushing.
Advices to `magit-push-current-to-*' trigger this query."
  (if (bound-and-true-p my-magit-ask-before-push)
      ;; Arglist is (ARGS)
      (if (y-or-n-p (format "Run 'git %s'? "
                               (magit-git-to-string (ad-get-args 0))))
          ad-do-it
        (error "Push aborted by user"))
    ad-do-it))

(ad-activate 'magit-push-current-to-upstream)
(ad-activate 'magit-push-current-to-pushremote)
(ad-activate 'magit-run-git-async)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config générale

(use-package tabbar
  :ensure t
  :init
  (tabbar-mode t))

(setq column-number-mode t
      line-number-mode t
      isearch-allow-scroll t
      enable-recursive-minibuffers t
      require-final-newline t)

(global-set-key (kbd "C-x C-S-q") #'view-mode)

;; Marquage des parenthèses
(load-library "paren")
(show-paren-mode 1)

;; Marquage des problèmes d'espace
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face empty tabs trailing space-after-tab space-before-tab))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'(lambda () (unless (derived-mode-p 'makefile-mode)
                                         (setq indent-tabs-mode nil))))
(add-hook 'sgml-mode-hook #'(lambda () (setq indent-tabs-mode nil)))
(add-hook 'nxml-mode-hook #'(lambda () (setq indent-tabs-mode nil)))

;; Adapted from:
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(defun tapfa-match-paren-direct (arg)
  "Go to the matching paren if on a paren; otherwise insert µ."
  (interactive "p")
  (cond ((looking-back "[])}]") ;;(forward-char 1)
         (backward-list 1))
        ((looking-at "[{([]") (forward-list 1) ;;(backward-char 1)
         )
        (t (self-insert-command (or arg 1)))))
(global-set-key "µ" #'tapfa-match-paren-direct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Proposition des "raccourcis Windows" C-c/C-x/C-v/C-z
;; au lieu de M-w/C-w/C-y/C-_ par défaut dans GNU Emacs

(defgroup tapfa-init nil
  "Tapfa-Init Emacs Settings."
  :group 'convenience
  :prefix "tapfa-init-")

(defcustom tapfa-init-cua nil
  "Initial setup for `cua-mode'.

The following values are meaningful:
nil ask on startup
-1  `cua-mode' is disabled
 1  `cua-mode' is enabled"
  :type '(choice (const :tag "Ask on startup" nil)
                 (const :tag "Disabled (emacs expert mode)" -1)
                 (const :tag "Enabled (windows' shortcuts)" 1))
  :group 'tapfa-init)

(defun tapfa-init-cua (&optional batch)
  "Ask to set (cua-mode 1) or (cua-mode -1) or to ask again.
Always ask if BATCH is nil, e.g., called interactively."
  (interactive)
  (if (or (null batch) (null tapfa-init-cua))
      (let ((newval
             (condition-case _sig
                 (x-popup-dialog
                  t '("Presse-papier & Annulation :\n\nVoulez-vous forcer l'utilisation des raccourcis Windows\n(C-c, C-x, C-v, et C-z)\n\nOu garder les raccourcis Emacs/shell standards\n(M-w, C-w, C-y, et C-_)\n\n?"
                      ("Windows" . 1) ("Emacs/shell" . -1)
                      ("Me redemander" . nil)))
               (quit nil))))
        (customize-save-variable 'tapfa-init-cua newval)
        (if newval
            (message-box "Configuration enregistrée.\n\nSi jamais vous voulez rechanger, tapez M-x tapfa-init-cua RET")
          (message-box "Configuration supprimée.\n\nSi jamais vous voulez rechanger, tapez M-x tapfa-init-cua RET\nou redémarrez Emacs."))))
  (cond ((eq tapfa-init-cua 1) (cua-mode 1))
        ((eq tapfa-init-cua -1) (cua-mode -1))
        (t (cua-mode -1))))

(tapfa-init-cua t)
