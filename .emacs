;;; .emacs --- Emacs conf file -*- coding: utf-8; lexical-binding: t -*-

;; Téléchargez et placez ce fichier à la racine de votre homedir (=> ~/.emacs)
;; puis lancez GNU Emacs en exécutant la commande "emacs &" dans un terminal.

;; L'installation des modes Emacs pour OCaml et Coq devrait se lancer
;; automatiquement et durer environ 2'30.

;; En cas de souci, faites "M-x package-install-selected-packages RET"
;; (M-x désignant Alt+X et RET la touche Entrée) et redémarrez emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper for macOS native fullscreen

(defun tapfa-init-frame-maximize (prefix)
  "Toggle the current frame fullscreen (resp. maximized if PREFIX holds)."
  (interactive "P")
  (if prefix (toggle-frame-maximized)
    (toggle-frame-fullscreen)))

;; Config des touches Option dans GNU Emacs 28 pour macOS

;; Utilisez la touche Option-de-gauche pour activer la touche Meta d'Emacs (M-)
;; Utilisez la touche Option-de-droite (⌥-) pour saisir des caractères spéciaux

;; & Corrige "Invalid image type: 'svg" https://emacs.stackexchange.com/a/77324

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'none)
  (add-to-list 'image-types 'svg t)
  (global-set-key (kbd "C-s-f") #'tapfa-init-frame-maximize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de package.el, MELPA et use-package

;; In Emacs-28, `magit` raises an emergency warning "requires seq >= 2.24".
;; Stefan: `transient` manque à l'appel dans le `package--builtin-versions`
;; de Emacs-28.
(and (not (assq 'transient package--builtin-versions))
     (>= emacs-major-version 28)
     (push '(transient 0 3 7) package--builtin-versions))

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
 '(warning-suppress-types '((bytecomp) (comp)))
 '(package-selected-packages
   (quote (learn-ocaml company merlin-iedit merlin-eldoc merlin tuareg auctex magit-gitflow magit yaml-mode markdown-mode helpful discover-my-major which-key dashboard page-break-lines centaur-tabs spaceline spacemacs-theme diminish use-package gnu-elpa-keyring-update))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "#add8e6"))))
 '(region ((t (:background "gold1" :distant-foreground "dim gray")))))

;; Install https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
;; to fix the bad-signature error that occurs in Debian GNU/Linux 12
;; (cf. https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1070664)
(unless (package-installed-p 'gnu-elpa-keyring-update)
  ;; over-approximation: Unset package-check-signature temporarily for
  ;; GNU/Linux, emacs < 29.3 (cf. https://packages.debian.org/emacs)
  (if (and (string-equal system-type "gnu/linux")
           (version< emacs-version "29.3"))
      (let ((package-check-signature nil))
        (package-refresh-contents)
        (package-install 'gnu-elpa-keyring-update))
    (progn (package-refresh-contents)
           (package-install 'gnu-elpa-keyring-update))))

;; Bootstrap use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defgroup tapfa-init nil
  "Tapfa-Init Emacs Settings."
  :group 'convenience
  :prefix "tapfa-init-")

(use-package diminish
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom gettext

(defun tapfa-init-getenv-lang ()
  "Read $LANG and return 'fr if it matches and 'en otherwise."
  (let ((lang (getenv "LANG")))         ; can be nil
    (if (or (string-prefix-p "fr_" lang)
            (string-equal "fr" lang))
        'fr
      'en)))

(defcustom tapfa-init-default-lang nil
  "Option to set the default language in tapfa-init strings.

The following values are meaningful:
nil use (tapfa-init-getenv-lang)
fr  use French
en  use English"
  :type '(choice (const :tag "Use (tapfa-init-getenv-lang)" nil)
                 (const :tag "Use French" 'fr)
                 (const :tag "Use English" 'en))
  :group 'tapfa-init)

(defun tapfa-init-get-lang ()
  "Read `tapfa-init-default-lang' or `tapfa-init-getenv-lang'."
  (if tapfa-init-default-lang
      tapfa-init-default-lang
    (let ((lang (tapfa-init-getenv-lang)))
      (customize-save-variable 'tapfa-init-default-lang lang))))

(defun tapfa-init-select-lang ()
  "Use `x-popup-dialog' to select the supplementary-help language."
  (interactive)
  (let ((newval
         (condition-case _sig
             (x-popup-dialog
              t '("What language should be used for the supplementary help?\n"
                  ("French" . fr) ("English" . en)))
           (quit nil))))
    (when newval
      (customize-save-variable 'tapfa-init-default-lang newval)
      (tapfa-init-easy-menu-define))))

(defun tapfa-init-get-text (fr en)
  "Return FR or EN, depending on `tapfa-init-get-lang'."
  (if (eq (tapfa-init-get-lang) 'fr) fr en))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (menu-bar-mode -1) Keep it!
;; M-x minor-mode-menu-from-indicator RET

(defcustom tapfa-init-tool-bar nil
  "Display Emacs' tool-bar.

  The following values are meaningful:
nil unset initially (seen as 1 on Darwin, -1 otherwise)
-1  hide the tool-bar
 1  show the tool-bar"
  :type '(choice (const :tag "Unset initially" nil)
                 (const :tag "Hide the tool-bar" -1)
                 (const :tag "Show the tool-bar" 1))
  :set
  (lambda (var value)
      (set-default-toplevel-value var value)
      (if value
          (tool-bar-mode value)
        (if (eq system-type 'darwin)
            (tool-bar-mode 1)
          (tool-bar-mode -1))))
  :group 'tapfa-init)

(unless tapfa-init-tool-bar
  (customize-save-variable 'tapfa-init-tool-bar
                           (if (eq system-type 'darwin) 1 -1)))

(defun tapfa-init-tool-bar-toggle ()
  "Toggle `tapfa-init-tool-bar'."
  (interactive)
  (cond ((eq tapfa-init-tool-bar 1)
         (customize-save-variable 'tapfa-init-tool-bar -1))
        ((eq tapfa-init-tool-bar -1)
         (customize-save-variable 'tapfa-init-tool-bar 1))
        (t
         (if (eq system-type 'darwin)
             (customize-save-variable 'tapfa-init-tool-bar -1)
           (customize-save-variable 'tapfa-init-tool-bar 1)))))

;; Custom spacemacs theme with nice tabs

(use-package spacemacs-theme
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

(use-package spaceline
  :ensure t
  :config
  (use-package spaceline-config
    :config
    (spaceline-emacs-theme)))

(defcustom tapfa-init-darkness nil
  "Should dark mode be enabled?

The following values are meaningful:
nil ask on startup
-1  setup dark mode
 1  setup light mode"
  :type '(choice (const :tag "Ask on startup" nil)
                 (const :tag "Setup spacemacs-light theme" -1)
                 (const :tag "Setup spacemacs-dark theme" 1))
  :group 'tapfa-init)

(defun tapfa-init-darkness-apply ()
  "Load themes accordingly as per `tapfa-init-darkness'."
  (load-theme (if (eq tapfa-init-darkness -1) 'spacemacs-dark 'spacemacs-light) t)
  (spaceline-emacs-theme))

(defun tapfa-init-darkness (&optional batch)
  "Ask to set tapfa-init-darkness to 1 or -1 or to ask again.
Always ask if BATCH is nil, e.g., called interactively."
  (interactive)
  (if (or (null batch) (null tapfa-init-darkness))
      (let ((newval
             (condition-case _sig
                 (x-popup-dialog
                  t (tapfa-init-get-text
		     '("Quel thème spacemacs voulez-vous activer ?\n"
                       ("Light" . 1) ("Dark" . -1)
                       ("Redemander" . nil))
		     '("What spacemacs theme do you want to enable?\n"
                       ("Light" . 1) ("Dark" . -1)
                       ("Ask again" . nil))))
               (quit nil))))
        (customize-save-variable 'tapfa-init-darkness newval)
        (let ((sdark (tapfa-init-get-text
		      "Thème enregistré : spacemacs-dark.\n\n"
		      "Theme stored: spacemacs-dark.\n\n"))
              (slight (tapfa-init-get-text
		       "Thème enregistré : spacemacs-light.\n\n"
		       "Theme stored: spacemacs-light.\n\n"))
              (sdflt (tapfa-init-get-text
		      "Thème par défaut : spacemacs-light.\n\n"
		      "Theme by default: spacemacs-light.\n\n"))
              (ssaved-end (tapfa-init-get-text
			   "Si jamais vous voulez rechanger, utiliser le menu (?)\n"
			   "If ever you want to change, use the menu (?)\n"))
              (sdflt-end (tapfa-init-get-text
			  "Si jamais vous voulez changer, utilisez le menu (?)\nou redémarrer Emacs."
			  "If ever you want to change, use the menu (?)\nor restart Emacs.")))
          (cond ((eq tapfa-init-darkness 1)
                 (message-box (concat slight ssaved-end)))
                ((eq tapfa-init-darkness -1)
                 (message-box (concat sdark ssaved-end)))
                (t
                 (message-box (concat sdflt sdflt-end)))))))
        (tapfa-init-darkness-apply))

(tapfa-init-darkness t)

(defun tapfa-init-fix-centaur-tabs-excluded-prefixes ()
  "Fix `centaur-tabs-excluded-prefixes'."
  (when (or
         ;; (member "*Keep" centaur-tabs-excluded-prefixes)
         (not (member "*tapfa" centaur-tabs-excluded-prefixes)))
    (customize-save-variable 'centaur-tabs-excluded-prefixes
                             (append '("*tapfa")
                                     (seq-filter
                                      (lambda (s)
                                        (not (member s '(;;"*Keep"
							 ))))
                                      centaur-tabs-excluded-prefixes)))))

(use-package centaur-tabs
  :ensure t
  :init
  (setq centaur-tabs-enable-key-bindings nil
        centaur-tabs-show-count nil
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-height 32
        centaur-tabs-left-edge-margin nil
        centaur-tabs-set-icons nil ;; improve?
        ;; centaur-tabs-label-fixed-length 14
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-down-tab-text " ⋮ "
        centaur-tabs-backward-tab-text "(←)"
        centaur-tabs-forward-tab-text "(→)")
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("<C-f5>" . centaur-tabs-extract-window-to-new-frame)
  ("<C-prior>" . centaur-tabs-backward)
  ("<C-next>" . centaur-tabs-forward)
  ("C-c <left>" . centaur-tabs-backward)
  ("C-c <right>" . centaur-tabs-forward)
  ("C-c <C-left>" . centaur-tabs-backward)
  ("C-c <C-right>" . centaur-tabs-forward)
  ("<C-S-prior>" . centaur-tabs-move-current-tab-to-left)
  ("<C-S-next>" . centaur-tabs-move-current-tab-to-right)
  ("C-c <C-S-left>" . centaur-tabs-move-current-tab-to-left)
  ("C-c <C-S-right>" . centaur-tabs-move-current-tab-to-right)
  :demand t
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (tapfa-init-fix-centaur-tabs-excluded-prefixes)
  (defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ;; ((memq major-mode '(org-mode org-agenda-mode diary-mode)) "OrgMode")
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode)) "OrgMode")
    ((memq major-mode '(tex-mode latex-mode)) "TeX")
    ((memq major-mode '(tuareg-mode caml-mode)) "OCaml")
    ((memq major-mode '(coq-mode
                        proof-splash-mode
                        coq-shell-mode
                        coq-goals-mode
                        coq-response-mode)) "Coq")
    ((memq major-mode '(helpful-mode help-mode)) "Help")
    ;; ((derived-mode-p 'prog-mode) "Editing")
    ;; ((and (buffer-file-name) (file-remote-p (buffer-file-name))) "Remote")
  ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '( magit-process-mode
                             magit-status-mode
                             magit-diff-mode
                             magit-log-mode
                             magit-file-mode
                             magit-blob-mode
                             magit-blame-mode)))
     "Emacs")
    ((derived-mode-p 'shell-mode) "Shell")
    ((derived-mode-p 'eshell-mode) "EShell")
    ((derived-mode-p 'emacs-lisp-mode) "Elisp")
    ((derived-mode-p 'dired-mode) "Dired")
    (t
     (centaur-tabs-get-group-name (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config pour afficher au démarrage un dashboard + joli et utile

(use-package page-break-lines
  :ensure t
  :hook
  (dashboard-mode . page-break-lines-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to GNU Emacs!")
  (dashboard-page-separator "\f\n")
  (dashboard-items '((recents . 5)
                     (bookmarks . 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config pour afficher une aide sur les raccourcis de base (C-f1)

(defun tapfa-init-help-display ()
  (interactive)
  (let ((buf (get-buffer-create "*tapfa-init-help*")))
    (with-current-buffer buf
      (read-only-mode 0)
      (erase-buffer)
      (insert (tapfa-init-get-text
;; French:
"Principaux raccourcis — taper \"q\" pour fermer cette aide, \"SPC\" pour défiler :

C-g (= Ctrl+g) ; annuler la saisie en cours dans le minibuffer (≈ barre d'état)
ESC ESC ESC    ; pour annuler plus violemment (commande plus puissante que C-g)
C-x C-f …  ; pour ouvrir un fichier existant ou créer un nouveau fichier
C-x C-s    ; sauver le fichier courant (ProTip : garder la touche Ctrl appuyée)
C-x k RET  ; pour fermer le buffer courant
C-x 2      ; pour découper la vue courante en 2 zones empilées verticalement
C-x o      ; pour déplacer le curseur d'une zone à l'autre
C-x 1      ; pour garder la zone courante et masquer les autres zones affichées
F7  ; pour désactiver xterm-mouse-mode, permettant le copier-coller à la souris

C-c …  ; préfixe spécifique au mode courant (dépend du langage, cf. ci-dessous)

Principaux raccourcis des modes Tuareg et Merlin (pour OCaml) :

C-x C-s  ; sauver le fichier courant et surligner les erreurs OCaml en rouge

C-c C-s RET  ; pour lancer un toplevel OCaml (Ctrl+c Ctrl+s Entrée)
C-c C-e  ; pour évaluer toute la phrase OCaml autour du curseur
C-c C-b  ; pour évaluer tout le buffer dans le toplevel OCaml

C-c C-t  ; pour afficher dans le minibuffer le type OCaml du code sélectionné
C-c C-d  ; pour afficher dans le minibuffer la doc de la fonction sélectionnée
C-c C-l  ; pour localiser et sauter à la définition de l'identificateur courant
C-c C-&  ; pour revenir à la dernière position avant d'avoir fait C-c C-l
C-c C-o  ; pour renommer toutes les occurrences du curseur (terminer avec C-g)

Principaux raccourcis du mode Proof-General (pour Coq) :

C-c RET  ; pour évaluer le code Coq jusqu'au curseur
. .      ; (2 appuis rapides sur .) écrit 1 point final et évalue le code Coq
C-c C-u  ; pour revenir à la phrase précédente
C-c C-n  ; pour évaluer la phrase suivante
C-c C-b  ; pour évaluer tout le buffer
C-u C-c RET  ; pour évaluer le code Coq, en remplaçant \"Qed.\" par \"Admitted.\"

C-c C-f  ; pour effectuer une recherche (Search) sans l'écrire dans le code
C-c C-l C-c C-p  ; pour réafficher les 3 buffers standard en mode preuve

C-u C-c C-x  ; pour tuer le processus (coqtop)

M-x p-u-e-p RET  ; mettre à jour tous les modes Emacs (= Alt+x p-u-e-p Entrée)
"
;; English:
"Main key bindings — press \"q\" to dismiss this help, or \"SPC\" to scroll:

C-g (= Ctrl+g) ; abort the current editing in the minibuffer (≈ status bar)
ESC ESC ESC    ; to abort more aggressively (command more powerful than C-g)
C-x C-f …  ; to open an existing file or create a new file
C-x C-s    ; to save the current file (ProTip: keep the Ctrl key down)
C-x k RET  ; to kill (= close) the current buffer
C-x 2      ; to split the current view in 2 zones, vertically stacked
C-x o      ; to move the cursor from a zone to another one
C-x 1      ; to keep the current zone and hide the other displayed ones
F7  ; to disable xterm-mouse-mode and thus allow copy-and-paste with the mouse

C-c …  ; prefix specific to the current mode (dep. on the language, see below)

Main key bindings of the Tuareg and Merlin modes (for OCaml):

C-x C-s  ; to save the current file and highlight the OCaml errors in red

C-c C-s RET  ; to launch an OCaml toplevel (Ctrl+c Ctrl+s Return)
C-c C-e  ; to evaluate the whole OCaml phrase around the cursor
C-c C-b  ; to evaluate the whole buffer in the OCaml toplevel

C-c C-t  ; to display in the minibuffer the OCaml type of the selected code
C-c C-d  ; to display in the minibuffer the doc of the selected function
C-c C-l  ; to jump to the definition of the identifier under the cursor
C-c C-&  ; to jump back to the last position before typing C-c C-l
C-c C-o  ; to rename all the occurrences of the cursor (stop with C-g)

Main key bindings of the Proof-General mode (for Coq):

C-c RET  ; to evaluate the Coq code up to the cursor
. .      ; (2 quick press on .) write 1 final stop and evaluate the Coq code
C-c C-u  ; to come back to the previous phrase
C-c C-n  ; to evaluate the next phrase
C-c C-b  ; to evaluate the whole buffer
C-u C-c RET  ; to evaluate the Coq code, replacing \"Qed.\" with \"Admitted.\"

C-c C-f  ; to perform some lemma search (Search) without writing it in the code
C-c C-l C-c C-p  ; to redisplay the 3 standard buffers in proof mode

C-u C-c C-x  ; to kill the Coq process (coqtop)

M-x p-u-e-p RET  ; to upgrade all the Emacs modes (= Alt+x p-u-e-p Return)
"))
      (read-only-mode 1)
      (goto-char (point-min))
      (lisp-mode)
      (view-mode 1)
      (local-set-key (kbd "q") (lambda () (interactive) (quit-window t)))
      (switch-to-buffer-other-window buf))))

;; (spaceline-define-segment ?)

(defvar tapfa-init-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<f1>") #'tapfa-init-help-display)
    (define-key map (kbd "M-<f1>") #'tapfa-init-help-display)
    map)
  "Keymap for tapfa-init-help.")

(define-minor-mode tapfa-init-help-mode
  "Minor mode to display basic key bindings help (in French)."
  :lighter " (?)"
  :global t
  :keymap tapfa-init-help-map
  (if tapfa-init-help-mode
      (message (tapfa-init-get-text
                "Tapez Ctrl+F1 ou Alt+F1 pour plus d'aide sur les principaux raccourcis"
                "Type Ctrl+F1 or Alt+F1 for more help about the main key bindings"))))

;; (define-globalized-minor-mode global-tapfa-init-help-mode tapfa-init-help-mode
;;  (lambda () (tapfa-init-help-mode 1)))

(defun tapfa-init-easy-menu-define ()
  (interactive)
  (easy-menu-define
    tapfa-init-help-mode--menu
    tapfa-init-help-map
    "tapfa-init help menu"
    `("(?) - Tapfa Init Help"
          :label "(?)"
          [,(tapfa-init-get-text
             "Changer la langue de ce menu (EN / FR)"
             "Set the language for this menu (FR / EN)")
           tapfa-init-select-lang
           :help "M-x tapfa-init-set-lang"]
          "-------"
          [,(tapfa-init-get-text
            "Ouvrir un signet existant"
            "Jump to an existing bookmark")
           bookmark-jump
           :help "bookmark-jump"]
          [,(tapfa-init-get-text
            "Sauver la position dans un signet"
            "Store the position in a bookmark")
           bookmark-set
           :help "bookmark-set"]
          [,(tapfa-init-get-text
            "Afficher la liste des signets"
            "Display the list of bookmarks")
           bookmark-bmenu-list
           :help "bookmark-bmenu-list"]
          "-------"
          [,(tapfa-init-get-text
            "Afficher le dashboard d'Emacs"
            "Display the Emacs dashboard")
           dashboard-open
           :help "M-x dashboard-open RET"]
          "-------"
          [,(tapfa-init-get-text
            "Afficher / masquer la barre d'outils"
            "Show / hide the tool-bar")
           tapfa-init-tool-bar-toggle
           :help "M-x tapfa-init-tool-bar-toggle RET"]
          [,(tapfa-init-get-text
            "Changer le thème spacemacs : light / dark"
            "Change the spacemacs theme: light / dark")
           tapfa-init-darkness
           :help "M-x tapfa-init-darkness RET"]
          [,(tapfa-init-get-text
            "Changer les raccourcis : shell / windows"
            "Change the shortcuts: shell / windows")
           tapfa-init-cua
           :help "M-x tapfa-init-cua RET"]
          [,(tapfa-init-get-text
            "Installer les modes Coq - si nécessaire"
            "Install the Coq modes - if need be")
           tapfa-init-coq
           :help "M-x tapfa-init-coq RET"]
          "-------"
          [,(tapfa-init-get-text
            "Aide sur les raccourcis de base"
            "Help on the main key bindings")
           tapfa-init-help-display
           :help ,(tapfa-init-get-text
                  "Afficher les principaux raccourcis à connaître"
                  "Display the main key bindings to know")])))

(tapfa-init-easy-menu-define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config pour alternative à dired-mode

;;; (use-package neotree
;;;   :ensure t
;;;   :config
;;;   (global-set-key (kbd "<f6>") #'neotree-toggle))
;;; ;; cf. https://github.com/jaypei/emacs-neotree#keybindings
;;; ;; pour les raccourcis du buffer Neotree

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config pour augmenter la découvrabilité

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package discover-my-major
  :ensure t
  :config
  (global-set-key (kbd "C-h C-m") #'discover-my-major)
  (global-set-key (kbd "C-h M-m") #'discover-my-mode))

;; Recall we also have the standard keybinding "C-h m".

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;;; Look up Functions (excludes macros).
  ;; (global-set-key (kbd "C-h F") #'helpful-function)
  ;;; Look up Commands (= keybindings).
  ;; (global-set-key (kbd "C-h K") #'helpful-command)
  ;;; COMMENTED-OUT as "Info-goto-emacs[-key]-command-node" are more useful.
  (add-hook 'emacs-lisp-mode-hook #'(lambda ()
    (local-set-key (kbd "C-c C-.") #'helpful-at-point))))

;; Note we can also type "C-h" after a prefix to list its expansions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de markdown-mode (to edit README.md files easily!) and yaml-mode

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config d'AUCTeX

(use-package tex
  :ensure auctex)

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

(setq ad-redefinition-action 'accept)

(ad-activate 'magit-push-current-to-upstream)
(ad-activate 'magit-push-current-to-pushremote)
(ad-activate 'magit-run-git-async)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de Tuareg, Merlin et Company

(defun tapfa-init-reset-custom-variables-faces ()
  "Helper function to be run from CI/CD: .emacs.d batch prebuild."
  (interactive)
  (message "Running tapfa-reset-custom-variables-faces...")
  (customize-save-variable 'diff-switches "-u")
  (customize-save-variable 'warning-suppress-types '((bytecomp) (comp)))
  (mapc (lambda (face-spec)
          ;; https://www.emacswiki.org/emacs/CustomizingAndSaving#h5o-4
          (put (car face-spec) 'customized-face (cadr face-spec))
          (face-spec-set (car face-spec) (cadr face-spec)))
        '((proof-locked-face ((t (:background "#add8e6"))))
          (region ((t (:background "gold1" :distant-foreground "dim grey"))))))
  ;; (customize-customized)
  (customize-save-customized))

(defcustom tapfa-init-always-install-opam-switch-mode nil
  "Should opam-switch-mode be installed even if opam is not detected?
Option useful for .emacs.d batch prebuild.

The following values are meaningful:
nil do not enforce the installation
 t  always install opam-switch-mode"
  :type '(choice (const :tag "Do not enforce the installation" nil)
                 (const :tag "Always install opam-switch-mode" t))
  :group 'tapfa-init)

(setq tapfa-init-opam-available
      (let* ((temp (get-buffer-create " *temp"))
             (status (shell-command "opam var bin" temp)))
        (kill-buffer temp)
        (eq status 0)))

(when (or tapfa-init-opam-available tapfa-init-always-install-opam-switch-mode)
  (use-package opam-switch-mode
    :ensure t
    :hook
    ((coq-mode tuareg-mode) . opam-switch-mode)))

(use-package tuareg
  :ensure t
  :defer t)

;;;  Désactivé car le raccourci associé à <home> est malencontreux
;; (use-package bifocal
;;   :ensure t
;;   :hook
;;   (tuareg-interactive-mode . bifocal-mode))

(use-package merlin
  :ensure t
  :hook
  ((tuareg-mode caml-mode) . merlin-mode))

(use-package whitespace
  :defer t
  :diminish whitespace-mode)

(use-package eldoc
  :defer t
  :diminish)

(use-package merlin-eldoc
  :ensure t
  :hook
  ((tuareg-mode caml-mode) . merlin-eldoc-setup)
  :bind (:map merlin-mode-map
              ("C-c <left>" . merlin-eldoc-jump-to-prev-occurrence)
              ("C-c <right>" . merlin-eldoc-jump-to-next-occurrence)))

(use-package merlin-iedit
  :ensure t
  :after merlin-eldoc
  :bind (:map merlin-mode-map
              ("C-c C-o" . merlin-iedit-occurrences)))

(use-package company
  :ensure t
  :diminish
  :hook
  ((tuareg-mode caml-mode) . company-mode)
  :config
  (bind-key "<backtab>" 'company-complete))

(use-package learn-ocaml
  :ensure t)

(global-set-key (kbd "<f12>") #'learn-ocaml-mode)

;; Useful toggle when copying in Gitpod Browser Terminal
(global-set-key (kbd "<f7>") #'xterm-mouse-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Proposition d'installer les modes (proof-general, company-coq) pour Coq

(defcustom tapfa-init-coq nil
  "Should coq modes be installed?

The following values are meaningful:
nil ask on startup
-1  do not install coq related packages
 1  install proof-general + company-coq"
  :type '(choice (const :tag "Ask on startup" nil)
                 (const :tag "Do not install coq related packages" -1)
                 (const :tag "Install proof-general + company-coq" 1))
  :group 'tapfa-init)

(defun tapfa-init-coq (&optional batch)
  "Ask to set tapfa-init-coq to 1 or -1 or to ask again.
Always ask if BATCH is nil, e.g., called interactively."
  (interactive)
  (if (or (null batch) (null tapfa-init-coq))
      (let ((newval
             (condition-case _sig
                 (x-popup-dialog
                  t (tapfa-init-get-text
                     '("Voulez-vous installer les modes (plugins) associés au langage Coq ?\n"
                       ("Oui" . 1) ("Non" . -1)
                       ("Plus tard" . nil))
                     '("Do you want to install the modes (plugins) related to the Coq prover?\n"
                       ("Yes" . 1) ("No" . -1)
                       ("Later" . nil))))
               (quit nil))))
        (customize-save-variable 'tapfa-init-coq newval)
        (cond ((eq tapfa-init-coq 1)
               (package-refresh-contents))
              ((eq tapfa-init-coq -1)
               t)
              (t
               (message-box (tapfa-init-get-text
                             "Saute l'installation des modes liés à Coq.\n\nPour les installer tout de même plus tard, utiliser le menu (?)\n"
                             "Skip the installation of modes for the Coq prover.\n\nTo install these later on if need be, use the menu (?)\n")))))))

(tapfa-init-coq t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de ProofGeneral et company-coq

(when (eq tapfa-init-coq 1)

  (use-package proof-general
    :ensure t
    :mode ("\\.v\\'" . coq-mode)
    :init
    (setq overlay-arrow-string ""
          coq-double-hit-enable t))

  (use-package company-coq
    :ensure t
    :hook
    (coq-mode . company-coq-mode)
    :init
    (setq company-coq-disabled-features '(hello prettify-symbols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config générale

(setq column-number-mode t
      line-number-mode t
      isearch-allow-scroll t
      enable-recursive-minibuffers t
      require-final-newline t)

(global-set-key (kbd "C-x C-S-q") #'view-mode)

;; Enable XTerm mouse support for menus in "emacs -nw" and so on
(xterm-mouse-mode 1)

;; Adapted from: https://emacs.stackexchange.com/a/78288/19818
(defun tapfa-init--menu-bar-get-minimal-x (menu-symbol x y)
  (let ((xx (- x 1)))
    (if (< xx tty-menu--initial-menu-x)
        x
      (if (equal (menu-bar-menu-at-x-y xx y (selected-frame)) menu-symbol)
          (tapfa-init--menu-bar-get-minimal-x menu-symbol xx y)
        x))))
(defun tapfa-init--menu-bar-open-x-y (mouse-event)
  (interactive "e")
  (pcase mouse-event
    (`(mouse-1 (,_ menu-bar (,x . ,y) . ,_))
     (let ((menu (menu-bar-menu-at-x-y x y)))
       (popup-menu (or
                    (lookup-key-ignore-too-long
                     global-map (vector 'menu-bar menu))
                    (lookup-key-ignore-too-long
                     (current-local-map) (vector 'menu-bar menu))
                    (cdar (minor-mode-key-binding (vector 'menu-bar menu)))
                    (mouse-menu-bar-map))
                   (posn-at-x-y (tapfa-init--menu-bar-get-minimal-x menu x y) y nil t)
                   nil t)))
    (_ (message "unsupported event %S" (car mouse-event)))))
(define-key global-map (kbd "<menu-bar> <mouse-1>")
  (lambda (ev) (interactive "e")
    (if tty-menu-open-use-tmm (tmm-menubar-mouse ev) (tapfa-init--menu-bar-open-x-y ev))))

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
                  t (tapfa-init-get-text
                     '("Presse-papier & Annulation :\nVoulez-vous les raccourcis Windows ? (C-c, C-x, C-v, C-z)\nOu garder les raccourcis standards (M-w, C-w, C-y, C-_)\n"
                       ("Windows" . 1) ("Emacs/shell" . -1)
                       ("Me redemander" . nil))
                     '("Clipboard & Cancelling:\nDo you want the Windows keybindings? (C-c, C-x, C-v, C-z)\nOr keep the standard keybindings (M-w, C-w, C-y, C-_)\n"
                       ("Windows" . 1) ("Emacs/shell" . -1)
                       ("Ask again" . nil))))
               (quit nil))))
        (customize-save-variable 'tapfa-init-cua newval)
        (let ((ssaved (tapfa-init-get-text "Configuration enregistrée. "
                                           "Configuration stored. "))
              (sremov (tapfa-init-get-text "Pas de configuration stockée.\n"
                                           "No configuration stored.\n"))
              (swindo (tapfa-init-get-text "\nUtilise : raccourcis Windows (C-c, C-x, C-v, et C-z)\n"
                                           "\nUsing: Windows keybindings (C-c, C-x, C-v, and C-z)\n"))
              (semacs (tapfa-init-get-text "Utilise : raccourcis Emacs/shell standards\n( M-w  pour copier, c.-à-d.  Alt+w,\n  C-w  pour couper, c.-à-d.  Ctrl+w,\n  C-y  pour coller, c.-à-d.  Ctrl+y,\n  C-_  pour annuler, c.-à-d.  Ctrl+_ )\n"
                                           "Using: Emacs/shell standard keybindings\n( M-w to copy, i.e. Alt+w,\n  C-w to cut, i.e. Ctrl+w,\n  C-y to paste, i.e. Ctrl+y,\n  C-_ to undo, i.e. Ctrl+_ )\n"))
              (ssaved-end (tapfa-init-get-text
                           "Si jamais vous voulez rechanger, utilisez le menu (?)\n"
                           "If ever you want to change, use the menu (?)\n"))
              (sremov-end (tapfa-init-get-text
                           "Si jamais vous voulez rechanger, utilisez le menu (?) or restart Emacs.\n"
                           "If ever you want to change, use the menu (?) or restart Emacs.\n")))
          (cond ((eq newval 1)
                 (message-box (concat ssaved swindo ssaved-end)))
                ((eq newval -1)
                 (message-box (concat ssaved semacs ssaved-end)))
                (t
                 (message-box (concat sremov sremov-end)))))))
        (cond ((eq tapfa-init-cua 1) (cua-mode 1))
              ((eq tapfa-init-cua -1) (cua-mode -1))
              (t (cua-mode -1))))

(tapfa-init-cua t)
(add-hook 'emacs-startup-hook (lambda () (tapfa-init-help-mode 1))) ;; after dashboard-initialize
