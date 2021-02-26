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
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; cf. rem. ci-dessous
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Si vous avez Emacs 26.1 (qui est la version fournie dans Debian 10)
;; vous pourriez avoir le message d'erreur "Failed to download 'melpa'
;; archive during the package refresh step". C'est un bug connu
;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341) qui a été
;; corrigé dans Emacs 26.3 et 27.1; un contournement simple consiste à
;; décommenter la ligne (setq gnutls-algorithm-priority ...) ci-dessus
;; (enlever les ";;").

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-switches "-u")
 '(package-selected-packages
   (quote (company-coq proof-general company merlin tuareg use-package))))

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
  (setq coq-prog-name "coqtop"))        ; or "C:/Coq/bin/coqtop.exe"…

(use-package company-coq
  :ensure t
  :hook
  (coq-mode . company-coq-mode)
  :init
  (setq company-coq-disabled-features '(hello prettify-symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config générale

(setq column-number-mode t
      line-number-mode t
      require-final-newline t)

(global-set-key (kbd "C-x C-S-q") #'view-mode)

;; Marquage des parenthèses
(load-library "paren")
(show-paren-mode 1)

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

;; Raccourcis C-c/C-x/C-v/C-z standards
;; au lieu de M-w/C-w/C-y/C-_ par défaut dans GNU Emacs
(cua-mode 1)
