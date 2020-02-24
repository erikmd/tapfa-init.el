# Environnement de TP pour OCaml et Coq (avec Emacs)

Cette configuration nécessite d'installer [GNU
Emacs](https://www.gnu.org/software/emacs/) et
[opam](https://ocaml.org/) (*the OCaml package manager*).

La procédure ci-dessous concerne uniquement les distributions
GNU/Linux basées sur Debian. La procédure pour macOS est similaire.

Pour Windows 10, consulter
<https://github.com/erikmd/tapfa-init-win64.el#readme>.

## Installation sous GNU/Linux Ubuntu 16.04+ ou Debian 9+

1. Installer `emacs` (version `>= 24.4`) et `rlwrap` (*optionnel*) :

        sudo apt-get update
        sudo apt-get install emacs25 rlwrap

1. Installer les dépendances d'`opam` :

        sudo apt-get install aspcud bubblewrap build-essential curl git m4 tar unzip

1. Installer `opam` 2.0 (comme les paquets Debian/Ubuntu sont trop
   anciens, mieux vaut utiliser le [script d'installation
   officiel](https://opam.ocaml.org/doc/Install.html)) :

        curl -fOL https://github.com/ocaml/opam/raw/master/shell/install.sh
        sh ./install.sh

1. Configurer `opam` puis installer `merlin`, `utop`,
   `learn-ocaml-client` et `coq` (déjà installés sur les PC de l'UPS,
   sauter alors cette étape) :

        opam init --auto-setup --yes --compiler=ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y merlin utop learn-ocaml-client
        
        opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
        opam pin add -n -k version coq 8.11.0
        opam install -j 2 coq

1. **Ne pas exécuter `opam user-setup install`**.

## Installation des modes Emacs pour OCaml et Coq

Pour installer automatiquement les modes
[tuareg](https://github.com/ocaml/tuareg),
[merlin-eldoc](https://github.com/Khady/merlin-eldoc),
[company](https://github.com/company-mode/company-mode),
[ProofGeneral](https://github.com/ProofGeneral/PG) et
[company-coq](https://github.com/cpitclaudel/company-coq) :

1. Téléchargez et placez le fichier `.emacs` fourni à la racine de
   votre *homedir* (`~/`) :

        cd                    # pour revenir à la racine du homedir (~/)
        mv -f .emacs .emacs~  # pour sauvegarder votre fichier
        curl -fOL https://github.com/erikmd/tapfa-init.el/raw/master/.emacs

    > *Note* : Si vous n'utilisez pas `curl` mais la fonctionnalité de
    > téléchargement de votre navigateur, veillez à ce que celui-ci
    > n'enlève pas le point au début du fichier
    > ([`.emacs`](https://github.com/erikmd/tapfa-init.el/raw/master/.emacs),
    > pas `emacs`).

1. Lancer Emacs :

        emacs &

    L'installation des modes Emacs pour OCaml et Coq devrait se lancer
    automatiquement et durer environ 2'30.

    En cas de souci, faites
    <kbd>M-x package-install-selected-packages RET</kbd>
    (<kbd>M-x</kbd> désignant <kbd>Alt+X</kbd>
    et <kbd>RET</kbd> la touche Entrée) et redémarrez emacs.

    Vous pouvez alors **créer ou ouvrir un fichier OCaml** en tapant
    <kbd>C-x C-f tp1.ml RET</kbd>

1. Et pour utiliser le mode
   [`learn-ocaml`](https://github.com/pfitaxel/learn-ocaml.el#usage),
   vous pouvez taper <kbd>M-x learn-ocaml-mode RET</kbd>.
