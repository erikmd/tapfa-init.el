# Environnement de TP pour OCaml et Coq (avec Emacs)

Ce tutoriel vise à guider l'installation de [GNU
Emacs](https://www.gnu.org/software/emacs/) et
[opam](https://opam.ocaml.org/) (*the OCaml package manager*)
sous GNU/Linux, Windows 10 (64 bits) et macOS.

La procédure ci-dessous concerne uniquement les distributions
GNU/Linux basées sur Debian.

<details><summary>Installation sous GNU/Linux Ubuntu 18.04+ ou Debian 9+</summary>

(*Ces composants sont déjà installés sur les PC de l'UPS, sauter alors
ces 5 étapes.*)

1. Installer `emacs` (version `>= 25.1`) et `rlwrap` (*optionnel*) :

        sudo apt-get update
        sudo apt-get install emacs25 rlwrap

1. Installer les dépendances d'`opam` :

        sudo apt-get install aspcud bubblewrap build-essential curl git m4 tar unzip
        sudo apt-get install pkg-config libssl-dev

1. Installer `opam` 2.0 (comme les paquets Debian/Ubuntu sont trop
   anciens, mieux vaut utiliser le [script d'installation
   officiel](https://opam.ocaml.org/doc/Install.html)) :

        curl -fOL https://github.com/ocaml/opam/raw/master/shell/install.sh
        sh ./install.sh

1. Configurer `opam` puis installer `merlin`, `utop`,
   `learn-ocaml-client` et `coq` :

        opam init --auto-setup --yes --compiler=ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y merlin utop learn-ocaml-client
        
        opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
        opam pin add -n -k version coq 8.11.2
        opam install -j 2 coq

1. **Ne pas exécuter `opam user-setup install`**.

## Installation des modes Emacs pour OCaml et Coq

(*Reprendre à cette étape si vous travaillez sur un PC de l'UPS.*)

Pour installer automatiquement les modes
[tuareg](https://github.com/ocaml/tuareg),
[merlin-eldoc](https://github.com/Khady/merlin-eldoc),
[company](https://github.com/company-mode/company-mode),
[learn-ocaml](https://github.com/pfitaxel/learn-ocaml.el),
[ProofGeneral](https://github.com/ProofGeneral/PG) et
[company-coq](https://github.com/cpitclaudel/company-coq) :

1. Téléchargez et placez le fichier `.emacs` fourni à la racine de
   votre *homedir* (`~/`) :

        cd                    # pour revenir à la racine du homedir (~/)
        mv -f .emacs .emacs~  # pour sauvegarder votre fichier au cas où
        # si la ligne précédente renvoie une erreur, ne pas en tenir compte
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

</details>

<details><summary>Installation sous macOS</summary>

1. Installer [Aquamacs](http://aquamacs.org) (version GUI, recommandé)
   ou [Emacs avec Homebrew Cask](https://formulae.brew.sh/cask/emacs).

1. Installer `gpatch`, `pkg-config` et `openssl` 1.1 avec Homebrew :

        brew install gpatch      # comme opam utilise des options GNU
        brew install pkg-config
        brew install openssl@1.1 # suivre les instructions si besoin

1. Installer `opam` 2.0 avec Homebrew :

        brew install opam

1. Configurer `opam` puis installer `merlin`, `utop`,
   `learn-ocaml-client` et `coq` :

        opam init --auto-setup --yes --compiler=ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y merlin utop learn-ocaml-client
        
        opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
        opam pin add -n -k version coq 8.11.2
        opam install -j 2 coq

1. **Ne pas exécuter `opam user-setup install`**.

## Installation des modes Emacs pour OCaml et Coq

(*Reprendre à cette étape si vous travaillez sur un PC de l'UPS.*)

Pour installer automatiquement les modes
[tuareg](https://github.com/ocaml/tuareg),
[merlin-eldoc](https://github.com/Khady/merlin-eldoc),
[company](https://github.com/company-mode/company-mode),
[learn-ocaml](https://github.com/pfitaxel/learn-ocaml.el),
[ProofGeneral](https://github.com/ProofGeneral/PG) et
[company-coq](https://github.com/cpitclaudel/company-coq) :

1. Téléchargez et placez le fichier `.emacs` fourni à la racine de
   votre *homedir* (`~/`) :

        cd                    # pour revenir à la racine du homedir (~/)
        mv -f .emacs .emacs~  # pour sauvegarder votre fichier
        # si la ligne précédente renvoie une erreur, ne pas en tenir compte
        curl -fOL https://github.com/erikmd/tapfa-init.el/raw/master/.emacs

    > *Note* : Si vous n'utilisez pas `curl` mais la fonctionnalité de
    > téléchargement de votre navigateur, veillez à ce que celui-ci
    > n'enlève pas le point au début du fichier
    > ([`.emacs`](https://github.com/erikmd/tapfa-init.el/raw/master/.emacs),
    > pas `emacs`).

1. Si vous n'utilisez pas GNU Emacs mais *Aquamacs*, pour éviter un
   souci de connexion TLS avec MELPA (le gestionnaire de paquets
   d'Emacs), vous pourriez avoir besoin d'exécuter :

        brew install libressl
        brew install gnutls

    puis d'ajouter ces deux lignes au début du fichier `~/.emacs` :

        (with-eval-after-load 'tls
          (push "/usr/local/etc/libressl/cert.pem" gnutls-trustfiles))

    (*Source* : [davidswelt/aquamacs-emacs#133](https://github.com/davidswelt/aquamacs-emacs/issues/133))

1. Lancer Aquamacs (ou Emacs).

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

</details>

<details>
<summary>Installation sous Windows 10 (64 bits) avec WSL</summary>

1.  Installer GNU Emacs 26 à partir de
    <https://vigou3.gitlab.io/emacs-modified-windows/>

1.  Activer WSL dans Windows 10 :
    <https://docs.microsoft.com/en-us/windows/wsl/install-win10>

1.  Installer une distribution GNU/Linux
    ([Debian](https://www.microsoft.com/store/apps/9MSVKQC78PK6) ou
    [Ubuntu](https://www.microsoft.com/store/apps/9N9TNGVNDL3Q)) à
    partir de Microsoft Store.

1.  Ouvrir un terminal WSL (par ex. en tapant le nom de la distribution
    dans le Menu Démarrer) et assurez-vous que vous avez un compte
    utilisateur Linux "normal" (**pas `root`**)

1.  Mettre à jour l'index des paquets :

    ```
    sudo apt-get update
    ```

1.  Installer les dépendances d'`opam` :

    ```
    sudo apt-get install aspcud bubblewrap build-essential curl git m4 tar unzip
    sudo apt-get install pkg-config libssl-dev
    ```

1.  Installer `opam` 2.0 (comme les paquets Debian/Ubuntu sont trop
    anciens, mieux vaut utiliser le [script d'installation
    officiel](https://opam.ocaml.org/doc/Install.html)) :

    ```
    curl -fOL https://github.com/ocaml/opam/raw/master/shell/install.sh
    sh ./install.sh
    ```

1.  Configurer `opam` et installer `merlin`, `utop`,
    `learn-ocaml-client` et `coq` (**`--disable-sandboxing` est requis**) :

    ```
    opam init --disable-sandboxing --auto-setup --yes --compiler=ocaml-base-compiler.4.05.0
    eval $(opam env)
	opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
    opam pin add -n -k version coq 8.11.2
    opam install -y -j 2 merlin utop learn-ocaml-client coq
    ```

    (*Les commandes précédentes doivent être copiées ligne à ligne !*)

    **Ne pas exécuter `opam user-setup install`**.

1.  Installer `wsl-alias` :

    ```
    curl -fOL https://github.com/leongrdic/wsl-alias/raw/master/install.sh
    bash ./install.sh
    ```

    et valider les questions posées.

1.  Ajouter comme indiqué, le chemin suivant à votre `PATH` Windows :
    `%userprofile%\wsl-alias` (vous pouvez vous inspirer de la page
    <https://stackoverflow.com/a/44272417>).

1.  Ouvrir un terminal cmd.exe (a.k.a. MS-DOS, **pas WSL**) et taper les
    commandes suivantes :

    ```
    b wsl-alias add opam opam
    b wsl-alias add ocaml "opam exec -- ocaml"
    b wsl-alias add ocamlc "opam exec -- ocamlc"
    b wsl-alias add ocamlmerlin "opam exec -- ocamlmerlin"
    b wsl-alias add learn-ocaml-client "wrapper-learn-ocaml-client"
    b wsl-alias add utop "opam exec -- utop"
    b wsl-alias add coqtop "opam exec -- coqtop"
	b wsl-alias add coqidetop "opam exec -- coqidetop"
    b wsl-alias add coqc "opam exec -- coqc"
    b wsl-alias list  # pour vérifier
    b                 # sans argument, pour passer en mode WSL
    ```

    (*Les commandes précédentes doivent être copiées ligne à ligne !*)

1.  Vérifier que vous êtes bien dans le répertoire
    `/mnt/c/Users/VOTRELOGIN` (dossier personnel Windows) et télécharger
    le fichier `.emacs` fourni :

    ```
    mv -f .emacs .emacs.bak  # pour sauvegarder votre fichier au cas où
    # si la ligne précédente renvoie une erreur, ne pas en tenir compte
    curl -fOL https://github.com/erikmd/tapfa-init.el/raw/master/win64/.emacs
    ```

1.  Toujours dans la **ligne de commande WSL**, éditer le fichier
     `~/.wsl-alias/env.sh` en tapant :

    ```
    nano ~/.wsl-alias/env.sh
    ```
	
	Ajouter à la fin de ce fichier (qui doit déjà exister) :

    ```
    wrapper-learn-ocaml-client() {
        declare -a args
        args=()
        for arg; do
        args[${#args[@]}]="$(sed -e 's|htt/mnt/p\\\?|http://|; s|http/mnt/s\\\?|https://|' <<< "$arg")"
        done
        exec opam exec -- learn-ocaml-client "${args[@]}"
    }
    ```

    Sauver avec <kbd>Ctrl+O</kbd> <kbd>Entrée</kbd> et quitter avec
    <kbd>Ctrl+X</kbd>.

1.  Lancer Emacs à partir de Windows.

    L'installation des modes Emacs pour OCaml et Coq
	([tuareg](https://github.com/ocaml/tuareg),
	[merlin](https://github.com/ocaml/merlin),
	[company](https://github.com/company-mode/company-mode),
	[learn-ocaml](https://github.com/pfitaxel/learn-ocaml.el),
	[ProofGeneral](https://github.com/ProofGeneral/PG) et
	[company-coq](https://github.com/cpitclaudel/company-coq)) devrait
	se lancer automatiquement et durer environ 2'30.

    En cas de souci, faites
    <kbd>M-x package-install-selected-packages RET</kbd>
    (<kbd>M-x</kbd> désignant <kbd>Alt+X</kbd>
    et <kbd>RET</kbd> la touche Entrée) et redémarrez emacs.

    Vous pouvez alors **créer ou ouvrir un fichier OCaml** en tapant
    <kbd>C-x C-f tp1.ml RET</kbd>

1. Et pour utiliser le mode
   [`learn-ocaml`](https://github.com/pfitaxel/learn-ocaml.el#usage),
   vous pouvez taper <kbd>M-x learn-ocaml-mode RET</kbd>.

</details>

## Remarque

En cas de problème avec cette configuration, ouvrez une [issue](https://github.com/erikmd/tapfa-init.el/issues) ou envoyez-moi un [e-mail](https://github.com/erikmd).
