# Environnement de TP pour OCaml+Coq (avec Emacs)

Ce tutoriel vise à guider l'installation de [GNU
Emacs](https://www.gnu.org/software/emacs/) et
[opam](https://opam.ocaml.org/) (*the OCaml package manager*)
sous GNU/Linux, macOS et Windows 10 (64 bits).

*Notes :*

* La procédure "GNU/Linux" concerne uniquement les distributions basées sur Debian ou Ubuntu.

* Le mode `learn-ocaml.el` (qui requiert `learn-ocaml-client`) a été surtout testé sous GNU/Linux jusqu'à présent.

<details><summary><b>Installation sous GNU/Linux Ubuntu 18.04+ ou Debian 9+</b></summary>

(*Ces composants sont déjà installés sur les PC de l'UPS, sauter alors
ces 5 étapes.*)

1. Installer `emacs` (version `>= 25.1`) et `rlwrap` (*optionnel*) :

        sudo apt-get update
        sudo apt-get install emacs25 rlwrap

   Il peut être nécessaire de remplacer `emacs25` par `emacs` sur certaines distributions
   (Ubuntu 20.04+ ou Debian 10+) pour avoir une version plus récente.

1. Installer les dépendances d'`opam` :

        sudo apt-get install aspcud bubblewrap build-essential curl git m4 tar unzip
        sudo apt-get install pkg-config libssl-dev

1. Installer `opam` 2.0 (comme les paquets Debian/Ubuntu sont trop
   anciens, mieux vaut utiliser le [script d'installation
   officiel](https://opam.ocaml.org/doc/Install.html)) :

        curl -fOL https://github.com/ocaml/opam/raw/master/shell/install.sh
        sh ./install.sh

1. Configurer `opam` puis installer `merlin` et `coq` :

        opam init --auto-setup --bare
        opam switch create 4.05.0 ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y merlin
        
        opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
        opam pin add -n -k version coq 8.12.2
        opam install -j 2 coq

   **Ne pas exécuter `opam user-setup install`**.

1. **Optionnel** : installer `learn-ocaml-client` (utile seulement pour [`learn-ocaml.el`](https://github.com/pfitaxel/learn-ocaml.el))
   et `utop` (pour un toplevel en ligne de commande plus riche que `rlwrap ocaml`) :

        opam install utop learn-ocaml-client

## Installation des modes Emacs pour OCaml et Coq

(*Reprendre à cette étape si vous travaillez sur un PC de l'UPS.*)

Pour installer automatiquement les modes
[tuareg](https://github.com/ocaml/tuareg),
[bifocal](https://github.com/riscy/bifocal-mode),
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

   > ***Note pour les utilisateurs de Debian*** : si vous avez **Emacs 26.1**
   > (la version packagée [dans Debian 10](https://packages.debian.org/buster/emacs)),
   > vous pourriez avoir le message d'erreur
   > "Failed to download 'melpa' archive during the package refresh step".
   > C'est un bug connu ([debbug #34341](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341))
   > qui a été corrigé dans Emacs 26.3 et 27.1. En gardant Emacs 26.1,
   > un contournement simple consiste à décommenter la ligne
   > `(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")`
   > dans votre fichier `.emacs` (enlever les `;;` devant cette ligne)
   > et redémarrer emacs.

   En cas d'un autre type de souci durant l'installation, faites
   <kbd>M-x package-install-selected-packages RET</kbd>
   (<kbd>M-x</kbd> désignant <kbd>Alt+X</kbd>
   et <kbd>RET</kbd> la touche Entrée) et redémarrez emacs.

1. Vous pouvez alors **créer ou ouvrir un fichier OCaml** en tapant
   <kbd>C-x C-f tp1.ml RET</kbd> (<kbd>C-x</kbd> désignant <kbd>Ctrl+X</kbd>).
    
    Tester alors l'installation en effectuant les deux choses suivantes :
    
    * Écrire une erreur de type (`let n = 1 + true`) et taper sur
      <kbd>C-x C-s</kbd> pour sauvegarder.
      Assurez-vous que l'IDE a bien mis en surbrillance l'erreur de type
      (et si vous placez le curseur dessus (ou tapez <kbd>C-c C-x</kbd>),
      vous devriez voir le message d'erreur complet :
      *This expression has type bool but an expression was expected of type int*).

    * Corriger l'expression précédemment tapée (`let n = 1 + 2`) puis taper sur
      <kbd>C-c C-e</kbd> pour évaluer l'expression courante dans un toplevel
      (si l'IDE affiche `OCaml REPL to run:` la première fois, valider avec
      <kbd>RET</kbd> = touche Entrée).

1. Et si vous avez installé `learn-ocaml-client`, pour utiliser le mode
   [`learn-ocaml`](https://github.com/pfitaxel/learn-ocaml.el#usage),
   vous pouvez taper <kbd>M-x learn-ocaml-mode RET</kbd>.

</details>

<details><summary><b>Installation sous macOS</b></summary>

1. Installer [Aquamacs](https://aquamacs.org)
   ou [Emacs avec Homebrew Cask](https://formulae.brew.sh/cask/emacs).

1. Installer `gpatch`, `pkg-config` et `openssl` 1.1 avec Homebrew :

        brew install gpatch      # comme opam utilise des options GNU
        brew install pkg-config
        brew install openssl@1.1 # suivre les instructions si besoin

1. Installer `opam` 2.0 avec Homebrew :

        brew install opam

1. Configurer `opam` puis installer `merlin` et `coq` :

        opam init --auto-setup --bare
        opam switch create 4.05.0 ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y merlin
        
        opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
        opam pin add -n -k version coq 8.12.2
        opam install -j 2 coq

   **Ne pas exécuter `opam user-setup install`**.

1. **Optionnel** : installer `learn-ocaml-client` (utile seulement pour [`learn-ocaml.el`](https://github.com/pfitaxel/learn-ocaml.el))
   et `utop` (pour un toplevel en ligne de commande plus riche que `rlwrap ocaml`) :

        opam install utop learn-ocaml-client

## Installation des modes Emacs pour OCaml et Coq

(*Reprendre à cette étape si vous travaillez sur un PC de l'UPS.*)

Pour installer automatiquement les modes
[tuareg](https://github.com/ocaml/tuareg),
[bifocal](https://github.com/riscy/bifocal-mode),
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

1. Vous pouvez alors **créer ou ouvrir un fichier OCaml** en tapant
   <kbd>C-x C-f tp1.ml RET</kbd> (<kbd>C-x</kbd> désignant <kbd>Ctrl+X</kbd>).
    
    Tester alors l'installation en effectuant les deux choses suivantes :
    
    * Écrire une erreur de type (`let n = 1 + true`) et taper sur
      <kbd>C-x C-s</kbd> pour sauvegarder.
      Assurez-vous que l'IDE a bien mis en surbrillance l'erreur de type
      (et si vous placez le curseur dessus (ou tapez <kbd>C-c C-x</kbd>),
      vous devriez voir le message d'erreur complet :
      *This expression has type bool but an expression was expected of type int*).

    * Corriger l'expression précédemment tapée (`let n = 1 + 2`) puis taper sur
      <kbd>C-c C-e</kbd> pour évaluer l'expression courante dans un toplevel
      (si l'IDE affiche `OCaml REPL to run:` la première fois, valider avec
      <kbd>RET</kbd> = touche Entrée).

1. Et si vous avez installé `learn-ocaml-client`, pour utiliser le mode
   [`learn-ocaml`](https://github.com/pfitaxel/learn-ocaml.el#usage),
   vous pouvez taper <kbd>M-x learn-ocaml-mode RET</kbd>.

</details>

<details>
<summary><b>Installation sous Windows 10 (64 bits) avec WSL</b></summary>

1.  Installer GNU Emacs 27 à partir de
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

1.  Configurer `opam` et installer `merlin` et `coq`
    (**`--disable-sandboxing` est requis**) :

    ```
    opam init --disable-sandboxing --auto-setup --bare
    opam switch create 4.05.0 ocaml-base-compiler.4.05.0
    eval $(opam env)
    opam install -y merlin
        
    opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released
    opam pin add -n -k version coq 8.12.2
    opam install -j 2 coq
    ```

    (*Les commandes précédentes doivent être copiées ligne à ligne !*)

    **Ne pas exécuter `opam user-setup install`**.

1. **Optionnel** : installer `learn-ocaml-client` (utile seulement pour [`learn-ocaml.el`](https://github.com/pfitaxel/learn-ocaml.el))
   et `utop` (pour un toplevel en ligne de commande plus riche que `rlwrap ocaml`) :

        opam install utop learn-ocaml-client

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

1.  Dans la **ligne de commande WSL** (vérifier que vous êtes bien dans le répertoire
    `/mnt/c/Users/VOTRELOGIN` = dossier personnel Windows), télécharger
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
    [bifocal](https://github.com/riscy/bifocal-mode),
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

1. Vous pouvez alors **créer ou ouvrir un fichier OCaml** en tapant
   <kbd>C-x C-f tp1.ml RET</kbd> (<kbd>C-x</kbd> désignant <kbd>Ctrl+X</kbd>).
    
    Tester alors l'installation en effectuant les deux choses suivantes :
    
    * Écrire une erreur de type (`let n = 1 + true`) et taper sur
      <kbd>C-x C-s</kbd> pour sauvegarder.
      Assurez-vous que l'IDE a bien mis en surbrillance l'erreur de type
      (et si vous placez le curseur dessus (ou tapez <kbd>C-c C-x</kbd>),
      vous devriez voir le message d'erreur complet :
      *This expression has type bool but an expression was expected of type int*).

    * Corriger l'expression précédemment tapée (`let n = 1 + 2`) puis taper sur
      <kbd>C-c C-e</kbd> pour évaluer l'expression courante dans un toplevel
      (si l'IDE affiche `OCaml REPL to run:` la première fois, valider avec
      <kbd>RET</kbd> = touche Entrée).

1. Et si vous avez installé `learn-ocaml-client`, pour utiliser le mode
   [`learn-ocaml`](https://github.com/pfitaxel/learn-ocaml.el#usage),
   vous pouvez taper <kbd>M-x learn-ocaml-mode RET</kbd>.

</details>

## Remarque

En cas de problème avec cette configuration, ouvrez une [issue](https://github.com/erikmd/tapfa-init.el/issues) ou envoyez-moi un [e-mail](https://github.com/erikmd).
