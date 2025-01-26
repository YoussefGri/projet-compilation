# Description du projet

Ce projet réalisé par GRARI Youssef et BENOMAR Fadel, étudiants du M1 Génie Logiciel, consiste en la réalisation d'un compilateur et d'une machine virtuelle en LISP. Le compilateur prend en entrée du code LISP et le transforme en code assembleur pour la machine virtuelle. On commence par charger le code généré dans La machine virtuelle puis nous l'exécutons dans cette dernière.

## Démarrage 

- **`(clisp gotest.lisp)` Charge tout le code et lance les tests contenus dans `/tests/all-tests`**.
 
- La fonction `(make-machine)` définie dans `machine/machine.lisp` et appelée dans `all-tests` Créé la machine `'mv` de nom `'mvtest` avec une mémoire de 10000 par défaut (dans nos tests nous utilisons une vm de taille 5000).
- Les fonctions de la forme `(run-test-...)` permettent de lancer le compilateur, récupèrent le code généré et le chargent dans la machine virtuelle avant de l'executer avec `(run-machine)`.
- Exemple : `(run-test-case mv 5 '(+ 2 3) "ADD")`
- Les tests de `fibo` et `fact` sont dans le fichier `/tests/tests/fibo_fact.lisp`

## Organisation des fichiers

Vue d'ensemble: :

- `gotest.lisp` charge le code de la machine et du compilateur et lance des tests. 
- Les répertoires `/machine`, `/compilateur` et `/tests` contiennent respectivement le code de la machine, du compilateur et des tests.

Répertoire machine:

- `machine.lisp` contient les fonctions principales de la machine virtuelle `make-machine`, `run-machine` (lance la machine et renvoie le contenu du registre R0), `load-machine` (chargeur), `exec-inst` (fonction résponsable de l'exécution des instructions asm), `reset-memoire` (nettoie et prépare les propriétés de la vm).
- `instructions.lisp` contient les instructions gérées par la machine (add sub move...).
- `all-fonctions.lisp` et `/fonctions` contiennent les fonctions d'abstraction du fonctionnement de la machine, de gestion de ses composants mémoire / propriétés / étiquettes / pile / exécution.

Répertoire compilateur:

- `compilateur.lisp` contient la fonction principale du compilateur ("compilation").
- `all-cas.lisp` et `/cas` contiennent les différents cas que l'on pourrait rencontrer dans du code lisp pour la compilation (booleens, boucles, fonctions, appels de fonctions ...).

Répertoire test:

- `all-tests.lisp` et `/tests` contiennent des tests simples (arithmétique, booleens...) ainsi que les tests fibo et fact.
- Deux versions de run-test-fonction existent, une qui affiche le code assembleur généré et une autre qui ne l'affiche pas. (dans l'etat actuel la deuxième version est commentée)
- Nous affichons également l'état de la machine après l'exécution du code généré pour chaque test. Ceci peut être désactivé en commentant la ligne `(print-machine mv)` dans la fonction `run-machine`, ligne 93 du fichier `/machine/machine.lisp`.

## Structure de la VM

* nom : Nom de la machine.
* R0, R1, R2, R3 : Registres.
* BP : Base Pointer initialisé à 100, pile montante.
* SP : Stack Pointer, si pile vide SP = BP.
* FP : Frame Pointer
* DPP : Drapeau de comparaison "plus petit".
* DE : Drapeau de comparaison "égalité".
* DPG : Drapeau de comparaison "plus grand".
* taille : Taille allouée à la mémoire (pile + tas + code).
* memtab : Mémoire de la machine.
* PC : Program Counter, compteur ordinal, position dans le code.
* LC : Load Counter, position du chargement du code.
* etiq : Table de hashage pour les étiquettes.
* etiqNR : Table de hashage des étiquettes non résolues.

## Précisions sur les tests

- Le fichier `all-tests` contient la création de la vm ainsi qu'une liste de tests importés du répertoire `/tests`. Les tests sont de la forme `(run-test...)` qui est un appel de fonctions qui s'occupent de lancer le compilateur et charger le code généré dans la vm puis l'exécuter.

- Pour un meilleur affichage nous avons commenté les lignes correspondant aux tests unitaires et laissé celles des tests `fibo` et `fact`. Vous pouvez donc décommenter à votre convenance les tests que vous souhaitez lancer.

- Le code assembleur généré est affiché uniquement pour les fonctions (`run-test-fonction`).

## Affichage tests Fact 10 et Fibo 10

> Machine virtuelle : 
> --- Nom : MVTEST 
> --- Taille : 5000
> - Registres : 
> --- R0 : 0 
> --- R1 : 0 
> --- R2 : 0 
> --- R3 : 0
> - Pointeurs : 
> --- BP : 100 
> --- SP : 100 
> --- VP : 1 
> --- FP : 0
> - Drapeaux : 
> --- DPP : 0 
> --- DE : 0 
> --- DPG : 0
> - Compteurs : 
> --- PC : 4999 
> --- LC : 4999 
> 
> Tests de fonctions Fibo et Fact :
> F  : (DEFUN FACT (N) (IF (<= N 1) 1 (* N (FACT (- N 1)))))
> Compiled Function Body: 
> ((FENTRY) (@ FACT) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (:DIESE 1) :R0) (PUSH :R0) (POP :R0) (POP :R1) (CMP :R1 :R0) (MOVE (:DIESE T) :R0)
>  (JLE (@ #:|finTest2878|)) (MOVE (:DIESE NIL) :R0) (@ #:|finTest2878|) (CMP :R0 (:DIESE NIL)) (JEQ (@ #:|sinon2876|)) (MOVE (:DIESE 1) :R0)
>  (JMP (@ #:|finSi2877|)) (@ #:|sinon2876|) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (:DIESE 1) :R0)
>  (PUSH :R0) (POP :R1) (POP :R0) (SUB :R1 :R0) (PUSH :R0) (PUSH (:DIESE 1)) (MOVE :FP :R1) (MOVE :SP :FP) (MOVE :SP :R2) (SUB (:DIESE 1) :R2)
>  (SUB (:DIESE 1) :R2) (PUSH :R2) (PUSH :R1) (PUSH (:DIESE 0)) (JSR (@ FACT)) (PUSH :R0) (POP :R1) (POP :R0) (MULT :R1 :R0) (@ #:|finSi2877|)
>  (RTN) (FEXIT))
> Compiled Function Call: 
> ((MOVE (:DIESE 10) :R0) (PUSH :R0) (PUSH (:DIESE 1)) (MOVE :FP :R1) (MOVE :SP :FP) (MOVE :SP :R2) (SUB (:DIESE 1) :R2) (SUB (:DIESE 1) :R2)
>  (PUSH :R2) (PUSH :R1) (PUSH (:DIESE 0)) (JSR (@ FACT)))
> 
> Machine virtuelle : 
> --- Nom : MVTEST 
> --- Taille : 5000
> - Registres : 
> --- R0 : 3628800 
> --- R1 : 362880 
> --- R2 : 163 
> --- R3 : 0
> - Pointeurs : 
> --- BP : 100 
> --- SP : 100 
> --- VP : 1 
> --- FP : 0
> - Drapeaux : 
> --- DPP : 0 
> --- DE : 0 
> --- DPG : 0
> - Compteurs : 
> --- PC : 4946 
> --- LC : 4946 
> Real time: 0.003537 sec.
> Run time: 0.003535 sec.
> Space: 144952 Bytes
> OK : "Factorielle" : (FACT 10) = 3628800
> 
> F  : (DEFUN FIBO (N) (IF (< N 2) N (+ (FIBO (- N 1)) (FIBO (- N 2)))))
> Compiled Function Body: 
> ((FENTRY) (@ FIBO) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (:DIESE 2) :R0) (PUSH :R0) (POP :R0) (POP :R1) (CMP :R1 :R0) (MOVE (:DIESE T) :R0)
>  (JL (@ #:|finTest2881|)) (MOVE (:DIESE NIL) :R0) (@ #:|finTest2881|) (CMP :R0 (:DIESE NIL)) (JEQ (@ #:|sinon2879|)) (MOVE (LOC -1 0) :R0)
>  (JMP (@ #:|finSi2880|)) (@ #:|sinon2879|) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (:DIESE 1) :R0) (PUSH :R0) (POP :R1) (POP :R0)
>  (SUB :R1 :R0) (PUSH :R0) (PUSH (:DIESE 1)) (MOVE :FP :R1) (MOVE :SP :FP) (MOVE :SP :R2) (SUB (:DIESE 1) :R2) (SUB (:DIESE 1) :R2)
>  (PUSH :R2) (PUSH :R1) (PUSH (:DIESE 0)) (JSR (@ FIBO)) (PUSH :R0) (MOVE (LOC -1 0) :R0) (PUSH :R0) (MOVE (:DIESE 2) :R0) (PUSH :R0)
>  (POP :R1) (POP :R0) (SUB :R1 :R0) (PUSH :R0) (PUSH (:DIESE 1)) (MOVE :FP :R1) (MOVE :SP :FP) (MOVE :SP :R2) (SUB (:DIESE 1) :R2)
>  (SUB (:DIESE 1) :R2) (PUSH :R2) (PUSH :R1) (PUSH (:DIESE 0)) (JSR (@ FIBO)) (PUSH :R0) (POP :R1) (POP :R0) (ADD :R1 :R0) (@ #:|finSi2880|)
>  (RTN) (FEXIT))
> Compiled Function Call: 
> ((MOVE (:DIESE 10) :R0) (PUSH :R0) (PUSH (:DIESE 1)) (MOVE :FP :R1) (MOVE :SP :FP) (MOVE :SP :R2) (SUB (:DIESE 1) :R2) (SUB (:DIESE 1) :R2)
>  (PUSH :R2) (PUSH :R1) (PUSH (:DIESE 0)) (JSR (@ FIBO)))

