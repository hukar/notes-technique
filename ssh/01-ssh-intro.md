# 01 introduction SSH

Accéder à la ligne de commande d'une machine distante.

Accéder à un terminal (terminal virtuel).

#### Astuce effacer l'écran du terminal `ctrl + l`

#### `ps -f` 

#### `-f` pour afficher les `uid` 

Communication client serveur SSH est le client SSHD est le serveur (open SSH Daemon)

Le serveur doit avoir SSHD installé.

Communication *"en clair"* avec le protocol telnet (rfc n°15 1969)

rfc : request for comment => description des normes sur internet

On veut se protéger de l'interception.

On veut identifier la machine distante (est-ce la bonne machine ?).

On fait appellle à la cryptographie, on transforme les caractères.

**Chiffrement symétrique** : même pour chiffrer et pour déchiffrer

cryptogramme : message chiffré

Comment faire pour communiquer la clef ??

C'est la limite du chiffrement symétrique.

**Chiffrement à clé publique** : deux clés

ssh : le premier message sera une clé symétrique

#### SSH est disponnible nativement sur windows 10

