# 05 les fichiers `authorized_keys` et `known_hosts`

### Traduction de l'article :  https://security.stackexchange.com/questions/20706/what-is-the-difference-between-authorized-keys-and-known-hosts-file-for-ssh/20710#20710 

Le fichier `known_hosts` permet au client d'authentifier le serveur, pour vérifier qu'il ne se connecte pas à un imposteur. Le fichier `authorized_keys` permet au serveur d'authentifier l'utilisateur.

### Authentification du serveur
Une des premières choses qui se produit lorsque la connexion SSH est établie est que le serveur envoie sa clé publique au client, et prouve (grâce à la cryptographie à clé publique) au client qu'il connaît la clé privée associée. Cela authentifie le serveur : si cette partie du protocole est réussie, le client sait que le serveur est celui qu'il prétend être.

Le client peut vérifier que le serveur est bien connu, et non qu'un serveur malveillant tente de le faire passer pour le bon. SSH ne fournit qu'un mécanisme simple pour vérifier la légitimité du serveur : il se souvient des serveurs auxquels vous vous êtes déjà connecté, dans le fichier `~/.ssh/known_hosts` sur la machine cliente (il y a aussi un fichier système `/etc/ssh/known_hosts`). La première fois que vous vous connectez à un serveur, vous devez vérifier par un autre moyen que la clé publique présentée par le serveur est bien la clé publique du serveur auquel vous souhaitez vous connecter. Si vous avez la clé publique du serveur auquel vous êtes sur le point de vous connecter, vous pouvez l'ajouter à `~/.ssh/known_hosts` sur le client manuellement.

D'ailleurs, `known_hosts` peut contenir n'importe quel type de clé publique supportée par l'implémentation SSH, pas seulement DSA (aussi RSA et ECDSA).

L'authentification du serveur doit être effectuée avant que vous ne lui envoyiez des données confidentielles. En particulier, si l'authentification utilisateur implique un mot de passe, celui-ci ne doit pas être envoyé à un serveur non authentifié.

### Authentification des utilisateurs
Le serveur ne permet à un utilisateur distant de se connecter que si cet utilisateur peut prouver qu'il a le droit d'accéder à ce compte. En fonction de la configuration du serveur et du choix de l'utilisateur, celui-ci peut présenter l'une des différentes formes d'identifiants (la liste ci-dessous n'est pas exhaustive).

L'utilisateur peut présenter  **le mot de passe du compte** auquel il essaie de se connecter ; le serveur vérifie alors que **le mot de passe** est correct.

L'utilisateur peut présenter **une clé publique** et prouver qu'il possède **la clé privée** associée à cette clé publique. C'est exactement la même méthode qui est utilisée pour authentifier le serveur, mais maintenant l'utilisateur essaie de prouver son identité et le serveur le vérifie. La tentative de connexion est acceptée si l'utilisateur prouve qu'il connaît la clé privée et que la clé publique est dans la liste des autorisations du compte (`~/.ssh/authorized_keys` sur le serveur).

Un autre type de méthode consiste à déléguer une partie du travail d'authentification de l'utilisateur à la machine cliente. Cela se produit dans des environnements contrôlés tels que les entreprises, lorsque plusieurs machines partagent les mêmes comptes. Le serveur authentifie la machine cliente par le même mécanisme que celui utilisé dans l'autre sens, puis s'appuie sur le client pour authentifier l'utilisateur.

Traduit avec www.DeepL.com/Translator