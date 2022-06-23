# 02 commande ssh

La première chose que va faire le serveur en connection ssh à la première connection, c'est proposé sa clé de chiffrement publique :

`ssh address_ip`

```bash
ssh 172.16.10.131

The authenticity of host '172.16.10.131 (172.16.10.131)' can't be established.
ECDSA key fingerprint is SHA256:GpjjqF7+zmqgTMktoaVm8XjDZmHMUdufcnZnLG+UIhI.
Are you sure you want to continue connecting (yes/no)?
```

Ce qui ajoute une entrée au fichier `~/.ssh/know_hosts` :

```
172.16.10.131 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAIwfnBABjnuYU3jf8MTqF+ACnQkTbOPtbFg2ebpdnAH44AmdwgBaH7SUc4tim9/P+k3oUYHd0cxn7xZgvelP4A=
```

## Génération de clés

### `ssh-keygen`

`~/.ssh/id_rsa` : clé privée

`~/.ssh/id_rsa.pub` : clé public

La clé public se recopie dans le fichier `~/.ssh/authorized_keys`.

## Installation d'apache sur une machine ubuntu en réseau

```bash
sudo apt install apache2 -y
```

`-y`  pour dire `yes` à tout.

## Sortir du terminal à distance `exit`

## ssh-keygen

```bash
ssh-keygen

Generating public/private rsa key pair.
Enter file in which to save the key (/home/ubuntu/.ssh/id_rsa):
```
Par défaut les clés sont créée dans /home/user/.ssh

```bash
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
```
On peut ajouter un mot de passe en plus.

```bash
Your identification has been saved in /home/ubuntu/.ssh/id_rsa.
Your public key has been saved in /home/ubuntu/.ssh/id_rsa.pub.
```
Génération des deux clés
privée : `id_rsa`
public : `id_rsa.pub`

```bash
The key fingerprint is:
SHA256:dwT82sJe82Pcs1KRfwOtpE3UhHljqVnT0k32088c6GI ubuntu@ubuntu-VirtualBox
The key's randomart image is:
+---[RSA 2048]----+
|         ..   =+*|
|          .. +oX*|
|           .o.B+*|
|           .o* *+|
|        S..E*.o *|
|         .=o=o oo|
|         . o +..o|
|          .  .=..|
|             ..oo|
+----[SHA256]-----+
```

Maintenant on les retrouve ici :

```bash
ls .ssh

id_rsa  id_rsa.pub
```

