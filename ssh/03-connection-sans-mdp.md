# Se connecter en `ssh` sans mot de passe

De base une connection en `ssh` vous demandera le mot de passe de l'utilisateur :

```bash
ssh ubuntu@172.16.3.22
ubuntu@172.16.3.22's password:
```



## Pour ne plus avoir à saisir un mot de passe :

La machine `unix-1` veut se connecter sur la machine unix-2

### 01 - créer un jeu de clés asymétriques sur `unix-1`

```bash
ssh-keygen
Generating public/private rsa key pair.
Enter file in which to save the key (/Users/kms/.ssh/id_rsa):
/Users/kms/.ssh/id_rsa already exists.
Overwrite (y/n)? y
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /Users/kms/.ssh/id_rsa.
Your public key has been saved in /Users/kms/.ssh/id_rsa.pub.
The key fingerprint is:
SHA256:yUOFmG9p5GppoaeWX1ENUIXSKECAm5qEtUy07dbW2xU kms@titi
The key's randomart image is:
+---[RSA 2048]----+
| oooo. o.*+o.    |
|. oo  + =.oo     |
|.*...  =.o. E    |
|+.o. ..+*o   .   |
|o.  o.o*S   .    |
|o  ...*  = .     |
|     *  o .      |
|    +  .         |
|   . ..          |
+----[SHA256]-----+
```

les clés sont créée dans `.ssh`

```bash
ls .ssh
id_rsa          id_rsa.pub
```



### 02 - Copier sa clé publique sur la machine distante `unix-2`

On va utiliser la commande `cat` et `ssh`

```bash
cat ~/.ssh/id_rsa.pub | ssh user_unix-2@ip_unix-2 "cat >> ~/.ssh/authorized_keys"
ubuntu@172.16.3.22's password:
```

C'est la dernière fois qu'il nous demande un mot de passe :)

### 03 - La connection sur `unix-2` ne nécéssite plus de mot de passe

```bash
unix-1: ~ $ ssh unix-2@172.16.3.22
Welcome to Ubuntu 18.04.2 LTS (GNU/Linux 5.0.0-29-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage


 * Canonical Livepatch is available for installation.
   - Reduce system reboots and improve kernel security. Activate at:
     https://ubuntu.com/livepatch

253 packages can be updated.
0 updates are security updates.

Your Hardware Enablement Stack (HWE) is supported until April 2023.
Last login: Wed Sep 25 11:09:48 2019 from 172.16.4.124
unix-2@ubuntu-VirtualBox:~$
```

