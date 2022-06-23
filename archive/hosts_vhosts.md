# éditer le fichier hosts sur mac
<span style="font-size:18px">**etc** : **e**diting **t**ext **c**onfig fichier éditable de configuration</span>

```sh
sudo pico /etc/hosts

# le modifier flêche du clavier - ctrl + o - enter - ctrl + x

exemple :
127.0.0.1		titi
```
permet d'utiliser l'URL `http://titi:8888` pour joindre le localhost

# sur windows

changer simplement les droits du fichier host
il se trouve `windows/system32/drivers/etc/hosts`

```sh
# écrire la ligne :
10.0.2.2		titi
```

# adresse du hôte sous virtualbox

### 10.0.2.2

ces deux réglages permettent d'accéder au localhost de la machine hôte sur la machine virtuel en tapant par exemple
`http://titi:8888` dans edge sur windows.

# Modifier la config de MAMP

il nous faut localhost et non localhost:8888, le port standard étant le port 80.

Pour ça on règle préférence/port dans mamp

# Configurer un virtual host

Dans le fichier `/Applications/MAMP/conf/apache/httpd.conf`

il faut dé-commenter la ligne 

```sh
# Virtual hosts
Include /Applications/MAMP/conf/apache/extra/httpd-vhosts.conf
```

Puis dans `/Applications/MAMP/conf/apache/extra/httpd-vhosts.conf`

On ajoute ses lignes

```sh
<VirtualHost *:80>
    DocumentRoot "/Users/hukar/Dropbox/htdocs/wfc/"
    ServerName www.wfc.loc
    ErrorLog "logs/www.wfc.loc-error_log"
    CustomLog "logs/www.wfc.loc-access_log" common
</VirtualHost>
```