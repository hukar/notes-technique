# Réglage pour un accès en réseau local

### ! Ne pas toucher au fichier `httpd.conf`

## Éditer le fichier `httpd-vhost.conf`

```bash
<VirtualHost *:80>
  ServerName localhost
  ServerAlias localhost
  DocumentRoot "${INSTALL_DIR}/www"
  <Directory "${INSTALL_DIR}/www/">
    Options +Indexes +Includes +FollowSymLinks +MultiViews
    AllowOverride All
    Require local
  </Directory>
</VirtualHost>
```

Modifier `Require local` par `Require granted all` :

```bash
<Directory "${INSTALL_DIR}/www/">
    Options +Indexes +Includes +FollowSymLinks +MultiViews
    AllowOverride All
    Require all granted
  </Directory>
```

