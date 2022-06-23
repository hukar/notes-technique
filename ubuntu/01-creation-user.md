# Création d'un nouveau user

```bash
sudo adduser titi

Adding user `titi' ...
Adding new group `titi' (1002) ...
Adding new user `titi' (1002) with group `titi' ...
Creating home directory `/home/titi' ...
Copying files from `/etc/skel' ...
Enter new UNIX password:
Retype new UNIX password:
passwd: password updated successfully
Changing the user information for titi
Enter the new value, or press ENTER for the default
	Full Name []: titi grosminet
	Room Number []:
	Work Phone []:
	Home Phone []:
	Other []:
Is the information correct? [Y/n] Y
```

Maintenant lui donner le privilège `sudo`

```bash
id titi
uid=1002(titi) gid=1002(titi) groups=1002(titi)

sudo usermod -aG sudo titi

id titi
uid=1002(titi) gid=1002(titi) groups=1002(titi),27(sudo)
```

On voit que le privilège `sudo` est accordé à titi

