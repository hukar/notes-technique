#  03 Créer un compte Atlas MongoDB

Login: mon compte `google`

## Créer un cluster sandbox

![Screenshot 2020-02-08 at 14.31.30](assets/Screenshot 2020-02-08 at 14.31.30.png)

Il crée 3 nœux si un est indisponnible il bascule sur un autre nœud.

## Sécurité

On authorise tout le monde

![Screenshot 2020-02-08 at 14.36.08](assets/Screenshot 2020-02-08 at 14.36.08.png)

## Créer un utilisateur

- username: **m001-student**
- password: **m001-mongodb-basics**

![Screenshot 2020-02-08 at 14.47.53](assets/Screenshot 2020-02-08 at 14.47.53.png)

## Connexion avec le `mongo shell`

![Screenshot 2020-02-08 at 15.05.01](assets/Screenshot 2020-02-08 at 15.05.01.png)

copier la ligne dans le **terminal** et renseigner son mot de passe.

Ou bien directement mettre con mot de passe dans la commande :

```bash
mongo "mongodb+srv://sandbox-zvyhz.mongodb.net/test"  --username m001-student --password m001-mongodb-basics
```

On est connecté au nœud `primary` qui est le seul nœud authorisé en écriture.

```bash
MongoDB Enterprise sandbox-shard-0:PRIMARY> 
```

