# 04 Installation d'une application `NodeJS`

## 1. mise à jour du package manager

```bash
sudo apt-get update
```

## 2. Installer NodeJs et NPM

```bash
sudo apt-get install nodejs npm
```

## 3. Installer Express-generator

```bash
sudo npm install -g express-generator
```

<img src="assets/Screenshot2020-07-14at10.00.21.png" alt="Screenshot 2020-07-14 at 10.00.21" style="zoom:50%;" />

## 4. Créer une application express

```bash
express test
cd test
npm install
```

## 5. lancer l'application

```bash
sudo PORT=80 npm run start
```

<img src="assets/Screenshot2020-07-14at10.09.43.png" alt="Screenshot 2020-07-14 at 10.09.43" style="zoom:50%;" />

L'`IP` étant celle de notre machine virtuelle :

<img src="assets/Screenshot2020-07-14at10.10.24-4714316.png" alt="Screenshot 2020-07-14 at 10.10.24" style="zoom:50%;" />
