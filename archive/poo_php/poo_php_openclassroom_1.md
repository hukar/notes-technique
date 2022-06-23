#POO PHP Openclassroom

### exiger une class en paramètre

```php
public function frapper(Personnage $unPerso) { ... }
```

Possible aussi avec les tableau

```php
public function frapper(array $unTableau) { ... }
```

On ne peut pas de cette manière demander un entier ou une chaîne de caractère.

### underscore pour les attributs "private"

```php
private $_nom = "maurice";  // avec _
public $metier = "pompier";  // sans _
```

### trigger_error

declenche une erreur utilisateur

```php
triger_error("mon message d'erreur",E_USER_WARNING); 
// E_USER_[WARNING,NOTICE,ERROR]
```

### auto-chargement des classes

```php
function loadClass ($class) {
  include $class.".php";
}
spl_autoload_register('loadClass');
```

On peut aussi écrire juste

```php
spl_autoload_register();
```
On utilise l'implémentation par defaut d'une fonction d'autoload insensible à la casse (classe __Perso__ et fichier __perso.php__ par exemple)

__*gérer l'ordre d'appel avec le troisième argument*__

```php
spl_autoload_register('loadClass',false);

// on met la fonction en tête de pile
// avec le prepend à true
spl_autoload_register('loadClass2',false,true);
```
__amélioration de l'upload pour éviter des warning__

```php
function loadClass ($class) {
  echo "je suis dans loadClass<br>";
  $file = $class.".php";
  if(file_exists($file))
    include $file;
}

function loadClass2 ($class) {
    echo "je suis dans loadClass 2<br>";
    $file = $class.".class.php";
    if(file_exists($file))
      include $file;
}
spl_autoload_register('loadClass',true);
spl_autoload_register('loadClass2',false,true);
```

`file_exists`permet de tester l'existance du fichier et empêche de déclencher un warning.
### Constante de classe

```php
class Perso
{
	const PETIT = 20;
	const GRAND = 1000;
}
```
Pour utiliser des constantes de classe on se sert de l'opérateur de résolution de portée `::`

### vérifier si une valeur est dans un tableau

```php
in_array($force,[self::PETIT,self::MOYEN,self::GRAND]
```

Le mot clé __self__ désigne la classe elle-même, celà permet d'aller chercher les constantes de classe.

### Méthode statique

```php
  public static function parler() {
    echo "salut je suis un Trüg<br>";
  }
```

Trois moyens de l'utiliser :

```php
$lotrug = new Personnage;
	
$lotrug::parler();
$lotrug->parler();  
// dans ce cas on ne comprend pas 
// que parler est une méthode statique
Personnage::parler();
```

$this n'est pas défini dans une méthode statique, l'apeller génère une erreur.
self est défini :

```php
public static function parler() {
	echo "voici MOYEN ".self::MOYEN."<br>";
}
```

### attribut statique

Attention à la syntaxe `self::$_attr` cette fois ci on met le dollar  

```php
private static $_message = "salut je suis un static Trüg<br>";

public static function parler() {
	echo self::$_message;
	echo "voici MOYEN ".self::MOYEN."<br>";
}
```
### Connection PDO

```php
$host = "localhost";
$database = "poo_openlassroom";
$user = "root";
$pwd = "root";

try
{
  $pdo = new PDO('mysql:host='.$host.';dbname='.$database.';charset=utf8',$user,$pwd
  // , array(PDO::ATTR_PERSISTENT => true)
);
  // $pdo->setAttribute(PDO::ATTR_ERRMODE,PDO::ERRMODE_WARNING);
}
catch(Exception $e)
{
  echo 'erreur de connexion à la base de données : '.$e;
  exit();
}
```

### afficher les erreurs de SQL avec PDO

```php
$pdo->setAttribute(PDO::ATTR_ERRMODE,PDO::ERRMODE_WARNING);
```

activer cette ligne afin de voire les erreurs SQL

### foreach

```php
foreach($tab as $value)

// ou

foreach($tab as $key=>$value)
```

### requête préparée en lecture

```php
$request= $pdo->prepare('SELECT *
                          FROM personnage
                          WHERE id = :id');
$request->bindParam('id',$id,PDO::PARAM_INT);

$request->execute();
```

### hydrater les objet

fonction hydrate

```php
public function hydrate (array $donnees) {
	foreach ($donnees as $key=>$value) {
	  $method = "set".ucfirst($key);
	  if (method_exists($this,$method)) {
        $this->$method($value);
      }
	}
}
```

`ucfirst` passe la premiére lettre en capitale pour respecter le camelCase -> __setAttribut__

`method_exists` permet de vérifier que l'objet a bien la méthode `$method`

### implémentation du constructeur

```php
public function __construct ($donnees) {
    $this->hydrate($donnees);
  }
```
### récupération des données

```php
include("pdo.php");

$request = $pdo->query('SELECT *
                        FROM personnage');

while ($donnees = $request->fetch(PDO::FETCH_ASSOC)) {
  $perso = new Personnage($donnees);
  echo $perso->nom(),' a ',$perso->forcePerso(),'<br>';
}
```

`$donnees` est un tableau associatif construit à partir de la table en bdd :

```php
  ["id"]=>string(1) "3"
  ["nom"]=>string(8) "mini tax"
  ["forcePerso"]=>string(2) "40"
  ["degats"]=>string(2) "67"
  ["niveau"]=>string(1) "2"
  ["experience"]=>string(3) "560"
```

### ajouter un objet en base de données

Utilistation de la requête `INSERT INTO`


```php
    $request = $this->_db->prepare(
    'INSERT INTO personnage
     (nom,forcePerso,degats,niveau,experience)
     VALUES (:nom,:forcePerso,:degats,:niveau,:experience)');
```

On ne peut pas passer directement le résultat d'une méthode à bindParam :

```php
$nom = $perso->nom();
$forcePerso = $perso->forcePerso();
$degats = $perso->degats();
$niveau = $perso->niveau();
$experience = $perso->experience();

$request->bindParam('nom',$nom,PDO::PARAM_STR);
$request->bindParam('forcePerso',$forcePerso,PDO::PARAM_INT);
$request->bindParam('degats',$degats,PDO::PARAM_INT);
$request->bindParam('niveau',$niveau,PDO::PARAM_INT);
$request->bindParam('experience',$experience,PDO::PARAM_INT);
```
### Effacer un objet en BDD

```php
$request = $this->_db->prepare('DELETE FROM personnage
                                WHERE id = :id');
$id = $perso->id();
$request->bindParam('id',$id,PDO::PARAM_INT);
$request->execute();
```
La commande SQL est `DELETE FROM ... WHERE ...`

__effacer une plage d'objet :__

```php
for(	$i = 5;
		$i < 19;
		$persoManager->delete($persoManager->get($i)),
		$i++
	);
```

Syntaxe issue du C et fonctionnant aussi en PHP

### Récupérer un seul enregistrement

```php
$request = $this->_db->prepare('SELECT *
                                FROM personnage
                                WHERE id = :id');
$request->bindParam('id',$id,PDO::PARAM_INT);

$request->execute();
$donnees = $request->fetchAll()[0];
```

`$request->fetchAll()` étant un tableau de tableaux de données on récupère le premier enregistrement à son indice 0 `$request->fetAll()[0]`

### Utiliser exec et query pour les requêtes

`exec()`

```php
  $request = $this->_db->exec('DELETE FROM personnage
                               WHERE id ='.$perso->id());
```

`query()`

```php
  public function get ($id) {
    $request = $this->_db->query('SELECT *
                                    FROM personnage
                                    WHERE id = '.$id);
	// fetch est plus simple que fetchAll dans ce cas
    $donnees = $request->fetch(PDO::FETCH_ASSOC);

    return new Personnage($donnees);
  }
```
### fetch

`$q->fetch` renvoie la ligne suivante ou false

### fetchColumn

`$q->fetchColumn()`récupère la première colonne de la première ligne (ou false si plus ou pas d'enregistrement)

`$q->fetchColumn(3)` récupère la quatrième colonne de la ligne suivante

### lastInsertId

`$db->lastInsertId()` récupère le dernier id inséré en base de données

### Requête préparée

```php
$query = $this->_db->prepare('SELECT *
                                FROM personnages
                                WHERE nom <> :nom');
$query->execute(["nom"=>$nom]);
```

On peut aussi passer un tableau à `execute($tab)`

`<>` signifie `!=`

### code utile

`unset($var)` detruit une variable

`$GLOBALS` tableau de toutes les variables globales

#### détruire une variable globale

```php
unset($GLOBALS["mavar"];
```

### Détruire la séssion

```php
session_destroy();
```

### échapper les caractères html

```php 
echo htmlspecialchars("<strong> &amp; & coco</strong>"),"<br>";
// va écrire <strong> et pas mettre en gras
```
### envoyer une requête GET avec un lien

```html
    <p class="alert alert-danger">
      <a href="?cnx=1">dé-connexion</a>
    </p>
```