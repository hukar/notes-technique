### Notation PEAR
```php
private $_private;
protected $protected;
_privateMethod();

protected function protectedAction();
```

### accès à la classe parent
`parent::methodparent ()`

```php
public function gagnerExperience () {
	parent::gagnerExperience();
	$this->_magie += 13;
}
```

### Classe Finale

Classe ne pouvant pas être hérité.

```php
final class Cat
{
  ...

class Black_Cat extends Cat
{
  ...
```

```php
Fatal error: Class Black_Cat may not inherit 
from final class (Cat)
```
De même une méthode finale ne peut être redéfini dans une classe fille.

```php
class Cat
{
  final public function parler () {
	...

class Black_Cat extends Cat
{
  public function parler () {
    ...
```

```php
Fatal error: Cannot override final method Cat::parler()
```

### Résolution statique à la volée

Utilisation de `static::`à la place de `self::` permet de changer le contexte d'appel de la mère à la fille.

### mettre en minuscule le nom de la classe 

```php
echo strtolower(static::class);
```

### Méthode magique

```php
public function __construct ();
public function __destruct ();
__get;
__set;
__call;
__callStatic;
```

### Transformer un tableau en string

```php
echo implode($tab,' : ');
```
`$tab` le tableau, `' : '` le séparateur.

### serialize et unserialize

`serialize` transforme n'importe quoi en chaîne de caractère de ce type :

```php
a:4:{s:4:"lolo";i:2;s:7:"noneuil";i:2;s:3:"age";i:78;}
```

`unserialize` execute l'opération inverse

```php
$str = serialize($tab);
$tab2 = unserialize($str);
echo $tab == $tab2?"vrai":"faux";

"vrai"
```

#### utilisation pour stocker des 'array' en fichier 
```php
$fp = fopen('mimi.txt','a+');
fwrite($fp,serialize($tab));
fclose($fp);
```

#### passer des tableaux par l'url

```php
$neoTab = ['lolo'=>2,'noneuil'=>2,'langue'=>'fr','age'=>78];
$ser = serialize($neoTab);

header('location:titi.php?data='.urlencode($ser));
```

Puis dans le fichier titi.php

```php
$data = urldecode($_GET["data"]);

echo $data;

$tab = unserialize($data);

echo "<pre>";
print_r($tab);
echo "</pre>";
```

Utilisation de `urlencode` et `urldecode` pour passer les caractères speciaux dans l'url.

#### enregistrer un tableau en base de données

```php
$query = $pdo->prepare('INSERT INTO blobby (serz)
                        VALUES (:serz)');
$query->execute(['serz'=>addslashes($serz)]);

$query = $pdo->prepare('SELECT *
                        FROM blobby');
$query->execute();

while ($enr = $query->fetch()) {
  echo $enr["serz"],'<br>';

  echo '<pre>';
  print_r(unserialize($enr["serz"]));
  echo '</pre>';
}
```
utilistation de `addslashes($str)` pour ajouter un backslash aux guillemets et ainsi les échapper en base de données.

### Sérializer les objets
#### \_\_sleep et \_\_wakeup

\_\_sleep est appelé lors de la sérialisation

\_\_wakeup est appelé avec unserialize

```php
  public function __sleep () {
    $this->_valeur += 5;
    // doit retourner le nom de l'attribut
    return array('_valeur');
  }

  public function __wakeup () {
    $this->_valeur += 7;
  }
```

Maintenant l'utilisation dans le code :

```php
$test = new Test;

echo '<pre>';
print_r($test);
echo '</pre>';

> Test Object
> (
>     [_valeur:Test:private] => 10
> )

$testser = serialize($test);

echo $testser;
> O:4:"Test":1:{s:13:"Test_valeur";i:15;}

$neotest = unserialize($testser);

echo '<pre>';
print_r($neotest);
echo '</pre>';
> Test Object
> (
>     [_valeur:Test:private] => 22
> )

$neotest->parler();
> salut  // on retrouve les méthodes de l'objet
```

### une classe de connexion utilisant serialize

```php
class Connexion
{
  private $_pdo,$_host,$_user,$_pwd,$_database;

  public function __construct ($host,$user,$pwd,$database) {
    $this->_host = $host;
    $this->_user = $user;
    $this->_pwd = $pwd;
    $this->_database = $database;

    $this->_connexionBDD();
  }

  private function _connexionBDD () {
    try
    {
      $this->_pdo = new PDO('mysql:host='.$this->_host.';dbname='.$this->_database.';charset=utf8',$this->_user,$this->_pwd);
      $this->_pdo->setAttribute(PDO::ATTR_ERRMODE,PDO::ERRMODE_WARNING);
    }
    catch(Exception $e)
    {
      echo 'erreur de connexion à la base de données : '.$e;
      exit();
    }
  }
	// on ne peut pas envoyer pdo 
	// car cela déclenche une erreur
  public function __sleep () {
    return ['_host','_user','_pwd','_database'];
  }
	// il faut donc au réveil
	// ré-instancier PDO
  public function __wakeup () {
    $this->_connexionBDD();
  }

  public function PDO () {
    return $this->_pdo;
  }
}
```

On débranche PDO dans le `sleep` et on le rebranche dans le `wakeup`

ensuite dans la page 1 :

```php
session_start();

$connexion = new Connexion('localhost','root','root','poo_openclassroom');

// ... plus loin dans le code ...

$_SESSION["connexion"] = serialize($connexion);
```

Puis dans la page deux :

```php
$connexion = unserialize($_SESSION["connexion"]);
```

#### Résumé

 serialize | appel de \_\_sleep() 
---|---
 unserialize | appel de \_\_wakeup()
 
 `__sleep` renvoie un tableau avec tous les **attributs** à sauver.
 
### Simplification

Tout ça est fait automatiquement par `$_SESSION`

Pour ne pas avoir une erreur **\_\_PHP\_Incomplete\_Class Object** il faut mettre le `session_start` après le chargement des classes

```php
// page 1
$_SESSION["connexion"] = $connexion;

// page 2

spl_autoload_register(); // par exemple

session_start();

$connexion = $_SESSION["connexion"];
```

### __toString

Est invoqué lorsqu'on essaye de lire un objet comme un *string*

```php
class Mouton
{
  public $nom = "mouton";
  public $prenom = "loulou";
}

$mouton = new Mouton;

echo $mouton;
// ou bien même echo (string) $mouton;
```

On obtient l'erreur :

`Catchable fatal error: Object of class Mouton` 
`could not be converted to string`

Implémentons la méthode magique `__toString`

```php
class Mouton
{
  public $nom = "mouton";
  public $prenom = "loulou";

  public function __toString () {
    return "hello jacko!!";
  }
}
```

Utilisation :

```php
  $string = (string) $mouton;
  echo '<br>string : ',$string;
> string : hello jacko!!
```
 
### \_\_set\_state et var\_export
 
 `__set_state` est appelée lorsque `var_export` prend l'objet en argument.
  
`var_export($var,$retour(true || false)`  si retour à true renvoie une représentation de la variable sinon renvoie NULL et affiche.

`eval` execute une chaîne de caractère comme du code valide
 
```php
class Mouton
{
  public $nom = "mouton";
  public $prenom = "loulou";

  public static function __set_state ($arr) {
    $obj = new Mouton;
    $obj->nom = $arr["nom"];
    $obj->prenom = $arr["prenom"];

    return $obj;
  }
}
```

On a donc var_export qui appel `__set_stat(array("attr"=>value)` et on passe tous ça à `eval()`

```php
  eval('$moutonBis = '.var_export($mouton,true).';');

  print_r($moutonBis);
```
### Objet transformé en fonction grace à __evoke()

Si je veux utiliser mon onjet comme un fonction je dois implémenter `invoke()`

```php
class Robot
{
  private $_pv = 100;
  public function __invoke ($argument) {
    $bonus = (int) $argument;
    $this->_pv += $bonus;
    echo "point de vie : ".$this->_pv."<br>";
  }
}
```

Et puis une fois une instance créée je peux l'utiliser comme une fonction :

```php
$robi = new Robot;

$robi("coucou");
$robi("45");
$robi(5.9876);

> 100
> 145
> 150
```

### __debugInfo et var_dump

`__debugInfo` est appelée avec l'utilisation de `var_dump`.

```php
class Secret
{
  private $_pv = 100;
  private $_nom = "michel";
  private $_parfum = "coco chanel";

  public $_yeux = "bleus";
  // public function __debugInfo () {
  //   return ["yeux"=>"verts"];
  // }
}
$secret = new Secret;
var_dump(secret);

> object(Secret)#5 (4) {
  ["_pv":"Secret":private]=>
  int(100)
  ["_nom":"Secret":private]=>
  string(6) "michel"
  ["_parfum":"Secret":private]=>
  string(11) "coco chanel"
  ["_yeux"]=>
  string(5) "bleus"
}
```

Ce qui peut être un problème d'afficher des attributs privés

```php
class Secret
{
  private $_pv = 100;
  private $_nom = "michel";
  private $_parfum = "coco chanel";

  public $_yeux = "bleus";
  public function __debugInfo () {
    return ["yeux"=>"verts"];
  }
}
$secret = new Secret;
var_dump(secret);

> object(Secret)#5 (1) {
  ["yeux"]=>
  string(5) "verts"
}
```

### début d'une classe d'un file reader

```php
class FileReader
{
  private $_file;

  public function __construct ($path) {
    $this->_file = fopen($path,"c+");
  }

  public function __debugInfo () {
    return ['_file'=>fstat($this->_file)];
  }
}
```

`fopen` ouvre un fichier `c+` en lecture et écriture.

`fstat` lit des information sur un fichier.




