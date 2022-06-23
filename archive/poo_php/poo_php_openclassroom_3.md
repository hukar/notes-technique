### cloner un objet

```php
$a = new Obj;
$b = $a;
```

`$b` et `$a` on le même objet enréférence et si l'on modifie `$b`on modifie `$a`.

```php
$a = new Obj;
$b = clone $a;
```

Ainsi `$a`et `$b` sont bien deux objets séparé, lamodification de l'un ne se repercutera pas sur l'autre.

### implémentation de __clone

`__clone` permet de spécifier le comportement quand un objet est cloné.
Par exemple transformer son nom :

```php
  public function __clone () {
    $nomArr = preg_split('//',$this->nom,-1,PREG_SPLIT_NO_EMPTY);
    shuffle($nomArr);
    $this->nom = join('',$nomArr);
    self::$instance++;
  }
```

`preg_split`transforme une chaîne en tableau de caractère.

`shuffle` mélange un tableau.

`join` transforme un tableau en chaîne de caractère.

### Test d'égalité

Si on ne modifie pas les attributs des objets clônés on obtient :

```php
$a = new obj;

$b = clone $a;

$a == $b;  // vrai
$a === $b; // faux
```

### lire les attributs d'un objet

Affiche les attributs public de l'objet.

```php
foreach ($c as $key=>$value) {
  echo $key,"=>",$value,'<br>';
}
```

### Les interfaces

```php
interface Movable
{
  // toutes les méthodes doivent être public
  // elles ne peuvant pas être final
  public function move($dest);
}

class Robot implements Movable
{
  // obligation de définir la méthode move
  public function move ($dest) {
    echo "je vais à ",$dest,'<br>';
  }
}
```

### constante d'interface

```php
interface Movable
{
  public function loadstr($dest);

  const VITESSE = "vrouum!!<br>";
}

echo Movable::VITESSE;
> vrouum!!
```

### héritage multiple

Une interface peut hérité de plusieurs interfaces

```php
interface Movable
{
  public function move($dest);

  const VITESSE = "vrouum!!<br>";
}

interface SuperMovable
{
  public function flight();
}

interface Winter extends Movable,SuperMovable
{
  public function winterMove();
}
```

Une classe peut implémenter plusieurs interfaces

```php
class Robot implements Movable,SuperMovable
{
  public function move ($dest) {
    return "je vais à ".$dest;
  }

  public function flight () {

  }
}
```

### une variable propre à une fonction

Utilisation du mot clé `static`

```php
function suivant($animaux) {

	// la valeur de $position ne sera initialisé 
	// que lors du premier appel
	static $position =0;
	$position = ($position + 1)%count($animaux);
	echo $position.'<br>';
}
```

### Implémentation de l'interface Iterator

```php
class Zoo implements Iterator
{
  public $animaux;
  private $position = 0;

  public function __construct (array $tab) {
    foreach ($tab as $animal) {
      $this->animaux[] = $animal;
    }
  }

  public function current () {
    return $this->animaux[$this->position];
  }

  public function key () {
    return $this->position;
  }

  public function next () {
    $this->position++;
  }

  public function rewind () {
    $this->position = 0;
  }

  public function valid () {
    return isset($this->animaux[$this->position]);
  }
}
```

Cela permet de changer le comportement de foreach

```php
  foreach ($monZoo as $key=>$value) {
    echo $key," => ",$value,'<br>';
  }
  
> 0 => cigale
> 1 => grillon
> 2 => chauve-souris
> 3 => pipistrelle
```

### Interface SeekableIterator

SeekableIterator hérite de Iterator

ajout de la méthode `seek`

```php
class Zoo implements seekableIterator
{
  public $animaux;
  private $position = 0;

  public function __construct (array $tab) {
    ...

  public function current () {
    ...

  public function seek ($ind) {
    $anciennePosition = $this->position;

    $this->position = $ind;

    if (!$this->valid()) {
      trigger_error("coco culcul<br>",E_USER_WARNING);
      $this->position = $anciennePosition;
    }

  }
}
```

On peut ensuite changer la position courante

```php
$monZoo->seek(2);
echo $monZoo->current(),'<br>';
```

### atribut dynamique

```php
$str = "nom";
$bonom = new bonom;
echo $bonom->$str;

> michel // public $nom = "michel";
```

Quelques exemples

```php

$str = "nom";
$nom = "pipi";

echo ${$str};
> pipi
// ou bien
echo $$str;
> pipi
```

### Utiliser un objet avec les crochets ArrayAccess

```php
class Zoo implements ArrayAccess
{
  public $nbAnimaux = 100;
  public $ville = "Anvers";
  public $prix = 5;


  public function offsetExists ($key) {
    return isset($this->$key);
  }

  public function offsetGet ($key) {
    return $this->$key;
  }

  public function offsetSet ($key,$value) {
    $this->$key = $value;
  }

  public function offsetUnset ($key) {
    unset($this->$key);
  }
}
```
Utilisation des objets avec la notation des tableaux

```php
  $monZoo = new Zoo();
  echo $monZoo["prix"];

  unset($monZoo["prix"]);
```

### Interface Countable

```php
class Zoo implements ArrayAccess,Countable
{
  public $nbAnimaux = 100;
  public $ville = "Anvers";
  public $prix = 5;
  ...
  
  ...
    public function count () {
    $i = 0;
    foreach ($this as $count) {
      $i++;
    }
    return $i;
  }
}
```
Test dans le code

```php
  $monZoo = new Zoo;
  echo $monZoo->count(),'<br>';
  > 3
  unset($monZoo["prix"]);

  echo $monZoo->count(),'<br>';
  > 2
```

### ArrayIterator

C'est une classe PHP implémentant les quatres interfaces ci-dessus.
