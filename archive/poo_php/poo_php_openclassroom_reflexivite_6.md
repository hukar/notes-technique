## API refléxivité

```php
$magicien = new Magicien('michel', 45);

$classeMagicien = new ReflectionClass('Magicien');
```

### Savoir si un attribut appartient à la classee :

```php
function testProperty ($classeName, $property) {
  echo "attribut $property : ".
  ($classeName->hasProperty($property)?"oui<br>":"non<br>");
}

testProperty('_nom);
```
`$className->hasProperty('_nom')`
  Ne renvoie `true` que pour les attributs de la classe elle-même et pas pour les attributs de la classe parente sauf s'il sont `public`.

### Savoir si une méthode appartient à la classe

```php
function testMethod ($className, $method) {
  echo "méthode $method : ".
  ($className->hasMethod($method)?"oui<br>":"non<br>");
}

testMethod($classeMagicien, 'parler');
testMethod($classeMagicien, '__construct');
```
`$className->hasMethod('parler')`  
  renvoie `true` pour les méthode de la mère et la fille.

### Les constantes

```php
function testConstant ($className, $const) {
  echo "Constante $const : ".
  ($className->hasConstant($const)?"oui<br>":"non<br>");
}
```

`$className->hasConstant('PIPI')`

Afficher la valeur de la constante   
 `$className->getConstant('PIPI')`
 
Fonction utile pour l'affichage

```php
function pre ($something) {
  echo '<pre>';
  // true pour renvoyer les données et pas les afficher
  // sinon false par default
  echo print_r($something,true);  
  echo '</pre>';
}
```

### Les parents

```php
$parent = $classeMagicien->getParentClass();
echo $parent->getName();
```
`$className->getParentClass()` renvoie `false` ou le parent.

`$parent->getName()`  renvoie le nom de la classe.

`$classeMagicien->isSubclassOf('Personnage')` Bool.

### Autres outils

* `$className->isAbstract()` Bool.
* `$className->isFinal()` Bool.
* `$className->isInstantiable()` Bool.
* `$className->isInterface()` Bool.
* `$className->implementsInterface('IClasse)` Bool.

### ReflectionProperty

```php
$attributMagie = new ReflectionProperty('Magicien','_magie');
echo $attributMagie->getName();

> _magie
```

Deuxième méthode

```php
$attributMagie = $classeMagicien->getProperty('_magie');
```
Récupérer tous les attributs

```php
$attributs = $classeMagicien->getProperties();
```

Les afficher

```php
$attributs = $classeMagicien->getProperties();
  foreach($attributs as $attribut)
	  echo $attribut->getName(),
	  ' ',
	  $attribut->getValue($magicien),
	  '<br>';
```

Si l'attribut est private

```php
  foreach ($attributs as $attribut) {
    $attribut->setAccessible(true);
    echo $attribut->getName(),' ',$attribut->getValue($magicien),'<br>';
    $attribut->setAccessible(false);
  }
```
`$attribut->setAccessible(true)` pour rendre un attribut private accessible.

Remettre à `false` après lecture.

Modifier la valeur d'un attribut private

```php
$attribut->setAccessible(true);
if ($attribut->getName() == '_obscur')
  $attribut->setValue($magicien,459);
```

Portée de l'attribut

* `$attr->isPrivate()`
* `$attr->isProtected()`
* `$attr->isPublic()`
* `$attr->isStatic()` 
* si static `$attr->getValue()` sans spécifié d'objet
* ou bien `$className->getStaticPropertyValue('count')`
* `$className->setStaticPropertyValue('count',47);`

Tableau de toutes les propriétés static

`$className->getStaticProperties()` Array

### ReflectionMethod

```php
  $method = new ReflectionMethod('Magicien','parler');

  echo $method->getName();
```

ou bien 

```php
$method = $classeMagicien->getMethod('parler');
```

Test

* `isPublic()`
* `isProtected()`
* `isPrivate()`
* `isAbstract()`
* `isFinal()`
* `isStatic()`
* `isConstructor()` __construct ou méthode du même nom que la classe

### invoke

```php
$crier = $classeMagicien->getMethod('crier');

$crier->invoke($magicien,'arggghh....');

// dans magicien
public function crier ($str) {
	echo "<strong>",$str," ... ",$this->_nom,"</strong><br>";
}

> arggghh.... ... michel
```

De même `invokeArgs` fonctionne pareil mais avec un tableau d'arguments

```php
$crier->invokeArgs($magicien,
  					['arggghh...','...iyii...',...]);
```

Si la méthode n'est pas accessible on peut la rendre disponnible `setAccessible`

```php
$crier->setAccessible(true);
```

## Annotation

### syntaxe doc block

```php
/**
* @version 2.0
*/
```
### Trois fichiers + librairie addendum

```php
annotation -|
			|-addendum  // contient la librairie  
			|-index.php
			|-MyAnnotations.php
			|-Personnage.class.php
```

#### index.php

```php
require 'addendum/annotations.php';
require 'MyAnnotations.php';
require 'Personnage.class.php';

$reflectedClass = new ReflectionAnnotatedClass('Personnage');

echo $reflectedClass->getAnnotation('Table')->value;
echo print_r($reflectedClass->getAnnotation('Type')->value,true);

```

#### Personnage.class.php

```php
/**
* @Table('personnages')
* @TYPE({'brute','guerrier','magicien'})
*/

class Personnage
{

}
```

#### MyAnnotations.php

```php
class Table extends Annotation {}
class Type extends Annotation {}
```

utilisation de `getAnnotation('Type')` qui renvoie une annotation puis accès à l'attribut `value`

On peut imbriquer des tableaux et ajouter des clés

```php
/**
* @Table('personnages')
* @TYPE({'brute','guerrier',special={'dark','light','shadow'}})
*/
```

#### savoir si une classe a une annotation

```php
echo $reflectedClass->hasAnnotation('Tupe')?"oui<br>":"non<br>";
> non
```

#### Annotation avec plusieurs valeurs

MyAnnotations.php

```php
class Infos extends Annotation
{
  public $author;
  public $version;
}
```

Personnage.class.php

```php
/**

* @Infos(author='mimine',version='1.0')
*/

class Personnage
{
	...
}
```

index.php

```php
$infos =  $reflectedClass->getAnnotation('Infos');
echo $infos->author,'<br>';
echo $infos->version,'<br>';
```
#### Ajouter une contrainte sur les valeurs de l'annotation

MyAnnotations.php

```php
class Infos extends Annotation
{
  public $author;
  public $version;

  public function checkConstraints ($target) {
    if (!is_string($this->author)) {
      throw new Exception("l'auteur doit être une chaîne de caractère");
    }
    if (!is_numeric($this->version)) {
      throw new Exception("la version doit être une valeur numérique");
    }
  }
}
```

index.php

```php
try {
 	$reflectedClass = new 	ReflectionAnnotatedClass('Personnage');
} catch (Exception $exc) {
	echo "coucou :".$exc->getMessage();
}

```
#### Annotation de méthode et d'attribut

```php
$refAtt = new ReflectionAnnotatedProperty('Personnage','force');
$refMeth = new ReflectionAnnotatedMethod('Personnage','deplacer');

var_dump($reflectedAtt->getAnnotation('AttInf'));
print_r($reflectedMeth->getAnnotation('MethInf'));
print_r($reflectedMeth->getAllAnnotations());
```