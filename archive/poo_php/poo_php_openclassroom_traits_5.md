### Simple et double quotes

#### Simple quote

Les caractère spéciaux et le variables ne sont pas interprétés.

#### double quotes

Oui.

#### caractères spéciaux utiles
 carctère | utilisation 
---|---
\n	|Fin de ligne (LF ou 0x0A (10) en ASCII)
\r	|Retour à la ligne (CR ou 0x0D (13) en ASCII)
\t	|Tabulation horizontale (HT or 0x09 (9) en ASCII)
\v	|Tabulation verticale (VT ou 0x0B (11) en ASCII)
\e	|échappement (ESC or 0x1B (27) en ASCII)
\f	|Saut de page

### nl2br($text)

insère des `<br>` à chaque saut de ligne du texte `$text`

### file_put_contents

Revient à appeler les fonctions `fopen()`, `fwrite()` et `fclose()` successivement.

```php
file_put_contents('monFichier.txt',$text);
```

crée le fichier s'il nexiste pas.

### mail

`bool mail ( string $to , string $subject , string $message)` 

### Mon premier trait

```php
trait MaCollection
{
  private function francky () {
    echo "je suis un trait ",$this->nom,'<br>';
  }
}

class A
{
  private $nom = "michel";

  use MaCollection;

  public function parle () {
    $this->francky();
  }
}

class B
{
  private $nom = "marcel";

  public function parle () {
    $this->francky();
  }
	// on peut l'appeler après sans problème
  use MaCollection;
}
```

Le `$this` fait référence à chaque objet créé

```php
$a = new A;
$b = new B;

$a->parle();

$b->parle();
> je suis un trait michel
> je suis un trait marcel
```

### Utilisation de plusieurs traits

```php
trait MaCollection
{
  private function format ($text) {
    $text = '<p> Date du jour : '.date('d/m/Y').'</p><p> Texte : '.nl2br($text).'</p>';
    return $text;
  }
}

trait PowerPlus
{
  private function generateText () {
    $text = "";
    $max = rand(156,789);
    for ($i = 0;$i < $max;++$i) {
      $sp = rand(1,5);
      if ($sp == 1) $text .=" ";
      $text .= chr(rand(97,122));
    }
    return $text;
  }
}

class WriteFile
{
  use MaCollection,PowerPlus;

  public function write ($text) {
    file_put_contents('mimicracra.txt',$this->format($this->generateText()));
  }
}
```

`rand($min,$max)` renvoi un entier  

`str char(int)` renvoi le carctère corespondant au code ascii  
`int ord(str)` fait l'inverse

### priorité des traits

```php
trait MaCollection
{
  private function cornflacke () {
    echo "j'aime les cornflackes";
  }
}

trait PowerPlus
{
  private function cornflacke () {
    echo "j'aime pas du tout les cornflackes";
  }
}

class WriteFile
{
  use MaCollection,PowerPlus
  {
    PowerPlus::cornflacke insteadof MaCollection;
  }

  public function parler () {
    $this->cornflacke();
  }
}
```

La méthode de classe a encore une priorité supérieur

```php
class WriteFile
{
  use MaCollection,PowerPlus
  {
    PowerPlus::cornflacke insteadof MaCollection;
  }
  // ce sera celle-ci qui sera utilisée
  private function cornflacke () {
    echo "rien à voire!!";
  }

  public function parler () {
    $this->cornflacke();
  }
}
```

Par contre le Trait est plus fort que la classe Mère

### Modifier la visibilité d'une méthode

```php
trait MaCollection
{
  private function bip () {
    echo "BIP!!";
  }
}

class Testy
{
  use MaCollection
  {
    // sous entendu public bip
    bip as public;
  }
}

$wf = new Testy;
$wf->bip(); // fonctionne 
```

`as` permet du créer un alias du nom de la méthode

```php
class Testy
{
  use MaCollection
  {
    // ici on rend la méthode privée et on crée un alias bop
    bip as private bop;
  }
}
```

### méthode abstraite dans les traits

```php
trait MaCollection
{
  abstract public function bip();
}

class Testy
{
  use MaCollection;

  public function bip () {
    echo "BIP!!";
  }
}
```
Obligation de définir les méthodes abstraites par les classes utilisant le trait.