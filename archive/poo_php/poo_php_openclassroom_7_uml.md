## UML convention d'écriture

`<<leaf>>` final

_methode_ italique = abstract

<u>methode</u> méthode statique

* \# protected
* \- private
* \+ public

* \- _attribut: type
* \+ method(arg:type,arg:type):type retour

#### Héritage
[Mere]<|--[fille]

#### Interface
[Class]- - -|>[Interface]

#### Association
[NewsManager]<sup>1</sup>---<sup>*</sup>[News]

#### Agrégation
[Voiture]<>--<sup>4</sup>[Roue]

Si la voiture est détruite les roues ne sont pas détruite

#### Composition

[Voiture]__<+>__--<sup>4</sup>[Roue]

Si la voiture set détruite, les roues sont aussi détruite

## Design pattern

### Pattern Factory

Permet de ne plus utiliser `new` directement dans le code.

```php
class PDOFactory {

  public static function getMysqlConnexionDev() {
    try{
      $db = new \PDO('mysql:host=localhost;dbname=wf_dbc;charset=utf8', 'root', 'root');
      $db->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
      return $db;
      
    } catch(\PDOException $e) {
      echo $e->getMessage();

    }
    return null;
  }
  
  public static function getMysqlConnexionProd() {
    try{
      $db = new \PDO('mysql:host=localhost;dbname=wf_dbc;charset=utf8', 'root', 'root');
      $db->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
      return $db;
      
    } catch(\PDOException $e) {
      echo $e->getMessage();

    }
    return null;
  }
}

$pdo = PDOFactory::getMysqlConnexionDev();
```

### Observer

Observer les changement d'un objet

Implémente deux interface Spl :
* SplObserver
* SplSubject

```php
class Observee implements SplSubject
{


  protected $observers = [];

  protected $nom;

  public function attach(SplObserver $observer) {
    $this->observers[] = $observer;
    return $this;
  }

  public function detach (SplObserver $observer) {
    if (is_int($key = array_search($observer,$this->obeservers,true))) {
      unset($this->observers[$key]);
    }
  }

  public function notify () {
    foreach ($this->observers as $observer) {
      $observer->update($this);
    }
  }

  public function getNom () {
    return $this->nom;
  }

  public function setNom ($nom) {
    $this->nom = $nom;
    $this->notify();
  }
}
```

`array_search($motif,$tableau,$strict:bool)`
si `$strict` vaut true alors le type doit être le même

```php
class Observer1 implements SplObserver
{

  public function update (SplSubject $observee) {
    echo "<strong>Observer 1 a été notifier : </strong>".$observee->getNom()." a changé de nom<br>";
  }

}

idem pour Observer2
```

Dans le code 

```php
$subject = new Observee;

$subject->attach(new Observer1)
        ->attach(new Observer2);

$subject->setNom("michel");

$subject->setNom("andrew");

> Observer 1 a été notifier : michel a changé de nom
> Observer 2 a été notifier : michel a changé de nom
> Observer 1 a été notifier : andrew a changé de nom
> Observer 2 a été notifier : andrew a changé de nom
```