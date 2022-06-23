### Lancer une exception

```php
function addition ($a, $b) {
  if(!is_numeric($a) || !is_numeric($b))
    throw new Exception("coucou le sgars il faut deux chiffres");

  return $a + $b;
}

echo addition ("4",7);
echo addition ("michel",7);
```

On obtient le message d'erreur :

```php
Fatal error: 
Uncaught Exception:
coucou les gars il faut deux chiffres 
in ... on line 11
```

### Attraper une Exception

```php
try {
    echo addition ("4",7),'<br>';
    echo addition ("michel",7),'<br>';
  } catch ( Exception $exc) {
    echo "Exception attrapée :".$exc->getMessage()."<br>";
  }
```

Le script n'est pas bloqué, une instruction après le bloc Try and Catch sera lu normalement.
Par contre dans le bloc Try, aucune instruction ne sera lu après la ligne lançant l'exception.

### La classe Exception

```php
class Exception
{
  protected $message = 'exception inconnu'; // Message de l'exception.
  protected $code = 0; // Code de l'exception défini par l'utilisateur.
  protected $file; // Nom du fichier source de l'exception.
  protected $line; // Ligne de la source de l'exception.
  
  final function getMessage(); // Message de l'exception.
  final function getCode(); // Code de l'exception.
  final function getFile(); // Nom du fichier source.
  final function getLine(); // Ligne du fichier source.
  final function getTrace(); // Un tableau de backtrace().
  final function getTraceAsString(); // Chaîne formattée de trace.
  
  /* Remplacable */
  function __construct ($message = NULL, $code = 0);
  function __toString(); // Chaîne formatée pour l'affichage.
}
```

### créer son Exception

On peut redéfinir `__construct` et `__toString`

```php
class MonException extends Exception
{
  public function __construct ($message) {
    $this->message = $message;
  }

  public function __toString () {
    return $this->message;
  }
}
```

Du coup dans la fonction on envoie :

```php
if(!is_numeric($a) || !is_numeric($b))
    throw new MonException("coucou les gars il faut deux chiffres<br>");
```

Et dans le catch on récupère la bonne exception

```php
catch ( MonException $exc) {
	echo "Exception attrapée :".$exc."<br>";
}
```

Comme on a défini `__toString`, on peut directement écrire `echo $exc;`

### enchaînement de bloc catch

```php
try {
    echo addition ("4",7),'<br>';
    mechante();
    echo addition ("michel",7),'<br>';
    echo "salut<br>";
  } catch ( MonException $exc) {
    echo "Exception attrapée :".$exc."<br>";
  } catch (Exception $ex) {
    echo $ex->getMessage()." méchante<br>";
  }
```

Une fois l'exception attrapée, le script court-circuite le bloc try et va à la suite.

### Utilisation des exceptions prévues par le langage


* BadFunctionCallException
* BadMethodCallException
* DomainException
* InvalidArgumentException
* LengthException
* LogicException
* OutOfBoundsException
* OutOfRangeException
* OverflowException
* RangeException
* RuntimeException
* UnderflowException
* UnexpectedValueException

### Bloc finally

Ce sont des actions qui seront effectuées quoi qu'il arrive, nettoyage de variable (unset), fermeture de fichier ...

```php
  try {
    ...
  } catch ( InvalidArgumentException $exc) {
    echo "Exception attrapée :".$exc."<br>";
  } catch (Exception $ex) {
    echo $ex->getMessage()." méchante<br>";
  } finally {
    echo "je serais affichée quoi qu'il arrive!!<br>";
  }
```

### Convertir les erreurs en exception

Il faut donner une fonction à `set_error_handler`

```php
// d'abord créer sa classe d'exception
// héritant de ErrorException
class MonException extends ErrorException
{


  public function __toString () {
    switch ($this->severity) {
    	// ne foncionne pas avec E_ERROR 
    	// qu'on ne peut pas transformer en exception
      case E_USER_ERROR :
      $type = "K Erreur Fatale";
      break;
      case E_WARNING :
      case E_USER_WARNING :
      $type = "K Attention";
      break;
      case E_NOTICE :
      case E_USER_NOTICE :
      $type = "K Note";
      break;
      default :
      $type = "K Erreur Inconnue";
      break;
    }
    return '<strong>'.
    		$type.' : </strong> code : '.$this->code.
    		' fichier :'.$this->file.
    		' ligne : '.$this->line.
    		' message : '.$this->message.'<br>';
  }
}

function error2exception ($severity,$message,$fichier,$ligne) {
  throw new MonException($message,0,$severity,$fichier,$ligne);
}

set_error_handler('error2exception');
```

Et maintenant dans le code 

```php
try {
    ...

    echo $ty;
} catch (MonException $ex) {
   echo " mon exception : ".$ex."<br>";
}

echo $tylkjdu; 
```
on obtient ces messages :

```php 
// ici attrapée dans le try and catch
mon exception : K Note : 
code : 0 
fichier :/Users/hukar/Dropbox/htdocs/poo_openclassroom/index.php 
ligne : 72 
message : Undefined variable: ty

// ici sans try arrête le script
Fatal error: Uncaught <strong>K Note : </strong> 
code : 0 
fichier :/Users/hukar/Dropbox/htdocs/poo_openclassroom/index.php 
ligne : 77 
message : Undefined 
variable: tylkjdu<br> thrown in /Users/hukar/Dropbox/htdocs/poo_openclassroom/index.php on line 77
```

### Gestion des exceptions hors du try

`set_exception_handler` prend en paramètre une fonction permettant de personnaliser ses messages d'exception en dehors d'un bloc try

```php
function customException ($exc) {
  echo "mon message d'erreur custom <br> message : ".$exc->getMessage()."<br> ligne : ".$exc->getLine()."<br>fichier : ".$exc->getFile()."<br><strong>code : ".$exc->getCode()."</strong><br>";
}

set_exception_handler('customException');
```

Du coup le dernier message plus haut devient :

```php
// ici attrapée dans le try and catch
mon exception : K Note : 
code : 0 
fichier :/Users/hukar/Dropbox/htdocs/poo_openclassroom/index.php 
ligne : 72 
message : Undefined variable: ty

// ici sans try arrête le script
mon message d'erreur custom 
message : Undefined variable: tylkjdu
ligne : 82
fichier : /Users/hukar/Dropbox/htdocs/poo_openclassroom/index.php
code : 0
```