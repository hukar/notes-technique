# reflection API

## obtenir le nom de la classe

Directement sur une classe : ```.class```

```java
Class reflectClass = DarkSheep.class;
		
String className = reflectClass.getName();

System.out.println(className);
```

```shell
be.hukar.reflection.DarkSheep
```

Autre méthode sur un objet ```objet.getClass()``` :

```java
Book livre = new Book("le petit Nicolas");
		
Class<?> cl = livre.getClass();

System.out.println(cl);
System.out.println(cl.getSimpleName());
System.out.println(cl.getName());
```

```shell
class be.hukar.reflection.Book
Book
be.hukar.reflection.Book
```





## Vérifier les mots clés : modifier

```java
import java.lang.reflect.Modifier;

int classModifier = reflectClass.getModifiers();
			
//isAbstract, isFinal, isInterface, isPrivate, isProtected, 
// isStatic, isStrict, isSynchronized, isVolatile
System.out.println(
    Modifier.isPublic(classModifier) + "  "
    + Modifier.isFinal(classModifier) + "  " 
    + Modifier.isInterface(classModifier) + "  ");
```

```shell
true  true  false 
```

## Obtenir les interfaces

```java
Class[] interfaces = reflectClass.getInterfaces();
		
for(Class i:interfaces) {
    System.out.println(i.getName());
}
```

```shell
java.lang.Comparable
java.io.Serializable
```

## Retrouver la classe mère

```java
Class classSuper = reflectClass.getSuperclass();
		
System.out.println(classSuper.getName());
```

```shell
be.hukar.reflection.SoulSheep
```

## Lister les méthodes

```java
import java.lang.reflect.Method;

Method[] classMethods = reflectClass.getMethods();
		
for(Method m:classMethods) {
    System.out.println("Method name " + m.getName());

    if(m.getName().startsWith("get")) {
        System.out.println("Getter Method");
    } else if(m.getName().startsWith("set")) {
        System.out.println("Setter Method");
    }

    System.out.println("return type " + m.getReturnType());

    Class[] parametersType = m.getParameterTypes();
    System.out.println("parameters");
    for(Class pt:parametersType) {
        System.out.println(pt.getName());
    }

    System.out.println();
}
```

```shell
Method name : compareTo
return type : int
parameters :
bu.hukar.reflection.DarkSheep

Method name : darkAttack
return type : void
parameters

Method name : getName
Getter Method
return type : class java.lang.String
parameters : ...
```

##Récupérer et utiliser le constructeur :

```java
Constructor constructor = null;
		
Object constructor2 = null;

try {
    constructor = reflectClass.getConstructor(new Class[] {SoulSheep.class});

    constructor2 = reflectClass.getConstructor(String.class, int.class).newInstance("rico", 23);
} catch (NoSuchMethodException | SecurityException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
} catch (InstantiationException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
} catch (IllegalAccessException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
} catch (IllegalArgumentException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
} catch (InvocationTargetException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
}

System.out.println("constructor : ");
```

```shell
constructor : 
parameter : bu.hukar.reflection.SoulSheep
```

#### Récupérer les constructeurs

```java
Constructor<?>[] constructors = cl.getConstructors();
		
System.out.println(constructors.length);
```

```shell
2
```

Instancier un objet : ```newInstance(param1, param2, ...)```

```java
Object myObj = reflectClass.getConstructor(String.class, int.class).newInstance("rico", 23);
```

####Instanciation dynamique :

```java
// instanciation dynamique
Constructor<?> constructor = constructors[1];
Object bob = null;

try {
    bob = constructor.newInstance("les aventure de tintin");
} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {

    e.printStackTrace();
}

System.out.println(bob);
```

exemple plus complexe :

```java
DarkSheep newDarkSheep = null;
SoulSheep soulSheep = null;

try {
    soulSheep = SoulSheep.class.getConstructor(String.class, int.class).newInstance("toto", 33);
} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e1) {

	e1.printStackTrace();
}

try {
	newDarkSheep = (DarkSheep)constructor.newInstance(soulSheep);
} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {

	e.printStackTrace();
}

System.out.println("newDarkSheep : " + newDarkSheep.getName());
```

```shell
[name:toto,age:33]
```

## Afficher un champ privé

```java
import java.lang.reflect.Field;

Field privateAutor = null;

DarkSheep dsp = new DarkSheep(soulSheep);
String valueOfAutor = "";


try {
	
	String privateAttribut = "autor";
	privateAutor = DarkSheep.class.getDeclaredField(privateAttribut);
	
	privateAutor.setAccessible(true);
	
	valueOfAutor = (String)privateAutor.get(dsp);
} catch (NoSuchFieldException | SecurityException e) {
	// TODO Auto-generated catch block
	e.printStackTrace();
} catch (IllegalArgumentException e) {
	// TODO Auto-generated catch block
	e.printStackTrace();
} catch (IllegalAccessException e) {
	// TODO Auto-generated catch block
	e.printStackTrace();
}

System.out.println("private autor : " + privateAutor);
System.out.println("value of autor : " + valueOfAutor);
```

```shell
private autor : private static final java.lang.String bu.hukar.reflection.DarkSheep.autor
value of autor : hukar
```

## Invoquer une méthode privée

```java
String methodName = "getPrivate";

try {
	Method privateMethod = DarkSheep.class.getDeclaredMethod(methodName, null);
	
	privateMethod.setAccessible(true);
	
	String privateReturnVal = (String)privateMethod.invoke(dsp, null);
	
	System.out.println(privateReturnVal);
	
	// with parameters
	
	Class[] methodParameters = new Class[] {Integer.TYPE, String.class};
	
	Object[] params = new Object[] {new Integer(10), new String("random")};
	
	privateMethod = DarkSheep.class.getDeclaredMethod("getOtherPrivate", methodParameters);
	privateMethod.setAccessible(true);
	
	privateReturnVal = (String)privateMethod.invoke(dsp, params);
	
	System.out.println(privateReturnVal);
	
} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
	// TODO Auto-generated catch block
	e.printStackTrace();
}
```

```shell
hey ! I'm private!!
How did you get here :10 random
```

