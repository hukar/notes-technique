## utilisation de la reflection pour invoquer la méthode de la mère sur la fille

```pb.getClass().getSuperclass().getMethod("messageBot", null).invoke(pb.getClass().getSuperclass().newInstance(),null);```

Et maintenat dans le code :



```java
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

ProtoBot pb;
pb = new XenoBot();

pb.messageBot();

Class<?> xenobot =  pb.getClass();

// Class protobot = xenobot.getSuperclass();
Class protobot = pb.getClass().getSuperclass();
System.out.println(protobot.getName());

try {
    // Method methodProtobot = protobot.getMethod("messageBot", null);
    Method methodProtobot = pb.getClass().getSuperclass().getMethod("messageBot", null);
    System.out.println("messageBot de proto");

    // on passe un object de la classe ProtoBot => protobot.newInstance()
    // methodProtobot.invoke(protobot.newInstance(),null);
    pb.getClass().getSuperclass().getMethod("messageBot", null).invoke(pb.getClass().getSuperclass().newInstance(),null);
} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | InstantiationException e) {
    // TODO Auto-generated catch block
    e.printStackTrace();
}
```

```shell
xeno : message bot
proto : message bot
```

