#Java EE

### où placer les .jsp

>fichier jsp -> dans le dossier WebContent


### Éléments JSP

element|syntax
---|---
JSP expression|```<%= expression en java %>```
JSP scriptlet|```<% code java d'une ou plusieurs lignes %>```
JSP declaration|```<%! déclaration de variable ou de méthode %>```

Dans une __JSP expression__ la méthode ```toString()``` est appelé automatiquement.

###Pour écrire dans un scriptlet : ```out.println()```

```java
<%
	for(int i = 0; i < 12; ++i) {
		out.println(i + "<br />");
	}
%>
```

###JSP declaration

```java
<%!
	String bonjour(){ 
		return "Bonjour";
	};
%>

<%= bonjour() %>
```

### Les Objets Intégrés au Serveur

Object|description
---|---
request| Contient l'en-tête de la requête HTTP ainsi que le contenu du formulaire
response|fournis le support HTTP pour l'envoie de la réponse
out|JspWriter pour inclure du contenu HTML dans la page
session|une session unique pour chaque utilisateur du site
application|des données partagées pour tous les utilisateurs du site

### Inclure des pages

```java
<body>
	<jsp:include page="header.jsp" />
	
	<jsp:include page="footer.html" />
</body>
```

### Récupérer les données de formulaire

Formulaire HTML :

```html
<form action="built-in.jsp" method="get">
		
	<input type="text" name="firstName"/> <br />
	
	<button>Send form</button>
</form>
```

Côté de la page JSP

```java
<div>

	<p>first name : <%= request.getParameter("firstName") %></p>
</div>
```

Syntaxe alternative ```${param.nameOfMyParam}```

```java
<p>first name : <%= request.getParameter("firstname") %></p>
<p>last name : ${param.lastname}</p>
```

Récupérer des données mulitiples ```request.getParameterValues("paramName")``` :

```java
<%
    String[] myFruits = request.getParameterValues("fruits");

    if(myFruits == null) {
    myFruits = new String[]{"I hate fruits !"};
    }

    for(String f:myFruits) {
    out.println("<li>" + f + "</li>");
    }
%>
```

### regler l'encodage

```java
<%@ page language="java" pageEncoding="UTF-8"%>

<!doctype html>
<html lang="fr">
...
```

###Session

Permet d'enregistrer un objet pour la session :
```java
session.setAttribute(String name, Object myObject);
```

La méthode pour récupérer l'objet en session :

```java
Object session.getAttribute(String name)
```
Exemple complet avec un ```ArrayList<String>```

```java
List<String> list;

if(session.getAttribute("todolist") == null) {
	list = new ArrayList<>();
} else {
	list = (List<String>)session.getAttribute("todolist");
}
```

Autres méthodes utiles :

Méthode|Description
---|---
isNew() Boolean|renvoie true si la session est nouvelle
invalidate(): void|invalide la session et détache l'objet associé

### Cookies



### JSTL

import :

```<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>```



Une boucle ```for-each```

```java
<c:forEach items="${STUDENT_LIST}" var="student">
	<tr class="table-${student.color()}">
		<td>${student.id}</td>
		<td>${student.firstName}</td>
		<td>${student.lastName}</td>
		<td>${student.email}</td>
		<td><a href="update-student-form.jsp?idStudent=${student.id}">Update</a></td>
	</tr>		
</c:forEach>		
```

Un tag pour les liens :

```java
<!--  set up a link for each student -->
<c:url var="tempLink" value="StudentControllerservlet">
	<c:param name="command" value="LOAD" />
    <c:param name="sudentId" value="${student.id}" />
</c:url>

<td><a href="${tempLink}">Update</a></td>
<!-- ce qui compile en : -->
<a href="StudentControllerservlet?command=LOAD&sudentId=6">Update</a>
```

