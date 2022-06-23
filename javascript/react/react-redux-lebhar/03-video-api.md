# L'api de `The movie db`

le site https://www.themoviedb.org/  propose une `API` gratuite en lien avec le cinema.

## Les clés

### V3 auth

````
85ea758eb55ba2b673d727893d5de70d
````

exemple de requête :

````
https://api.themoviedb.org/3/movie/550?api_key=85ea758eb55ba2b673d727893d5de70d
````

### V4 auth

````
eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiI4NWVhNzU4ZWI1NWJhMmI2NzNkNzI3ODkzZDVkZTcwZCIsInN1YiI6IjVkOTYwMmU5ZWE0MjYzMDAyYTk1MDBmNSIsInNjb3BlcyI6WyJhcGlfcmVhZCJdLCJ2ZXJzaW9uIjoxfQ.Xzf7pgpGnoyuxGe9Qdaef2upbZFEu9sySnuNd0Ai15M
````

The Movie DB : URL

#### URL TheMovieDB

**Les films les plus populaires du moment en français.**

https://api.themoviedb.org/3/discover/movie?language=fr&sort_by=popularity.desc&include_adult=false&api_key=votreClé

**Recommendation de films pour un id de film particulier en français**

https://api.themoviedb.org/3/movie/idDeFilm/recommendations?api_key=votreClé&language=fr&include_adult=false

**Rechercher un film par son titre en français**

https://api.themoviedb.org/3/search/movie?api_key=votreClé&query=Un+Titre+De+film&language=fr&include_adult=false

**Rechercher des url de video pour un id de film en particulier (en anglais)**

Note : si vous appliquez un filtre avec language=fr vous aurez malheureusement moins de résultats qu'en anglais.

https://api.themoviedb.org/3/movie/[id]?api_key=votreClé&append_to_response=videos&include_adult=false

**Définition des constantes**

const **API_END_POINT** = "https://api.themoviedb.org/3/"
const **POPULAR_MOVIES_URL** = "discover/movie?language=fr&sort_by=popularity.desc&include_adult=false&append_to_response=images"
const **API_KEY** = "api_key=[votreClé]"