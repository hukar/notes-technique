# Absolute Path Python

Fonction pour résoudre un chemin relatif en chemin absolu :

```python

import os

chemin = "/../Users//Thibh/Desktop//../Dossier_01/..///Dossier_02/Udemy"

# chemin="/../../titi/../toto"

def sanitize(path):
    path_tab = path.split("/")

    print(path_tab)

    cpt = 0
    for i, level in enumerate(path_tab[1:], 1):
        #print(i, level)
        if level == "":
            #print(i)
            del path_tab[i-cpt]
            cpt += 1
    
    return "/".join(path_tab)

def absolute(path, sanitize_token = True):

    print("sanitize_token", sanitize_token)

    if sanitize_token:
        print("sanitize")
        path = sanitize(path)

    path_tab = path.split("/")

    if ".." not in path_tab:
        return  "/".join(path_tab)
    else:
        if len(path_tab) == 2:
            return "/"
        else:
            index = path_tab.index("..")
            path_tab.remove("..")
            if index-1 > 0:
                path_tab.remove(path_tab[index-1])
            #print(path_tab)
            new_path = "/".join(path_tab)
            #print(new_path)
            return absolute(new_path, False)

# print(sanitize(chemin))
print(absolute(chemin))
print(os.path.normpath(chemin))

```

