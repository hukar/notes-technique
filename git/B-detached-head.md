# Annexe B Detached HEAD

## Procédure de secours au cas où il y a un commit *"détaché"*

```bash
git checkout <copied hash of required commit from the list>
git checkout -b <new branch name>
git checkout master
git merge <branch name>
```

On fusionne les deux branches.