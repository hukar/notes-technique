# 04 `async` et `await`

## Code synchrone

Pour effectuer des requêtes http on utilise le package `requests`:

```python
import requests

resp = requests.get(url)

print(resp.text)
```



Pour chronométrer:

```python
def main():
  t0 : datetime.datetime.now()
  my_process()
  dt = datetime.datetime.now() - t0
  print(f"Done in {dt.total_seconds():.2f} sec.")
```



## Code Complet Synchrone

```python
import datetime

import requests
import bs4 # rechercher des éléments dans du html
from colorama import Fore


def get_html(episode_number: int) -> str:
    print(Fore.YELLOW +
          f"Getting HTML for episode {episode_number}", flush=True)

    url = f'https://talkpython.fm/{episode_number}'
    resp = requests.get(url)
    resp.raise_for_status()

    return resp.text


def get_title(html: str, episode_number: int) -> str:
    print(Fore.CYAN +
          f"Getting TITLE for episode {episode_number}", flush=True)
    soup = bs4.BeautifulSoup(html, 'html.parser')
    header = soup.select_one('h1')
    if not header:
        return "MISSING"

    return header.text.strip()


def main():
    t0 = datetime.datetime.now()
    get_title_range()
    dt = datetime.datetime.now() - t0
    print(f"Done in {dt.total_seconds():.2f} sec.")


def get_title_range():
    # Please keep this range pretty small to not DDoS my site. ;)
    for n in range(270, 280):
        html = get_html(n)
        title = get_title(html, n)
        print(Fore.WHITE + f"Title found: {title}", flush=True)


if __name__ == '__main__':
    main()
```

Ce programme pour `10` requête mets :

```bash
Done in 4.55 sec.
```

C'est un peu plus de `4s`.



## `HTTPX` requête Asynchrone

```python
async def get_html(episode_number: int) -> str:
  url = f"https://talkpython.fm/{episode_number}"
  
  async with httpx.AsyncClient() as client:
    resp = await client.get(url)
    resp.raise_for_status()
    
    return resp.text
```

Pour exécuter les tâches 