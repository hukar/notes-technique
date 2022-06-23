# Pure Django API partie 2

#### Envoie de `JSON`

## Les 2 Endpoints

`urls.py`

```python
from django.urls import path
from .views import product_list, product_detail

urlpatterns = [
    path("products/", product_list, name="product-list"),
    path("products/<int:pk>", product_detail, name="product-detail"),
]
```

On retrouve la clé primaire `pk`

## Modification de `views.py`

### Pour envoyer tous les produits

```python
from django.http import JsonResponse
from .models import Product, Manufacturer


def product_list(request):
    products = Product.objects.all()  # [:30]
    data = {"products": list(products.values("pk", "name"))}
    response = JsonResponse(data)

    return response
```

On importe `JsonResponse`

Pour récupérer tous les produits : `Product.objects.all()`

Si on voulais les 30 premiers : `Product.objects.all()[:30]`

#### Seul les `dict` sont authorisés comme data pour `JsonResponse`

#### `JsonResponse(data<type:dict>)`

### Pour envoyer un seul produit

```python
def product_detail(request, pk):
    try:
        product = Product.objects.get(pk=pk)
        data = {"product": {
            "name": product.name,
            "manufacturer": product.manufacturer.name,
            "decription": product.description,
            "photo": product.photo.url,
            "price": product.price,
            "shipping_cost": product.shipping_cost,
            "quantity": product.quantity
        }
        }
        response = JsonResponse(data)
    except Product.DoesNotExist:
        response = JsonResponse({
            "error": {
                "code": 404,
                "message": "product not found"
            }
        }, status=404)

    return response
```

On utilise `try - except` pour gérer le cas où la clé n'existe pas et envoyer une erreur `404`

`Product.objects.get(pk=pk)` permet de récupérer un seul produit.

Dans ce cas on doit construire le `dict` *manuellement*

