# Exercice Vendeur de voiture

```erlang
-module(car2).

-export([listPrices/1]).

listPrices(Currency) ->
    CarList = getCarList(),
    printPrice(CarList, Currency).

printPrice([], Currency) ->
    true;
printPrice([Car | Rest], Currency) ->
    CarMap = getCarMap(),
    Price = maps:get(Car, CarMap),
    ConvertedPrice = convertPrice(Price, Currency),
    io:fwrite("Price " ++ integer_to_list(ConvertedPrice) ++ "\n"),
    printPrice(Rest, Currency).

convertPrice(Price, Currency) ->
    case Currency of
        eur -> round(Price * 0.9);
        gbp -> round(Price * 0.75);
        usd -> Price
    end.
getCarList() -> %% plutôt une ofnction qu'une variable ?
    ["bmw","laborghini","ferrari"].

getCarMap() ->
    #{"bmw"=>150000,"laborghini"=>500000,"ferrari"=>700000}.
```

`round(Float)`  transforme un décimal en entier

`io:fwrite`  n'accepte que des chaînes de caractère ou des listes.

`integer_to_list`  convertit un entier en liste pour pouvoir par exemple l'afficher avec `io:fwrite`.