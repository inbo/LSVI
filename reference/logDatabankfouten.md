# Lijst alle nog op te lossen databankfouten op

Deze functie maakt een tabel met alle problemen die nog in de databank
zitten. Enerzijds is er een beperkte tabel met problemen die op een
hoger niveau opgelost kunnen worden en anderzijds een detail met alle
Voorwaarden waar nog een fout in zit. Problemen die op beide niveaus
kunnen opgelost worden (bv. benoemen van analysevariabelen), staan op
beide niveaus vermeld.

## Usage

``` r
logDatabankfouten(ConnectieLSVIhabitats = NULL)
```

## Arguments

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Deze functie geeft een list met 2 dataframes terug
