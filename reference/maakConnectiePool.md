# Maak een connectiepool met de databank met LSVI-indicatoren in het package

Deze functie maakt een connectiepool met de in het package toegevoegde
databank met LSVI-indicatoren, wat nodig is om de functies te kunnen
gebruiken. Deze connectiepool moet eenmalig aangemaakt worden, en
functies zullen automatisch deze connectiepool gebruiken om te
connecteren met de databank. Alternatief is om een connectie aan te
maken met de functie [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md)
en deze bij elke functie mee te geven.

## Usage

``` r
maakConnectiePool()
```

## Value

Deze functie maakt een Environment-object aan dat de connecties regelt
met de betreffende databank.

## Examples

``` r
library(LSVI)
maakConnectiePool()
geefVersieInfo()
#>   VersieLSVI               Referentie
#> 1 Versie 2.0    T'Jollyn et al., 2009
#> 2   Versie 3 Oosterlynck et al., 2016
#> 3     RBB v1      De Bie et al., 2018
#>                                                                                                  Beschrijving
#> 1 gedegradeerde staat (C) is als de staat minder goed is dan de voorwaarde opgegeven onder kwaliteitsniveau 1
#> 2                                                                                                        <NA>
#> 3                                                                                                        <NA>
#>     Kwaliteitsniveau1 Kwaliteitsniveau2
#> 1 Voldoende staat (B)   Goede staat (A)
#> 2      Gunstige staat      Streefwaarde
#> 3 niet van toepassing      Streefwaarde
library(pool)
poolClose(ConnectiePool)
```
