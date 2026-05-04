# Connecteer met de databank met LSVI-indicatoren in het package

Deze functie maakt een connectie met de in het package toegevoegde
databank met LSVI-indicatoren, wat nodig is om de functies te kunnen
gebruiken. Deze connectie moet als argument meegegeven worden bij elke
functie functie die informatie uit de databank ophaalt. Alternatief is
om eenmalig een connectiepool aan te maken met de functie
[`maakConnectiePool()`](maakConnectiePool.md).

## Usage

``` r
connecteerMetLSVIdb()
```

## Value

Deze functie geeft een open `odbc`-connectie naar de SQLite-databank in
de installatie-file van het package.

## Examples

``` r
library(LSVI)
ConnectieLSVIhabitats <- connecteerMetLSVIdb()
geefVersieInfo(ConnectieLSVIhabitats)
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
library(DBI)
dbGetQuery(ConnectieLSVIhabitats,
           "SELECT VersieLSVI, Referentie FROM Versie")
#>   VersieLSVI               Referentie
#> 1 Versie 2.0    T'Jollyn et al., 2009
#> 2   Versie 3 Oosterlynck et al., 2016
#> 3     RBB v1      De Bie et al., 2018
dbDisconnect(ConnectieLSVIhabitats)
```
