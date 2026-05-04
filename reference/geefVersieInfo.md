# Geeft informatie over de verschillende versies voor de berekening van de LSVI

Deze functie geeft een overzicht van alle versies die er zijn voor de
berekening van de de Lokale Staat van Instandhouding, met naast de
opsomming van de versies en de referenties een overzicht van de 2
kwaliteitsniveaus of types van beoordelingscriteria die in deze versie
gedefinieerd zijn.

## Usage

``` r
geefVersieInfo(ConnectieLSVIhabitats = NULL)
```

## Arguments

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Deze functie geeft de tabel Versie uit de databank.

## Examples

``` r
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
