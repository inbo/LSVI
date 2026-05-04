# Haalt vertaallijst op uit databank

Deze functie haalt een lijst uit de databank van de gebruikte schalen
(bv. Tansley-schaal en beheermonitoringschaal uit 2017) met telkens de
verschillende mogelijke categorieën en een overeenkomstige
bedekkingswaarde. De functie [`berekenLSVIbasis()`](berekenLSVIbasis.md)
gebruikt deze waarden om de nodige omzettingen te doen tussen deze
verschillende schalen

## Usage

``` r
geefVertaallijst(ConnectieLSVIhabitats)
```

## Arguments

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Dataframe met Naam, Waarde, Volgnummer, Omschrijving, Ondergrens,
Gemiddelde en Bovengrens. Telkens is een waarde tussen 0 en 1 opgegeven
die afkomstig is van het delen van het percentage door 100)
