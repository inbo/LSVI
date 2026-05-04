# Invoercontrole voor waarde versie

Om te vermijden dat we meermaals dezelfde invoercontrole moeten
uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor
elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.
Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando
gerund worden (maar worden wel gerund als de functie waarin ze
voorkomen, aangeroepen wordt).

## Usage

``` r
invoercontroleVersie(Versie, ConnectieLSVIhabitats)
```

## Arguments

- Versie:

  Waarde waarop invoercontrole moet gebeuren.

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).
