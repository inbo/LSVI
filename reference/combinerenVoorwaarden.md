# combineert de Status van voorwaarden via de opgegeven formule

Technische hulpfunctie die in een formule de `VoorwaardeID`'s vervangt
door opgegeven logische waarden en het resultaat van de formule
teruggeeft.

## Usage

``` r
combinerenVoorwaarden(Formule, VoorwaardeID, Status)
```

## Arguments

- Formule:

  string van `VoorwaardeID`'s gecombineerd met EN en OF, bijvoorbeeld
  "(720 EN 721) OF 15"

- VoorwaardeID:

  vector van alle `VoorwaardeID`'s die voorkomen in de `Formule`

- Status:

  vector met voor elke `VoorwaardeID` een overeenkomstige logische
  waarde status (TRUE of FALSE)

## Value

logische waarde (TRUE of FALSE) die de uitkomst van de `Formule` is
(gecombineerd met `VoorwaardeID` en `Status`)

## Examples

``` r
#onderstaand voorbeeld geeft problemen bij het testen van het package door
#devtools, maar buiten deze context werkt het wel
if (FALSE) { # \dontrun{
combinerenVoorwaarden(
  "(720 AND 721) OR 15",
  c(720, 721, 15),
  c(TRUE, FALSE, TRUE)
)
} # }
```
