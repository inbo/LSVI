# Hulpfunctie voor het uitvoeren van foutcontroles

Deze technische hulpfunctie bevat een standaardroutine om te controleren
of de door een gebruiker ingevoerde waarde(n) voorkomen in een
gespecifieerde tabel in de databank. Indien niet, dan geeft de functie
een informatieve error (welke waarde is fout, en welk zijn de mogelijke
invoerwaarden voor de variabele?).

## Usage

``` r
controleerInvoerwaarde(
  Beschrijving,
  Invoerwaarden,
  Tabelnaam,
  Veldnaam,
  ConnectieLSVIhabitats,
  Tolower = TRUE
)
```

## Arguments

- Beschrijving:

  Hoe de invoerwaarde beschreven moet worden in de error

- Invoerwaarden:

  De waarden die de gebruiker ingevoerd heeft

- Tabelnaam:

  De naam van de tabel waarin het veld zich bevindt (String)

- Veldnaam:

  De naam van het veld (in de bij Tabelnaam opgegeven tabel) waarvan de
  waarden moeten opgezocht worden (String)

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

- Tolower:

  default (als TRUE) wordt
  [`tolower()`](https://rdrr.io/r/base/chartr.html) uitgevoerd op de
  invoerwaarden en gegevens uit de databank vooraleer de vergelijking
  uitgevoerd wordt, FALSE zorgt dat dit niet uitgevoerd wordt, maar
  idealiter worden deze stap voor stap vervangen door `Tolower = TRUE`

## Value

Deze functie geeft geen waarde terug, maar gooit een error als er een
foute waarde ingevoerd is
