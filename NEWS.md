# LSVI 0.1.4

* hervorming van de afhandeling van soortenlijsten met gebruik van
GbifUsageKey en Rank in plaats van een volledige taxonomische boom,
invoer van GbifUsageKey als 'soort_gbif' wordt toegevoegd en
invoer van soort_NBN wordt afgebouwd, alsook functie `parseTaxonnaam()`
* extra output 'Soortenlijst' in functie `berekenLSVIbasis()` laat toe om te
controleren hoe de functie de ingevoerde soorten interpreteert
* veld habitattype.y verwijderd in output van tabellen van functie
`berekenLSVIbasis()`
* verwijdering van argument Taxonlijsttype in `geefSoortenlijst()` en
`geefInfoHabitatfiche()`
* toevoeging van functionaliteit om taxa in 1 laag te deselecteren in functie
deselecteerSoortenKenmerken() (issue #231)

# LSVI 0.1.3

* toevoeging van schaal 'BEHEERMONITORINGSCHAAL2021'

# LSVI 0.1.2

* toevoeging van regionaal belangrijke biotopen (rbb)
* toevoeging van habitattype 2190_a

# LSVI 0.1.1

* `NEWS.md` toegevoegd om aanpassingen in het package weer te geven.
* aanpassing in de installatie-instructies
