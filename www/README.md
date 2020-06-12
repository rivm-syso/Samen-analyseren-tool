# RIVM-huisstijl-shiny
Dit bevat de bestanden die nodig zijn voor het toevoegen van de RIVM-huisstijl aan een shiny-tool. Een voorbeeld van het gebruik ervan is de [samen analyseren tool](https://rivm.shinyapps.io/samenanalyserentool/).

## Hoe te gebruiken?
Als je een shiny-tool hebt gemaakt met de driedeling: global.R, server.R en ui.R. Dan kan je aan de ui.R een html_template toevoegen, die van deze RIVM-huisstijl-shiny de huisstijl invult. Zie de [code](https://github.com/rivm-syso/Samen-analyseren-tool) van de samen analyseren tool voor voorbeeld.

Stappenplan:
- Kopieer uit de 'ui.R' de eerste regels tot de regel *" # Vanaf hier begint de tool zelf"*
- In deze regels worden de bestanden van github geladen en toegepast.
- LET OP: bekijk goed hoe het moet aansluiten met je eigen fluidpage
- Plak deze regels in je eigen 'ui.R'
- Zet het juiste template (template.html of template_wide.html)
- Verander de PageTitle
- Verander de tekst voor de verantwoording van je app

## NOTE
Deze huisstijlbestanden zijn nog tijdelijke bestanden die bij de pilot van shiny horen. Deze pilot duurt tot eind 2020. Daarna kunnen er weer zaken veranderen. Houdt deze github in de gaten.
