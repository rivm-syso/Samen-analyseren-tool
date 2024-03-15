NB: De nieuwste versie van de tool kun je vinden in een nieuw [repo](https://github.com/rivm-syso/Analyse-Together)

We hebben de opzet veranderd en zijn gaan werken met modules, vandaar dat we een nieuw repository zijn gestart.

Inhoudsopgave
-------------

-   [Achtergrond](#achtergrond)
-   [Documentatie](#documentatie-samen-analyseren-tool)
-   [Gebruik](#gebruik)

Achtergrond
===========

Introductie
-----------

Steeds meer burgers, bedrijven en overheden meten de luchtkwaliteit met
sensoren. Het [Samen Meten programma](samenmeten.nl) van het
[RIVM](www.rivm.nl) Rijksinstituut voor Volksgezondheid en Milieu
ondersteunt deze ontwikkeling. Zo onderzoeken we de werking en
toepassing van deze sensoren, laten we de data zien in een
[dataportaal](www.samenmeten.rivm.nl), en proberen we partijen bij
elkaar te brengen. We werken zo toe naar een meetnet waaraan iedereen
kan bijdragen.

Op het Samen Meten dataportaal kun je de gemeten sensorwaarden
gemakkelijk terugvinden. Naast een actueel beeld van alle sensoren zie je hier ook hoe de luchtkwaliteit varieerde
over de afgelopen zeven dagen. Maar vaak wil je meer weten dan alleen
hoe de luchtkwaliteit verliep over de afgelopen dagen. Je wilt misschien
weten of de luchtkwaliteit doordeweeks slechter is dan in het weekend,
of hoe de luchtkwaliteit afhangt van de windrichting, enzovoorts. Om
deze vragen te beantwoorden moet je de sensordata analyseren. Deze
analyse is niet altijd eenvoudig. Steeds vaker klinkt daarom de vraag of
er geen ondersteuning geboden kan worden bij deze analyse. De “Samen
Analyseren” tool is hiervoor ontwikkeld: een interactieve tool voor
eenvoudige datavisualisatie. Het gaat hierbij dus niet alleen over de
real-time waardes, maar ook over de samenhang en de interpretatie. Zo
zijn groepsgemiddelden bijvoorbeeld betrouwbaarder dan de waarden van individuele
sensoren, en daarom is het in de tool ook mogelijk om groepsgemiddelden gemakkelijk
te bepalen. De huidige tool is een prototype, er wordt nog volop aan
gewerkt. De tool is uitgeprobeerd in het project [Hollandse
Luchten](https://hollandseluchten.waag.org/), waar het enthousiast is
ontvangen.

Deze versie van de Samen Analyseren Tool bestaat uit 2 gedeeltes, die in 2 verschillende tabbladen zijn onderscheiden:
- Data downloaden
Hier kan de sensordata van alle sensoren in een gemeente of in een aangegeven project worden gedownload voor een 
zelf-in-te-stellen periode. Daarna kunnen ook de gegevens van de officiele luchtmeetnetstations van luchtmeetnet worden gedownload en 
de windgegevens van KNMI-stations. Deze gegevens worden direct getoond in de tool, maar kun je ook opslaan.
Het ophalen van de gegevens kan enkele minuten tot uur duren, het is verstandig om de gegevens op te slaan. Er is namelijk
de mogelijkheid om een bestand in te laden. Het format is een .csv bestand, zie [Input data](#input-data) voor de noodzakelijke opbouw.
- Data bekijken
Hier zijn verschillende visualisatie mogelijk voor de sensordata al dan niet in combinatie met de gegevens van
het luchtmeetnet en het KNMI. 

Hieronder volgt een
beschrijving van de hoofdelementen van de tool. Algemene vragen en
opmerkingen kan je stellen via
[samenmeten](https://www.samenmetenaanluchtkwaliteit.nl/contact).
Meldingen van errors of opmerkingen over de code graag via een *Issue*
op GitHub zelf, je moet daarvoor wel inloggen op GitHub.

Documentatie Samen Analyseren Tool
==================================

Inhoudsopgave
-------------

-   [Kennis vooraf](#kennis-vooraf)
-   [Run de tool](#run-de-tool)
-   [Opbouw en structuur](#opbouw-en-structuur)
-   [Input data](#input-data)
-   [global.R](#globalr)
-   [ui.R en server.R](#uir-en-serverr)
-   [ui.R](#uir)
-   [server.R](#serverr)
    -   [Het opzetten van een interactief dataframe](#het-opzetten-van-een-interactief-dataframe)
    -   [Het maken van de kaart](#het-maken-van-de-kaart)
    -   [Het downloaden van data](#het-downloaden-van-data)
    -   [Het maken van tabellen](#het-maken-van-tabellen)
    -   [Het maken van grafieken](#het-maken-van-grafieken)
    -   [Overzicht van de functies](#overzicht-van-de-functies)
    -   [Overzicht van de ObserveEvents](#overzicht-van-de-observeevents)
-   [Nawoord](#nawoord)

Kennis vooraf
-------------

De scripts staan hier op GitHub.
[GitHub](https://guides.github.com/activities/hello-world/) is een zeer
geschikt platform om software te delen en met verschillende partijen te
ontwikkelen. Als je een account hebt kun je ook bijdragen aan deze tool.
Gebruik hiervoor ‘fork’ en ‘Pull request’, meer info vind je in de
[handleiding](https://help.github.com/en/github/getting-started-with-github/fork-a-repo)
van GitHub. Met een account kan je ook bugs of verbeteringen melden via
de 'Issues'.  Als je geen account hebt, kun je de bestanden downloaden
en op je eigen pc aanpassingen maken.

De Samen analyseren tool is gemaakt in de programmeertaal
[R](https://www.r-project.org/). Voor het gebruik van de tool via de
interface ([hier](https://rivm.shinyapps.io/samenanalyserentool))
is geen verstand van R nodig. Maar gezien je al hier bent, wil je
waarschijnlijk zelf aan de slag. Meer informatie over programmeren in R
en het downloaden van RStudio kan je op de [website van RStudio](https://rstudio.com/) vinden.

Het is een interactieve tool, de interactie wordt gegenereerd via Shiny.
[Shiny](https://shiny.rstudio.com/) is een package in R die interactieve
webapps faciliteert. We gaan enkele gebruikte functies toelichten.

Voor het maken van de kaart en de markers op de kaart is leaflet
gebruikt. Ook voor leaflet kun je een uitgebreide handleiding met
voorbeelden online vinden
([leaflet](https://rstudio.github.io/leaflet/)).

Voor de combinatie van shiny en leaflet bekijk ook deze
[uitleg](https://rstudio.github.io/leaflet/shiny.html).

Ten slotte zijn er nog de verschillende visualisaties/grafieken. Deze
worden gemaakt via het package
[OpenAir](http://davidcarslaw.github.io/openair/). OpenAir is speciaal
ontwikkeld voor het onderzoeken naar luchtkwaliteit. We hebben een
selectie van de visualisaties uit dit pakket gebruikt. Er is nog veel
meer mogelijk, mocht je andere visualisaties zoeken. Het toevoegen van
andere visualisaties is relatief simpel. Voor alle opties, voorbeelden
en uitleg kijk [hier](https://davidcarslaw.com/files/openairmanual.pdf).

Run de tool
-----------

Laten we eerst de huidige tool op je eigen pc draaien! Start daarvoor
RStudio, maak een nieuw project aan en zet daar alle bestanden van
deze GitHub in. Dat kan je doen door de bestanden te downloaden (zie
groene 'Clone or download'-knop). Als je al ervaring hebt met Git, kan 
je ook  een clone maken. 

Voor deze tool heb je ook een aantal packages nodig, waaronder
openair, leaflet, leaflet.extras, dplyr, shinythemes, shinyWidgets,
purrr, sp en devtools.  Deze packages kan je installeren via
de package manager van RStudio of via het `install.packages`
commando. Naast deze packages heb je ook het geoshaper package en de samanapir package nodig,
deze moet je vanaf GitHub installeren.  Je kan dit pakket vinden in de
[RedOakStrategic/geoshaper](https://github.com/RedOakStrategic/geoshaper)
repository en de [samanapir](https://github.com/rivm-syso/samanapir). 
Installeren doe je met de volgende R commando's:

```
library(devtools)
install_github("RedOakStrategic/geoshaper")
install_github("https://github.com/rivm-syso/samanapir")
```

Open het script global.R. Rechtsboven bevindt zich een groen driekhoekje
met de tekst ‘Run app’. Klik hierop en tool die wordt geopend. Mooi zo,
nu heb je de tool werkend.

Hieronder volgt een beschrijving van de verschillende onderdelen. Ook
lichten we enkele stappen en functies toe. Aan het eind heb je inzicht
in de opbouw en de gebruikte functionaliteiten.

Opbouw en structuur
-------------------

De app bestaat uit 4 hoofdgedeeltes:

-   input data
-   global.R
-   server.R
-   ui.R

### Input data

Er zijn 3 verschillende mogelijkheden om gegevens in de tool te bekijken. Voor alle drie geldt
dat er aparte bestanden voor sensordata, luchtmeetnetdata en knmi-data zijn.

- Voorbeelddata: er zit voor een korte periode data in de tool. Deze kan met de button *laad voorbeeld data* worden geladen.
Met deze data kan je de verschillende visualisaties uit proberen. Ook kan je deze data als csv-bestand downloaden.

- Data downloaden: je kan de sensordata van alle sensoren in een gemeente of in een aangegeven project downloaden voor een 
zelf-in-te-stellen periode. Daarna kunnen ook de gegevens van de officiele luchtmeetnetstations van luchtmeetnet worden gedownload en 
de windgegevens van KNMI-stations. Deze gegevens worden direct getoond in de tool, maar kun je ook opslaan.
Het ophalen van de gegevens kan enkele minuten tot uur duren, het is verstandig om de gegevens op te slaan.

- Een csv-bestand inladen: Je kunt ook een csv-bestand inladen. Dit kan het bestand zijn dat je hebt gedownload of een bestand
dat je zelf hebt samengesteld. Vooral voor dat laatste is het belangrijk dat de volgende opbouw
wordt gebruikt. De kolomnamen dienen de eerste regel van het bestand te zijn.

#### Voor sensordata
kolomnaam | beschrijving | noodzakelijk  
--- | --- | ---  
"date" | de datum en het begin-uur van het uurgemiddelde (Etc/GMT-1) | x 
"kit\_id" | de naam van de sensor | x 
"lat" | de latitude van de sensorlocatie | x 
"lon" | de longitude van de sensorlocatie | x 
"pm10" | de sensorwaarde voor PM10 | 
"pm10\_kal" | de gekalibreerde sensorwaarde voor PM10 | 
"pm25" | de sensorwaarde voor PM2.5 |  
"pm25\_kal" | de gekalibreerde sensorwaarde voor PM2.5 | 

#### Voor Luchtmeetnetdata
kolomnaam | beschrijving | noodzakelijk  
--- | --- | ---  
"date" | de datum en het begin-uur van het uurgemiddelde (Etc/GMT-1) | x 
"station\_number" | het nummer van het luchtmeetnetstation bijv. NL10131 | x
"pm25" | de (ongevalideerde) waarde voor PM2.5 gemeten op het station van luchtmeetnet | 
"pm10" | de (ongevalideerde) waarde voor PM10 gemeten op het station van luchtmeetnet | 


#### Voor KNMI-data
kolomnaam | beschrijving | noodzakelijk  
--- | --- | --- 
"date" | de datum en het begin-uur van het uurgemiddelde (Etc/GMT-1) | x  
"wd" | de windrichting volgens het KNMI (missing data: -999, windstil = 0, veranderlijk = 990) | x 
"ws" | windsnelheid volgens het KNMI | x
"station\_number" | het nummer van het KNMI station bijv. 370 | x
"station\_code" | het nummer van het KNMI station met KNMI ervoor bijv. KNMI370 | x

### global.R

Hierin staat de initialisatie van de tool, alle benodigdheden worden
geladen. In dit geval: **packages, functies, symbolen en de keuze-opties.** De
functies die worden geladen zijn specifiek voor deze tool en staan in
een eigen R-script.

Daarnaast zijn er een aantal initialisaties zoals de gebruikte kleuren,
de naamgeving en de labels.

### ui.R en server.R

De ui en de server zijn als een boekenkast en boeken. Om een bibliotheek
te hebben, heb je een boekenkast (de structuur) en boeken (de inhoud)
nodig. Zo kun je het ook zien bij deze tool: de ui is de boekenkast en
de server zijn de boeken.

De connectie tussen de server en de ui gaat via unieke labels. Net zoals
in een boekenkast van de bibliotheek labels staan van de schrijvers, op
de bovenste plank de schrijvers beginnende met ‘A-B’, daaronder ‘C-D’
etc. Een voorbeeld in de tool:

> In de ui.R staat -&gt; plotOutput(**"timeplot"**) 

> In de server.R staat -&gt; output$**timeplot** &lt;- renderPlot({..})

### ui.R

Hierin wordt dus het **frame** (de boekenkast) van de tool gemaakt: de
tabbladen worden gedefinieerd, de positie van de kaart gemaakt etc.

Het script begint met de opmaak. De eerste regels halen een
html-template op. Dit template bevat de RIVM-look.

Daarna volgt een stukje code (wellPanel) waarin de banner wordt gemaakt
die verschijnt als de tool bezig is, bijvoorbeeld bij het downloaden van de data.

Vervolgens is de pagina opgebouwd uit 2 delen: **de sidebar en het
mainpanel**.

De inhoud van de **sidebar** is afhankelijk of het 'Data laden en downloaden'-tabblad 
wordt getoond of het 'Visualisatie en Analyse'-tabblad. In het eerste geval bevat de 
sidebar de legenda, voorbeelddataset en een welkomstekst, in het tweede geval de legenda 
en verschillende check-boxes om de data te selecteren en groeperen.

Het **mainpanel** bestaat uit de **leaflet-kaart** en de tabbladen 'Data laden en downloaden'
en 'Visualisatie en Analyse'. Om de code overzichtelijk te houden, is
voor elk tabblad een eigen functie geschreven. 

Voor 'Visualisatie en Analyse' is dat *tabPanelsAnalyse.R*. Dit tabblad is weer onderverdeeld
in tabbladen met in elke tabblad de titel, de toelichting en de grafiek (output).

Voor 'Data laden en downloaden' is dat *tabPanelsData.R*. Ook dit tabblad is onderverdeeld
in tabbladen. Elk tabblad zijn eigen bron om gegevens te downloaden: sensor, luchtmeetnet en knmi.
Er zijn verschillende buttons om de data te downloaden en laden en er is een invoerveld om een eigen bestand in te laden.


### server.R

De server genereert **de inhoud** van de tool. Hierin vind je **alle
functionaliteiten**: de markers kleuren, de selectie maken, de grafiek
tonen, de kaart weergeven etc.

Hier volgen eerst een aantal begrippen, die veel voorkomen in de code.

-   Voor alles wat als output/visualisatie in de tool komt, heeft het
    woord ‘output’. Bijvoorbeeld de kaart: **output$map**

-   De data wil je interactief kunnen selecteren, vandaar dat je een
    interactief dataframe nodig hebt. Dat kan via
    **‘reactiveValues(df=dataframe)’**.

-   Met de keuzes zoals ‘kies component’ wil je direct de waardes zien
    veranderen. Dit soort directe interacties worden bijgehouden in een
    **‘observeEvent’**. Ook het selecteren van de sensoren gaat hiermee.

-   Voor het genereren van visualisaties of de kaart gebruik je
    ‘render’; **renderPlot voor grafieken en renderLeaflet voor de
    kaart**. Deze visualisaties of kaart zijn dan in *‘output$naamplot’*
    neergezet.

#### Het opzetten van een interactief dataframe

De verschillende data (sensor/luchtmeetnet/knmi) heeft elk zijn eigen
interactieve dataframe (*reactiveValues*). Dat houdt in dat je 
**interactief aanpassingen** kunt maken in het dataframe; 
bijvoorbeeld het aanpassen van de kleur van de sensor.
Deze 3 dataframes hebben: een attribute *statinfo* met informatie over elk station/sensor
zoals bijvoorbeeld de kleur, een attribute *..._data* met de meetgegevens erin.

Lijst van informatie in *statinfo* (niet bij elk in gebruik):

-   Selected: geeft aan of is geselecteerd (TRUE/FALSE)
-   Kleur: geeft de kleur aan voor in de grafiek
-   lijn_stat: geeft de lijntype aan voor in de grafiek
-   Groep: geeft de groepsnaam aan. Wanneer niet in een groep is deze
    leeg: “”
-   name_icon: voor luchtmeetnet- en knmi-stations, welk icoon gebruikt 
dient te worden op de kaart


Daarnaast zijn er nog een aantal andere *reactiveValues* voor algemene
gegevens zoals de tijdsperiode, welke dataset gekozen is en om de keuzes 
voor de dropdown-menus goed te zetten.

#### Het maken van de kaart

De kaart wordt gemaakt door
[leaflet](https://rstudio.github.io/leaflet/). Bij het opstarten van de
tool wordt de kaart volledig aangemaakt:

-   Geef aan dat je leaflet wilt gebruiken. Met ‘addTiles()’ wordt
    automatisch een **openstreetmap achtergrond** geladen
-   Stel het zoomniveau in 'setview'
-   Voeg de **markers** toe voor: knmi-stations, lml-stations en de
    sensoren
-   Voeg de edit- en zoombuttons toe

Verdere aanpassingen aan de kaart, bijvoorbeeld kleurverandering van de
sensoren na het selecteren, gebeurt via de functie
*‘add\_sensors\_map’*. Deze functie werkt alsvolgt:

-   Maakt een **proxymap**, deze komt als het ware over de huidige kaart
    heen
-   Verwijdert alle sensoren die er nu op staan
-   Zet de sensoren er weer op. De karakteristieken van de sensoren (de
    kleur) is in de data aangepast, dus wanneer je de sensoren er weer
    opzet hebben ze de nieuwe kleur.
    
#### Het downloaden van data
Er zijn verschillende plekken in de tool waarop de gebruiken de gegevens kan downloaden.
Hieronder de code van hoe de gegevens vanuit de grafiek kunnen worden gedownload:
```
  output$downloadData_grafiek <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_grafiek', 'csv', sep=".")
    },
    # Geef de data op
    content = function(file) {
      print("Download data uit de grafiek.")
      write.table(overig_reactive$data_grafiek, file, sep = ',',
                  row.names = FALSE)
    }
  ) 
```
#### Het maken van tabellen
Binnen shiny kan data heel gemakkelijk in een tabel worden weergegeven. Daarvoor heb je *renderTable* en een dataframe nodig.

```
 # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_lml <- renderTable({
    stations_df <- data.frame('Naam' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('naam')],
                              'Nummer' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('station_number')],
                              'Organisatie' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('organisatie')])
  })
```


#### Het maken van grafieken

Voor de **visualisatie in grafieken** maken we deels gebruik van het package
[OpenAir](http://davidcarslaw.github.io/openair/). De verschillende
grafieken gaan via hetzelfde structuur:

-   Bekijk welk **component** er gevisualiseerd moet worden en geef het juiste label
- Bekijk of er sensoren zijn **geselecteerd**
- Maak de sensordata klaar met de functie **filter\_sensor\_data\_plot()
- Zorg voor de juiste kleuren en lijntype
-   Maak de grafiek via de **functie van openair**




#### Overzicht van de functies

Er zijn verschillende functies gemaakt voor de functionaliteiten van de
tool. De functies zijn direct in server.R
gedefineerd, omdat die directe aanpassingen maken in het interactieve
dataframe. Van deze functies volgt hier een korte functie-omschrijving
om een inzicht te geven in de structuur.

Veel handelingen worden voor de sensoren, luchtmeetnetstations en de knmi-stations hetzelfde uitgevoerd. Echter zijn ze verschillend in reactieValue en in data. Daarom hebben ze elk een eigen functie. Hieronder worden alleen de functies van de sensordata benoemd.

*check\_kolommen\_sensor* - functie: Check of de alle kolommem voor de sensordata aanwezig zijn, zo niet vul deze dan aan met NA, en geef een waarschuwing

*set\_sensor\_deselect* – functie om de eigenschappen van de sensor weer
op de **default deselect** te zetten

*set\_sensor\_select* – functie om de eigenschappen van de sensoren op
**select** te zetten. Bij het toekennen van een kleur wordt bepaald
welke kleur nog vrij is. Als de groepsselectie aan staat
(actiegroep==TRUE) wordt de kleur en naam van die groep toegekend.

*add\_sensors\_map* – functie voor het toevoegen van de sensoren op de
kaart

*set\_view\_map*  -  functie om de zoom/view te centreren rond de sensoren

*calc\_groep\_mean* – functie om per groep het gemiddelde te berekenen

*insert\_nieuwe\_data\_sensor* -  functie om de sensor data in te laden, deze functie is onderverdeeld in het laden van de voorbeelddata, het downloaden van de API en het laden vanuit een csv-bestand.

*get\_sensor\_data\_api* - functie voor het ophalen van de sensor data vanuit een API

*filter\_sensor\_data\_plot* -  functie om de sensor data klaar te maken voor de visualisatie

*check\_selected\_id* -  functie om de verschillende acties uit te voeren bij selectie van een sensor of station.

#### Overzicht van de ObserveEvents

Met de keuzes zoals *‘kies component’* wil je direct de waardes zien
veranderen. Dit soort directe interacties worden bijgehouden in een
**‘observeEvent’**. Ook het selecteren van de sensoren gaat hiermee.

Net als bij de functies volgt hier een korte beschrijving van de meeste
verschillende observeEvents:

 *observeEvent({input$sensor_hoofdgroep},{...}* - Check waarop de data geselecteerd wordt: zet de choices klaar -gemeente of -project
 

  *observeEvent({req(input$eigen_datafile_sensoren)}* - observe of er een eigen data set is ingeladen voor de sensoren


  *observeEvent({input$voorbeeld_data}* - Observe of de voorbeeld dataset weer ingeladen moet worden

*observeEvent({input$DateStart},{...}* - Observe of de datum wordt aangepast voor de plots (dus het visualisatie gedeelte)

  *observeEvent({input$map_marker_click$id}, {...}* - Observe if user selects a sensor
  
 *observeEvent(input$reset_huidig, {...}* - Observe of de huidige selectie moet worden gereset

  *observeEvent(input$groeperen,{...}* - Observe of de selectie moet worden toegevoegd aan de groep

*observeEvent(input$map\_draw\_new\_feature … )* en
*observeEvent(input$map\_draw\_deleted\_features …)* – voor de
multiselect die al bij de leaflet-kaart wordt meegegeven. Om gebruik te
kunnen maken van en handmatig en via multiselect te selecteren, zijn
deze functies om die beide te combineren. Het houdt expliciet bij wat
geselecteerd is met multiselect, om dat ook met de reset button te
kunnen deselecteren.



Nawoord
-------

Ik hoop dat je hiermee voldoende rondleiding hebt gehad, dat je de
scripts zelf kunt bekijken en uitrbeiden. We horen graag je feedback
via: [samenmeten](https://www.samenmetenaanluchtkwaliteit.nl/contact).
Meldingen van errors of opmerkingen over de code graag via een *Issue*
op GitHub zelf, je hebt dan wel een account nodig.

Gebruik
===========
We maken de samen analyseren tool opensource, zodat ook de analysemethode voor
iedereen beschikbaar is. Iedereen is welkom om de tool te gebruiken en
aan te passen (volgens de [GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html) licentie). 
NB. De Samen Analyseren Tool maakt gebruik van de RIVM huisstijl. 
Gebruikers die de tool aanpassen en op een andere plaats publiceren, 
verzoeken wij om de RIVM huisstijl niet te gebruiken.


>>>>>>> upstream/master
