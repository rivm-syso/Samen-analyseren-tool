Inhoudsopgave
-------------

-   [Achtergrond](#achtergrond)
-   [Documentatie](#documentatie-samen-analyseren-tool)

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
gemakkelijk terugvinden. Hier zie je ook hoe de luchtkwaliteit varieerde
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
zijn groepsgemiddelden bijvoorbeeld betrouwbaarder dan individuele
sensoren, en daarom is het ook mogelijk om groepsgemiddelden gemakkelijk
te bepalen. De huidige tool is een prototype, er wordt nog volop aan
gewerkt. De tool is uitgeprobeerd in het project [Hollandse
Luchten](https://hollandseluchten.waag.org/), waar het enthousiast is
ontvangen.

Samen analyseren tool opensource
--------------------------------

We maken de samen analyseren tool opensource, zodat ook de analyse voor
iedereen beschikbaar is. Iedereen is welkom om de tool te gebruiken en
aan te passen (volgens de licentie GPL v3). Hieronder volgt een
beschrijving van de hoofdelementen van de tool. Algemene vragen en
opmerkingen kan je stellen via
[samenmeten](https://www.samenmetenaanluchtkwaliteit.nl/contact).
Meldingen van errors of opmerkingen over de code graag via een *Issue*
op github zelf.

Documentatie Samen Analyseren Tool
==================================

Inhoudsopgave
-------------

-   [Kennis vooraf](#kennis-vooraf)
-   [Run de tool](#run-de-tool)
-   [Opbouw en structuur](#opbouw-en-structuur)
-   [Input data](#input-data)
-   [global.R](#global.r)
-   [ui.R en server.R](#ui.r-en-server.r)
-   [ui.R](#ui.r)
-   [server.R](#server.r)
    -   [Het maken van de kaart](#het-maken-van-de-kaart)
    -   [Het maken van de grafieken](#het-maken-van-de-grafieken)
    -   [Het opzetten van een interactief
        dataframe](#het-opzetten-van-een-interactief-dataframe)
    -   [Overzicht van de functies](#overzicht-van-de-functies)
    -   [Overzicht van de
        ObserveEvents](#overzicht-van-de-observeevents)
-   [Nawoord](#nawoord)

Kennis vooraf
-------------

De scripts staan hier op Github.
[Github](https://guides.github.com/activities/hello-world/) is een zeer
geschikt platform om software te delen en met verschillende partijen te
ontwikkelen. Als je een account hebt kun je ook bijdragen aan deze tool.
Gebruik hiervoor ‘fork’ en ‘Pull request’. Mocht je bugs tegenkomen,
meldt deze dan via een ‘Issue’. Als je geen account hebt, kun je de
bestanden downloaden en op je eigen pc aanpassingen maken.

De Samen analyseren tool is gemaakt in de programmeertaal R. Voor het
gebruik van de tool via de interface
([hier](https://rivm.shinyapps.io/samenanalyserentool)) is geen verstand
van R nodig. Maar gezien je al hier bent, wil je waarschijnlijk zelf aan
de slag. Meer informatie over programmeren in R en het downloaden van
R-studio kan je op de [website](https://rstudio.com/) vinden.

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
R-studio, maak een nieuwe repository aan en zet daar alle bestanden van
deze github in. Als je een account hebt, gebruik om de bestanden op te
halen ‘fork’. Als je geen account hebt, kun je de bestanden downloaden.

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

De data die in deze tool gebruikt wordt, is een dataframe met de
volgende kolommen: 

kolomnaam | beschrijving | gebruikt in huidige tool  
--- | --- | ---  
"date" | de datum en het begin-uur van het uurgemiddelde (Etc/GMT-1) | x 
"kit\_id" | de naam van de sensor | x 
"lat" | de latitude van de sensorlocatie | x 
"lon" | de longitude van de sensorlocatie | x 
"pm10" | de sensorwaarde voor PM10 | x 
"pm10\_kal" | de gekalibreerde sensorwaarde voor PM10 | x 
"pm25" | de sensorwaarde voor PM2.5 | x 
"pm25\_kal" | de gekalibreerde sensorwaarde voor PM2.5 | x 
"wd" | de windrichting volgens het KNMI (missing data: -999, windstil = 0, veranderlijk = 990) | x 
"ws" | windsenelheid volgens het KNMI | x
"rh" | de relatieve luchtvochtigheid | 
"temp" | de temperatuur |
"pm25\_lml" | de (ongevalideerde) waarde voor PM2.5 gemeten op het dichtstbijzijnde station van luchtmeetnet | x 
"pm10\_lml" | de (ongevalideerde) waarde voor PM10 gemeten op het dichtstbijzijnde station van luchtmeetnet | x 
"knmi\_id" | het nummer van het dichtstbijzijnde KNMI station, waarvan de weergegevens zijn meegegeven |
"lml\_id" | het nummer van het dichtstbijzijnde luchtmeetnetstation |

### global.R

Hierin staat de initialisatie van de tool, alle benodigdheden worden
geladen. In dit geval: **packages, functies, symbolen en de data.** De
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

Daarna is de pagina opgebouwd uit 2 delen: **de sidebar en het
mainpanel**.

In de **sidebar** worden de **leaflet-kaart** en de verschillende
buttons neergezet. In het **mainpanel** komen de tabbladen met daarin de
**grafieken** en de toelichting. Om de code overzichtelijk te houden, is
voor elk tabblad een eigen functie geschreven. Dus **de structuur van de
tabbladen** kun je vinden in het script *tabbladen.R*. Hier zie je voor
elk tabblad de titel, de toelichting en de grafiek (output). Als je goed
kijkt, herken je ook hierin de elementen sidebar en mainpanel, voor
resp. de toelichting en de grafiek.

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

#### Het maken van de grafieken

Voor de **visualisatie in grafieken** maken we gebruik van het package
[OpenAir](http://davidcarslaw.github.io/openair/). De verschillende
grafieken gaan via hetzelfde structuur:

-   Bekijk welk **component** er gevisualiseerd moet worden
-   Bekijk in welke **tijdsperiode**
-   Ga na welke sensoren er **geselecteerd** zijn
-   Als er groepen zijn gedefinieerd, bereken daarvoor het
    **groepsgemiddelde**
-   Maak de grafiek via de **functie van openair**

#### Het opzetten van een interactive dataframe

Het dataframe is niet een normaal dataframe, maar een interactief
dataframe. Dat houdt in dat je **interactief aanpassingen** kunt maken
in het dataframe; bijvoorbeeld het aanpassen van de kleur van de sensor.
In het dataframe is kleur een attribute. Deze kan worden gewijzigd door
de *selectfunctie* en meteen door de *add\_sensors\_map*-functie op de
kaart worden getoond.

In het interactieve dataframe (het heeft **de naam ‘values’**) zijn
verschillende kolommen:

-   df: het dataframe met de **eigenschappen van de sensoren** en de
    meetwaardes erin
-   groepsnaam: de waarde die de gebruiker heeft ingetypt voor de naam
    van de **groep**
-   actiegroep: boolean of is aangevinkt dat de sensor bij de groep
    hoort (True/False)
-   df\_gem: het dataframe met de **gemiddeldes per groep** erin

Het df is het **input dataframe**, dat wordt vanuit een .RDS bestand
ingeladen in **global.R**. Het bestaat uit verschillende
basiseigenschappen zoals de meetwaardes en locatie. Nadat het in
global.R is ingeladen, wordt het in **server.R** in een **interactief
dataframe** gezet.

De volledige **kolomnamen** zijn: "date", "kit\_id", "lat", "lon",
"pm10", "pm10\_kal", "pm25", "pm25\_kal", "wd", "ws", "rh", "temp",
"pm25\_lml", "pm10\_lml", "knmi\_id", "lml\_id"

Daarnaast zijn er later in de tool nog een aantal eigenschappen per
sensor toegevoegd.

-   Selected: geeft aan of de sensor geselecteerd is (TRUE/FALSE)
-   Kleur: geeft de kleur van de sensor aan
-   Groep: geeft de groepsnaam aan. Wanneer niet in een groep is deze
    leeg: “”

#### Overzicht van de functies

Er zijn verschillende functies gemaakt voor de functionaliteiten van de
tool. Enkele functies konden in een eigen R-script worden gezet en zijn
in global.R ingeladen. Andere functies zijn direct in server.R
gedefineerd, omdat die directe aanpassingen maken in het interactieve
dataframe. Van deze functies volgt hier een korte functie-omschrijving
om een inzicht te geven in de structuur.

*set\_sensor\_deselect* – functie om de eigenschappen van de sensor weer
op de **default deselect** te zetten

*set\_sensor\_select* – functie om de eigenschappen van de sensoren op
**select** te zetten. Bij het toekennen van een kleur wordt bepaald
welke kleur nog vrij is. Als de groepsselectie aan staat
(actiegroep==TRUE) wordt de kleur en naam van die groep toegekend.

*add\_sensors\_map* – functie voor het toevoegen van de sensoren op de
kaart

*calc\_groep\_mean* – functie om per groep het gemiddelde te berekenen

#### Overzicht van de ObserveEvents

Met de keuzes zoals *‘kies component’* wil je direct de waardes zien
veranderen. Dit soort directe interacties worden bijgehouden in een
**‘observeEvent’**. Ook het selecteren van de sensoren gaat hiermee.

Net als bij de functies volgt hier een korte beschrijving van de
verschillende observeEvents:

*observeEvent({input$A\_groep} …)* – houdt in de gaten of er een groep
moet worden geselecteerd of een losse sensor.

*observeEvent({input$Text\_groep} …)* – houdt in de gaten welke
groepsnaam er is opgegeven

*observeEvent({input$map\_marker\_click$id} …)* – houdt in de gaten of
er sensor wordt aangeklikt. Zoja, dan selecteert-ie de sensor en laat de
nieuwe kleur op de kaart zien.

*observeEvent(input$reset, …)* – wanneer er op de reset button wordt
geklikt, wordt de kleur en selected van alle sensoren weer op default
gezet

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
op github zelf.
