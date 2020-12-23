## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het ui.R script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: april 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## Het eerste gedeelte bevat de opmaak/styling
## ---------------------------------------------------------

# HTML template voor de opmaak/styling ---- 
htmlTemplate("./www/template_samenmeten.wide.html",
             pageTitle="Samen Analyseren tool",
             
             aboutSite=div(
               p("De Samen Analyseren Tool is door het RIVM ontwikkeld voor het downloaden en analyseren van sensordata. 
                 Daarnaast kun je met deze tool de sensordata  eenvoudig combineren met officiële meetdata van het 
                 Landelijk Luchtmeetnet en met de weergegevens van het KNMI. De tool maakt gebruik van het R package",
                 a("openair", href = "http://davidcarslaw.github.io/openair/", target="_blank"),
                 " en wordt geheel open source aangeboden. "),
               h3("Data"),
               p("De sensordata is afkomstig uit de",
                 a("Samen Meten database", href = "https://samenmeten.rivm.nl/dataportaal/", target = 'blank'),
                 ". De getoonde waarden geven een indicatie van de fijnstofconcentratie. De data is niet geschikt 
                 om te toetsen aan grenswaarden."),
               p("Elke sensor kan worden vergeleken met een meetstation van het ", 
                 a("Luchtmeetnet", href = "https://luchtmeetnet.nl", target = 'blank'), 
                 ". De windgegevens voor het berekenen van de windroos komen van een weerstation van het", 
                 a("KNMI", href = "http://projects.knmi.nl/klimatologie/uurgegevens/selectie.cgi", target="blank"), 
                 ". De locaties van de gebruikte luchtmeetnet- en weerstations worden met icoontjes op de kaart getoond."),
               p("In het tabblad 'Data laden en downloaden' kunt u de data van de sensoren van een project of gemeente, 
                 van de meetstations van het luchtmeetnet en van de meetstations van het KNMI laden. Deze gegevens worden 
                 via API's bij de desbetreffende partij opgevraagd. Het opvragen en ophalen van de gegevens kan enige 
                 tijd duren. Met de downloadfunctie kunt u de gegevens opslaan, zodat u niet iedere keer de gegevens 
                 opnieuw hoeft op te vragen."),
               p("In het csv-bestand van de download is de tijd weergegevens in UTC. In de grafieken zelf wordt de tijd in Europe/Amsterdam getoond."),
               h3("Gekalibreerde waarden"),
               p("De gedownloade data bevat ruwe én gekalibreerde data. Details over de kalibratieprocedure zijn te 
                 vinden op het", a("Samen Meten Kennisportaal.",
                                 href = "https://www.samenmetenaanluchtkwaliteit.nl/dataportaal/kalibratie-van-fijnstofsensoren", target = 'blank'),
                 " De kalibratie van sensordata wordt gedaan met behulp van secundaire data die niet altijd beschikbaar zijn. 
                 Het kan daarom voorkomen dat voor sommige tijdsperioden geen gekalibreerde waarden in de database aanwezig zijn. "),
               p("We krijgen steeds meer begrip van situaties waarin de sensoren – ook na kalibratie – minder betrouwbare
                 waarden geven. Dit zijn bijvoorbeeld situaties met zeer hoge luchtvochtigheid (vanaf 97 à 98% zoals 
                 gemeten op KNMI stations). Met name in de wintermaanden in de nacht en vroege ochtend kan de luchtvochtigheid 
                 zeer hoog kan zijn. Daarnaast kan het zijn dat deze sensoren altijd of vaak afwijken van de patronen die andere 
                 sensoren laten zien. Het is op dit moment nog niet mogelijk om de meetwaarden van deze uren of sensoren uit de 
                 gegevens te filteren. Daar werken we wel aan."),
               h3("Open source"),
               p("De broncode voor deze tool wordt geheel open source aangeboden via onze ",
                 a("GitHub-pagina",
                   href = "https://github.com/rivm-syso/Samen-analyseren-tool", target = "_blank"),
                 ". Iedereen mag deze tool verder ontwikkelen of aanpassen, mits de aanpassingen weer open source beschikbaar komen.")
),
  # Vanaf hier begint de tool zelf ----
  fluidPage=fluidPage(
    
  # wellPanel voor grijze boxing
  wellPanel(
    # Deze code zorgt voor een loading message bovenaan de pagina
    tags$head(tags$style(type="text/css", 
                         "#loadmessage {
                          position: fixed;
                          top: 200px;
                          left: 0px;
                          width: 100%;
                          padding: 5px 0px 5px 0px;
                          text-align: center;
                          font-weight: bold;
                          font-size: 100%;
                          color: #FFFFFF;
                          background-color: #c7005d;
                          z-index: 1100;}")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("De gegevens worden opgehaald, dit kan een tijd duren....",id="loadmessage")),
  # Sidebar layout met input en output definities ----
  sidebarLayout( 
    # Sidebar panel voor leaflet map om sensoren te selecteren
    sidebarPanel(width=3,
                 # Set de Legenda
                 checkboxInput("cLegenda", "Legenda"),
                 conditionalPanel("input.cLegenda==true",
                   div(
                   div("Sensor: ", br(),img(src="symbol_sensor_black.svg"), "Met gegevens"),
                   div(img(src="symbol_sensor_grey.svg"), "Zonder gegevens"),
                   div(img(src="symbol_sensor_geel.svg"), "Geselecteerd"),
                   div("KNMI-station: ", br(),img(src="symbol_knmi_black.svg"), "Met gegevens"),
                   div(img(src="symbol_knmi_grey.svg"), "Zonder gegevens"),
                   div(img(src="symbol_knmi_wijnrood.svg"), "Geselecteerd"),
                   div("Luchtmeetnet-station: ", br(),img(src="symbol_lml_black.svg"), "Met gegevens"),
                   div(img(src="symbol_lml_grey.svg"), "Zonder gegevens"),
                   div(img(src="symbol_lml_wijnrood.svg"), "Geselecteerd"),
                   br())),
                 # Set de opties voor de Data laden en downloaden
                 conditionalPanel(condition="input.tabset_data_analyse=='Data laden en downloaden'",  
                                  div(h4("Welkom bij de Samen Analyseren tool"),
                                      p("Klik op de button om de tool te testen met een voorbeeld data set."),
                                      # Button om terug te gaan naar de standaard voorbeeld data
                                      actionButton("voorbeeld_data","Laad voorbeeld data"),
                                      p("Wilt u de sensoren binnen uw eigen gemeente of project bekijken. 
                                      Volg dan de stappen in de tabbladen."),
                                      p("Real-time metingen van de sensoren kunt u vinden op het Samen Meten Dataportaal:", 
                                        a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank'))
                                      )),
                # Set de opties voor de visualisatie en analyse
                conditionalPanel(condition="input.tabset_data_analyse=='Visualisatie en Analyse'", 
                                 # Set de tijdreeks
                                 checkboxInput("cTijdreeks", "Tijdreeks"),
                                 conditionalPanel("input.cTijdreeks==true",
                                    # Input: Blokjes voor de datum
                                    dateInput("DateStart", label="Selecteer begin tijdreeks:", format='dd-mm-yyyy',value = '2019-01-01'
                                    ),
                                    dateInput("DateEind", label="Selecteer einde tijdreeks:", format='dd-mm-yyyy', value = '2020-12-31' 
                                    )),
                                 # Set keuze voor de component
                                 checkboxInput("cComponent", "Component"),
                                 conditionalPanel("input.cComponent==true",
                                                  # Input: Selecteer de component uit de choices lijst
                                                  selectInput(inputId = "Component", label = "Kies component:", choices = component_choices, selected = 'pm10_kal', multiple = FALSE,
                                                              selectize = TRUE, width = NULL, size = NULL)),
                                # Set voor het groeperen van de sensoren
                                checkboxInput("cGroeperen", "Groeperen"),
                                conditionalPanel("input.cGroeperen==true",
                                    # Input: Tekst voor de groepselectie
                                    textInput(inputId = "Text_groep",'Maak nieuwe groep:', value = ''),
                                    # Input: kies groep uit lijst bestaande groepen (gaat via een selectInput)
                                    uiOutput("bestaande_groep"),
                                    # Button: knop om de selectie aan de groep toe te voegen
                                    actionButton("groeperen", "Groepeer selectie"),
                                    
                                    # Button om de huidige selectie van sensoren te resetten
                                    actionButton("reset_huidig", "Reset selectie"),
                                    
                                    # Output: tabel met de geslecteerde kitids, voor toekenning aan groep
                                    tableOutput("huidig")),
                                
                                    
                               # Button om de alles wat geselecteerd is te resetten
                               actionButton("reset_all", "Reset alle sensoren"),
                               
                               # Button om de gegevens zoals getoond in de grafiek te downloaden
                               div(style="padding-top: 10px;",downloadButton("downloadData_grafiek",'Download data uit grafiek'))
                   )
      ),
    
    # Main panel voor outputs ----
    mainPanel(width=9,
      #Output: Leaflet map voor sensorselectie
      leafletOutput("map", height = "300px"),

      # Output: Tabset voor openair plots, zie voor de inhoud het script: tabPanels.R
      tabsetPanel(type = "tabs",
                  tpData(),
                  tpAnalyse(),
                  id='tabset_data_analyse')
      )
  )
    )
  )
)

