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
             pageTitle=paste("Prototype Samen Analyseren tool: project ", projectnaam),
             
             aboutSite=div(
               h3("Gebruikershandleiding"),
               p("In de ", a("handleiding", href = "https://www.samenmetenaanluchtkwaliteit.nl/documenten/gebruikershandleiding-samen-analyseren-tool", 
                             target = 'blank'), "vind je meer informatie over het algemene gebruik van de Samen Analyseren Tool."),
               h3("Verantwoording"),
                           
                           p("Dit dashboard is door het ", a("RIVM", href = "https://rivm.nl", target = 'blank'), "ontwikkeld voor snelle analyse van sensordata.
                             Het maakt gebruik van de R-package",
                             a("openair.", 
                               href = "http://davidcarslaw.github.io/openair/", target="_blank"),
                             
                             p("Het huidige dashboard is een prototype en nog volop in ontwikkeling. 
                               Het wordt gebruikt om verschillende analyses en visualisaties te testen.
                               De broncode van de tool is te vinden via",
                              a("GitHub",
                                href = "https://github.com/rivm-syso/Samen-analyseren-tool", target = "_blank"),
                                ", zodat we samen met jullie het dashboard verder kunnen ontwikkelen."),
                             
                             h4("Data"),
                             p("De getoonde sensordata zijn afkomstig uit de", 
                               a("Samen Meten database.",
                                 href = "https://samenmeten.rivm.nl/dataportaal/", target = 'blank'),
                               "De sensormetingen die in dit prototype gevisualiseerd worden, zijn gedaan met goedkope fijnstofsensoren van het project",
                               a("Hollandse Luchten.",
                                 href = "https://hollandseluchten.waag.org/", target = 'blank'),
                               "De gegevens worden eens per maand ge-update. De getoonde waarden geven een indicatie van de fijnstofconcentratie. 
                               De data zijn niet geschikt om te toetsen aan grenswaarden."
                             ),
                             p("Elke sensor kan worden vergeleken met het dichtstbijzijnde meetstation van het", a("Luchtmeetnet",
                                                                                                                  href = "https://luchtmeetnet.nl", target = 'blank'), ".
        De windgegevens voor het berekenen van de windroos komen van het dichtstbijzijnde weerstation van het ", a("KNMI",
                                                                                                                  href = "https://knmi.nl", target = 'blank'),".
        De locaties van de gebruikte luchtmeetnet- en weerstations worden met icoontjes op de kaart getoond."),
                             h4("Gekalibreerde waarden"),
                             p("Details over de kalibratieprocedure zijn te vinden op het", 
                               a("Samen Meten Kennisportaal.",
                                 href = "https://www.samenmetenaanluchtkwaliteit.nl/dataportaal/kalibratie-van-fijnstofsensoren", target = 'blank'),
                               "De kalibratie is nog niet met terugwerkende kracht uitgevoerd. Hierdoor kan er minder gekalibreerde data aanwezig zijn. 
        We krijgen steeds meer begrip van situaties waarin sensoren – ook na kalibratie – minder betrouwbare waarden geven. 
        Dit zijn bijvoorbeeld situaties met zeer hoge luchtvochtigheid (vanaf 97 à 98% zoals gemeten op KNMI stations). 
        Met name in de wintermaanden in de nacht en vroege ochtend kan de luchtvochtigheid zeer hoog kan zijn. 
        Maar het kan ook gaan om sensoren die altijd of vaak afwijken van de patronen die andere sensoren laten zien.  
        Het is op dit moment nog niet mogelijk om de meetwaarden van deze uren of sensoren uit de gegevens te filteren. Daar werken we wel aan.
        "
                             )),
                           h3("Nieuw in deze tool"),
                           p("De tool is nog in ontwikkeling. Hieronder vindt u de laatste aanpassingen (sinds 12 juni 2020):"),
                            tags$ul(tags$li("Layout: grotere kaart beschikbaar"),
                           tags$li("Tijdreeksselectie: makkelijker vanaf een specifieke dag te filteren"),
                           tags$li("Groepsselectie: makkelijker de sensoren te clusteren in een groep zodat groepsgemiddeldes kunnen worden vergeleken"))
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
  # Sidebar layout met input en output definities
  sidebarLayout(
    # Sidebar panel voor leaflet map om sensoren te selecteren
    sidebarPanel(width=3,
                 div(h4("Legenda"),
                 div("Sensor: ", br(),img(src="symbol_sensor_black.svg"), "Met gegevens"),
                 div(img(src="symbol_sensor_grey.svg"), "Zonder gegevens"),
                 div(img(src="symbol_sensor_pink.svg"), "Geselecteerd"),
                 div("KNMI-station: ", br(),img(src="symbol_knmi_black_triangle.svg"), "Met gegevens"),
                 div(img(src="symbol_knmi_grey_triangle.svg"), "Zonder gegevens"),
                 div(img(src="symbol_knmi_white_triangle.svg"), "Geselecteerd"),
                 div("Luchtmeetnet-station: ", br(),img(src="symbol_lml_black.svg"), "Met gegevens"),
                 div(img(src="symbol_lml_grey.svg"), "Zonder gegevens"),
                 div(img(src="symbol_lml_white.svg"), "Geselecteerd")),
      # Button om de alles wat geselecteerd is te resetten
      actionButton("reset_all", "Reset alle sensoren"),
      br(),
      # Input: Selecteer de component uit de choices lijst
      selectInput(inputId = "Component", label = "Kies component:", choices = component_choices, selected = NULL, multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
      
      # Input: Blokjes voor de datum
      dateInput("DateStart", label="Selecteer begin tijdreeks:", format='dd-mm-yyyy',value = '2019-01-01'
                ),
      dateInput("DateEind", label="Selecteer einde tijdreeks:", format='dd-mm-yyyy', value = '2020-12-31' 
                ),
      # Input: Selecteer het knmi station waarvan de windriching en snelheid wordt gebruikt
      selectInput(inputId = "knmi_stat_wdws", label = "Kies KNMI-station voor de windsnelheid en windrichting:", choices = '', selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL), 
      br(),

      # Input: Tekst voor de groepselectie
      textInput(inputId = "Text_groep",'Maak nieuwe groep:', value = ''),
      # Input: kies groep uit lijst bestaande groepen (gaat via een selectInput)
      uiOutput("bestaande_groep"),
      # Button: knop om de selectie aan de groep toe te voegen
      actionButton("groeperen", "Groepeer selectie"),
      
      # Button om de huidige selectie van sensoren te resetten
      actionButton("reset_huidig", "Reset selectie"),
      
      # Output: tabel met de geslecteerde kitids, voor toekenning aan groep
      tableOutput("huidig")
      ),
    
    # Main panel voor outputs
    mainPanel(width=9,
      #Output: Leaflet map voor sensorselectie
      leafletOutput("map", height = "300px"),
      # Output: Tabset voor openair plots, zie voor de inhoud het script: tabPanels.R
      tabsetPanel(type = "tabs",
                  tpData(),
                  tpAnalyse()
                  )
      )
    ),
  )
)
)
