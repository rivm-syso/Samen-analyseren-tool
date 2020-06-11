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

## Load folder voor de huisstijl ----

# Hier worden de bestanden opgehaald voor de huissijl. Deze staan ook op github en zullen
# ook ge-update worden, dus verwijder zo nu en dan je www folder.
# Let op: zorg ervoor dat je niet zelf ook een folder 'www' hebt, anders worden de bestanden niet
# opgehaald. 
if (!dir.exists('www')){
  download.file('https://github.com/rivm-syso/RIVM-huisstijl-shiny/archive/master.zip', destfile = "wwwdest.zip")
  unzip('wwwdest.zip', exdir='.')
  file.rename('RIVM-huisstijl-shiny-master', 'www')
}

# HTML template voor de opmaak/styling
htmlTemplate("./www/template_samenmeten.wide.html",
             pageTitle=paste("Prototype Samen Analyseren tool: project ", projectnaam),
             
             aboutSite=div(h3("Verantwoording"),
                           
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
                             ))),
             
  # Vanaf hier begint de tool zelf
  fluidPage=fluidPage(
    
  # wellPanel voor grijze boxing
  wellPanel(
  # Sidebar layout met input en output definities
  sidebarLayout(
    # Sidebar panel voor leaflet map om sensoren te selecteren
    sidebarPanel(width=3,
      
      # Button om de alles wat geselecteerd is te resetten
      actionButton("reset_all", "Reset alle sensoren"),
      br(),
      # Input: Selecteer de component uit de choices lijst
      selectInput(inputId = "Var", label = "Kies component:", choices = choices, selected = NULL, multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
      
      # Input: Blokjes voor de datum
      dateInput("DateStart", label="Selecteer begin tijdreeks:", format='dd-mm-yyyy',value = min(input_df$date), 
                min = min(input_df$date), max = max(input_df$date)),
      dateInput("DateEind", label="Selecteer einde tijdreeks:", format='dd-mm-yyyy', value = max(input_df$date), 
                min = min(input_df$date), max = max(input_df$date)),
      
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
                  tpTimeplot(),
                  tpKalender(),
                  tpTimevariation(),
                  tpPercentileRose(),
                  tpPollutionRose(),
                  tpWindRose()
      )
      
    ) 
    ),
  )
)
)
