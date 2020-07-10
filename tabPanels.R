## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het tabPanels script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: april 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## In dit script worden de verschillende tabbladen gemaakt
## ---------------------------------------------------------

tpData <- function(){
  library(shiny)
  
  tp <-  tabPanel("Data",
                  tabsetPanel(
                    tabPanel("Start",
                             p("Welkom bij de Samen Anlayseren tool. 
                               Klik op de buttom om de tool te testen met een voorbeeld data set. Wilt u de sensoren binnen uw eigen gemeente of project bekijken. Volg dan de stappen in de andere tabbladen."),
                             p("Real-live metingen van de sensoren kunt u vinden op het Samen Meten Dataportaal:", a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank')),
                             # Button om terug te gaan naar de standaard voorbeeld data
                             actionButton("voorbeeld_data","Laad voorbeeld data")),
                    tabPanel("Stap 1: Sensoren",
                             helpText("Selecteer hieronder het project of de gemeente waarvan u de sensoren wilt opvragen.
                                      Vraag daarna de gegevens op. LET OP: het opvragen van de gegevens kan enkele minuten tot een half uur duren.
                                      De opgevraagde gegevens worden direct getoond. U kunt ze ook downloaden en op een ander moment weer inladen, 
                                      zodat u niet elke keer hoeft te wachten."),
                             # Kies eerst waarvan je data wilt selecteren: gemeente of project
                             selectInput(inputId = "sensor_hoofdgroep", label = "Selecteer op project of gemeente:", choices = hoofd_choices, selected = NULL, multiple = FALSE,
                                         selectize = TRUE, width = NULL, size = NULL),
                             # Specificeer waarvan de data gedownload kan worden (de choices worden reactive gemaakt door de selectinput hierboven)
                             selectInput(inputId = "sensor_specificeer", label = "Maak uw keuze:", choice="",selected = NULL, multiple = FALSE,
                                         selectize = TRUE, width = NULL, size = NULL),                   
                             
                             # Button om de gegevens van de sensore op t halen via de API
                             actionButton("API_samenmeten","Haal de metingen van de sensoren op"),
                             # Button om de sensor data te downloaden
                             downloadButton("downloadData_sensor",'Download de metingen van de sensoren'),
                             # Mogelijkheid om je eigen data in te laden:
                             fileInput("eigen_datafile", "Laad dataset sensormetingen(csv-bestand): ",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
                             ),
                    tabPanel("Stap 2: Luchtmeetnet",
                             helpText("Laadt eerst de locaties van de meetstations van Luchtmeetnet. 
                                      Selecteer daarna de stations waarvan u de meetgegevens wilt opvragen.
                                      De opgevraagde gegevens worden direct getoond. U kunt ze ook downloaden 
                                      en op een ander moment weer inladen, 
                                      zodat u niet elke keer hoeft te wachten.
                                      LET OP/TODO dit werkt nog niet allemaal"),
                            # Button om de luchtmeetnetsations op de kaart te zetten
                            actionButton("show_luchtmeetnet", "Laadt locaties luchtmeetnetstations"),
                            # Output: tabel met de geslecteerde LML station, voor het downloaden van de data
                            tableOutput("stations"),
                            # Button om de gegevens van de luchtmeetnetstations op et halen via de API
                            actionButton("API_luchtmeetnet","Haal de metingen van de stations op"),
                            # Button om de LML data te downloaden
                            downloadButton("downloadData_luchtmeetnet",'download metingen luchtmeetnetstations'),
                            p("Selecteer luchtmeetnetstations om de data op te halen. De eigenaar van het station staat erbij. DCMR, GGD Amsterdam, Provincie Limburg, ODRA, LML"),
                            h4("Toelichting"),
                            p("",style = "font-size:12px")
                    ),
                    tabPanel("Stap 3: KNMI",
                             helpText("TODO")
                             )
    )
  )
  
  return(tp)
} 

tpTimeplot <- function(){
  
library(shiny)
  
 tp <-  tabPanel("Tijdreeks",
                 helpText("Selecteer een sensor. Deze grafiek laat de tijdreeks van de sensor in vergelijking met het meetstation zien."),
                    plotOutput("timeplot"),
                    h4("Toelichting"),
                    p("Als je een sensor aanklikt, zie je een tijdreeks van de uurlijkse sensorwaarden voor de geselecteerde periode.
                      Deze waarden worden vergeleken met het dichtstbijzijnde meetstation van het landelijk luchtmeetnet.
                      Dit maakt het mogelijk om de sensorwaarden snel te vergelijken met de referentiemetingen.",
                      style = "font-size:12px")
 )
          
  return(tp)
} 

tpKalender <- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Kalender",
                  helpText("Deze grafiek laat het gemiddelde van de (groep) sensor(en) zien per dag."),
                      plotOutput("calendar"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt de gemiddelde concentratie per dag getoond in een standaard kalenderformaat.
                        Dit maakt het mogelijk om snel inzicht te krijgen op welke dagen de concentraties hoog (of laag) waren.
                        Op dit moment worden de kleuren gekozen op basis van een schaal van 0 tot 150. 
                        Concentraties in de buurt van de 150 worden donkerpaars.",
                        style = "font-size:12px")
                    
  )
  return(tp)
} 

tpTimevariation <- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Gemiddelden",
                  helpText("Deze grafieken laten het gemiddelde zien voor verschillende tijdsperioden per sensor of sensorgroep."),
                      plotOutput("timevariation"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt de gemiddelde concentratie per tijdsperiode getoond. 
                        De bovenste grafiek laat de gemiddelde uurwaarde, uitgesplitst naar weekdag, zien.
                        Onder zie je de gemiddelde concentratie op elk uur van de dag (links), in het midden zie je de gemiddelde concentratie per maand en
                        rechts zie je de gemiddelde concentratie per dag van de week.", 
                        style = "font-size:12px")
                  )
  
  return(tp)
} 

tpWindRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Windroos",
                  helpText("Deze grafiek toont per windrichting hoe vaak en hoe hard de wind waaide per sensor of sensorgroep."),
                      plotOutput("windplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een windroos getoond.
                        Deze windroos laat de windsnelheid en -richting zien van het dichtstbijzijnde KNMI-station. Voor elke windsector toont de 
                        grafiek in hoeveel procent van de tijd de wind vanuit die richting waaide.  
                        De gekleurde blokken geven de windsnelheid aan. Bijvoorbeeld: wanneer de wind voornamelijk uit het zuidwesten komt, 
                        zie je de langste blokken linksonder (tussen zuid en west in). Als je wilt weten hoe hard de wind waaide, 
                        bekijk je de kleur van de blokken. Hoe donkerder de kleur, hoe harder de wind.", 
                        style = "font-size:12px")
                  )
  
  return(tp)
} 


tpPercentileRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Pollutieroos",
                  helpText("Deze grafiek toont de gemiddelde concentratie per windrichting per sensor of sensorgroep."),
                  plotOutput("percentileplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een pollutieroos getoond. 
                                   Deze toont per windsector het gemiddelde van de sensormetingen wanneer de wind uit die richting waaide.",
                        style = "font-size:12px"
                      )
                  )
  
  return(tp)
} 

tpPollutionRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Pollutieroos (%)",
                  helpText("Deze grafiek toont per windrichting de relatieve bijdrage aan de totale gemiddelde concentratie per sensor of sensorgroep."),
                      plotOutput("pollutionplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een gewogen pollutieroos getoond. 
                                   Deze berekent per windsector het aandeel (in %) van deze sector in de totale gemiddelde concentratie.
                                   De gemiddelde concentratie per sector wordt hiervoor gewogen naar hoe vaak deze windrichting voorkomt.
                                   ",
                        style = "font-size:12px")
                  )
  
  return(tp)
} 
