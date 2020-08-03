## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het tabPanelsData script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: juli 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## In dit script worden de verschillende tabbladen gemaakt voor het ophalen en instellen van de data
## ---------------------------------------------------------

tpData <- function(){
  library(shiny)
  
  tp <-  tabPanel("Data laden en downloaden",
                  tabsetPanel(
                    # tabPanel("Start",
                    #          p("Welkom bij de Samen Anlayseren tool. 
                    #            Klik op de button om de tool te testen met een voorbeeld data set. Wilt u de sensoren binnen uw eigen gemeente of project bekijken. Volg dan de stappen in de andere tabbladen."),
                    #          p("Real-time metingen van de sensoren kunt u vinden op het Samen Meten Dataportaal:", a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank')),
                    #          # Button om terug te gaan naar de standaard voorbeeld data
                    #          actionButton("voorbeeld_data","Laad voorbeeld data")),
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
                             # Input: Blokjes voor de datum
                             dateInput("DateStart_tpData", label="Selecteer begin tijdreeks:", format='dd-mm-yyyy',value = '2019-01-01'
                             ),
                             dateInput("DateEind_tpData", label="Selecteer einde tijdreeks:", format='dd-mm-yyyy', value = '2019-01-08' 
                             ),
                             # Button om de gegevens van de sensore op t halen via de API en om de sensor data te downloaden
                             downloadButton("downloadData_sensor",'Download de metingen van de sensoren'),
                             # Mogelijkheid om je eigen data in te laden:
                             fileInput("eigen_datafile_sensoren", "Laad dataset sensormetingen(csv-bestand): ",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
                             ),
                    tabPanel("Stap 2: Luchtmeetnet",
                             helpText("Laad eerst de locaties van de meetstations van Luchtmeetnet. 
                                      Selecteer daarna de stations waarvan u de meetgegevens wilt opvragen.
                                      De opgevraagde gegevens worden direct getoond. U kunt ze ook downloaden 
                                      en op een ander moment weer inladen, 
                                      zodat u niet elke keer hoeft te wachten.
                                      LET OP/TODO dit werkt nog niet allemaal"),
                            # Button om de luchtmeetnetsations op de kaart te zetten
                            actionButton("show_luchtmeetnet", "Laad locaties luchtmeetnetstations"),
                            # Output: tabel met de geslecteerde LML station, voor het downloaden van de data
                            tableOutput("stations_lml"),
                            # Output: met de begin en eind datum erin, kan in stap1 worden ingesteld
                            textOutput('tijdreeks_tpdata_lml'),
                            # Button de gegevens van de luchtmeetnetstations op et halen via de API en om de LML data te downloaden
                            downloadButton("downloadData_luchtmeetnet",'download metingen luchtmeetnetstations'),
                            # Mogelijkheid om je eigen data in te laden:
                            fileInput("eigen_datafile_lml", "Laad dataset luchtmeetnetmetingen(csv-bestand): ",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            p("Selecteer luchtmeetnetstations om de data op te halen. De eigenaar van het station staat erbij. DCMR, GGD Amsterdam, Provincie Limburg, ODRA, LML"),
                            h4("Toelichting"),
                            p("",style = "font-size:12px")
                    ),
                    tabPanel("Stap 3: KNMI",
                             helpText("Laad eerst de locaties van de meetstations van het KNMI.
                                      Selecteer daarna de stations waarvan u de meetgegevens wilt opvragen.
                                      De opgevraagde gegevens worden direct getoond. U kunt ze ook downloaden
                                      en op een ander moment weer inladen,
                                      zodat u niet elke keer hoeft te wachten.
                                      LET OP/TODO dit werkt nog niet allemaal"),
                             # Button om de luchtmeetnetsations op de kaart te zetten
                             actionButton("show_knmi", "Laad locaties knmi-stations"),
                             # # Output: tabel met de geslecteerde knmi station, voor het downloaden van de data
                             tableOutput("stations_knmi"),
                             # Output: met de begin en eind datum erin, kan in stap1 worden ingesteld
                             textOutput('tijdreeks_tpdata_knmi'),
                             # # Button om de gegevens van de knmi-stations op te halen via de API en om de knmi data te downloaden
                             downloadButton("downloadData_knmi",'download metingen knmi-stations'),
                             # Mogelijkheid om je eigen data in te laden:
                             fileInput("eigen_datafile_knmi", "Laad dataset knmi-metingen(csv-bestand): ",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h4("Toelichting"),
                             p("Zie knmi website voor meer informatie",style = "font-size:12px")
                             )
    )
  )
  
  return(tp)
} 
