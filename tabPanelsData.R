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
  tp <-  shiny::tabPanel("Data laden en downloaden",
                  tabsetPanel(
                    tabPanel(title=div("Stap 1: Sensoren ", img(src="symbol_sensor_black.svg")),
                             helpText("Selecteer hieronder eerst het project of de gemeente waarvan u de data van de sensoren wilt opvragen.
                                      Vraag daarna de gegevens op. LET OP: het opvragen van de gegevens kan enkele minuten tot een half uur duren.
                                      De opgevraagde data wordt direct getoond als sensoren op de kaart. U kunt de data vervolgens downloaden en op een ander moment weer inladen. 
                                      Zo hoeft u niet elke keer dat u de tool gebruikt, de data opnieuw op te halen."),
                             # Kies eerst waarvan je data wilt selecteren: gemeente of project
                             selectInput(inputId = "sensor_hoofdgroep", label = "Selecteer op project of gemeente:", choices = hoofd_choices, selected = 'gemeente', multiple = FALSE,
                                         selectize = TRUE, width = NULL, size = NULL),
                             # Specificeer waarvan de data gedownload kan worden (de choices worden reactive gemaakt door de selectinput hierboven)
                             selectInput(inputId = "sensor_specificeer", label = "Maak uw keuze:", choice="",selected = NULL, multiple = FALSE,
                                         selectize = TRUE, width = NULL, size = NULL),                   
                             # Input: Blokjes voor de datum, met als startdatum 2 weken geleden en einddatum vandaag
                             dateInput("DateStart_tpData", label="Selecteer begin tijdreeks:", format='dd-mm-yyyy', value = Sys.Date()-14
                             ),
                             dateInput("DateEind_tpData", label="Selecteer einde tijdreeks:", format='dd-mm-yyyy', value = Sys.Date(), min = Sys.Date()-13
                             ),
                             # Button om de gegevens van de sensore op t halen via de API en om de sensor data te downloaden
                             div(style="padding-top: 0px;", downloadButton("downloadData_sensor",'Haal de metingen van de sensoren op en download')),
                             # Button om de gegevens van de sensore op t halen via de API en om de sensor data te downloaden
                             div(style="padding-top: 5px;", downloadButton("downloadData_sensor2",'Extra: download de sensoren van de kaart')),
                             hr(),
                             # Mogelijkheid om je eigen data in te laden:
                             div(style="padding-top: 10px;",fileInput("eigen_datafile_sensoren", "Laad dataset sensormetingen(csv-bestand): ",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))),
                             h4("Toelichting"),
                             p("De data van de sensoren wordt opgehaald met de API van het Samen Meten dataportaal. Meer informatie is te vinden op:  ", a("Samenmeten.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank')
                               ,style = "font-size:12px")
                             ),
                    tabPanel(title=div("Stap 2: Luchtmeetnet ",img(src="symbol_lml_black.svg")),
                             helpText("Laad eerst de locaties van de meetstations van Luchtmeetnet via de button. 
                                      Selecteer daarna de stations waarvan u de meetgegevens wilt opvragen.
                                      De opgevraagde data wordt direct getoond. Meteen kunt u ze ook downloaden 
                                      en op een ander moment weer inladen, 
                                      zodat u niet elke keer hoeft te wachten."),
                            # Button om de luchtmeetnetsations op de kaart te zetten
                            actionButton("show_luchtmeetnet", "Laad locaties luchtmeetnetstations"),
                            # Output: tabel met de geslecteerde LML station, voor het downloaden van de data
                            p('Geselecteerde stations:'),
                            tableOutput("stations_lml"),
                            # Output: met de begin en eind datum erin, kan in stap1 worden ingesteld
                            textOutput('tijdreeks_tpdata_lml'),
                            # Button de gegevens van de luchtmeetnetstations op et halen via de API en om de LML data te downloaden
                            div(style="padding-top: 5px;", downloadButton("downloadData_luchtmeetnet",'Haal de metingen luchtmeetnetstations op en download')),
                            # Button om de gegevens van de sensore op t halen via de API en om de sensor data te downloaden
                            div(style="padding-top: 5px;", downloadButton("downloadData_luchtmeetnet2",'Extra: download de luchtmeetnetstations van de kaart')),
                            hr(),
                            # Mogelijkheid om je eigen data in te laden:
                            div(style="padding-top: 5px;",fileInput("eigen_datafile_lml", "Laad dataset luchtmeetnetmetingen(csv-bestand): ",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))),
                            h4("Toelichting"),
                            p("De gegevens worden opgehaald met de API van luchtmeetnet. Meer informatie over de gegevens is te vinden op:  ", a("luchtmeetnet.nl", href ='https://www.luchtmeetnet.nl/', target = 'blank')
                              ,style = "font-size:12px")
                            
                    ),
                    tabPanel(title=div("Stap 3: KNMI ",img(src="symbol_knmi_black.svg")),
                             helpText("Laad eerst de locaties van de meetstations van het KNMI via de button.
                                      Selecteer daarna de stations waarvan u de meetgegevens wilt opvragen.
                                      De opgevraagde data wordt direct getoond. Meteen kunt u ze ook downloaden
                                      en op een ander moment weer inladen,
                                      zodat u niet elke keer hoeft te wachten."),
                             
                             # h3("Door technische problemen bij het KNMI is het downloaden van deze data momenteel niet mogelijk."),
                             br(),
                             # Button om de luchtmeetnetsations op de kaart te zetten
                             actionButton("show_knmi", "Laad locaties knmi-stations"),
                             # # Output: tabel met de geslecteerde knmi station, voor het downloaden van de data
                             p('Geselecteerde stations:'),
                             tableOutput("stations_knmi"),
                             # Output: met de begin en eind datum erin, kan in stap1 worden ingesteld
                             textOutput('tijdreeks_tpdata_knmi'),
                             # # Button om de gegevens van de knmi-stations op te halen via de API en om de knmi data te downloaden
                             div(style="padding-top: 5px;", downloadButton("downloadData_knmi",'Haal de metingen KNMI-stations op en download')),
                             # Button om de gegevens van de sensore op t halen via de API en om de sensor data te downloaden
                             div(style="padding-top: 5px;", downloadButton("downloadData_knmi2",'Extra: download de KNMI-stations van de kaart')),
                             hr(),
                             # Mogelijkheid om je eigen data in te laden:
                             div(style="padding-top: 5px;", fileInput("eigen_datafile_knmi", "Laad dataset knmi-metingen(csv-bestand): ",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))),
                             h4("Toelichting"),
                             p("De gegevens worden opgehaald van het KNMI. Meer informatie over de gegevens is te vinden via het ", a("KNMI", href = 'https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script', target='blank')
                               ,style = "font-size:12px")
    )
                  ))
  
  return(tp)
} 
