## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het tabPanels script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: juli 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## In dit script worden de verschillende tabbladen gemaakt die gebruikt kunnen worden voor de analyse en visualisatie
## ---------------------------------------------------------

tpAnalyse <- function(){
  library(shiny)
  
  tp <-  tabPanel("Visualisatie en Analyse",
                  tabsetPanel(
                    tpTimeplot(),
                    tpBarplot(),
                    tpKalender(),
                    tpTimevariation(),
                    tpPercentileRose(),
                    tpPollutionRose(),
                    tpWindRose()
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

tpBarplot <- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Barplot",
                  helpText("Selecteer een sensor. Deze grafiek laat de tijdreeks van de sensor in vergelijking met het meetstation zien."),
                  plotOutput("barplot"),
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
                  p("LET OP: als het KNMI-station geen gegevens over de wind heeft, kan er geen windroos worden getoond."),
                  
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
                  p("LET OP: als het KNMI-station geen gegevens over de wind of de sensor alleen 0 gemeten heeft, is deze pollutieroos vreemd. Check of er een windroos voor dit KNMI-station is. Check in de tijdreeks of de sensor metingen boven de 0 heeft."),
                  textOutput('knmi_stat_wdws_2'),
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
                  p("LET OP: als het KNMI-station geen gegevens over de wind heeft, kan er geen pollutieroos (%) worden getoond. Check of er een windroos voor dit KNMI-station is."),
                  textOutput('knmi_stat_wdws'),
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
