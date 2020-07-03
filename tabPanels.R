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

tpTimeplot <- function(){
  
library(shiny)
  
 tp <-  tabPanel("Tijdreeks",
                    plotlyOutput("timeplot"),
                    h4("Toelichting"),
                    p("Als je een sensor aanklikt, zie je een tijdreeks van de uurlijkse sensorwaarden voor de geselecteerde periode.
                      Deze waarden worden vergeleken met het dichtstbijzijnde meetstation van het landelijk luchtmeetnet.
                      Dit maakt het mogelijk om de sensorwaarden snel te vergelijken met de referentiemetingen.
                      ",
                      style = "font-size:12px")
 )
          
  return(tp)
} 

tpKwalindex <- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Kwaliteitsindex",
                  plotlyOutput("kwalindex"),
                  h4("Toelichting"),
                  p("",
                    style = "font-size:12px")
  )
  
  return(tp)
} 

tpKalender <- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Kalender",
                     
                      helpText("Deze grafiek laat het gemiddelde van de aangeklikte sensoren zien"),
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
                      plotOutput("timevariation"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt de gemiddelde concentratie per tijdsperiode getoond. 
                        De bovenste grafiek laat de gemiddelde uurwaarde, uitgesplitst naar weekdag, zien.
                        Onder zie je de gemiddelde concentratie op elk uur van de dag (links), in het midden zie je de gemiddelde concentratie per maand en
                        rechts zie je de gemiddelde concentratie per dag van de week.
                        ")
                  )
  
  return(tp)
} 

tpWindRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Windroos",
                      plotOutput("windplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een windroos getoond.
                        Deze windroos laat de windsnelheid en -richting zien van het dichtstbijzijnde KNMI-station. Voor elke windsector toont de 
                        grafiek in hoeveel procent van de tijd de wind vanuit die richting waaide.  
                        De gekleurde blokken geven de windsnelheid aan. Bijvoorbeeld: wanneer de wind voornamelijk uit het zuidwesten komt, 
                        zie je de langste blokken linksonder (tussen zuid en west in). Als je wilt weten hoe hard de wind waaide, 
                        bekijk je de kleur van de blokken. Hoe donkerder de kleur, hoe harder de wind.")
                  )
  
  return(tp)
} 


tpPercentileRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Pollutieroos",
                  
                      plotOutput("percentileplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een pollutieroos getoond. 
                                   Deze toont per windsector het gemiddelde van de sensormetingen wanneer de wind uit die richting waaide."
                      )
                  )
  
  return(tp)
} 

tpPollutionRose<- function(){
  
  library(shiny)
  
  tp <-  tabPanel("Pollutieroos (%)",
                  
                      plotOutput("pollutionplot"),
                      h4("Toelichting"),
                      p("Als je een sensor aanklikt, wordt een gewogen pollutieroos getoond. 
                                   Deze berekent per windsector het aandeel (in %) van deze sector in de totale gemiddelde concentratie.
                                   De gemiddelde concentratie per sector wordt hiervoor gewogen naar hoe vaak deze windrichting voorkomt.
                                   ")
                  )
  
  return(tp)
} 
