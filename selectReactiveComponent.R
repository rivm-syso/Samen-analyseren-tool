## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het functie script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: april 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## In dit script worden 2 functies gemaakt die het interactieve gedeelte
## van de componentkeuze en de slider maken.
## ---------------------------------------------------------

selectReactiveComponent <- function(input){ 
  
comp <- switch(input$Var, 
               "PM10" = "pm10",
               "PM10 - gekalibreerd" = "pm10_kal",
               "PM2.5" = "pm25",
               "PM2.5 - gekalibreerd" = "pm25_kal")

  return(comp)
} 


selectReactiveDates <- function(input){ 
  
  dates_reactive <- reactive({
    start <-   format(as.POSIXct(input$TimeRange[1],format='%Y-%m-%d %H:%M:%S'),format='%d/%m/%Y')
    end <- format(as.POSIXct(input$TimeRange[2],format='%Y-%m-%d %H:%M:%S'),format='%d/%m/%Y')
    
    combo <- list(start = start, end = end)
    combo
  })
  
} 
  