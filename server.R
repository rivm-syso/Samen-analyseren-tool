## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het server.R script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: april 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## 
## ---------------------------------------------------------


function(input, output, session){ 
  
  
  ## INITIALISATIE ----
  # Generate base map----
  # Hierop staan de knmi-stations, de luchtmeetnetstations en de sensoren
  # Daarnaast zijn er edit buttons toegevoegd
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(5.12446,52.105, zoom = 6) %>%
      # addMarkers(icon = icons_stations["knmi"],data = knmi_stations, ~lon, ~lat, layerId = ~code, label = lapply(knmi_labels, HTML)) %>%
      # addMarkers(icon = icons_stations["lml"], data = lml_stations, ~lon, ~lat, layerId = ~code, label = lapply(lml_labels, HTML)) %>%
      # addCircleMarkers(data = sensor_unique, ~lon, ~lat, layerId = ~kit_id, label = lapply(sensor_labels, HTML),
      #                  radius = 8, color = ~kleur, fillOpacity = 1, stroke = ~selected, group = "sensoren")%>%
      addDrawToolbar(
        targetGroup = 'Selected',
        polylineOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'black'
                                                                              ,weight = 1.5)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })

  # Zet reactive dataframe op----
  values <- reactiveValues(df = sensor_unique, sensor_data = input_df,groepsnaam = geen_groep,
                           df_gem = data.frame(), startdatum = 0, einddatum=0,data_set = 'voorbeeld')
  overzicht_shapes <- reactiveValues(add = 0, delete = 0) # nodig om selectie ongedaan te maken

  lml_stations_reactive <- reactiveValues(statinfo=lml_stations_all, lml_data = input_df_lml)
  knmi_stations_reactive <- reactiveValues(statinfo=knmi_stations_all, knmi_data = input_df_knmi)
  
  choices_api_reactive <- reactiveValues(choices=gemeente_choices)

  ## FUNCTIES ----
  
  # Functie: Set the sensor as deselect and change color to base color
  set_sensor_deselect <- function(id_select){
    values$df[values$df$kit_id == id_select, "selected"] <- FALSE 
    values$df[values$df$kit_id == id_select, "huidig"] <- FALSE 
    values$df[values$df$kit_id == id_select, "kleur"] <- kleur_marker_sensor
    values$df[values$df$kit_id == id_select, "groep"] <- geen_groep
  }
  
  # Functie: Set sensor as select and specify color
  set_sensor_select <- function(id_select){
    values$df[values$df$kit_id == id_select, "selected"] <- TRUE
    values$df[values$df$kit_id == id_select, "huidig"] <- TRUE
    # Select een kleur en geef dit mee aan de sensor
    # Kies de eerste kleur in de lijst kleur_cat die aanwezig is
    count  <- 1
    # Zorg ervoor dat je blijft zoeken tot sensor een kleur heeft of dat de kleuren op zijn
    while (kleur_sensor == "leeg" & count < length(kleur_cat)){
      for (kleur_code in kleur_cat){
        if (kleur_code %in% unique(values$df$kleur)){
          count <- count + 1
          next # Als de kleur al is toebedeeld, sla deze dan over
        }else{ 
          kleur_sensor <- kleur_code # Vrije kleur voor de sensor
        }
      }
    }
    # Als alle kleuren gebruikt zijn: kies zwart
    if (count == length(kleur_cat)){
      kleur_sensor <- "black"
    }
    
    # Geef kleur aan de sensor
    values$df[values$df$kit_id == id_select, "kleur"] <- kleur_sensor
    kleur_sensor <- "leeg"
  }
  
  # Functie: Set the LML stations as deselect and change color to base color
  #TODO dit is nog niet af, maak iets met de symbolen in een andere kleur
  set_lml_station_deselect <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "selected"] <- FALSE 
    }
  
  # Functie: Set station as select and specify color
  #TODO dit is nog niet af, maak iets met de symbolen in een andere kleur
  set_lml_station_select <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "selected"] <- TRUE
    # Select een kleur en geef dit mee aan de station
    # Geef kleur aan de station
    # values$df[values$lml$kit_id == id_select, "kleur"] <- kleur_sensor
  }
  
  # Functie: Set the stations as deselect and change color to base color
  #TODO dit is nog niet af, maak iets met de symbolen in een andere kleur
  set_knmi_station_deselect <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$statcode == id_select, "selected"] <- FALSE 

  }
  
  # Functie: Set station as select and specify color
  #TODO dit is nog niet af, maak iets met de symbolen in een andere kleur
  set_knmi_station_select <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$statcode == id_select, "selected"] <- TRUE 
    
      # Select een kleur en geef dit mee aan de station
      
      # Geef kleur aan de station
      # values$df[values$knmi$kit_id == id_select, "kleur"] <- kleur_sensor
  }
  
  # Functie: plaats sensoren met juiste kleur op de kaart  
  add_sensors_map <- function(){ 
    # Regenerate the sensors for the markers
    sensor_loc <- unique(select(values$df, kit_id, lat, lon, kleur, selected))
    
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("sensoren") # Clear sensor markers
    proxy %>% addCircleMarkers(data = sensor_loc, ~lon, ~lat, layerId = ~kit_id, label = lapply(as.list(sensor_loc$kit_id), HTML),
                               radius = 8, color = ~kleur, fillOpacity = 1,stroke = ~selected, group = "sensoren")}
  
  # TODO: Universele namen voor de statcode etc van lml en knmi, nu in lmldata anders dan in lml idem knmi  
  # TODO: ook de labels van KNMi en LML stations mooi maken, met eigenaar erbij etc.
  add_knmistat_map <- function(){
    # Regenerate the knmi stations for the markers
    station_loc <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$hasdata==T),]
    print(paste0("stations op kaart: ", station_loc))
    print(names(station_loc))
    # Update map with new markers to show selected
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("knmistations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~nummer, label = lapply(as.list(station_loc$nummer), HTML),
                         icon = icons_stations["knmi"], group = "knmistations")}

  # Functie: plaats lml stations  op de kaart  
  add_lmlstat_map <- function(){ 
    # Regenerate the lml stations for the markers
    station_loc <- lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$hasdata==T),]
    print(paste0("stations op kaart: ", station_loc))
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("luchtmeetnetstations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~statcode, label = lapply(as.list(station_loc$statcode), HTML),
                               icon = icons_stations["lml"], group = "luchtmeetnetstations")}
  
  # Functie om de zoom/view te centreren rond de sensoren
  set_view_map <- function(lat, lon){
    # create a proxy map and zoom to the location
    proxy <- leafletProxy('map')
    proxy %>% setView(lon, lat, zoom = 10)
  }
  
  # Functie om van alle groepen in de dataset een gemiddelde te berekenen  
  calc_groep_mean <- function(){
    # LET OP: wind moet via vectormean. Zie openair timeAverage
    gemiddeld_all <- data.frame()
    for(groepen in unique(values$df$groep)){
      if (groepen != geen_groep){
        # Haal de kit_ids van de sensoren in de groep op
        sensor_groep <- values$df[which(values$df$groep == groepen),'kit_id']
        # Zoek de gegevens van de groep op
        te_middelen <- values$sensor_data[which(values$sensor_data$kit_id %in% sensor_groep),]
        # Bereken het gemiddelde van de groep. LET OP; vector middeling
        gemiddeld <- timeAverage(te_middelen, avg.time='hour', vector.ws=TRUE)
        gemiddeld$kit_id <- groepen
        gemiddeld_all <- rbind(gemiddeld_all,gemiddeld)
      }} 
    # Maak de gemiddeld_all de reactive
    values$df_gem <- gemiddeld_all
  }

  # Functie om de voorbeeld data in te laden
  insert_voorbeeld_data <- function(){
    # Deze functie haalt de voorbeelddataset op en laat deze zien op de kaart
    # TODO maak die in het format van de data zoals ook de rest, dus in 3 delen.
    print('voorbeeld')
    values$sensor_data <- readRDS(file)
    
    ## Default locatie, kleur en label opzetten
    values$sensor_data$kit_id <- gsub('HLL_hl_', '', values$sensor_data$kit_id) #remove HLL-string from values$sensor_data for shorter label
    # Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
    sensor_unique <- aggregate(values$sensor_data[,c('lat','lon')], list(values$sensor_data$kit_id), FUN = mean) # gemiddelde om per sensor een latlon te krijgen
    names(sensor_unique)[names(sensor_unique)=='Group.1'] <-'kit_id'
    sensor_unique$selected <-FALSE
    sensor_unique$huidig <- FALSE
    sensor_unique$groep <- geen_groep
    sensor_unique$kleur <- kleur_marker_sensor
    sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info
    
    # Voor de multiselect tool: omzetten lat/lon naar spatialpoints
    ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)
    
    # Voeg de sensor locaties ed toe aan interactive dataframe
    values$df <- sensor_unique
    # voeg de sensoren toe aan de kaart
    add_sensors_map()
    # zoom naar de nieuwe sensoren
    mean_lat <- mean(sensor_unique$lat)
    mean_lon <- mean(sensor_unique$lon)
    set_view_map(mean_lat, mean_lon)
  }
  
  # Functie om de sensor data in te laden
  insert_nieuwe_data_sensor <- function(){
    # Deze functie laadt de sensor data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    if(values$data_set=='API_samenmeten'){
      # Haal de gegevens op van de sensoren via de samenmeten API
      values$sensor_data <- get_sensor_data_api()

    }else if(values$data_set=='eigen_dataset_sensoren'){
      # Lees de csv uit en sla de gegevens op in interactive
      values$sensor_data <- read.csv(input$eigen_datafile_sensoren$datapath, sep=",")
      # Zet de date naar een posixct
      values$sensor_data$date <- as.POSIXct(values$sensor_data$date, tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }

    # Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
    sensor_unique <- aggregate(values$sensor_data[,c('lat','lon')], list(values$sensor_data$kit_id), FUN = mean) # gemiddelde om per sensor een latlon te krijgen
    names(sensor_unique)[names(sensor_unique)=='Group.1'] <-'kit_id'
    sensor_unique$selected <-FALSE
    sensor_unique$huidig <- FALSE
    sensor_unique$groep <- geen_groep
    sensor_unique$kleur <- kleur_marker_sensor
    sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info

      # Voor de multiselect tool: omzetten lat/lon naar spatialpoints
      ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

    # Voeg de sensor locaties ed toe aan interactive dataframe
    values$df <- sensor_unique
    # voeg de sensoren toe aan de kaart
    add_sensors_map()
    # zoom naar de nieuwe sensoren
    mean_lat <- mean(sensor_unique$lat)
    mean_lon <- mean(sensor_unique$lon)
    set_view_map(mean_lat, mean_lon)
  } 
  
  # Functie om de luchtmeetnet data in te laden
  insert_nieuwe_data_lml<- function(){ 
    # Deze functie laadt de luchtmeentnet data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    
    # Data die er al is wordt overschreven, dus zet de hasdata op FALSE
    lml_stations_reactive$statinfo$hasdata <- FALSE
    
    if(values$data_set=='API_luchtmeetnet'){
      # Haal de gegevens op van de stations via de luchtmeetnet API
      print('ophalen api luchtmeetnet')
      lml_stations_reactive$lml_data <- get_lml_data_api()
      
    }else if(values$data_set=='eigen_dataset_lml'){
       # Haal de gegevens op van de stations via de luchtmeetnet API
       print('ophalen api luchtmeetnet')

       # Lees de csv uit en sla de gegevens op in interactive
       lml_stations_reactive$lml_data <- read.csv(input$eigen_datafile_lml$datapath, sep=",")
       print(head(lml_stations_reactive$lml_data))
       # Zet de date naar een posixct
       lml_stations_reactive$lml_data$date <- as.POSIXct(lml_stations_reactive$lml_data$timestamp_measured , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
       print(summary(lml_stations_reactive$lml_data))
       }

    # Geef aan van welke stations nu databeschikbaar is:
    station_metdata <- unique(lml_stations_reactive$lml_data$station_number)
    lml_stations_reactive$statinfo$hasdata[which(lml_stations_reactive$statinfo$statcode %in% station_metdata)] <- TRUE
    # Laat vervolgens alleen de stations zien waarvan ook data beschikbaar is:
    add_lmlstat_map()
  }

  # Functie om de knmi data in te laden
  insert_nieuwe_data_knmi <- function(){
    # Deze functie laadt de knmi data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    # Data die er al is wordt overschreven, dus zet de hasdata op FALSE
    knmi_stations_reactive$statinfo$hasdata <- FALSE
    if(values$data_set=='API_knmi'){
      # Haal de gegevens op van de stations via de KNMI API
      print('ophalen api KNMI')
      knmi_stations_reactive$knmi_data <- get_knmi_data_api()
    }else if(values$data_set=='eigen_dataset_knmi'){
      # Haal de gegevens op van de stations vanuit een ingelezen bestand
      print('ophalen uit bestand KNMI')
      # Lees de csv uit en sla de gegevens op in interactive
      knmi_stations_reactive$knmi_data <- read.csv(input$eigen_datafile_knmi$datapath, sep=",")
      print(head(knmi_stations_reactive$knmi_data))
      # Zet de date naar een posixct
      knmi_stations_reactive$knmi_data$date <- as.POSIXct(knmi_stations_reactive$knmi_data$tijd , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
      print(summary(knmi_stations_reactive$knmi_data))
    }
     # Geef aan van welke stations nu databeschikbaar is:
     station_metdata <- unique(knmi_stations_reactive$knmi_data$STNS)
     print(station_metdata)
     knmi_stations_reactive$statinfo$hasdata[which(knmi_stations_reactive$statinfo$nummer %in% station_metdata)] <- TRUE
     # Laat vervolgens alleen de stations zien waarvan ook data beschikbaar is:
     add_knmistat_map()
 }
    
  # Ophalen van de luchtmeetnet data vanuit een API
  get_lml_data_api <- function(){
    # Deze functie roept de API functies aan om de gegevens uit de API op te halen.
    # Tevens zit hier een progress bar in verwerkt met een update functie
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    # Voor het maken van een progresbar voor het laden van de data via de lmlapi
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Ophalen van de gegevens", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    ## Om de gegevens van het Luchtmeetnet op te halen
    # Maak een dataframe waar het in past
    station_data_all <- data.frame()
    # Maak een lijst van de statcodes die je wilt ophalen
    lml_stats <- lml_stations_reactive$statinfo$statcode[which(lml_stations_reactive$statinfo$selected==T)]
    print(lml_stats)
    
    # Ga elk station af en haal de gegevens op uit de API
    for(ind in seq(1,length(lml_stats))){
      stat <- lml_stats[ind]
      # Voor de progress messages
      text <- paste0("Luchtmeetnet station: ", stat, " - stap (",ind,"/",length(lml_stats) ,")")
      updateProgress(value = (ind/length(lml_stats))*0.8, detail = text)
      
      # Haal voor elk station de data op van luchtmeetnet API
      station_data_ruw <- GetLMLAPI(stat, format(values$startdatum, '%Y%m%d'), format(values$einddatum, '%Y%m%d'))
      # Voeg alle meetwaardes vam de stations samen
      station_data_all <- rbind(station_data_all, station_data_ruw$data)}
    
    # Alle gegevens zijn van de api opgehaald, retun de totale set
    updateProgress(value = 0.90 ,detail = "Alle data van Luchtmeetnet opgehaald.")
    #return de totale dataset
    return(station_data_all)
  }
  
  # Ophalen van de KNMI data vanuit een API
  get_knmi_data_api <- function(){
    # Deze functie roept de API functies aan om de gegevens uit de API op te halen.
    # Tevens zit hier een progress bar in verwerkt met een update functie
    
    # Voor het maken van een progresbar voor het laden van de data via de KNMI api
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Ophalen van de gegevens", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    #Maak een dataframe waar het in past
    station_data_all <- data.frame()
    #Maak een string van de statcodes die je wilt ophalen
    knmi_stats <- knmi_stations_reactive$statinfo$nummer[which(knmi_stations_reactive$statinfo$selected==T)]
    print(paste0('API ophalen: ',knmi_stats))
    
    # Voor de progress message
    progress$set(message = paste0("van stations: ", knmi_stats), value = 0.3)
    
    # ophalen van de gegevens via de API
    station_all_data <- GetKNMIAPI(knmi_stats,format(values$startdatum, '%Y%m%d'), format(values$einddatum, '%Y%m%d'))
    
    # Je hebt voor nu alleen het data deel van de gegevens uit de api nodig
    station_all_data <- station_all_data$data
    
    # Voor de progress message dat het bijna is afgerong
    progress$set(message = "Gegevens opgehaald", value = 1)
    
    # Return de data
    return(station_all_data)
  }
 
 # Ophalen van de sensor data vanuit een API
  get_sensor_data_api <- function(){
   # Deze functie roept de API functies aan om de gegevens uit de API op te halen.
   # Tevens zit hier een progress bar in verwerkt met een update functie
   
   # Voor het maken van een progresbar voor het laden van de data via de samenmetenAPI
   # Create a Progress object
   progress <- shiny::Progress$new()
   progress$set(message = "Ophalen van de gegevens", value = 0)
   # Close the progress when this reactive exits (even if there's an error)
   on.exit(progress$close())
   
   # Create a callback function to update progress.
   # Each time this is called:
   # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
   #   distance. If non-NULL, it will set the progress to that value.
   # - It also accepts optional detail text.
   updateProgress <- function(value = NULL, detail = NULL) {
     if (is.null(value)) {
       value <- progress$getValue()
       value <- value + (progress$getMax() - value) / 5
     }
     progress$set(value = value, detail = detail)
   }
   
    # Maak een lege lijst aan. Hier worden alle gegevens in opgeslagen en 
    # doorgegeven aan de helperfuncties,
    # zodat die ook alle gegevens ter beschikking hebben.
    data_opslag_list <- list()
  
    # Vanuit de gebruiker wordt aangegeven welke gegevens moeten worden opgehaald.
    # hier wordt het stukje van de url voor project en gemeente gezet.
    if(input$sensor_hoofdgroep=='project'){
      projectnaam <- paste0("project eq'",input$sensor_specificeer,"'")
      
    }else if(input$sensor_hoofdgroep=='gemeente'){
      projectnaam <- paste0("codegemeente eq'",input$sensor_specificeer,"'")}
    
    print('aanroepen api')
    # Aanroepen van de API
    sensor_data_ruw <- GetSamenMetenAPI(projectnaam, format(values$startdatum, '%Y%m%d'), 
                                        format(values$einddatum, '%Y%m%d'), data_opslag_list,
                                        updateProgress) 
    print('api opgehaald:')
    print(summary(sensor_data_ruw))
    # Dit bestaat uit 2 delen: 
    # sensordata die kan worden gebruikt voor de markers(values$df) 
    # metingen met de meetwaardes voor de grafieken (alleen de sensormetingen) dus deel van input_df
    # Deze worden samengevoegd in 1 wide tabel
    sensor_data_metingen <- distinct(sensor_data_ruw$metingen)
    sensor_data_all <- merge(sensor_data_metingen, sensor_data_ruw$sensordata[,c('kit_id','lat','lon')],
                             all.x, by.x='kit_id', by.y='kit_id')
    
    print('data samenvoegen:')
  
    # Omzetten naar een wide dataframe, dat er per kit_id en timestamp 1 rij data is
    sensor_data_all_wide <- pivot_wider(distinct(sensor_data_all),names_from = 'grootheid', values_from='waarde')
    
    print(head(sensor_data_all_wide))
    print(tail(sensor_data_all_wide))
    
    # Hernoemen van de tijd, zodat hetzelfde is als de input_df
    names(sensor_data_all_wide)[names(sensor_data_all_wide) == "tijd"] <- "date"
    #TODO: de error kolom weglaten.
    updateProgress(100, 'Alles geladen')
    return(sensor_data_all_wide)
  }


 
  ## OBSERVE EVENTS ----
  # Check waarop de data geselecteerd wordt: zet de choices klaar -gemeente of -project ----
  observeEvent({input$sensor_hoofdgroep},{
    if(input$sensor_hoofdgroep=='gemeente'){
      choices_api_reactive$choices <- gemeente_choices
    }else{
      choices_api_reactive$choices <- project_choices
    }
  })
  
  # Haal de choices in de SelectInput voor de sensoren API----
  observe({
    updateSelectInput(session, "sensor_specificeer",choices = choices_api_reactive$choices
    )})
  #Print welke datagroep bij samenmeten api wordt opgevraagd
  observeEvent({input$sensor_specificeer},{
    print(input$sensor_specificeer)
    print(input$sensor_hoofdgroep)
  })
  
  # observe of er een eigen data set is ingeladen voor de sensoren:----
  observeEvent({req(input$eigen_datafile_sensoren)},{
    values$data_set <- input$eigen_datafile_sensoren$datapath  
    values$data_set <- 'eigen_dataset_sensoren'
    print("databestand sensoren geladen")
    insert_nieuwe_data_sensor()})
  
  # observe of er een eigen data set is ingeladen voor de luchtmeetnetmetingen:----
  observeEvent({req(input$eigen_datafile_lml)},{
    values$data_set <- input$eigen_datafile_lml$datapath  
    values$data_set <- 'eigen_dataset_lml'
    print("databestand lml geladen")
    insert_nieuwe_data_lml()})
  
  # observe of er een eigen data set is ingeladen voor de knmi metingen:----
  observeEvent({req(input$eigen_datafile_knmi)},{
    values$data_set <- input$eigen_datafile_knmi$datapath  
    values$data_set <- 'eigen_dataset_knmi'
    print("databestand knmi geladen")
    insert_nieuwe_data_knmi()})  
  
  # Observe of de voorbeeld dataset weer ingeladen moet worden----
  observeEvent({input$voorbeeld_data},{
    values$data_set <- 'voorbeeld'
    insert_voorbeeld_data()})
  
  # Observe of de specifieke sensor dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_samenmeten},{
    values$data_set <- 'API_samenmeten'
    insert_nieuwe_data_sensor()})
  
  # Observe of de specifieke luchtmeetnet stations dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_luchtmeetnet},{
    print("Eerste aanroep api luchtmeetnet")
    values$data_set <- 'API_luchtmeetnet'
    insert_nieuwe_data_lml()})
  
  # Observe of de specifieke KNMI stations dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_knmi},{
    print("Eerste aanroep api knmi")
    values$data_set <- 'API_knmi'
    insert_nieuwe_data_knmi()
    })
  
  #Observe of de luchtmeetnetstations moeten worden getoond----
  observeEvent({input$show_luchtmeetnet},{
    print('laad zien')
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("luchtmeetnetstations")  # Clear sensor markers
    proxy %>% addMarkers(icon = icons_stations["lml"], data = lml_stations_reactive$statinfo, 
                         ~lon, ~lat, layerId = ~statcode, label = lapply(lml_labels, HTML),
                         group = 'luchtmeetnetstations')
    proxy %>% setView(5.12446,52.105, zoom = 6)
    print("op kaart getoond")
    })
  
  # #Observe of de knmi-stations moeten worden getoond----
  observeEvent({input$show_knmi},{
    print('laad zien')
    # Update map with new markers to show selected
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("knmistations")  # Clear sensor markers
    proxy %>% addMarkers(icon = icons_stations["knmi"], data = knmi_stations_reactive$statinfo,
                         ~lon, ~lat, layerId = ~statcode, label = lapply(knmi_labels, HTML),
                         group = 'knmistations')
    proxy %>% setView(5.12446,52.105, zoom = 6)
    print("op kaart getoond")
  })
  
  # Observe of de tekst wordt aangepast ----
  # Dan wil je dat er een nieuwe groep wordt aangemaakt
  # Bijvoorbeeld: je hebt een groep "Wijk aan Zee" aangemaakt, en je begint een nieuwe naam te typen "IJmuiden". 
  # Deze groep moet dan nieuw aangemaakt worden "IJmuiden".
  observeEvent({input$Text_groep},{
      values$groepsnaam <- input$Text_groep
    })
  
  observeEvent({input$bestaande_groep},{
    values$groepsnaam <- input$bestaande_groep
  })
  
  # Observe of de datum wordt aangepast ----
  observeEvent({input$DateStart},{
    values$startdatum <- input$DateStart
  })
  
  observeEvent({input$DateEind},{
    values$einddatum <- input$DateEind
  })
  
  # Observe if user selects a sensor ----
  observeEvent({input$map_marker_click$id}, {
    id_select <- input$map_marker_click$id
    #TODO hier wil iets dat het per tabblad uitmaakt wat er gebeurd? Of niet?
    # Wanneer er op een Luchtmeetnet of KNMI station marker geklikt wordt, gebeurt er niks
    if (is_empty(grep("^knmi|^NL", id_select)) ){
      # Check if sensor id already selected -> unselect sensor
      if((values$df$selected[which(values$df$kit_id == id_select)][1])){
        set_sensor_deselect(id_select)
      }
      # If sensor is not yet present -> select sensor
      else{
        set_sensor_select(id_select)
      }
      # Laad de sensoren op de kaart zien
      add_sensors_map()
      # Bij elke selectie of deselectie moet de gemiddelde voor de groep herberekend worden
    }else{ 
      # Check of het een lml station is:
      if(!is_empty(grep("^NL", id_select))){
        # Check if station id already selected -> unselect station
        if((lml_stations_reactive$statinfo$selected[which(lml_stations_reactive$statinfo$statcode == id_select)])){
          set_lml_station_deselect(id_select)
        }
        # If station is not yet present -> select station
        else{
          set_lml_station_select(id_select)
        }
      }else if(!is_empty(grep("^knmi", id_select))){ # Als het een KNMI station is
        # Check if station id already selected -> unselect station
        if((knmi_stations_reactive$statinfo$selected[which(knmi_stations_reactive$statinfo$statcode == id_select)])){
          set_knmi_station_deselect(id_select)
        }
        # If station is not yet present -> select station
        else{
          set_knmi_station_select(id_select)
        }
      }
      }
    })
  
  # Observe of de huidige selectie moet worden gereset ----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$reset_huidig, {
    values$df[which(values$df$huidig == TRUE), "selected"] <- FALSE 
    values$df[which(values$df$huidig == TRUE), "kleur"] <- kleur_marker_sensor
    values$df[which(values$df$huidig == TRUE), "groep"] <- geen_groep
    values$df[which(values$df$huidig == TRUE), "huidig"] <- FALSE 
    # Laad de sensoren op de kaart zien
    add_sensors_map()
  })
  
  # Observe of de alle geselecteerde sensoren moet worden gereset----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$reset_all, {
    values$df[, "selected"] <- FALSE 
    values$df[, "kleur"] <- kleur_marker_sensor
    values$df[, "groep"] <- geen_groep
    values$df[, "huidig"] <- FALSE 
    # Laad de sensoren op de kaart zien
    add_sensors_map()
  })
  
  # Observe of de selectie moet worden toegevoegd aan de groep----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$groeperen, {
    # Check of een groep gekozen is, anders geen groepering
    if (values$groepsnaam == geen_groep){
      values$df[values$df$huidig, "huidig"] <- FALSE
    }else{
    # Als de groep al bestaat, zoek die kleur op
    if(values$groepsnaam %in% values$df$groep){
      kleur_sensor <- values$df[which(values$df$groep == values$groepsnaam),'kleur'][1]
    } else{
      kleur_sensor <- values$df[which(values$df$huidig),'kleur'][1]
    }

    # Geef aan dat de sensor bij die groep hoort.
    values$df[values$df$huidig, "groep"] <- values$groepsnaam
    values$df[values$df$huidig, "kleur"] <- kleur_sensor
    values$df[values$df$huidig, "huidig"] <- FALSE
    
    # Laad de sensoren op de kaart zien
    add_sensors_map()}
    # Set textinput op geen groep
    updateTextInput(session,"Text_groep",'Maak nieuwe groep:', value = geen_groep)
  })

  # Voor de bestaande groepen: maak de input-ui 
  output$bestaande_groep <- renderUI({
    selectizeInput('bestaande_groep', 'Kies bestaande groep: ', choices = c("select" = "", levels(as.factor(values$df$groep))))
  })
  
  # Observe voor multiselect ----
  observeEvent(input$map_draw_new_feature,{
    
    # Houd bij hoeveel features er zijn. Later nodig bij verwijderen, i.v.m. reset ook de losse selectie.
    overzicht_shapes$add <- overzicht_shapes$add + 1
    
    # Zoek de sensoren in de feature
    found_in_bounds <- findLocations(shape = input$map_draw_new_feature,
                                     location_coordinates = ms_coordinates,
                                     location_id_colname = "kit_id")
    # Ga elke sensor af en voeg deze bij de selectie
    for(id_select in found_in_bounds){
      # Wanneer er op een LML of KNMI station marker geklikt wordt, gebeurt er niks
      if (is_empty(grep("^knmi|^NL", id_select)) ){
        # Check if sensor id already selected -> unselect sensor
        if((values$df$selected[which(values$df$kit_id == id_select)][1])){
          set_sensor_deselect(id_select)
        }
        # If sensor is not yet present -> select sensor
        else{ 
          set_sensor_select(id_select)
        }
      }
      # Laad de sensoren op de kaart zien
      add_sensors_map()
    }
  })
  
  # Observe voor multiselect deselect ----
  # Er zijn namelijk twee manieren om sensoren te selecteren: d.m.v. los aangeklikte sensoren (1), en d.m.v.
  # de DrawToolBox (2). De delete knop op de DrawToolBox verwijderd enkel de sensoren die d.m.v. de DrawToolBox geselecteerd zijn,
  # dus niet de losse sensoren. Onderstaand stukzorgt ervoor dat zowel selectie via (1) als (2) worden verwijderd.
  
  observeEvent(input$map_draw_deleted_features,{
    # Aantal te verwijderen features
    overzicht_shapes$delete <- length(input$map_draw_deleted_features$features)
    # Check of alle features worden verwijderd. Als dat het geval is, zet dan alle markers ook op deselected
    # Dus ook degene die individueel zijn geklikt
    if(overzicht_shapes$delete == overzicht_shapes$add){
      values$df[, "selected"] <- FALSE 
      values$df[, "kleur"] <- kleur_marker_sensor
      values$df[, "groep"] <- geen_groep
    }
    else{
      # Als er maar één feature wordt verwijderd, ga dan de sensoren af en deselecteer deze een voor een
      for(feature in input$map_draw_deleted_features$features){
        bounded_layer_ids <- findLocations(shape = feature, location_coordinates = ms_coordinates, location_id_colname = "kit_id")
        for(id_select in bounded_layer_ids){
          # Wanneer er op een LML of KNMI station marker geklikt wordt, gebeurt er niks
          if (is_empty(grep("^knmi|^NL", id_select)) ){
            # Check if sensor id already selected -> unselect sensor
            if((values$df$selected[which(values$df$kit_id == id_select)][1])){
              set_sensor_deselect(id_select)
            }
          }
        }
      }
    }
    # Houd bij hoeveel shapes er nog zijn
    overzicht_shapes$add <- overzicht_shapes$add - overzicht_shapes$delete
    # Laat de sensoren op de kaart zien
    add_sensors_map()
  })
  
  
  ## GENEREER PLOTS -----
  
  # Download de data van de geselecteerde LML stations ----
  output$downloadData_luchtmeetnet <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('testLML', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      write.table(lml_stations_reactive$lml_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  # Download de data van de geselecteerde KNMI stations ----
  output$downloadData_knmi <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('testknmi', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      write.table(knmi_stations_reactive$knmi_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Download de data van de projectgeselecteerde sensoren ----
  output$downloadData_sensor <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('testsensor', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      write.table(values$sensor_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Create tabel huidige selectie ----
  output$huidig <- renderTable({
  huidig_df <- data.frame('Selectie' = values$df[which(values$df$huidig),'kit_id'])
  })
  
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_lml <- renderTable({
    stations_df <- data.frame('Selectie' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),'statcode'])
  })
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_knmi <- renderTable({
    stations_df <- data.frame('Selectie' = as.character(knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'nummer']))
  })
  
  # Create time plot vanuit openair ----
  output$timeplot <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }

    # if / else statement om correctie lml data toe te voegen  
    if(comp == "pm10" || comp == "pm10_kal"){
      # Bepaal de max voor de ylim
      ylim_max <- max(show_input$pm10)
     
      try(timePlot(selectByDate(mydata = show_input,start = values$startdatum, end = values$einddatum),
                   pollutant = c(comp, "pm10_lml"), wd = "wd", type = "kit_id", local.tz="Europe/Amsterdam", ylim=c(0, ylim_max)))
      # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    }
    else {
      # Bepaal de max voor de ylim
      ylim_max <- max(show_input$pm25)
      try(timePlot(selectByDate(mydata = show_input,start = values$startdatum, end = values$einddatum),
                   pollutant = c(comp, "pm25_lml"), wd = "wd", type = "kit_id", local.tz="Europe/Amsterdam", ylim=c(0, ylim_max)))
      # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    }
  })
  
  # Create kalender plot vanuit openair ----
  output$calendar <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }
    
    try(calendarPlot(selectByDate(mydata = show_input, start = values$startdatum, end = values$einddatum),
                     pollutant = comp, limits= c(0,150), cols = 'Purples', local.tz="Europe/Amsterdam")) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
  })
  
  # Create timevariation functie vanuit openair ----
  output$timevariation <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }
    
    ## Create array for the colours
    # get the unique kit_id and the color
    kit_kleur <- unique(values$df[which(values$df$selected),c('kit_id','kleur','groep')])
    
    # Als er een groep is, zorg voor 1 rij van de groep, zodat er maar 1 kleur is
    if (length(unique(kit_kleur$groep)>1)){
      kit_kleur[which(kit_kleur$groep != geen_groep),'kit_id'] <- kit_kleur[which(kit_kleur$groep != geen_groep),'groep']
      kit_kleur <- unique(kit_kleur)
    }
    
    # Sort by kit_id
    kit_kleur_sort <- kit_kleur[order(kit_kleur$kit_id),]
    # create colour array
    kleur_array <- kit_kleur_sort$kleur
    
    try(timeVariation(selectByDate(mydata = show_input, start = values$startdatum, end = values$einddatum),
                      pollutant = comp, normalise = FALSE, group = "kit_id",
                      alpha = 0.1, cols = kleur_array, local.tz="Europe/Amsterdam",
                      ylim = c(0,NA))) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    
  })
  
  # Create pollutionrose functie vanuit openair ----
  output$pollutionplot <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]    
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }
    
    
    try(pollutionRose(selectByDate(mydata = show_input,start = values$startdatum, end = values$einddatum),
                      pollutant = comp, wd = 'wd', ws = 'ws', type = 'kit_id' , local.tz="Europe/Amsterdam", cols = "Purples", statistic = 'prop.mean',breaks=c(0,20,60,100))) 
    
  })
  
  
  # Create windrose vanuit openair---- 
  output$windplot <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]    
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }
    
    
    try(windRose(selectByDate(mydata = show_input,start = values$startdatum, end = values$einddatum),
                 wd = 'wd', ws = 'ws', type = 'kit_id' , local.tz="Europe/Amsterdam", cols = "Purples")) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    
  })
  
  # Create percentilerose functie vanuit openair ----
  output$percentileplot <- renderPlot({
    
    comp <- selectReactiveComponent(input)
    selected_id <- values$df[which(values$df$selected & values$df$groep == geen_groep),'kit_id']
    show_input <-values$sensor_data[which(values$sensor_data$kit_id %in% selected_id),]    
    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(values$df$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,values$df_gem, all = T) }
    
    try(percentileRose(selectByDate(mydata = show_input,start = values$startdatum, end = values$einddatum),
                       pollutant = comp, wd = 'wd', type = 'kit_id', local.tz="Europe/Amsterdam", percentile = NA)) 
    
  })  
}
