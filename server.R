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

  # Zet reactive values op----
  groepering_reactive <- reactiveValues(groepsnaam = geen_groep, df_gem = data.frame())
  tijdreeks_reactive <- reactiveValues(startdatum = 0, einddatum=0)
  overig_reactive <- reactiveValues(data_set = 'voorbeeld', toon_all_knmi = FALSE, toon_all_lml = FALSE)
  overzicht_shapes <- reactiveValues(add = 0, delete = 0) # nodig om selectie ongedaan te maken
  choices_api_reactive <- reactiveValues(choices=gemeente_choices)
  
  # reactive values voor de data en info over de sensoren, lml stations en knmi stations
  sensor_reactive <- reactiveValues(statinfo=sensor_unique, sensor_data=input_df)
  lml_stations_reactive <- reactiveValues(statinfo=lml_stations_all, lml_data = input_df_lml)
  knmi_stations_reactive <- reactiveValues(statinfo=knmi_stations_all, knmi_data = input_df_knmi)
  
  
  ## FUNCTIES ----
  
  # Functie: Set the sensor as deselect and change color to base color
  set_sensor_deselect <- function(id_select){
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "selected"] <- FALSE 
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "huidig"] <- FALSE 
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "kleur"] <- kleur_marker_sensor
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "groep"] <- geen_groep
  }
  
  # Functie: Set sensor as select and specify color
  set_sensor_select <- function(id_select){
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "selected"] <- TRUE
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "huidig"] <- TRUE
    # Select een kleur en geef dit mee aan de sensor
    # Kies de eerste kleur in de lijst kleur_cat die aanwezig is
    count  <- 1
    # Zorg ervoor dat je blijft zoeken tot sensor een kleur heeft of dat de kleuren op zijn
    while (kleur_sensor == "leeg" & count < length(kleur_cat)){
      for (kleur_code in kleur_cat){
        if (kleur_code %in% unique(sensor_reactive$statinfo$kleur)){
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
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "kleur"] <- kleur_sensor
    kleur_sensor <- "leeg"
  }
  
  # Functie: Set the LML stations as deselect and change color to base color
  set_lml_station_deselect <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "selected"] <- FALSE 
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "name_icon"] <- 'lml_black'
  }
  
  # Functie: Set station as select and specify color
  set_lml_station_select <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "selected"] <- TRUE
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$statcode == id_select, "name_icon"] <- 'lml_white'
  }
  
  # Functie: Set the stations as deselect and change color to base color
  set_knmi_station_deselect <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_nummer == id_select, "selected"] <- FALSE 
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_nummer == id_select, "name_icon"] <- 'knmi_black'
  }
  
  # Functie: Set station as select and specify color
  set_knmi_station_select <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_nummer == id_select, "selected"] <- TRUE 
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_nummer == id_select, "name_icon"] <- 'knmi_white'
  }
  
  # Functie: plaats sensoren met juiste kleur op de kaart  
  add_sensors_map <- function(){ 
    # Regenerate the sensors for the markers
    sensor_loc <- unique(select(sensor_reactive$statinfo, kit_id, lat, lon, kleur, selected))
    
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("sensoren") # Clear sensor markers
    proxy %>% addCircleMarkers(data = sensor_loc, ~lon, ~lat, layerId = ~kit_id, label = lapply(as.list(sensor_loc$kit_id), HTML),
                               radius = 8, color = ~kleur, fillOpacity = 1,stroke = ~selected, group = "sensoren")}
  
  # TODO: Universele namen voor de statcode etc van lml en knmi, nu in lmldata anders dan in lml idem knmi  
  # TODO: ook de labels van KNMi en LML stations mooi maken, met eigenaar erbij etc.
  add_knmistat_map <- function(){
    if(overig_reactive$toon_all_knmi){
      print('Toon alle KNMI-stations')
      # Toon alle stations
      station_loc <- knmi_stations_reactive$statinfo
    }
    # Check of er al sensoren met data zijn
    else{
      print('Zoek de knmi stations die data hebben: ')
      # Toon alleen de stations die al data hebben
      station_loc <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$hasdata==T),]
    }
    
    print(paste0("stations op kaart: ", station_loc$station_nummer))
    print(names(station_loc))

    overig_reactive$toon_all_knmi <- FALSE
    
    # Update map with new markers to show selected
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("knmistations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~station_nummer, label = lapply(as.list(paste0('KNMI: ',station_loc$station_nummer)), HTML),
                         icon = ~icons_stations[name_icon], group = "knmistations")}

  # Functie: plaats lml stations  op de kaart  
  add_lmlstat_map <- function(){ 
    if(overig_reactive$toon_all_lml){
      # Toon alle stations
      station_loc <- lml_stations_reactive$statinfo
    }
    # Check of er al sensoren met data zijn
    else{
      # Toon alleen de stations die al data hebben
      station_loc <- lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$hasdata==T),]
    }

    print(paste0("stations op kaart: ", station_loc))
    print(station_loc$name_icon)
    
    overig_reactive$toon_all_lml <- FALSE
    
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("luchtmeetnetstations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~statcode, label = lapply(as.list(station_loc$statcode), HTML),
                               icon = ~icons_stations[name_icon], group = "luchtmeetnetstations")}
  
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
    for(groepen in unique(sensor_reactive$statinfo$groep)){
      if (groepen != geen_groep){
        # Haal de kit_ids van de sensoren in de groep op
        sensor_groep <- sensor_reactive$statinfo[which(sensor_reactive$statinfo$groep == groepen),'kit_id']
        # Zoek de gegevens van de groep op
        te_middelen <- sensor_reactive$sensor_data[which(sensor_reactive$sensor_data$kit_id %in% sensor_groep),]
        # Bereken het gemiddelde van de groep. LET OP; vector middeling
        gemiddeld <- timeAverage(te_middelen, avg.time='hour', vector.ws=TRUE)
        gemiddeld$kit_id <- groepen
        gemiddeld_all <- rbind(gemiddeld_all,gemiddeld)
      }} 
    # Maak de gemiddeld_all de reactive
    groepering_reactive$df_gem <- gemiddeld_all
  }

  # Functie om de sensor data in te laden
  insert_nieuwe_data_sensor <- function(){
    # Deze functie laadt de sensor data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    if(overig_reactive$data_set=='API_samenmeten'){
      # Haal de gegevens op van de sensoren via de samenmeten API
      sensor_reactive$sensor_data <- get_sensor_data_api()

    }else if(overig_reactive$data_set=='eigen_dataset_sensoren'){
      # Lees de csv uit en sla de gegevens op in interactive
      sensor_reactive$sensor_data <- read.csv(input$eigen_datafile_sensoren$datapath, sep=",")
      # Zet de date naar een posixct
      sensor_reactive$sensor_data$date <- as.POSIXct(sensor_reactive$sensor_data$date, tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }else if(overig_reactive$data_set=='voorbeeld'){
      # Lees de csv uit en sla de gegevens op in interactive
      sensor_reactive$sensor_data <- read.csv(sensor_file, sep=",")
      # Zet de date naar een posixct
      sensor_reactive$sensor_data$date <- as.POSIXct(sensor_reactive$sensor_data$date, tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }

    # Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
    sensor_unique <- unique(sensor_reactive$sensor_data[,c('kit_id','lat','lon')])
    print('sensor Unique gemaakt')
    print(head(sensor_unique))
    sensor_unique$selected <- FALSE
    sensor_unique$huidig <- FALSE
    sensor_unique$groep <- geen_groep
    sensor_unique$kleur <- kleur_marker_sensor
    sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info

    # Als de sensor geen coordinaten heeft, zet dan op 0,0 (anders werkt spatialpointsdataframe niet)
    sensor_unique$lat[which(is.na(sensor_unique$lat))] <- 0
    sensor_unique$lon[which(is.na(sensor_unique$lon))] <- 0
    
    # Voor de multiselect tool: omzetten lat/lon naar spatialpoints
    ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

    # Voeg de sensor locaties ed toe aan interactive dataframe
    sensor_reactive$statinfo <- sensor_unique
    # voeg de sensoren toe aan de kaart
    add_sensors_map()
    # zoom naar de nieuwe sensoren
    mean_lat <- mean(sensor_unique$lat[which(sensor_unique$lat>0)])
    mean_lon <- mean(sensor_unique$lon[which(sensor_unique$lon>0)])
    set_view_map(mean_lat, mean_lon)
  } 
  
  # Functie om de luchtmeetnet data in te laden
  insert_nieuwe_data_lml <- function(){ 
    # Deze functie laadt de luchtmeentnet data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    
    # Data die er al is wordt overschreven, dus zet de hasdata op FALSE
    lml_stations_reactive$statinfo$hasdata <- FALSE
    
    if(overig_reactive$data_set=='API_luchtmeetnet'){
      # Haal de gegevens op van de stations via de luchtmeetnet API
      print('ophalen api luchtmeetnet')
      lml_stations_reactive$lml_data <- get_lml_data_api()
      
    }else if(overig_reactive$data_set=='eigen_dataset_lml'){
       # Haal de gegevens op van de stations via een ingeladen csv
       print('ophalen csv luchtmeetnet')

       # Lees de csv uit en sla de gegevens op in interactive
       lml_stations_reactive$lml_data <- read.csv(input$eigen_datafile_lml$datapath, sep=",")
       print(head(lml_stations_reactive$lml_data))
       # Zet de date naar een posixct
       lml_stations_reactive$lml_data$date <- as.POSIXct(lml_stations_reactive$lml_data$timestamp_measured , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
       print(summary(lml_stations_reactive$lml_data))
    }else if(overig_reactive$data_set=='voorbeeld'){
      # Haal de gegevens op van de stations via de voorbeeld csv
      print('ophalen voorbeeld csv luchtmeetnet')
      # Lees de csv uit en sla de gegevens op in interactive
      lml_stations_reactive$lml_data <- read.csv(lml_file, sep=",")
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
    if(overig_reactive$data_set=='API_knmi'){
      # Haal de gegevens op van de stations via de KNMI API
      print('ophalen api KNMI')
      knmi_stations_reactive$knmi_data <- get_knmi_data_api()
    }else if(overig_reactive$data_set=='eigen_dataset_knmi'){
      # Haal de gegevens op van de stations vanuit een ingelezen bestand
      print('ophalen uit bestand KNMI')
      # Lees de csv uit en sla de gegevens op in interactive
      knmi_stations_reactive$knmi_data <- read.csv(input$eigen_datafile_knmi$datapath, sep=",")
      print(head(knmi_stations_reactive$knmi_data))
      # Zet de date naar een posixct
      knmi_stations_reactive$knmi_data$date <- as.POSIXct(knmi_stations_reactive$knmi_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
      print(summary(knmi_stations_reactive$knmi_data))
    }else if(overig_reactive$data_set=='voorbeeld'){
      # Haal de gegevens op van de stations vanuit het voorbeeld bestand
      print('ophalen uit voorbeeldbestand KNMI')
      # Lees de csv uit en sla de gegevens op in interactive
      knmi_stations_reactive$knmi_data <- read.csv(knmi_file, sep=",")
      print(head(knmi_stations_reactive$knmi_data))
      # Zet de date naar een posixct
      knmi_stations_reactive$knmi_data$date <- as.POSIXct(knmi_stations_reactive$knmi_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
      print(summary(knmi_stations_reactive$knmi_data))
    }
     # Geef aan van welke stations nu databeschikbaar is:
     station_metdata <- unique(knmi_stations_reactive$knmi_data$station_nummer)
     knmi_stations_reactive$statinfo$hasdata[which(knmi_stations_reactive$statinfo$station_nummer %in% station_metdata)] <- TRUE
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
    
    # Ga elk station af en haal de gegevens op uit de API
    for(ind in seq(1,length(lml_stats))){
      stat <- lml_stats[ind]
      # Voor de progress messages
      text <- paste0("Luchtmeetnet station: ", stat, " - stap (",ind,"/",length(lml_stats) ,")")
      updateProgress(value = (ind/length(lml_stats))*0.8, detail = text)
      
      # Haal voor elk station de data op van luchtmeetnet API
      station_data_ruw <- GetLMLAPI(stat, format(tijdreeks_reactive$startdatum, '%Y%m%d'), format(tijdreeks_reactive$einddatum, '%Y%m%d'))
      # Voeg alle meetwaardes vam de stations samen
      station_data_all <- rbind(station_data_all, station_data_ruw$data)}
    print(names(station_data_all))
    # Alle gegevens zijn van de api opgehaald, retun de totale set
    updateProgress(value = 0.90 ,detail = "Alle data van Luchtmeetnet opgehaald.")
    #return de totale dataset
    return(station_data_all)
  }
  
  # Ophalen van de KNMI data vanuit een API
  # TODO: als er een niet bekend station of NA wordt meegegeven, dan is er een oneindige 
  # call. Dus check in de knmi_stats echte nummers zijn
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
    knmi_stats <- knmi_stations_reactive$statinfo$station_nummer[which(knmi_stations_reactive$statinfo$selected==T)]
    print(paste0('API ophalen: ',knmi_stats))
    
    # Voor de progress message
    progress$set(message = paste0("van stations: ", knmi_stats), value = 0.3)
    
    # ophalen van de gegevens via de API
    station_all_data <- GetKNMIAPI(knmi_stats,format(tijdreeks_reactive$startdatum, '%Y%m%d'), format(tijdreeks_reactive$einddatum, '%Y%m%d'))
    
    # Je hebt voor nu alleen het data deel van de gegevens uit de api nodig
    station_all_data <- station_all_data$data
    
    # hernoem de kolommen en gooi overige kolommen weg
    station_all_data <- select(station_all_data, -c('YYYYMMDD', 'HH'))
    station_all_data <- plyr::rename(station_all_data, c('STNS'= 'station_nummer', 'DD' = 'wd', 'FF'='ws', 'TEMP'='temp','U' = 'rh', 'tijd'='date'))

    station_all_data$station_code <- paste0('KNMI', station_all_data$station_nummer)
    
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
    sensor_data_ruw <- GetSamenMetenAPI(projectnaam, format(tijdreeks_reactive$startdatum, '%Y%m%d'), 
                                        format(tijdreeks_reactive$einddatum, '%Y%m%d'), data_opslag_list,
                                        updateProgress) 
    print('api opgehaald:')
    print(summary(sensor_data_ruw))
    # Dit bestaat uit 2 delen: 
    # sensordata die kan worden gebruikt voor de markers(sensor_reactive$statinfo) 
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

  # Functie om de data klaar te maken voor de visualisatie
  filter_data_plot <- function(){
    # Deze functie maakt/filtert de data voor de visualisatie. De juiste component tijdreeks etc.
    # TODO hier wil je een input$knmistation waar de gebruiker een knmi station heeft gekozen voor de rozen
    # TODO  iets voor de lml data in timeplot ggplot
    comp <- input$Component
    selected_id <- sensor_reactive$statinfo[which(sensor_reactive$statinfo$selected & sensor_reactive$statinfo$groep == geen_groep),'kit_id']
    show_input <- sensor_reactive$sensor_data[which(sensor_reactive$sensor_data$kit_id %in% selected_id),]    

    
    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(sensor_reactive$statinfo$groep))>1){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- merge(show_input,groepering_reactive$df_gem, all = T) } 
    
    # Bepaal van welk knmi-stations de winddata wordt gebruikt:
    stat_wdws <- input$knmi_stat_wdws
    knmi_data_wdws <- knmi_stations_reactive$knmi_data[which(knmi_stations_reactive$knmi_data$station_nummer == stat_wdws),]
    
    # TODO wil je hier nog een waarschuwing als het knmi station geen winddata heeft? Dus alleen maar NA?
    
    print(head(knmi_data_wdws))
    print(head(show_input))
    show_input <- merge(show_input,knmi_data_wdws , all.x=T, by.x='date', by.y='date')
    print(head(show_input))
    show_input <- selectByDate(mydata = show_input,start = tijdreeks_reactive$startdatum, end = tijdreeks_reactive$einddatum)
    return(show_input)
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
  
  
  # Haal de choices in de SelectInput voor de KNMI stations die data hebben voor de plots----
  observe({
    print('Observe knmi wdws')
    choices_knmi <- knmi_stations_reactive$statinfo$station_nummer[which(knmi_stations_reactive$statinfo$hasdata==TRUE)]
    print(paste0('choices_knmi: ', choices_knmi))
    if(is_empty(choices_knmi)){
      choices_knmi <- c('Geen knmi data beschikbaar.')
    }
    updateSelectInput(session, "knmi_stat_wdws", choices = choices_knmi
    )})
  
  #Print welke datagroep bij samenmeten api wordt opgevraagd
  observeEvent({input$sensor_specificeer},{
    print(input$sensor_specificeer)
    print(input$sensor_hoofdgroep)
  })
  
  # observe of er een eigen data set is ingeladen voor de sensoren:----
  observeEvent({req(input$eigen_datafile_sensoren)},{
    overig_reactive$data_set <- input$eigen_datafile_sensoren$datapath  
    overig_reactive$data_set <- 'eigen_dataset_sensoren'
    print("databestand sensoren geladen")
    insert_nieuwe_data_sensor()})
  
  # observe of er een eigen data set is ingeladen voor de luchtmeetnetmetingen:----
  observeEvent({req(input$eigen_datafile_lml)},{
    overig_reactive$data_set <- input$eigen_datafile_lml$datapath  
    overig_reactive$data_set <- 'eigen_dataset_lml'
    print("databestand lml geladen")
    insert_nieuwe_data_lml()})
  
  # observe of er een eigen data set is ingeladen voor de knmi metingen:----
  observeEvent({req(input$eigen_datafile_knmi)},{
    overig_reactive$data_set <- input$eigen_datafile_knmi$datapath  
    overig_reactive$data_set <- 'eigen_dataset_knmi'
    print("databestand knmi geladen")
    insert_nieuwe_data_knmi()})  
  
  # Observe of de voorbeeld dataset weer ingeladen moet worden----
  observeEvent({input$voorbeeld_data},{
    overig_reactive$data_set <- 'voorbeeld'
    insert_nieuwe_data_sensor()
    insert_nieuwe_data_lml()
    insert_nieuwe_data_knmi()})
  
  # Observe of de specifieke sensor dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_samenmeten},{
    overig_reactive$data_set <- 'API_samenmeten'
    insert_nieuwe_data_sensor()})
  
  # Observe of de specifieke luchtmeetnet stations dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_luchtmeetnet},{
    print("Eerste aanroep api luchtmeetnet")
    overig_reactive$data_set <- 'API_luchtmeetnet'
    insert_nieuwe_data_lml()})
  
  # Observe of de specifieke KNMI stations dataset opgehaald en ingeladen moet worden----
  observeEvent({input$API_knmi},{
    print("Eerste aanroep api knmi")
    overig_reactive$data_set <- 'API_knmi'
    insert_nieuwe_data_knmi()
    })
  
  #Observe of de luchtmeetnetstations moeten worden getoond----
  observeEvent({input$show_luchtmeetnet},{
    print('laad zien')
    overig_reactive$toon_all_lml <- TRUE
    add_lmlstat_map()
    print("op kaart getoond")
    })
  
  # #Observe of de knmi-stations moeten worden getoond----
  observeEvent({input$show_knmi},{
    print('laad zien')
    overig_reactive$toon_all_knmi <- TRUE
    add_knmistat_map()
    print("op kaart getoond")
  })
  
  # Observe of de tekst wordt aangepast ----
  # Dan wil je dat er een nieuwe groep wordt aangemaakt
  # Bijvoorbeeld: je hebt een groep "Wijk aan Zee" aangemaakt, en je begint een nieuwe naam te typen "IJmuiden". 
  # Deze groep moet dan nieuw aangemaakt worden "IJmuiden".
  observeEvent({input$Text_groep},{
      groepering_reactive$groepsnaamnaam <- input$Text_groep
    })
  
  observeEvent({input$bestaande_groep},{
    groepering_reactive$groepsnaamnaam <- input$bestaande_groep
  })
  
  # Observe of de datum wordt aangepast ----
  observeEvent({input$DateStart},{
    tijdreeks_reactive$startdatum <- input$DateStart
  })
  
  observeEvent({input$DateEind},{
    tijdreeks_reactive$einddatum <- input$DateEind
  })
  
  # Observe if user selects a sensor ----
  observeEvent({input$map_marker_click$id}, {
    id_select <- input$map_marker_click$id
    print(id_select)
    # Bepaal of er op een sensr wordt geklikt, dus niet op lml station of knmi station.
    # Het lml station begint met 'NL', het knmi station is een numeric
    if (is_empty(grep("^knmi|^NL", id_select)) & !is.numeric(id_select) ){
      # Check if sensor id already selected -> unselect sensor
      if((sensor_reactive$statinfo$selected[which(sensor_reactive$statinfo$kit_id == id_select)][1])){
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
          add_lmlstat_map()
        }
        # If station is not yet present -> select station
        else{
          set_lml_station_select(id_select)
          add_lmlstat_map()
        }
      }else{ # Als het een KNMI station is
        # Check if station id already selected -> unselect station
        if((knmi_stations_reactive$statinfo$selected[which(knmi_stations_reactive$statinfo$station_nummer == id_select)])){
          set_knmi_station_deselect(id_select)
          add_knmistat_map()
        }
        # If station is not yet present -> select station
        else{
          set_knmi_station_select(id_select)
          add_knmistat_map()
        }
      }
      }
    })
  
  # Observe of de huidige selectie moet worden gereset ----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$reset_huidig, {
    sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig == TRUE), "selected"] <- FALSE 
    sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig == TRUE), "kleur"] <- kleur_marker_sensor
    sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig == TRUE), "groep"] <- geen_groep
    sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig == TRUE), "huidig"] <- FALSE 
    # Laad de sensoren op de kaart zien
    add_sensors_map()
  })
  
  # Observe of de alle geselecteerde sensoren moet worden gereset----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$reset_all, {
    if (!is_empty(sensor_reactive$statinfo)){
      sensor_reactive$statinfo[, "selected"] <- FALSE 
      sensor_reactive$statinfo[, "kleur"] <- kleur_marker_sensor
      sensor_reactive$statinfo[, "groep"] <- geen_groep
      sensor_reactive$statinfo[, "huidig"] <- FALSE 
      # Laad de sensoren op de kaart zien
      add_sensors_map()
      }
  })
  
  # Observe of de selectie moet worden toegevoegd aan de groep----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$groeperen, {
    # Check of een groep gekozen is, anders geen groepering
    if (groepering_reactive$groepsnaamnaam == geen_groep){
      sensor_reactive$statinfo[sensor_reactive$statinfo$huidig, "huidig"] <- FALSE
    }else{
    # Als de groep al bestaat, zoek die kleur op
    if(groepering_reactive$groepsnaamnaam %in% sensor_reactive$statinfo$groep){
      kleur_sensor <- sensor_reactive$statinfo[which(sensor_reactive$statinfo$groep == groepering_reactive$groepsnaamnaam),'kleur'][1]
    } else{
      kleur_sensor <- sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig),'kleur'][1]
    }

    # Geef aan dat de sensor bij die groep hoort.
    sensor_reactive$statinfo[sensor_reactive$statinfo$huidig, "groep"] <- groepering_reactive$groepsnaamnaam
    sensor_reactive$statinfo[sensor_reactive$statinfo$huidig, "kleur"] <- kleur_sensor
    sensor_reactive$statinfo[sensor_reactive$statinfo$huidig, "huidig"] <- FALSE
    
    # Laad de sensoren op de kaart zien
    add_sensors_map()}
    # Set textinput op geen groep
    updateTextInput(session,"Text_groep",'Maak nieuwe groep:', value = geen_groep)
  })

  # Voor de bestaande groepen: maak de input-ui 
  output$bestaande_groep <- renderUI({
    selectizeInput('bestaande_groep', 'Kies bestaande groep: ', choices = c("select" = "", levels(as.factor(sensor_reactive$statinfo$groep))))
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
        if((sensor_reactive$statinfo$selected[which(sensor_reactive$statinfo$kit_id == id_select)][1])){
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
      sensor_reactive$statinfo[, "selected"] <- FALSE 
      sensor_reactive$statinfo[, "kleur"] <- kleur_marker_sensor
      sensor_reactive$statinfo[, "groep"] <- geen_groep
    }
    else{
      # Als er maar één feature wordt verwijderd, ga dan de sensoren af en deselecteer deze een voor een
      for(feature in input$map_draw_deleted_features$features){
        bounded_layer_ids <- findLocations(shape = feature, location_coordinates = ms_coordinates, location_id_colname = "kit_id")
        for(id_select in bounded_layer_ids){
          # Wanneer er op een LML of KNMI station marker geklikt wordt, gebeurt er niks
          if (is_empty(grep("^knmi|^NL", id_select)) ){
            # Check if sensor id already selected -> unselect sensor
            if((sensor_reactive$statinfo$selected[which(sensor_reactive$statinfo$kit_id == id_select)][1])){
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
      write.table(sensor_reactive$sensor_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Create tabel huidige selectie ----
  output$huidig <- renderTable({
  huidig_df <- data.frame('Selectie' = sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig),'kit_id'])
  })
  
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_lml <- renderTable({
    stations_df <- data.frame('Selectie' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),'statcode'])
  })
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_knmi <- renderTable({
    stations_df <- data.frame('Selectie' = as.character(knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_nummer']))
  })
  
  # Create time plot vanuit openair ----
  output$timeplot <- renderPlot({
    
    show_input <- filter_data_plot()
    comp <- input$Component
    
    # TODO: Maak hier ook de selectie van de geselecteerde lml stataion en merge die met de andere data
    # misschien dan ook een lijn dikte of kleur meegeven. Nog over nadenken hoe of wat precies.
    
    # if / else statement om correctie lml data toe te voegen  
    if(comp == "pm10" || comp == "pm10_kal"){
      # Bepaal de max voor de ylim
      ylim_max <- max(show_input$pm10)
     
      try(timePlot(show_input,
                   pollutant = c(comp, "pm10_lml"), wd = "wd", type = "kit_id", local.tz="Europe/Amsterdam", ylim=c(0, ylim_max)))
      # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    }
    else {
      # Bepaal de max voor de ylim
      ylim_max <- max(show_input$pm25)
      try(timePlot(show_input,
                   pollutant = c(comp, "pm25_lml"), wd = "wd", type = "kit_id", local.tz="Europe/Amsterdam", ylim=c(0, ylim_max)))
      # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    }
  })
  
  # Create kalender plot vanuit openair ----
  output$calendar <- renderPlot({
    
    show_input <- filter_data_plot()
    try(calendarPlot(show_input,
                     pollutant = input$Component, limits= c(0,150), cols = 'Purples', local.tz="Europe/Amsterdam")) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
  })
  
  # Create timevariation functie vanuit openair ----
  output$timevariation <- renderPlot({
    
    show_input <- filter_data_plot()
    
    ## Create array for the colours
    # get the unique kit_id and the color
    kit_kleur <- unique(sensor_reactive$statinfo[which(sensor_reactive$statinfo$selected),c('kit_id','kleur','groep')])
    
    # Als er een groep is, zorg voor 1 rij van de groep, zodat er maar 1 kleur is
    if (length(unique(kit_kleur$groep)>1)){
      kit_kleur[which(kit_kleur$groep != geen_groep),'kit_id'] <- kit_kleur[which(kit_kleur$groep != geen_groep),'groep']
      kit_kleur <- unique(kit_kleur)
    }
    
    # Sort by kit_id
    kit_kleur_sort <- kit_kleur[order(kit_kleur$kit_id),]
    # create colour array
    kleur_array <- kit_kleur_sort$kleur
    
    try(timeVariation(show_input,
                      pollutant = input$Component, normalise = FALSE, group = "kit_id",
                      alpha = 0.1, cols = kleur_array, local.tz="Europe/Amsterdam",
                      ylim = c(0,NA))) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    
  })
  
  # Create pollutionrose functie vanuit openair ----
  output$pollutionplot <- renderPlot({
    show_input <- filter_data_plot()
    try(pollutionRose(show_input,
                      pollutant = input$Component, wd = 'wd', ws = 'ws', type = 'kit_id' , local.tz="Europe/Amsterdam", cols = "Purples", statistic = 'prop.mean',breaks=c(0,20,60,100))) 
    
  })
  
  
  # Create windrose vanuit openair---- 
  output$windplot <- renderPlot({
    # TODO hier wil je gewoon 1 windroos van de knmi stations, maar wel geknipt op de tijdselectie
    try(windRose(knmi_stations_reactive$knmi_data,
                 wd = 'wd', ws = 'ws', type = 'station_nummer' , local.tz="Europe/Amsterdam", cols = "Purples")) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
    
  })
  
  # Create percentilerose functie vanuit openair ----
  output$percentileplot <- renderPlot({
    show_input <- filter_data_plot()
    try(percentileRose(show_input,
                       pollutant = input$Component, wd = 'wd', type = 'kit_id', local.tz="Europe/Amsterdam", percentile = NA)) 
    
  })  
}
