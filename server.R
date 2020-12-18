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
  
  # Limiet voor het uploaden van data bestanden 
  options(shiny.maxRequestSize=100*1024^2) 
  
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
  tijdreeks_reactive <- reactiveValues(startdatum = 0, einddatum=0, startdatum_tpdata = 0, einddatum_tpdata=0)
  overig_reactive <- reactiveValues(data_set = 'voorbeeld')
  overzicht_shapes <- reactiveValues(add = 0, delete = 0) # nodig om selectie ongedaan te maken
  choices_api_reactive <- reactiveValues(choices=gemeente_choices)
  

  # reactive values voor de data en info over de sensoren, lml stations en knmi stations
  sensor_reactive <- reactiveValues(statinfo=sensor_unique, sensor_data=input_df)
  lml_stations_reactive <- reactiveValues(statinfo=lml_stations_all, lml_data = input_df_lml)
  knmi_stations_reactive <- reactiveValues(statinfo=knmi_stations_all, knmi_data = input_df_knmi)
  
  
  ## FUNCTIES ----
  
  # Functie: Check of de alle kolommem voor de sensordata aanwezig zijn,
  # zo niet vul deze dan aan met NA
  check_kolommen_sensor <- function(){
    namen_nodig <- c('kit_id',	'date',	'lat',	'lon', 'pm10_kal',	'pm10',	'pm25_kal',	'pm25',	'rh',	'temp')
    namen_huidig <- names(sensor_reactive$sensor_data)
    for(naam in namen_nodig){
      if(!(naam %in% namen_huidig)){
        sensor_reactive$sensor_data[naam] <- NA
      }
    }
  }
  
  # Functie: Set the sensor as deselect and change color to base color
  set_sensor_deselect <- function(id_select){
    # Als de sensor geen data heeft, dan moet je er ook niet op kunnen klikken
    if(sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "hasdata"]==TRUE){
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "selected"] <- FALSE 
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "huidig"] <- FALSE 
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "kleur"] <- kleur_marker_sensor
    sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "groep"] <- geen_groep
    }
  }
  
  # Functie: Set sensor as select and specify color
  set_sensor_select <- function(id_select){
    # Als de sensor geen data heeft, dan moet je er ook niet op kunnen klikken
    if(sensor_reactive$statinfo[sensor_reactive$statinfo$kit_id == id_select, "hasdata"]==TRUE){
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
  }
  
  # Functie: Set the LML stations as deselect and change color to base color
  set_lml_station_deselect <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select, "selected"] <- FALSE 
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select& 
                                     lml_stations_reactive$statinfo$hasdata == TRUE, "name_icon"]  <- 'lml_hasdata'
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select& 
                                     lml_stations_reactive$statinfo$hasdata == FALSE, "name_icon"]  <- 'lml_nodata'
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select, "lijn"] <- 'blank' 
  }
  
  # Functie: Set station as select and specify color
  set_lml_station_select <- function(id_select){
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select, "selected"] <- TRUE
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select, "name_icon"] <- 'lml_selected'
    
    # Select een lijntype en geef dit mee aan het station
    lijn_stat <- 'dashed' # Als er geen andere meer beschikbaar is, dan blijft het dit type
    
    # Kies de eerste type in de lijst lijn_cat die aanwezig is
    for (lijn_code in lijn_cat){
        if (lijn_code %in% unique(lml_stations_reactive$statinfo$lijn)){
          next # Als de type al is toebedeeld, sla deze dan over
        }else{ 
          lijn_stat <- lijn_code # Vrije lijntype voor de station
          break # Verlaat de forloop en ga met dit lijntype verder
        }
      }
    
    # Geef lijntype aan het station
    lml_stations_reactive$statinfo[lml_stations_reactive$statinfo$station_number == id_select, "lijn"] <- lijn_stat
  }
  
  # Functie: Set the stations as deselect and change color to base color
  set_knmi_station_deselect <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_number == id_select, "selected"] <- FALSE 
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_number == id_select & 
                                      knmi_stations_reactive$statinfo$hasdata == TRUE, "name_icon"] <- 'knmi_hasdata'
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_number == id_select& 
                                      knmi_stations_reactive$statinfo$hasdata == FALSE, "name_icon"] <- 'knmi_nodata'
    
    
      }
  
  # Functie: Set station as select and specify color
  set_knmi_station_select <- function(id_select){
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_number == id_select, "selected"] <- TRUE 
    knmi_stations_reactive$statinfo[knmi_stations_reactive$statinfo$station_number == id_select, "name_icon"] <- 'knmi_selected'
  }
  
  # Functie: plaats sensoren met juiste kleur op de kaart  
  add_sensors_map <- function(){ 
    # Regenerate the sensors for the markers
    sensor_loc <- unique(dplyr::select(sensor_reactive$statinfo, kit_id, lat, lon, kleur, selected))
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("sensoren") # Clear sensor markers
    proxy %>% addCircleMarkers(data = sensor_loc, ~lon, ~lat, layerId = ~kit_id, label = lapply(as.list(sensor_loc$kit_id), HTML),
                               radius = 8, color = ~kleur, fillOpacity = 1, stroke = ~selected, group = "sensoren")}
  
  # TODO: Universele namen voor de station_number etc van lml en knmi, nu in lmldata anders dan in lml idem knmi  
  # TODO: ook de labels van KNMi en LML stations mooi maken, met eigenaar erbij etc.
  add_knmistat_map <- function(){
    station_loc <- knmi_stations_reactive$statinfo

    # Update map with new markers to show selected
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("knmistations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~station_number, label = lapply(as.list(paste0('KNMI: ',station_loc$station_number)), HTML),
                         icon = ~icons_stations[name_icon], group = "knmistations")}

  # Functie: plaats lml stations  op de kaart  
  add_lmlstat_map <- function(){ 
    station_loc <- lml_stations_reactive$statinfo

    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("luchtmeetnetstations") # Clear sensor markers
    proxy %>% addMarkers(data = station_loc, ~lon, ~lat, layerId = ~station_number, label = lapply(as.list(station_loc$naam), HTML),
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
        sensor_groep <- sensor_reactive$statinfo$kit_id[which(sensor_reactive$statinfo$groep == groepen)]
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
      print('Sensor data inladen via API')
      # Haal de gegevens op van de sensoren via de samenmeten API
      sensor_reactive$sensor_data <- as.data.frame(get_sensor_data_api())
      # Bij de sensordata van de API zit ook informatie over de het dichtstbijzijnde knmistation en de luchtmeetnetstations
      # Deze informatie wil je doorgeven aan de andere tabbladen zodat die stations meteen geselecteerd worden
      knmicode_bij_sensor <- unique(sensor_reactive$sensor_data$knmicode)
      luchtmeetnetcode_bij_sensor <- unique(tidyr::pivot_longer(sensor_reactive$sensor_data[,c('kit_id','pm10closecode',
                                              'pm10regiocode','pm10stadcode','pm25closecode',
                                              'pm25regiocode', 'pm25stadcode')], cols=-kit_id)$value)
      # Zet de knmi stations alvast op geselecteerd
      for (knmicode in knmicode_bij_sensor){
        knmicode_id <- substr(knmicode, nchar(knmicode)-3+1, nchar(knmicode)) # Gebruik alleen de 3-cijferige code
        set_knmi_station_select(knmicode_id)
      }
      
      # Zet de luchtmeetnetstations alvast op geselecteerd
      for(luchtmeetnetcode in luchtmeetnetcode_bij_sensor){
        set_lml_station_select(luchtmeetnetcode)
      }
      
      # Laat de stations al op de kaart zien
      add_lmlstat_map()
      add_knmistat_map()      
      
    }else if(overig_reactive$data_set=='eigen_dataset_sensoren'){
      print('Sensor data inladen via eigen dataset csv')
      # Lees de csv uit en sla de gegevens op in interactive
      sensor_reactive$sensor_data <- read.csv(input$eigen_datafile_sensoren$datapath, sep=",")
      # Zet de date naar een posixct
      sensor_reactive$sensor_data$date <- as.POSIXct(sensor_reactive$sensor_data$date, tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }else if(overig_reactive$data_set=='voorbeeld'){
      print('Sensor data inladen via voorbeeld dataset csv')
      
      # Lees de csv uit en sla de gegevens op in interactive
      sensor_reactive$sensor_data <- read.csv(sensor_file, sep=",")
      # Zet de date naar een posixct
      sensor_reactive$sensor_data$date <- as.POSIXct(sensor_reactive$sensor_data$date, tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }
    
    # Check of alle benodigde kolommen aanwezig zijn 
    check_kolommen_sensor()
    
    # Check of er wel metingen zijn, bij de API kan het zo zijn dat er alleen maar de locatie
    # wordt opgehaald. Er zijn dan geen tijdwaardes en metingen
    sensor_reactive$sensor_data <- dplyr::filter(sensor_reactive$sensor_data, !(is.na(date)))
    
    # Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
    sensor_unique <- unique(sensor_reactive$sensor_data[,c('kit_id','lat','lon')])
    sensor_unique$selected <- FALSE
    sensor_unique$huidig <- FALSE
    sensor_unique$groep <- geen_groep
    sensor_unique$kleur <- kleur_marker_sensor
    sensor_unique$lijn <- 'solid'
    
    # Verwijder alle factoren: zet om naar characters
    sensor_unique <- taRifx::remove.factors(sensor_unique)
    sensor_reactive$sensor_data <- taRifx::remove.factors(sensor_reactive$sensor_data)
    
    # Als de sensor geen coordinaten heeft, zet dan op 0,0 (anders werkt spatialpointsdataframe niet)
    sensor_unique$lat[which(is.na(sensor_unique$lat))] <- 0
    sensor_unique$lon[which(is.na(sensor_unique$lon))] <- 0
    
    # Voor de multiselect tool: omzetten lat/lon naar spatialpoints
    sensor_reactive$ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

    # Check welke sensoren geen data hebben (=een error hebben meegegeven bij de api)
    sensor_error <- unique(sensor_reactive$sensor_data$kit_id[which(!is.na(sensor_reactive$sensor_data$error))])
    
    # Geef aan of er wel of geen data is
    sensor_unique$hasdata <- TRUE
    sensor_unique$hasdata[which(sensor_unique$kit_id%in%sensor_error)] <- FALSE
    sensor_unique$kleur[which(sensor_unique$kit_id%in%sensor_error)] <- kleur_marker_sensor_no_data
    
    # Voeg de sensor locaties ed toe aan interactive dataframe
    sensor_reactive$statinfo <- sensor_unique
    # voeg de sensoren toe aan de kaart
    add_sensors_map()
    # zoom naar de nieuwe sensoren
    mean_lat <- mean(sensor_unique$lat[which(sensor_unique$lat>0)])
    mean_lon <- mean(sensor_unique$lon[which(sensor_unique$lon>0)])
    set_view_map(mean_lat, mean_lon)
    
    print(min(sensor_reactive$sensor_data$date, na.rm = T))
    print(max(sensor_reactive$sensor_data$date, na.rm = T))
    
    # Pas ook de Tijdreeks aan voor het analyse gedeelte
    updateDateInput(session,"DateStart", value = min(sensor_reactive$sensor_data$date, na.rm = T))
    updateDateInput(session, "DateEind", value = max(sensor_reactive$sensor_data$date, na.rm = T))
    
  } 
  
  # Functie om de luchtmeetnet data in te laden
  insert_nieuwe_data_lml <- function(){ 
    # Deze functie laadt de luchtmeentnet data, dat kan op 2 manieren:
    # data ophalen van de API, data ophalen uit een csv bestand
    # Wanneer op een van de manieren de data is ingeladen, worden de gegevens in de 
    # reactive values opgeslagen en getoond op de kaart
    
    # Data die er al is wordt overschreven, dus zet de hasdata op FALSE
    lml_stations_reactive$statinfo$hasdata <- FALSE
    lml_stations_reactive$statinfo$name_icon <- 'lml_nodata'
    lml_stations_reactive$statinfo$lijn <- 'solid'
      
    if(overig_reactive$data_set=='API_luchtmeetnet'){
      # Haal de gegevens op van de stations via de luchtmeetnet API
      print('ophalen api luchtmeetnet')
      # Check of er wel stations geselecteerd zijn
      if(!TRUE %in% lml_stations_reactive$statinfo$selected){
        validate('Selecteer een LML-station.')
      }
      lml_stations_reactive$lml_data <- get_lml_data_api()

            # Zet de namen van de componenten PM10 en PM25 naar kleine letters (dan is het hetzelfde als de sensordata)
      lml_stations_reactive$lml_data$formula[which(lml_stations_reactive$lml_data$formula=='PM10')] <- 'pm10'
      lml_stations_reactive$lml_data$formula[which(lml_stations_reactive$lml_data$formula=='PM25')] <- 'pm25'
      
      # De gegevens zijn in long format, zet om naar wide : let op remove duplicates!
      lml_stations_reactive$lml_data <- dplyr::distinct(lml_stations_reactive$lml_data)
      lml_stations_reactive$lml_data <- tidyr::pivot_wider(lml_stations_reactive$lml_data, names_from='formula', values_from='value')
      
      # de lml_data is zo opgebouwd:
      # [1] "ggplot_lml: date"               "ggplot_lml: station_number"     "ggplot_lml: value"              "ggplot_lml: timestamp_measured"
      # [5] "ggplot_lml: formula"  
      
      # Er zijn stations die geen pm10 en geen pm25 meten, deze wil je ook uit de lml_stations_reactive$lml_data
      station_remove_pm10 <- NULL
      station_remove_pm25 <- NULL
      # Check welke stations geen pm10 en geen pm25 hebben doorgegeven
      if('pm10' %in% names(lml_stations_reactive$lml_data)){
        lml_summary <- lml_stations_reactive$lml_data %>% dplyr::group_by(station_number) %>% dplyr::summarise(totaal=dplyr::n(), aantal_na=sum(is.na(pm10)))
        lml_summary_filter <- filter(lml_summary, lml_summary[,'aantal_na']==lml_summary[,'totaal'])
        station_remove_pm10 <- lml_summary_filter$station_number
      }
      if('pm25' %in% names(lml_stations_reactive$lml_data)){
        lml_summary <- lml_stations_reactive$lml_data %>% dplyr::group_by(station_number) %>% dplyr::summarise(totaal=dplyr::n(), aantal_na=sum(is.na(pm25)))
        lml_summary_filter <- filter(lml_summary, lml_summary[,'aantal_na']==lml_summary[,'totaal'])
        station_remove_pm25 <- lml_summary_filter$station_number
      }
      
      # Neem alleen de stations mee die wel pm10 of pm25 hebben gemeten
      lml_stations_reactive$lml_data <- lml_stations_reactive$lml_data[which(!lml_stations_reactive$lml_data$station_number %in% station_remove_pm10 &
                                             !lml_stations_reactive$lml_data$station_number %in% station_remove_pm25),]
      
      print('stations zonder pm10: ')
      print(station_remove_pm10)
      
      print('stations zonder pm25: ')
      print(station_remove_pm25)

    }else if(overig_reactive$data_set=='eigen_dataset_lml'){
       # Haal de gegevens op van de stations via een ingeladen csv
       print('ophalen csv luchtmeetnet')

       # Lees de csv uit en sla de gegevens op in interactive
       lml_stations_reactive$lml_data <- read.csv(input$eigen_datafile_lml$datapath, sep=",", stringsAsFactors = F)
       
       # Zet de date naar een posixct
       lml_stations_reactive$lml_data$date <- as.POSIXct(lml_stations_reactive$lml_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
       
    }else if(overig_reactive$data_set=='voorbeeld'){
      # Haal de gegevens op van de stations via de voorbeeld csv
      print('ophalen voorbeeld csv luchtmeetnet')
      # Lees de csv uit en sla de gegevens op in interactive
      lml_stations_reactive$lml_data <- read.csv(lml_file, sep=",", stringsAsFactors = F)
      
      # Zet de date naar een posixct
      lml_stations_reactive$lml_data$date <- as.POSIXct(lml_stations_reactive$lml_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }

    # Geef aan van welke stations nu databeschikbaar is:
    station_metdata <- unique(lml_stations_reactive$lml_data$station_number)
    lml_stations_reactive$statinfo$hasdata[which(lml_stations_reactive$statinfo$station_number %in% station_metdata)] <- TRUE
    lml_stations_reactive$statinfo$name_icon[which(lml_stations_reactive$statinfo$station_number %in% station_metdata)] <- 'lml_hasdata'
    
    # Deselecteer voor de zekerheid alle stations
    lml_stations_reactive$statinfo$selected <- FALSE
    
    # Verwijder alle factoren: zet om naar characters
    lml_stations_reactive$statinfo <- taRifx::remove.factors(lml_stations_reactive$statinfo)
    lml_stations_reactive$lml_data <- taRifx::remove.factors(lml_stations_reactive$lml_data)
    
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
    knmi_stations_reactive$statinfo$name_icon <- 'knmi_nodata'

    if(overig_reactive$data_set=='API_knmi'){
      # Haal de gegevens op van de stations via de KNMI API
      print('ophalen api KNMI')
      # Check of er wel stations geselecteerd zijn
      if(!TRUE %in% knmi_stations_reactive$statinfo$selected){
        validate('Selecteer een KNMI-station.')
      }
      knmi_stations_reactive$knmi_data <- get_knmi_data_api()
    }else if(overig_reactive$data_set=='eigen_dataset_knmi'){
      # Haal de gegevens op van de stations vanuit een ingelezen bestand
      print('ophalen uit bestand KNMI')
      # Lees de csv uit en sla de gegevens op in interactive
      knmi_stations_reactive$knmi_data <- read.csv(input$eigen_datafile_knmi$datapath, sep=",")
      # Zet de date naar een posixct
      knmi_stations_reactive$knmi_data$date <- as.POSIXct(knmi_stations_reactive$knmi_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }else if(overig_reactive$data_set=='voorbeeld'){
      # Haal de gegevens op van de stations vanuit het voorbeeld bestand
      print('ophalen uit voorbeeldbestand KNMI')
      # Lees de csv uit en sla de gegevens op in interactive
      knmi_stations_reactive$knmi_data <- read.csv(knmi_file, sep=",")
      # Zet de date naar een posixct
      knmi_stations_reactive$knmi_data$date <- as.POSIXct(knmi_stations_reactive$knmi_data$date , tryFormat=c("%d/%m/%Y %H:%M","%Y-%m-%d %H:%M:%S"), tz='UTC')
    }
    
     # Geef aan van welke stations nu databeschikbaar is:
     station_metdata <- unique(knmi_stations_reactive$knmi_data$station_number)
     knmi_stations_reactive$statinfo$hasdata[which(knmi_stations_reactive$statinfo$station_number %in% station_metdata)] <- TRUE
     knmi_stations_reactive$statinfo$name_icon[which(knmi_stations_reactive$statinfo$station_number %in% station_metdata)] <- 'knmi_hasdata'
     knmi_stations_reactive$statinfo$selected[which(knmi_stations_reactive$statinfo$station_number %in% station_metdata)] <- FALSE
     
     # Verwijder alle factoren: zet om naar characters
     knmi_stations_reactive$statinfo <- taRifx::remove.factors(knmi_stations_reactive$statinfo)
     knmi_stations_reactive$knmi_data <- taRifx::remove.factors(knmi_stations_reactive$knmi_data)
     
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
    # Maak een lijst van de station_numbers die je wilt ophalen
    lml_stats <- lml_stations_reactive$statinfo$station_number[which(lml_stations_reactive$statinfo$selected==T)]
    
    # Ga elk station af en haal de gegevens op uit de API
    for(ind in seq(1,length(lml_stats))){
      stat <- lml_stats[ind]
      # Voor de progress messages
      text <- paste0("Luchtmeetnet station: ", stat, " - stap (",ind,"/",length(lml_stats) ,")")
      updateProgress(value = (ind/length(lml_stats))*0.8, detail = text)
      
      # Haal voor elk station de data op van luchtmeetnet API
      station_data_ruw <- GetLMLAPI(stat, format(tijdreeks_reactive$startdatum_tpdata, '%Y%m%d'), format(tijdreeks_reactive$einddatum_tpdata, '%Y%m%d'))
      # Voeg alle meetwaardes vam de stations samen
      station_data_all <- rbind(station_data_all, station_data_ruw$data)}

    # Alle gegevens zijn van de api opgehaald, retun de totale set
    updateProgress(value = 0.90 ,detail = "Alle data van Luchtmeetnet opgehaald.")
    
    #Hernoem de kolom timestamp naar date (dan kan ook openair er mee werken)
    station_data_all <- plyr::rename(station_data_all, c('timestamp_measured'= 'date'))
    
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
    #Maak een string van de station_numbers die je wilt ophalen
    knmi_stats <- knmi_stations_reactive$statinfo$station_number[which(knmi_stations_reactive$statinfo$selected==T)]
    print(paste0('API ophalen: ',knmi_stats))
    
    # Voor de progress message
    progress$set(message = paste0("van stations: ", knmi_stats), value = 0.3)
    
    # ophalen van de gegevens via de API
    station_all_data <- GetKNMIAPI(knmi_stats,format(tijdreeks_reactive$startdatum_tpdata, '%Y%m%d'), format(tijdreeks_reactive$einddatum_tpdata, '%Y%m%d'))
    
    # Je hebt voor nu alleen het data deel van de gegevens uit de api nodig
    station_all_data <- station_all_data$data
    
    # hernoem de kolommen en gooi overige kolommen weg
    station_all_data <- dplyr::select(station_all_data, -c('YYYYMMDD', 'HH'))
    station_all_data <- plyr::rename(station_all_data, c('STNS'= 'station_number', 'DD' = 'wd', 'FF'='ws', 'TEMP'='temp','U' = 'rh', 'tijd'='date'))

    station_all_data$station_code <- paste0('KNMI', station_all_data$station_number)
    
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
    print(paste0('projectnaam: ', projectnaam, ' startdatum: ',tijdreeks_reactive$startdatum_tpdata, ' einddatum: ', tijdreeks_reactive$einddatum_tpdata))
    # Aanroepen van de API
    sensor_data_ruw <- GetSamenMetenAPI(projectnaam, format(tijdreeks_reactive$startdatum_tpdata, '%Y%m%d'), 
                                        format(tijdreeks_reactive$einddatum_tpdata, '%Y%m%d'), data_opslag_list,
                                        updateProgress) 
    print('api opgehaald:')
    print('summary(sensor_data_ruw)')
    print(summary(sensor_data_ruw))
    print('head(sensor_data_ruw$sensordata)')
    print(head(sensor_data_ruw$sensordata))
    # Dit bestaat uit 2 delen: 
    # sensordata die kan worden gebruikt voor de markers(sensor_reactive$statinfo) 
    # metingen met de meetwaardes voor de grafieken (alleen de sensormetingen) dus deel van input_df
    # Deze worden samengevoegd in 1 wide tabel
    sensor_data_metingen <- dplyr::distinct(sensor_data_ruw$metingen)

    sensor_data_all <- sp::merge(sensor_data_metingen, sensor_data_ruw$sensordata[,c('kit_id','lat','lon','knmicode','pm10closecode',
                                                                                     'pm10regiocode','pm10stadcode','pm25closecode',
                                                                                     'pm25regiocode', 'pm25stadcode')],
                                 all.x, by.x='kit_id', by.y='kit_id')
    
    print('data samenvoegen:')
    
    # Omzetten naar een wide dataframe, dat er per kit_id en timestamp 1 rij data is
    # Om te zorgen dat elke rij uniek is gebruik unieke rownumber
    sensor_data_all_wide <- sensor_data_all %>%
      group_by(kit_id, tijd, grootheid) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = grootheid, values_from = waarde) %>%
      select(-row)

      
    print('head(sensor_data_all_wide)')
    print(head(sensor_data_all_wide))
    print('tail(sensor_data_all_wide)')
    print(tail(sensor_data_all_wide))
    
    # Hernoemen van de tijd, zodat hetzelfde is als de input_df
    names(sensor_data_all_wide)[names(sensor_data_all_wide) == "tijd"] <- "date"
    #TODO: de error kolom weglaten.
    updateProgress(100, 'Alles geladen')
    print('einde get api functie sensoren')
    return(sensor_data_all_wide)
  }

  # Functie om de sensor data klaar te maken voor de visualisatie
  filter_sensor_data_plot <- function(){
    # Deze functie maakt/filtert de sensor data voor de visualisatie. De juiste component tijdreeks etc.
    selected_id <- sensor_reactive$statinfo[which(sensor_reactive$statinfo$selected & sensor_reactive$statinfo$groep == geen_groep),'kit_id']
    # Op een of andere manier is de selected_id een dataframe wanneer net uit de api wordt geladen.
    # De datafiltering om show_input te maken werkt alleen met characters
    if(TRUE %in% (class(selected_id)=='tbl')){
      selected_id <- selected_id[[1]]
    }
    
    show_input <- sensor_reactive$sensor_data[which(sensor_reactive$sensor_data$kit_id %in% selected_id),]    

    # Als er groepen zijn geselecteerd, bereken dan het gemiddelde
    if (length(unique(sensor_reactive$statinfo$groep))>1 | unique(sensor_reactive$statinfo$groep) != geen_groep){
      calc_groep_mean() # berekent groepsgemiddeldes
      show_input <- sp::merge(show_input,groepering_reactive$df_gem, all = T) } 

    show_input <- openair::selectByDate(mydata = show_input,start = tijdreeks_reactive$startdatum, end = tijdreeks_reactive$einddatum)
    return(show_input)
  }
  
  # Functie om de knmi data klaar te maken voor de visualisatie
  filter_knmi_data_plot <- function(){
    # Deze functie maakt/filtert de knmi data voor de visualisatie. De juiste tijdreeks etc.
    selected_id <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_number']
    
    show_input <- knmi_stations_reactive$knmi_data[which(knmi_stations_reactive$knmi_data$station_number %in% selected_id),]    
    # TODO of wil je dat je de windroos alleen kan maken via de dropdown knmi_stat_wdws? dat je ze sowieso niet kan selecteren?
    show_input <- openair::selectByDate(mydata = show_input,start = tijdreeks_reactive$startdatum, end = tijdreeks_reactive$einddatum)
    
    return(show_input)
  }
  
  # Functie om de lml data klaar te maken voor de visualisatie
  filter_lml_data_plot <- function(){
    # Deze functie maakt/filtert de lml data voor de visualisatie. De juiste component tijdreeks etc.
    # Geef aan welke stations zijn geselecteerd
    lml_selected_id <- lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),'station_number']

    # Zoek daarbij de gegevens op
    lml_show_input <- lml_stations_reactive$lml_data[which(lml_stations_reactive$lml_data$station_number %in% lml_selected_id),] 

    # Maak een selectie op de geselecteerde tijdreeks
    lml_show_input <- openair::selectByDate(mydata = lml_show_input,start = tijdreeks_reactive$startdatum, end = tijdreeks_reactive$einddatum)
    
    # Geef de naam van het station mee als kit_id dan komt die in de labels van de grafieken
    lml_show_input <- merge(lml_show_input, lml_stations_reactive$statinfo[,c("station_number", "naam")], 
                            by.x="station_number", by.y="station_number",
                            all.x=T)
    lml_show_input$kit_id <- lml_show_input$naam

    # Zet de waardes ook in de pm10_kal en pm25_kal. Voor de luchtmeetnet stations zijn die gelijk aan pm10 en pm25
    lml_show_input$pm10_kal <- lml_show_input$pm10
    lml_show_input$pm25_kal <- lml_show_input$pm25
    
    return(lml_show_input)
  }

  check_selected_id <- function(id_select){
    # Bepaal of er op een sensr wordt geklikt, dus niet op lml station of knmi station.
    # Het lml station begint met 'NL', het knmi station is een numeric
    print(paste0('SELECTED ID: ', id_select))
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
        if((lml_stations_reactive$statinfo$selected[which(lml_stations_reactive$statinfo$station_number == id_select)])){
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
        if((knmi_stations_reactive$statinfo$selected[which(knmi_stations_reactive$statinfo$station_number == id_select)])){
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
    overig_reactive$data_set <- input$eigen_datafile_sensoren$datapath  
    overig_reactive$data_set <- 'eigen_dataset_sensoren'
    insert_nieuwe_data_sensor()})
  
  # observe of er een eigen data set is ingeladen voor de luchtmeetnetmetingen:----
  observeEvent({req(input$eigen_datafile_lml)},{
    overig_reactive$data_set <- input$eigen_datafile_lml$datapath  
    overig_reactive$data_set <- 'eigen_dataset_lml'
    insert_nieuwe_data_lml()})
  
  # observe of er een eigen data set is ingeladen voor de knmi metingen:----
  observeEvent({req(input$eigen_datafile_knmi)},{
    overig_reactive$data_set <- input$eigen_datafile_knmi$datapath  
    overig_reactive$data_set <- 'eigen_dataset_knmi'
    insert_nieuwe_data_knmi()})  
  
  # Observe of de voorbeeld dataset weer ingeladen moet worden----
  observeEvent({input$voorbeeld_data},{
    overig_reactive$data_set <- 'voorbeeld'
    # Zet alles dat al geselecteerd is op deselect
    lapply(lml_stations_reactive$statinfo$station_number[which(lml_stations_reactive$statinfo$selected)], set_lml_station_deselect)
    lapply(knmi_stations_reactive$statinfo$station_number[which(knmi_stations_reactive$statinfo$selected)], set_knmi_station_deselect)
    
    # Laadt de voorbeeld datasets
    insert_nieuwe_data_sensor()
    insert_nieuwe_data_lml()
    insert_nieuwe_data_knmi()})

  #Observe of de luchtmeetnetstations moeten worden getoond----
  observeEvent({input$show_luchtmeetnet},{
    add_lmlstat_map()
    })
  
  # #Observe of de knmi-stations moeten worden getoond----
  observeEvent({input$show_knmi},{
    add_knmistat_map()
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
  
  # Observe of de datum wordt aangepast voor de plots (dus het visualisatie gedeelte)----
  observeEvent({input$DateStart},{
    tijdreeks_reactive$startdatum <- input$DateStart
  })
  
  observeEvent({input$DateEind},{
    tijdreeks_reactive$einddatum <- input$DateEind
  })
  
  # Observe of de datum wordt aangepast voor de api (dus het data gedeelte----
  observeEvent({input$DateStart_tpData},{
    tijdreeks_reactive$startdatum_tpdata <- input$DateStart_tpData
  })
  
  observeEvent({input$DateEind_tpData},{
    tijdreeks_reactive$einddatum_tpdata <- input$DateEind_tpData
  })
  
  # Observe if user selects a sensor ----
  observeEvent({input$map_marker_click$id}, {
    id_select <- input$map_marker_click$id
    print('inputmarker select: id_select')
    print(id_select)
    check_selected_id(id_select)
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
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata == TRUE), "selected"] <- FALSE 
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata == TRUE), "kleur"] <- kleur_marker_sensor
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata == TRUE), "groep"] <- geen_groep
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata == TRUE), "huidig"] <- FALSE 
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
      kleur_sensor <- sensor_reactive$statinfo$kleur[which(sensor_reactive$statinfo$huidig)][1]
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
    print('map_draw_new_feature: mulitselect')
    # Houd bij hoeveel features er zijn. Later nodig bij verwijderen, i.v.m. reset ook de losse selectie.
    overzicht_shapes$add <- overzicht_shapes$add + 1
    
    # Zoek de sensoren in de feature
    found_in_bounds <- findLocations(shape = input$map_draw_new_feature,
                                     location_coordinates = sensor_reactive$ms_coordinates,
                                     location_id_colname = "kit_id")

    # Ga elke sensor af en voeg deze bij de selectie
    for(id_select in found_in_bounds){
      check_selected_id(id_select)
    }
    print('einde multiselsect')
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
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata), "selected"] <- FALSE 
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata), "kleur"] <- kleur_marker_sensor
      sensor_reactive$statinfo[which(sensor_reactive$statinfo$hasdata), "groep"] <- geen_groep
      add_sensors_map()
    }
    else{
      # Als er maar één feature wordt verwijderd, ga dan de sensoren af en deselecteer deze een voor een
      for(feature in input$map_draw_deleted_features$features){
        found_in_bounds <- findLocations(shape = feature, 
                                           location_coordinates = sensor_reactive$ms_coordinates, 
                                           location_id_colname = "kit_id")
        # ga dan de sensoren af en deselecteer deze een voor een
        for(id_select in found_in_bounds){
          check_selected_id(id_select)
        }
      }
    }
    # Houd bij hoeveel shapes er nog zijn
    overzicht_shapes$add <- overzicht_shapes$add - overzicht_shapes$delete
  })
  
  
  ## GENEREER PLOTS -----
  
  # Haal op en Download de data van de geselecteerde LML stations ----
  output$downloadData_luchtmeetnet <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_luchtmeetnet', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      print("Eerste aanroep api luchtmeetnet")
      overig_reactive$data_set <- 'API_luchtmeetnet'
      insert_nieuwe_data_lml()
      write.table(lml_stations_reactive$lml_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  # Download de data van de geselecteerde LML stations ----
  output$downloadData_luchtmeetnet2 <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_luchtmeetnet', 'csv', sep=".")
    },
    # Geef de data op: luchtmeetnetstations
    content = function(file) {
      write.table(lml_stations_reactive$lml_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Haal op en Download de data van de geselecteerde KNMI stations ----
  output$downloadData_knmi <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_knmi', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      print("Eerste aanroep api knmi")
      overig_reactive$data_set <- 'API_knmi'
      insert_nieuwe_data_knmi()
      write.table(knmi_stations_reactive$knmi_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Download de data van de geselecteerde KNMI stations ----
  output$downloadData_knmi2 <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_knmi', 'csv', sep=".")
    },
    # Geef de data op: knmi stations
    content = function(file) {
      write.table(knmi_stations_reactive$knmi_data, file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Haal op en Download de data van de projectgeselecteerde sensoren ----
  output$downloadData_sensor <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_sensoren', 'csv', sep=".")
    },
    # Geef de data op: deze wordt eerst met de API opgehaald
    content = function(file) {
      print("Eerste aanroep api samenmeten")
      overig_reactive$data_set <- 'API_samenmeten'
      insert_nieuwe_data_sensor()
      print('schrijf sensordata weg naar file: downloadwindow')
      write.table(sensor_reactive$sensor_data[,c('kit_id',	'date',	'lat',	'lon',	
                                                 'pm10_kal',	'pm10',	'pm25_kal',	
                                                 'pm25',	'rh',	'temp')], 
                  file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Download de data van de projectgeselecteerde sensoren ----
  # Op een of andere manier werkt de eerste button niet als de dataset te groot is.
  # Daarom nog maar eentje die alleen download.
  output$downloadData_sensor2 <- downloadHandler(
    # geef de filename op, zou via interactieve kunnen
    filename = function(){
      paste('data_sensoren', 'csv', sep=".")
    },
    # Geef de data op: de sensordata
    content = function(file) {
      write.table(sensor_reactive$sensor_data[,c('kit_id',	'date',	'lat',	'lon',	
                                                 'pm10_kal',	'pm10',	'pm25_kal',	
                                                 'pm25',	'rh',	'temp')], 
                  file, sep = ',',
                  row.names = FALSE)
    }
  )
  
  # Tekst voor bij de downloaden data van api voor welke tijdreeks wordt gebruikt
  output$tijdreeks_tpdata_lml <- renderText({
    paste0("Tijdreeks: ", tijdreeks_reactive$startdatum_tpdata, ' tot ', tijdreeks_reactive$einddatum_tpdata, '  (Zelf in te stellen in stap 1)')
  })
  
  # Elk id kan maar 1x worden gebruikt, dus voor ander tabblad nieuw id
  # Tekst voor bij de downloaden data van api voor welke tijdreeks wordt gebruikt
  output$tijdreeks_tpdata_knmi <- renderText({
    paste0("Tijdreeks: ", tijdreeks_reactive$startdatum_tpdata, ' tot ', tijdreeks_reactive$einddatum_tpdata, '  (Zelf in te stellen in stap 1)')
    
  })
  

  # Create tabel huidige selectie ----
  output$huidig <- renderTable({
    if(!TRUE %in% sensor_reactive$statinfo$huidig){
      validate("Geen sensor geselecteerd.")
    }
  huidig_df <- data.frame('Selectie' = sensor_reactive$statinfo[which(sensor_reactive$statinfo$huidig),'kit_id'])
  })
  
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_lml <- renderTable({
    stations_df <- data.frame('Naam' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('naam')],
                              'Nummer' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('station_number')],
                              'Organisatie' = lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('organisatie')])
  })
  # Create tabel geselecteerde stations voor de download pagina ----
  output$stations_knmi <- renderTable({
    stations_df <- data.frame('Nummer' = as.character(knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_number']))
  })
  
  # Create average bar plot met ggplot ----
  output$barplot <- renderPlot({
    # Geef aan welk component geplot moet worden
    comp <- input$Component
    
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    selected_lml_hasdata <- (TRUE %in% lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),'hasdata'])
    if(selected_sensor==FALSE & selected_lml_hasdata == FALSE){
      validate("Selecteer een sensor of luchtmeetnetstation.")
    }
    
    # Initieer
    show_input <- NULL
    kit_kleur <- NULL
    
    if(selected_sensor){
      # Maak de plot input van de sensoren
      show_input <- filter_sensor_data_plot()
      
      ## Create array for the colours
      # get the unique kit_id and the color
      kit_kleur <- unique(sensor_reactive$statinfo[which(sensor_reactive$statinfo$selected),c('kit_id','kleur','groep', 'lijn')])
      
      # Als er een groep is, zorg voor 1 rij van de groep, zodat er maar 1 kleur is
      if (length(unique(kit_kleur$groep)>1)){
        kit_kleur[which(kit_kleur$groep != geen_groep),'kit_id'] <- kit_kleur[which(kit_kleur$groep != geen_groep),'groep']
        kit_kleur <- unique(kit_kleur)
      }
      kit_kleur <- taRifx::remove.factors(kit_kleur)
    }
    
    # Check of er een luchtmeetnet station geselecteerd is:
    if(selected_lml_hasdata){
      # Filter de lml data op de juiste gegevens
      lml_show_input <- filter_lml_data_plot()
      # Zorg ook voor het kleur en lijntype overzicht
      lml_kit_kleur <- unique(lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('station_number','kleur','groep', 'lijn')])
      names(lml_kit_kleur)[names(lml_kit_kleur)=="station_number"] <- "kit_id"
      
      # Voeg de LML data aan de sensordata toe:
      show_input <- plyr::rbind.fill(show_input, lml_show_input)
      kit_kleur <- rbind(kit_kleur, lml_kit_kleur)
    }
    
    # Bepaal de kleuren en lijntypes voor in de plot
    # Sort by kit_id
    kit_kleur_sort <- kit_kleur[order(kit_kleur$kit_id),]
    # create colour array
    kleur_array <- kit_kleur_sort$kleur
    # Maak ook voor de lijntype een array
    lijn_array <- kit_kleur_sort$lijn
    
    # Bereken de statistic summary
    show_input <- Rmisc::summarySE(show_input, comp, groupvars='kit_id', na.rm = T)
    
    print(show_input)
    
    # maak de plot
    p_barplot <- ggplot(data = show_input, aes_string(x = 'kit_id', y=comp, group = "kit_id", fill="kit_id")) +
      geom_col() +
      geom_linerange(aes_string(ymin=paste0(comp,'-se'), ymax=paste0(comp,'+se'))) +
      scale_fill_manual(values = kleur_array) +
      labs(x='', y = 'gemiddelde concentratie (ug/m3)', title=paste0('Component: ',comp)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    plot(p_barplot)
  })
  
  
  
  
  # Create time plot met ggplot ----
  output$timeplot <- renderPlot({
    # Geef aan welk component geplot moet worden
    comp <- input$Component
    
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    selected_lml_hasdata <- (TRUE %in% lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),'hasdata'])
    if(selected_sensor==FALSE & selected_lml_hasdata == FALSE){
      validate("Selecteer een sensor of luchtmeetnetstation.")
    }
    
    # Initieer
    show_input <- NULL
    kit_kleur <- NULL
    
    if(selected_sensor){
      # Maak de plot input van de sensoren
      show_input <- filter_sensor_data_plot()

      ## Create array for the colours
      # get the unique kit_id and the color
      kit_kleur <- unique(sensor_reactive$statinfo[which(sensor_reactive$statinfo$selected),c('kit_id','kleur','groep', 'lijn')])
      # Als er een groep is, zorg voor 1 rij van de groep, zodat er maar 1 kleur is
      if (length(unique(kit_kleur$groep)>1)){
        kit_kleur[which(kit_kleur$groep != geen_groep),'kit_id'] <- kit_kleur[which(kit_kleur$groep != geen_groep),'groep']
        kit_kleur <- unique(kit_kleur)
      }
      
      kit_kleur <- taRifx::remove.factors(kit_kleur)
    }
    
    # Check of er een luchtmeetnet station geselecteerd is met data:
    if(selected_lml_hasdata){
      # Filter de lml data op de juiste gegevens
      lml_show_input <- filter_lml_data_plot()
      
      # Zorg ook voor het kleur en lijntype overzicht
      lml_kit_kleur <- unique(lml_stations_reactive$statinfo[which(lml_stations_reactive$statinfo$selected),c('station_number','kleur','groep', 'lijn')])
      names(lml_kit_kleur)[names(lml_kit_kleur)=="station_number"] <- "kit_id"
      
      # Voeg de LML data aan de sensordata toe:
      show_input <- plyr::rbind.fill(show_input, lml_show_input)
      kit_kleur <- rbind(kit_kleur, lml_kit_kleur)
    }
    
    print(kit_kleur)
    
    
    # Bepaal de kleuren en lijntypes voor in de plot
    # Sort by kit_id
    kit_kleur_sort <- kit_kleur[order(kit_kleur$kit_id),]
    # create colour array
    kleur_array <- kit_kleur_sort$kleur
    # Maak ook voor de lijntype een array
    lijn_array <- kit_kleur_sort$lijn
    
    glimpse(show_input)
    # maak de plot
    p_timeplot <- ggplot(data = show_input, aes_string(x = "date", y = comp, group = "kit_id")) +
      geom_line(aes(linetype= kit_id, col=kit_id)) +
      scale_color_manual(values = kleur_array) +
      scale_linetype_manual(values = lijn_array) +
      labs(x = "Tijd", y = 'concentratie (ug/m3)', title=paste0('Component: ',comp)) +
      expand_limits(y=0) + # Zodat er geen negatieve waardes worden getoond
      theme_bw()

    plot(p_timeplot)
    print('eindeplot')
  })
  
  
 
  # Create kalender plot vanuit openair ----
  output$calendar <- renderPlot({
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    if(selected_sensor==FALSE){
      validate("Selecteer een sensor.")
    }
    
    # haal de daat op om te plotten
    show_input <- filter_sensor_data_plot()

    try(calendarPlot(show_input,
                     pollutant = input$Component, limits= c(0,150), cols = 'Purples', local.tz="Europe/Amsterdam")) 
    # Call in try() zodat er geen foutmelding wordt getoond als er geen enkele sensor is aangeklikt 
  })
  
  # Create timevariation functie vanuit openair ----
  output$timevariation <- renderPlot({
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    if(selected_sensor==FALSE){
      validate("Selecteer een sensor.")
    }
    
    # haal de data op om te plotten
    show_input <- filter_sensor_data_plot()
    
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
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    if(selected_sensor==FALSE){
      validate("Selecteer een sensor.")
    }
    
    selected_knmi <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_number']
    selected_knmi_hasdata <- (TRUE %in% knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'hasdata'])
    if(is_empty(selected_knmi)){
      validate("Selecteer 1 KNMI-station.")
    }
    if(length(selected_knmi)>1){
      validate("U heeft meerdere KNMI-stations geslecteerd. Selecteer slechts 1 KNMI-station.")
    }
    if(!selected_knmi_hasdata){
      validate("Er is geen data beschikbaar van dit KNMI-station.")
    }
    
    # Zorg voor de juiste gegevens van de sensoren
    show_input <- filter_sensor_data_plot()
    # Zorg voor de KNMI winddata
    knmi_data_wdws <- filter_knmi_data_plot()
    
    # Voeg de winddata aan de sensordata toe
    show_input <- sp::merge(show_input, knmi_data_wdws, all.x=T, by.x='date', by.y='date')
    
    try(pollutionRose(show_input,
                      pollutant = input$Component, wd = 'wd', ws = 'ws', type = 'kit_id' , local.tz="Europe/Amsterdam", cols = "Purples", statistic = 'prop.mean',breaks=c(0,20,60,100))) 
  })
  
  
  # Create windrose vanuit openair---- 
  output$windplot <- renderPlot({
    # Check of er wel wat geselecteerd is om te plotten en dat er data is
    selected_id <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_number']
    selected_knmi_hasdata <- (TRUE %in% knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'hasdata'])
    
    if(is_empty(selected_id)){
      validate("Selecteer een KNMI-station.")
    }
    if(!selected_knmi_hasdata){
      validate("Er is geen data beschikbaar van dit KNMI-station.")
    }
    
    show_input <- filter_knmi_data_plot() 
    
    # TODO Check of er wel data is, of dat ws en wd NA zijn.
    try(windRose(show_input, wd = 'wd', ws = 'ws', type = 'station_code' , local.tz="Europe/Amsterdam", cols = "Purples")) 
  })
  
  # Create percentilerose functie vanuit openair ----
  output$percentileplot <- renderPlot({
    # Check of er wel wat geselecteerd is om te plotten
    selected_sensor <- (TRUE %in% sensor_reactive$statinfo$selected)
    if(selected_sensor==FALSE){
      validate("Selecteer een sensor.")
    }
    
    selected_knmi <- knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'station_number']
    selected_knmi_hasdata <- (TRUE %in% knmi_stations_reactive$statinfo[which(knmi_stations_reactive$statinfo$selected),'hasdata'])
    
    if(is_empty(selected_knmi)){
      validate("Selecteer 1 KNMI-station.")
    }
    if(length(selected_knmi)>1){
      validate("U heeft meerdere KNMI-stations geslecteerd. Selecteer slechts 1 KNMI-station.")
    }
    if(!selected_knmi_hasdata){
      validate("Er is geen data beschikbaar van dit KNMI-station.")
    }
    
    # Zorg voor de juiste gegevens van de sensoren
    show_input <- filter_sensor_data_plot()
    # Zorg voor de KNMI winddata
    knmi_data_wdws <- filter_knmi_data_plot()
    
    # Voeg de winddata aan de sensordata toe
    show_input <- sp::merge(show_input, knmi_data_wdws, all.x=T, by.x='date', by.y='date')
    
    try(percentileRose(show_input,
                       pollutant = input$Component, wd = 'wd', type = 'kit_id', local.tz="Europe/Amsterdam", percentile = NA)) 
    
  })  
}
