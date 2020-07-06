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
  
  ## Initializatie
  # Generate base map ----
  # Hierop staan de knmi-stations, de luchtmeetnetstations en de sensoren
  # Daarnaast zijn er edit buttons toegevoegd
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(4.720130, 52.408370, zoom = 10) %>%
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
  
  # Zet reactive dataframe op ----
  values <- reactiveValues(df = sensor_unique, sensor_data = input_df,groepsnaam = geen_groep, df_gem = data.frame(), startdatum = 0, einddatum=0,
                           data_set = 'voorbeeld') 
  overzicht_shapes <- reactiveValues(add = 0, delete = 0) # nodig om selectie ongedaan te maken
  
  ## FUNCTIES ----
  
  # Functie: Set the sensor as deselect and change color to base color ----
  set_sensor_deselect <- function(id_select){
    values$df[values$df$kit_id == id_select, "selected"] <- FALSE 
    values$df[values$df$kit_id == id_select, "huidig"] <- FALSE 
    values$df[values$df$kit_id == id_select, "kleur"] <- kleur_marker_sensor
    values$df[values$df$kit_id == id_select, "groep"] <- geen_groep
  }
  
  # Functie: Set sensor as select and specify color ----
  set_sensor_select <- function(id_select){
    values$df[values$df$kit_id == id_select, "selected"] <- TRUE
    values$df[values$df$kit_id == id_select, "huidig"] <- TRUE
    # Selecteen kleur en geef dit mee aan de sensor
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
  
  # Functie: plaats sensoren met juiste kleur op de kaart ----
  add_sensors_map <- function(){ 
    # Regenerate the sensors for the markers
    sensor_loc <- unique(select(values$df, kit_id, lat, lon, kleur, selected))
    
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("sensoren") # Clear sensor markers
    proxy %>% addCircleMarkers(data = sensor_loc, ~lon, ~lat, layerId = ~kit_id, label = lapply(as.list(sensor_loc$kit_id), HTML),
                               radius = 8, color = ~kleur, fillOpacity = 1,stroke = ~selected, group = "sensoren")}
  
  # Functie om de zoom/view te centreren rond de sensoren
  set_view_map <- function(lat, lon){
    # create a proxy map and zoom to the location
    proxy <- leafletProxy('map')
    proxy %>% setView(lon, lat, zoom = 10)
  }
  
  # Functie om van alle groepen in de dataset een gemiddelde te berekenen ----
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
  
  # Functie om een dataset in te lezen en vervolgens juist om te zetten
  # in de reactive dataframe etc om te tonen in de app.
  Insert_nieuwe_data <- function(){
    # deze functie neemt de data en verandert de values$sensor_data en de values$df
    # aan de hand van de invoer. Dat kan een eigen bestand zijn, of de voorbeeld dataset
    if(values$data_set == 'voorbeeld'){
      print('voorbeeld')
      values$sensor_data <- readRDS(file)
      
      ## Default locatie, kleur en label opzetten ----
      values$sensor_data$kit_id <- gsub('HLL_hl_', '', values$sensor_data$kit_id) #remove HLL-string from values$sensor_data for shorter label
      
    }else{
      print(values$data_set)
      values$sensor_data <- read.csv(input$eigen_datafile$datapath)
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
    print(head(values$sensor_data))
    # voeg de sensoren toe aan de kaart
    add_sensors_map()
    # zoom naar de nieuwe sensoren
    mean_lat <- mean(sensor_unique$lat)
    mean_lon <- mean(sensor_unique$lon)
    set_view_map(mean_lat, mean_lon)
  }
  
  ## OBSERVE EVENTS ----
  
  # observe of er een eigen data set is ingeladen:
  observeEvent({req(input$eigen_datafile)},{
    values$data_set <- input$eigen_datafile$datapath           
    print("databestand geladen")
    Insert_nieuwe_data()})
  
  # Observe of de voorbeeld dataset weer ingeladen moet worden
  observeEvent({input$voorbeeld_data},{
    values$data_set <- 'voorbeeld'
    Insert_nieuwe_data()})
  
  #Observe of de luchtmeetnetstations moeten worden getoond
  observeEvent({input$show_luchtmeetnet},{
    print('laad zien')
    # Update map with new markers to show selected 
    proxy <- leafletProxy('map') # set up proxy map
    proxy %>% clearGroup("sensoren")  # Clear sensor markers
    proxy %>% addMarkers(icon = icons_stations["lml"], data = lml_stations_all, ~lon, ~lat, layerId = ~statcode, label = lapply(lml_labels, HTML))
    proxy %>% setView(5.12446,52.105, zoom = 6)
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
  
  # Observe of de datum wordt aangepast
  observeEvent({input$DateStart},{
    values$startdatum <- input$DateStart
  })
  
  observeEvent({input$DateEind},{
    values$einddatum <- input$DateEind
  })
  
  
  # Observe if user selects a sensor ----
  observeEvent({input$map_marker_click$id}, {
    id_select <- input$map_marker_click$id
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
  
  # Observe of de alle geselecteerde sensoren moet worden gereset ----
  # De values selected worden weer FALSE en de markers kleur_sensor_marker gekleurd, groepen verwijderd
  observeEvent(input$reset_all, {
    values$df[, "selected"] <- FALSE 
    values$df[, "kleur"] <- kleur_marker_sensor
    values$df[, "groep"] <- geen_groep
    values$df[, "huidig"] <- FALSE 
    # Laad de sensoren op de kaart zien
    add_sensors_map()
  })
  
  # Observe of de selectie moet worden toegevoegd aan de groep ----
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

  
  # Voor de bestaande groepen: maak de input-ui ----
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
  
  
  ## Genereer plots -----
  
  # Create tabel huidige selectie ----
  output$huidig <- renderTable({
  huidig_df <- data.frame('Selectie' =values$df[which(values$df$huidig),'kit_id'])
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

    # if / else statement om correctie lml data toe te voegen ----
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
  
  
  # Create windrose vanuit openair ----
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
