########
# Functies voor de apis
##########

GetLMLAPI <- function(station, ymd_vanaf, ymd_tot){
  # Functie om de gegevens van een LML station (eigenlijk station van luchtmeetnet)
  # voor een bepaalde periode op te halen
  # het kan zijn dat er meer data wordt opgehaal dan gespecificeerd.
  # Voorbeeld: TEST <- GetLMLAPI("NL01908", "20190505", "20190710")
  #
  #input:
  #   station: string met stationsnummer bijv. NL01908
  #   ymd_vanaf: string met de datum van het begin van de periode bijv.
  #   ymd_tot: string met de datum van het eind van de periode bijv.
  # 
  #output:
  #   named list met:
  #   info: dataframe met de kolommen lon, lat, type, naam, id, error
  #   data: dataframe met de kolommen c("component","waarde","tijd","station")
  #       formula: het gemeten component, bijv. NO2
  #       value: de gemeten concentratie microgram per kubieke meter
  #       timestamp_measured: tijd in UTC (Eindtijd van het uurgemiddelde)
  #       station_number: het nummer/id van het station bijv. NL01908
  
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()
  
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
  ## Ophalen van de ststionsinformatie ----
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations/",station, sep="")
  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe(url_stat_info)

  tryCatch({
    # Neem de info eruit die je nodig hebt: 
    stat_componenten <- content_stat_info$data$components
    stat_coords <- content_stat_info$data$geometry$coordinates
    stat_type <- content_stat_info$data$type
    stat_naam <- content_stat_info$data$location
    
    # Sla op in een dataframe
    stat_info_df <- data.frame(lon=stat_coords[1],lat=stat_coords[2], type=stat_type, naam=stat_naam, id=station)
  }, error = function(e){
    stop("Error in URL station. Check of stationscode juist is.")
  })
  
  ## Ophalen van de meetgegevens ----
  ymd_vanaf <- as.POSIXct(ymd_vanaf, format="%Y%m%d") 
  ymd_tot <- as.POSIXct(ymd_tot, format="%Y%m%d") + 60*60*24*7 # neem een week extra, voor de cut functie
  
  # Deel de tijdsperiode op in weken, de api kan maar 1 week data leveren per keer
  week_opdeling <- cut(c(ymd_vanaf, ymd_tot),"weeks")
  
  # Ga elke week af en haal de gegevens op
  for(index_week in seq(1,length(levels(week_opdeling))-1)){
    # Stel de URL samen van de week en de stationnummer
    print(levels(week_opdeling)[index_week])
    startdatum <- format(as.POSIXct(levels(week_opdeling)[index_week], format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    einddatum <- format(as.POSIXct(levels(week_opdeling)[index_week+1], format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    url_week <- paste("https://api.luchtmeetnet.nl/open_api/measurements?station_number=",station,"&start=",startdatum,"&end=",einddatum, sep="")
    url_week <- gsub(" ","T", url_week) # Spaties mogen niet in de api, dan krijg je geen resultaat terug.
    print(url_week)
    # Haal de gegevens op
    content_measurements <-  GetAPIDataframe(url_week)
    if (length(content_measurements) == 1) {
      # Dan is er een error teruggekomen, bijvoorbeeld 502
      print(content_measurements)
      stat_info_df$error <- "Error in gegevens ophalen. Check of alle gegevens er zijn."
      next
    } else{
      # substract het stuk data 
      measurements_data <- content_measurements$data
      # Zet de tijd om naar POSTXct in de UTC tijdszone
      measurements_data$timestamp_measured <- as.POSIXct(sub('T',' ', measurements_data$timestamp_measured), tz='UTC')
      # Voeg de data aan de dataframe
      metingen_df <- rbind(metingen_df, measurements_data)
    }
  }
  
  # Maak een named list voor de output
  lml_info_data <- list(info=stat_info_df, data=metingen_df)
  
  return(lml_info_data)
}

# Functie voor het halen van data van de url ----
GetAPIDataframe <- function(url_api){
  ###
  # Deze functie roept de API aan met input url
  # haalt de inhoud op als text, dit is een JSON
  # pakt de Json uit 
  ###
  
  # Parameters voor de check op errors
  nog_eens_opvragen <- TRUE
  counter <- 1
  
  # While voor de check op errors, als de server iets heeft, wil dat niet zeggen 
  # dat de data er niet is. Dan gewoon nog eens aan server vragen
  while (nog_eens_opvragen){
    # Vraag de URL op via API
    raw_result <- httr::GET(url=url_api)
    
    # Check de status: is de api correct?
    # Er is iets met de server, gewoon nog eens proberen
    # Tenzij er iets met de data (bijv. je vraag een project op dat niet bestaat)
    # Daarvoor zit een counter dat je niet eeuwig in de while loop kunt blijven
    if (raw_result$status_code != 200){
      if (counter < 100){
        counter <- counter + 1 
        nog_eens_opvragen <- TRUE}
      # Als er na 100 keer nog geen antwoord gevonden is, return de error
      else{return(paste('Error', raw_result$status_code, sep=":"))}
    }
    # Het is gelukt, verwerk de data
    else{
      # Uitpakken van de content en omzetten naar list met dataframe
      raw_content <- rawToChar(raw_result$content)
      content <- jsonlite::fromJSON(raw_content) 
      
      # Check of er wel content is:
      if(length(content[[1]])<1){
        return('Error: Er is geen content gevonden.')
      }
      else{return(content)}
    }
  }
}
