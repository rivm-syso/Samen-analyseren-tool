########
# Functies voor de apis
##########

GetLMLallstatinfoAPI <- function(){
  # Functie om alle stations van het luchtmeetnet op te halen.
  # bijv. TEST <-GetLMLallstatinfoAPI()
  
  # output: dataframe met de kolommen:
  # station_number: id van het station
  # naam: naam van het stations
  # lat 
  # lon 
  # stattype: stationstype
  # organisatie: de organisatie van wie het station is 
  # Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
  
  
  # Initialisatie
  # Dataframe waar alles mag worden opgeslagen
  stat_info <- data.frame('id'=NULL,'naam'=NULL)
  stat_info_compleet <- data.frame('station_number'=NULL,'naam'=NULL, 'lat'=NULL, 'lon'=NULL, 'stattype'=NULL, 'organisatie'=NULL)
  
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
  ## Ophalen van de ststionsinformatie ----
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations")
  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe(url_stat_info)
  # print(paste0("url gebruikt: ", url_stat_info))
  
  # Het werkt met pagina's. 
  # Ga elke pagina af en haal de id en naam van de stations op
  verschillende_pages <- content_stat_info$pagination$page_list
  for(pagina in verschillende_pages){
    url_page <- paste0(url_stat_info, '/?page=', pagina)
    # haal de gegevens van de url op
    content_stat_info <- GetAPIDataframe(url_page)
    # print(paste0("url gebruikt: ", url_page))
    
    # Maak dataframe waar je de gegevens van deze pagina tijdelijk opslaat
    stat_info_new <- NULL
    stat_info_new$id <- content_stat_info$data$number
    stat_info_new$naam <- content_stat_info$data$location 
    
    # Voeg alles samen tot 1 dataframe
    stat_info <- rbind(stat_info, stat_info_new)
  }
  
  # Ga voor elk station ook de coordinaten ophalen
  for(station in stat_info$id){
    stat_info_single <- GetLMLstatinfoAPI(station)
    stat_info_compleet <- rbind(stat_info_compleet, stat_info_single)
  }
  
  return(stat_info_compleet)
}

GetLMLstatinfoAPI <- function(station){
  # Functie om van een bepaald luchtmeetnetstation de stationsinformatie op te halen
  # Bijv. TEST <- GetLMLstatinfoAPI("NL01908")
  # input: station: string met stationsnummer bijv. NL01908
  # output: info: dataframe met de kolommen lon, lat, stattype, naam, station_number
  # Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
  
  # Initialisatie

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
  ## Ophalen van de ststionsinformatie ----
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations/",station, sep="")
  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe(url_stat_info)
  # print(paste0("url gebruikt: ", url_stat_info))
  tryCatch({
    # Neem de info eruit die je nodig hebt: 
    stat_componenten <- content_stat_info$data$components
    stat_coords <- content_stat_info$data$geometry$coordinates
    stat_type <- content_stat_info$data$type
    stat_naam <- content_stat_info$data$location
    stat_organ <- content_stat_info$data$organisation
    
    # Sla op in een dataframe
    stat_info_df <- data.frame(lon=stat_coords[1],lat=stat_coords[2], stattype=stat_type, naam=stat_naam, station_number=station, organisatie=stat_organ)
  }, error = function(e){
    stop("Error in URL station. Check of stationscode juist is.")
  })
  
  return(stat_info_df)
}

GetLMLstatdataAPI <- function(station, ymd_vanaf, ymd_tot){
  # Functie om de meetwaardes van een luchtmeetstation op te halen voor een bepaalde periode
  # het kan zijn dat er meer data wordt opgehaal dan gespecificeerd.
  # Voorbeeld: TEST <- GetLMLstatdataAPI("NL01908", "20190505", "20190510") 
  #input:
  #   station: string met stationsnummer bijv. NL01908
  #   ymd_vanaf: string met de datum van het begin van de periode bijv.
  #   ymd_tot: string met de datum van het eind van de periode bijv.
  # 
  #output:
  #       metingen_df: dataframe met de kolommen c("component","waarde","tijd","station")
  #       formula: het gemeten component, bijv. NO2
  #       value: de gemeten concentratie microgram per kubieke meter
  #       timestamp_measured: tijd in UTC (Eindtijd van het uurgemiddelde)
  #       station_number: het nummer/id van het station bijv. NL01908
  # Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
  
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()
  
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
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
    content_measurements <- GetAPIDataframe(url_week)
    print("URL week van Luchtmeetnet opgehaald")
    if (length(content_measurements) == 1) {
      # Dan is er een error teruggekomen, bijvoorbeeld 502
      print(content_measurements)
      # stat_info_df$error <- "Error in gegevens ophalen. Check of alle gegevens er zijn."
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

  print("Alle data van Luchtmeetnet opgehaald")
  return(metingen_df)
}


GetLMLAPI <- function(station, ymd_vanaf, ymd_tot){
  # Functie om de gegevens van een LML station (eigenlijk station van luchtmeetnet)
  # voor een bepaalde periode op te halen
  # het kan zijn dat er meer data wordt opgehaal dan gespecificeerd.
  # Voorbeeld: TEST <- GetLMLAPI("NL01908", "20190505", "20190510")
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
  # Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
  
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()
  
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
  ## Ophalen van de ststionsinformatie ----
  stat_info_df <- GetLMLstatinfoAPI(station)
  
  ## Ophalen van de meetgegevens ----
  metingen_df <- GetLMLstatdataAPI(station, ymd_vanaf, ymd_tot)
  
  # Maak een named list voor de output
  lml_info_data <- list(info=stat_info_df, data=metingen_df)
  
  print("Alle data van Luchtmeetnet opgehaald")
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

##################### ----
# Hoofdfunctie ----
##################### ----
GetSamenMetenAPI <- function(projectnaam, ymd_vanaf, ymd_tot, data_opslag = data_opslag_list, updateProgress=NULL, debug=F){
  # Functie die de sensordata van het samenmetenportaal haalt
  # Voorbeeld: TEST <- GetSamenMetenAPI("project,'Amersfoort'","20190909", "20190912")
  # input:
  # projectnaam: string met projectnaam erin. Van dit project worden alle
  #              sensoren opgehaald
  # ymd_vanaf: string met datum van start van de periode Bijv:"20190909"
  # ymd_tot: string met datum van eind van de periode Bijv:"20190912"
  # updateProgress: functie om de progress te tonen in bijv. shiny tool
  # debug: boolean, om aan te geven of er prints moeten worden gemaakt die kunnen helpen bij debuggen.
  # output:
  #     named list met:
  #       sensordata: dataframe met de informatie over de sensor
  #              (sensor_id, kit_id, project, locatie_lat, locatie_lon, url)
  #       metingen: dataframe met de meetgegevens
  #               waarde = meet waarde
  #               tijd =  tijd van de meting in UTC
  #               name = naam van de grootheid (bijv. pm10)
  #               kit_id = naam van de sensor zoals in database samenmeten
  #               error = wanneer er geen metingen konden worden opgehaald staat hier de url die geprobeed is
  # LET OP: wanneer er geen metingen van een sensor zijn in de meegegeven periode
  # staat de sensor wel in de sensordata, maar heeft geen gegevens in metingen!
  
  ##################### ----
  # Helperfuncties
  ##################### ----
  GetlocatieAPI <- function(ind, data_opslag_loc = data_opslag){
    # Deze functie haalt de gegevens op uit de url_loc.
    # Hierbij gaat het om de lat en de lon.
    # Wanneer er een fout is, wordt de waarde NA meegegeven
    # input: 
    #     url_loc = string met de API waar de locatie staat
    #     kit_id = string, id zoals in de samenmetendatabase
    # output: dataframe met de kolommen $lat en $lon en $kit_id
    
    # Haal de betreffende url en kit_id op uit dataframe
    sensor_data_loc <- data_opslag_loc$sensor_data
    url_loc <- sensor_data_loc[sensor_data_loc$id==ind,'url_loc']
    kit_id <- sensor_data_loc[sensor_data_loc$id==ind, 'kit_id']
    
    # Ophalen van de informatie in API: LOCATIE
    content_loc_data <- GetAPIDataframe(url_loc)
    
    if (debug){print(content_loc_data)}
    
    # Voeg een error afvanging erin. Er kan iets mis gaan met de URL
    # of server waardoor er geen waarde terug komt, maar een string met error
    if(! is.character(content_loc_data)){
      content_loc_data_df <- content_loc_data$value
      # Extract de gegevens die je wilt gebruiken
      lon <- content_loc_data_df$location$coordinates[[1]][1]
      lat <- content_loc_data_df$location$coordinates[[1]][2]   
      # Voeg ze toe aan output
      locatie_df <- data.frame('lat' = lat, 'lon' = lon, 'kit_id' = kit_id)
    }else{
      locatie_df <- data.frame('lat' = NA, 'lon' = NA, 'kit_id' = kit_id)
    }
    return(locatie_df)
  }
  
  GeturlsmeetAPI <- function(ind, data_opslag_urlsmeet = data_opslag){
    # Deze haalt per sensor de verschillende type grootheden op (naam zoals PM10).
    # Daarnaast ook de url waar de verdere meetgeveens (observations) van deze grootheid staan.
    # input:
    #     kit_id
    #     url_datastream
    # output: dataframe met de kolommen
    #     kit_id (1 unieke waarde)
    #     grootheid (meerdere waardes)
    #     url_meet (meerdere waardes)
    
    # Haal de betreffende url en kit_id op uit dataframe
    sensor_data_urlsmeet <- data_opslag_urlsmeet$sensor_data
    url_datastream <- sensor_data_urlsmeet[sensor_data_urlsmeet$id==ind,'url_datastream']
    kit_id <- sensor_data_urlsmeet[sensor_data_urlsmeet$id==ind, 'kit_id']
    
    if(debug){print(paste0("urlsdatastream ",url_datastream ))}
    
    # Ophalen van de informatie in API: Datastream
    content_datastream_data <- GetAPIDataframe(url_datastream)
    content_datastream_data_df <- content_datastream_data$value
    
    # Er zijn natuurlijk meerdere grootheden gemeten. Ga elke apart af.
    # Maak een lijst met de urls voor elke grootheid en een voor de meetgegevens
    overzicht_url_grootheid <- content_datastream_data_df[,'ObservedProperty@iot.navigationLink']
    overzicht_url_meetgegevens <- content_datastream_data_df['Observations@iot.navigationLink'] # Waarom is dit nu een dataframe geworden???
    
    # Haal de verschillende grootheden op (de naam dus pm10)
    grootheden <- unlist(lapply(overzicht_url_grootheid, GetgrootheidAPI))
    if(debug){print(paste0("grootheden ",grootheden ))}
    
    # Maak de output dataframe, met de url van de meetgevens per grootheid
    output_df <- data_frame('url_meet' = overzicht_url_meetgegevens[,1], 'grootheid' = grootheden, 'kit_id' = kit_id)
    
    return(output_df)
  }
  
  GetgrootheidAPI <- function(url_grootheid){
    # Haalt het type van de grootheid op.
    # Leest de naam uit van de url en returned deze
    content_grootheid_df <- GetAPIDataframe(url_grootheid)
    grootheid <- content_grootheid_df['name']
    return(grootheid)
  }
  
  GetmeetgegevensAPI <- function(ind, data_opslag_meet = data_opslag){
    # Deze functie haalt de meetgegevens op van desbetreffende sensor en grootheid
    # voor een bepaalde periode
    # input: 
    #     url_meetgegevens = string; 
    #     grootheid = string, naam van de stof; 
    #     kit_id = string, id zoals in de samenmetendatabase
    #     ymd_vanaf = string met datum van start van de periode Bijv:"20190909"
    #     ymd_tot = string met datum van einde van de periode Bijv:"20190909"
    # output: dataframe met daaron de kolommen
    #     waarde = meetwaarde
    #     tijd = tijd van de meting in UTC
    #     grootheid = input grootheid
    #     kit_id = input kit_id
    #     error = als er een error was bij het ophalen van de data staat hierin de url. 
    
    # Sys.sleep(runif(1,0,1))
    
    # Haal de betreffende url en kit_id op uit dataframe
    urls_meet_meet <- data_opslag_meet$urls_meet
    url_meetgegevens <- urls_meet_meet[urls_meet_meet$id==ind,'url_meet']
    kit_id <- urls_meet_meet[urls_meet_meet$id==ind, 'kit_id']
    grootheid <- urls_meet_meet[urls_meet_meet$id==ind, 'grootheid']
    
    # Voor de progres bar in de tool: aangeven welke sensor er nu is en hoeveel er nog zijn
    # If we were passed a progress update function, call it
    aantal_sensoren <- length(urls_meet_meet$kit_id)
    if (is.function(updateProgress)) {
      text <- paste0("Sensor: ", kit_id, " - stap (",ind,"/",aantal_sensoren ,")")
      updateProgress(value = (ind/aantal_sensoren)*0.8+0.1 ,detail = text)
    }
    
    # Voeg filter toe die op tijdstip kan selecteren
    url_meetgegevens <- paste(url_meetgegevens,"?$filter=phenomenonTime+gt+%27",ymd_vanaf,"%27+and+phenomenonTime+lt+%27",ymd_tot,"%27&$orderby=phenomenonTime",sep='') # Met Orderby
    
    # De api splitst de gegevens op in meerdere pagina's. 
    # Alle pagina's wil je uitlezen, dus zolang er nog een nieuwe pagina is, pak die uit
    pagina_aanwezig_meetgegevens <- TRUE
    
    # Dataframe om alle metingen van de sensoren op te halen (id, grootheid, tijd, meetwaarde)
    # Dit is een longformat
    metingen_df <- data.frame()
    
    while(pagina_aanwezig_meetgegevens){
      # Uitvoeren van de API
      content_meetgegevens <-  GetAPIDataframe(url_meetgegevens)
      
      # Check of de URL klopte -> Als er geen data is meegegeven, dan is het een character
      if (! is.character(content_meetgegevens)){
        # Zet de data in een dataframe
        content_meetgegevens_df <- content_meetgegevens$value
        
        # Pak de meetwaardes en de desbetreffende tijd
        waardes <- content_meetgegevens_df$result
        tijd <- as.POSIXct(content_meetgegevens_df$phenomenonTime, format='%Y-%m-%dT%H:%M:%S', tz='UTC')

        # Voeg de meetwaarde met tijd toe aan de algemene metingen dataframe
        gegevens_toevoegen <- data.frame('waarde' = waardes, 'tijd' = tijd, 'grootheid' = grootheid, 'kit_id' = kit_id, 'error' = NA)
        
      } else{ # nu is er dus een error met het ophalen van de data
        gegevens_toevoegen <- data.frame('waarde' = NA, 'tijd' = as.POSIXct(NA, tz='UTC'), 'grootheid' = grootheid, 
                                         'kit_id' = kit_id, 'error' = paste('error in: ', url_meetgegevens, sep=""))
      }
      
      # Voeg de opgehaalde gegevens toe aan het totaal
      metingen_df <- rbind(metingen_df, gegevens_toevoegen)
      
      # Als er nog een pagina is, pak die dan ook uit
      if(length(content_meetgegevens) > 1){
        url_meetgegevens <- content_meetgegevens[[1]]
        if (debug){print('nieuwe pagina: +20')}
      } else{ # Is er geen pagina meer? Stop dan met de while loop.
        pagina_aanwezig_meetgegevens <- FALSE
      }
    }
    return(metingen_df)
  }
  
  ##################### ----
  # Initialisatie ----
  ##################### ----
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)
  
  # EQ,EQUALS is uniek voor elke gemeente en project
  # URL van de sensoren binnen een project
  # url_things <- paste("https://api-samenmeten.rivm.nl/v1.0/Things?$filter=properties/",projectnaam,")", sep='')
  # Ontwikkel api TEST of die ook werkt
  url_things <- paste("https://ontw.api-samenmeten.rivm.nl/v1.0/Things?$filter=(properties/",projectnaam,")", sep='')
  
  url_things <- gsub(' ','%20', url_things)
  
  if(debug){print(paste0("URL things: ", url_things))}
  
  # Dataframe om de basisgegevens van de sensoren op te slaan (naam, locatie, project, url)
  sensor_data <- data.frame()
  
  # Dataframe om alle metingen van de sensoren op te halen (id, grootheid, tijd, meetwaarde)
  # Dit wordt een longformat
  metingen_df <- data.frame()
  
  ##################### ----
  # OPHALEN van de sensorgegevens ----
  ##################### ----
  # De api splitst de gegevens op in meerdere pagina's. 
  # Alle pagina's wil je uitlezen, dus zolang er nog een nieuwe pagina is, pak die uit
  pagina_aanwezig_things <- TRUE
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- paste0("basisgegevens")
    updateProgress(value=0.1, detail = text)
  }
  # Ga alle sensoren binnen dit project af en haal basisgegevens ervan op
  # Daarna de locatie
  # Tot slot alle meetgegevens binnen de meegegevens tijdsperiode
  while(pagina_aanwezig_things){
    
    # Ophalen van de informatie in API: THINGS
    tryCatch({
      content_things <- GetAPIDataframe(url_things)
      content_things_df <- content_things$value
    }, error = function(e){
      stop("Error in URL things. Check of projectnaam bestaat.")
    })
    
    # bewaar de gegevens over de sensor
    sensor_data <- rbind(sensor_data, data.frame('sensor_id' = content_things_df[,'@iot.id'],
                                                 'kit_id' = content_things_df[,'name'],
                                                 'project' = content_things_df$properties['project'],
                                                 'url_loc' = content_things_df[,'Locations@iot.navigationLink'],
                                                 'url_datastream' = content_things_df[,'Datastreams@iot.navigationLink']))
    
    # Kijk of er nog een pagina is die je dan wilt uitlezen
    if (length(content_things)>1){
      url_things <- content_things[[1]]
      if(debug){print('nieuwe pagina')}
    } else{
      pagina_aanwezig_things <- FALSE
    }
  }
  
  # maak een index aan aan de sensordata voor de verdere apply
  sensor_data['id'] <- seq(1:nrow(sensor_data))
  # losse lijst met de index
  # ind <- sensor_data[,'id']
  ind <- seq(1:nrow(sensor_data))
  
  # Voeg de sensor_data toe aan de data opslag
  data_opslag[['sensor_data']] <- sensor_data
  
  print('Sensordata opgehaald')
  
  ##################### ----
  # OPHALEN van de locaties ----
  ##################### ----
  # haal locatie op voor alle sensors tegelijk met apply
  locaties <- lapply(ind, GetlocatieAPI)
  # Zet de list op naar 1 grote dataframe
  locaties <- do.call("rbind", locaties)
  
  # Voeg de locaties toe aan de sensor_data
  sensor_data <- merge(sensor_data, locaties, by = 'kit_id')
  
  # Overschrijf de sensor_data toe aan de data opslag
  data_opslag[['sensor_data']] <- sensor_data
  
  print('Locatiedata opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)))}
  
  ##################### ----
  # OPHALEN van alle meetgegevens urls per sensor ----
  ##################### ----  
  
  # Zoek alle urls bij elkaar waar meetgegevens inzitten
  # Deze neemt ook de types van de grootheid mee
  # De functie GeturlsmeetAPI doet dit
  urls_meet <- lapply(ind, GeturlsmeetAPI)

  # urls_meet <- lapply(ind, GeturlsmeetAPI)
  urls_meet <- do.call("rbind", urls_meet)
  
  if(debug){print(paste0("urls_meet: ",urls_meet))}
  
  # Voeg index toe voor de apply functie in de volgende stap
  urls_meet['id'] <- seq(1:nrow(urls_meet))
  
  # maak list met de index voor apply
  ind_meet <- seq(1:nrow(urls_meet)) # houd dit zo, het werkt niet als het een tibble is
  
  # Voeg de sensor_data toe aan de data opslag
  data_opslag[['urls_meet']] <- urls_meet
  
  print('URLS meet opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)*4))}
  
  
  ##################### ----
  # OPHALEN van alle data per sensor ----
  ##################### ----

  # Ga alle urls af waar meeteggevens bevinden (met vorige stap opgehaald)
  # Haal de gegevens op en zet in dataframe met kit_id en grootheid.
  # Dit is wat de functie GetmeetgegevensAPI doet
  
  meetgegevens <- lapply(ind_meet, GetmeetgegevensAPI) 
  meetgegevens <- do.call("rbind", meetgegevens)
  
  print('Meetgegevens opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)))}
  
  #################### ----
  # Combine output ----
  #################### ----
  # Om het progress aan te geven van de laatste stap
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- paste0("Verwerken naar Samen Analyseren Tool")
    updateProgress(value=0.95,detail = text)
  }
  # Maak een list van de sensordata en de metingen, zodat meegegeven kan worden als output
  all_data_list <- list('sensordata' = sensor_data, 'metingen' = meetgegevens,  'dataopslag'=data_opslag)
  return(all_data_list)
}

#############----
# FUNCTIE om via de API de KNMI data op te halen  ----
#############----

GetKNMIAPI <- function(stations, ymd_vanaf, ymd_tot){
  # Deze functie haalt de uurgemiddelde gegevens op van de KNMI stations:
  # windrichting, windsnelheid, temperatuur en relatieve luchtvochtigheid
  # Voorbeeld: TEST_API_KNMI <- GetKNMIAPI('260','20191214','20191215')
  #input: 
  # stations: lijstje met de stationnummers as character Bijv: c("235","280")
  # ymd_vanaf: string "yyyymmdd" 
  # ymd_tot: string "yyyymmdd" 
  #
  # LET OP: de je kunt ook uren meegeven: WIL je dit nog inbouwen? Nu niet mogelijk!
  # hh geeft het uur aan dat wordt meegenomen:
  # ymdh_vanaf = '2019121401', ymdh_vanaf = '2019121503'
  # Geeft: op 20191214 de uren 1,2,3 en 20191215 de uren 1,2,3
  # Voor hele dage gebruik ymdh_vanaf = '2019121401', ymdh_vanaf = '2019121524' 
  # 
  #output:
  # named list met daarin:
  #   info: dataframe met het nummer, lon, lat, hoogte, naam van het station
  #   data:
  # Dataframe met de volgende kolommen
  # STNS     = stationnummer
  # YYYYMMDD = datum (YYYY=jaar,MM=maand,DD=dag)
  # HH       = tijd (HH=uur, UT.12 UT=13 MET, 14 MEZT. Uurvak 05 loopt van 04.00 UT tot 5.00 UT;
  # DD       = Windrichting (in graden) gemiddeld over de laatste 10 minuten van het afgelopen uur 
  #             (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil 990=veranderlijk. 
  # FF       = Windsnelheid (in m/s) gemiddeld over de laatste 10 minuten van het afgelopen uur; 
  # TEMP     = Temperatuur (in graden Celsius) op 1.50 m hoogte tijdens de waarneming ( T in vars knmi);
  # U        = Relatieve vochtigheid (in procenten) op 1.50 m hoogte tijdens de waarneming;
  # tijd     = combinatie van YYYYMMDD en HH: as.POSIXct en UTC, eindtijd van uurgemiddelde
  
  
  # Zet de tijd bij de datum
  ymdh_vanaf <- paste(ymd_vanaf,'01',sep='')
  ymdh_tot <- paste(ymd_tot,'24',sep='')
  
  # Zet de lijstje stations om in 1 string die dan in de wget kan
  for(ind in seq(1:length(stations))){
    if(ind==1){
      samengevoegd <- stations[ind]
    }else{
      samengevoegd <- paste(samengevoegd,stations[ind],sep=':')
    }
  }
  stns <- samengevoegd

  # Ophalen van de meetgegevens van de stations
  knmi_uur_wget_string <- paste('wget -O - --post-data="stns=', stns, '&start=',ymdh_vanaf,'&end=',ymdh_tot,
                                '&vars=DD:FF:T:U" http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi', sep="") 
  print(knmi_uur_wget_string)
  knmi_uur_raw <- system(knmi_uur_wget_string,intern=T)
  
  print('KNMI: ruwe data opgehaald')
  
  # Bepaal aantal headers:
  aantal_headers <- length(grep("#",knmi_uur_raw))
  
  # Haal de kolomnamen op: 
  # Dit gaat bijna goed: nog 2 schoonheidsfoutjes: '# STN' en 'TRUE' ( de T van temperatuur wordt vertaald naar TRUE)
  header_names <- as.character(read.csv(textConnection(knmi_uur_raw[aantal_headers-1]),header=F, strip.white=T, stringsAsFactors=F))
  header_names[1] <- 'STNS'
  header_names[grep("TRUE",header_names)] <- 'TEMP'
  
  # Zet de ruwe tekst om in een dataframe
  knmi_uur_df <- read.csv(textConnection(knmi_uur_raw), header=F, skip=aantal_headers, na.strings="-", col.names=header_names)
  
  print('KNMI: data omgezet naar dataframe')

  # Zet de eenheid Temp om van 0.1 graden C naar 1 graden C
  knmi_uur_df['TEMP'] <- knmi_uur_df['TEMP']/10
  
  # Zet de eenheid FF om van 0.1 m/s naar 1 m/s
  knmi_uur_df['FF'] <- knmi_uur_df['FF']/10
  
  # Combineer de datum en uur naar het tijdstip in POSIXct: nieuwe kolom 'tijd'
  knmi_uur_df['tijd'] <- as.POSIXct(as.character(knmi_uur_df$YYYYMMDD), tz='UTC', format='%Y%m%d')
  knmi_uur_df['tijd'] <- knmi_uur_df['tijd'] + knmi_uur_df$HH*60*60
  
  # Haal de gegevens over de info van sensoren (naam en coordinaten)
  # Nodig om te weten hoeveel stations, hoeveel regels voor de coordinaten
  if (stations[1] == 'ALL'){
    aantal_stns <- 47 # er zijn in totaal 47 stations...
  }else{
    aantal_stns <- length(stations)
  }
  regeleinde_stns <- 5 + aantal_stns
  
  # Het gedeelte uit de API waar die sensor info instaat
  stn_info_ruw <- knmi_uur_raw[6:regeleinde_stns]
  # Zet wat waardes uit de tekst om
  stn_info_ruw <- sub(':','',stn_info_ruw)
  stn_info_ruw <- sub('#','',stn_info_ruw)
  # Van tekst naar dataframe
  stn_info_df <- (read.csv(textConnection(stn_info_ruw),header=F,sep=" " ,strip.white=T, stringsAsFactors = F))
  
  print('KNMI: metadata omgezet naar dataframe')

  # Helaas staan er nog veel spaties in de dataframe. Verwijder de kolommen met spaties
  stn_info_df <- stn_info_df[,colSums(is.na(stn_info_df[,]))==0]
  
  # Voeg de naam kolommen samen, door de spaties zijn de DE BILT in 2 kolommen
  if(length(names(stn_info_df))==6){
    stn_info_df[5] <- paste(stn_info_df[,5], stn_info_df[,6])
    stn_info_df[6] <- NULL}
  else if(length(names(stn_info_df))==7){
    stn_info_df[5] <- paste(stn_info_df[,5], stn_info_df[,6], stn_info_df[,7])
    stn_info_df[6] <- NULL
    stn_info_df[7] <- NULL}
  
  # Geef mooie kolomnamen
  colnames(stn_info_df) <- c("STNS","LON", "LAT", "ALT", "NAME")
  
  # Maak een list waarin de metingen en de coordinaten avn de stations staan
  knmi_info_data <- list(info=stn_info_df, data=knmi_uur_df)
  
  print('KNMI: metadata en data gecombineerd')
  
  return(knmi_info_data)
}




