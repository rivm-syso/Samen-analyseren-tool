## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het Global.R script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: april 2020
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## 
## ---------------------------------------------------------

## Load de packages 
# if (!require("pacman")) install.packages("pacman") # handig packacge dat packages checkt en installeert
# pacman::p_load(tidyr, openair, dplyr, tidyverse, leaflet,leaflet.extras, shiny, shinythemes,shinyWidgets, sp,devtools,geoshaper)
# 
# Om het te publiceren op de shinyserver kan je pacman niet gebruiken. Dat werkt namelijk niet.
library(openair)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(purrr)
library(sp)
library(devtools)
library(geoshaper)
library(ggplot2)
library(plotly)

## Load Functions ----

# Functies voor het genereren van de input opties voor openair call
source("selectReactiveComponent.R", local = TRUE) 
# Functies voor het genereren van de inhoud van de tabpanels
source("tabPanels.R", local = TRUE) 

## Initialise ----
projectnaam <- "Hollandse Luchten"
file <- "HLL_voorbeeld_data.RDS"
file <- "HLL_20190401_20200701.RDS"
kwal_file <- "kwalindex_uur_HLL.RDS"
# # 
# file <- "testdata_BB.RDS"
# kwal_file <- "kwalindex_uur_NBI_BV.RDS"


choices <- c( "PM10 - gekalibreerd", "PM2.5 - gekalibreerd","PM10", "PM2.5") #set up choices for shiny app
kleur_cat <- list('#42145f','#ffb612','#a90061','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
kleur_sensor <- "leeg"
kleur_marker_sensor <- "#525252" # default kleur sensor
geen_groep <- "" # default waarde als de sensor niet in een groep zit

icons_stations <- iconList(
  knmi = makeIcon("ionicons_compass.svg", 18, 18),
  lml = makeIcon("ionicons_analytics.svg", 15, 15))

## Inlezen van de data ----
input_df <- readRDS(file)
kwal_input_df <- readRDS(kwal_file)
kwal_input_df$date <- kwal_input_df$timestamp_from

# Voeg de uurlijkse kwalindex toe aan het input dataframe 
input_df <- merge(input_df, kwal_input_df, by=c('date', 'kit_id'))

# Maak ook een kolom met de kwal_index als text
input_df$kwalindex_pm25_t <- sprintf("%04d", input_df$kwalindex_pm25)

## Default locatie, kleur en label opzetten ----
input_df$kit_id <- gsub('HLL_hl_', '', input_df$kit_id) #remove HLL-string from input_df for shorter label

# Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
sensor_unique <- aggregate(input_df[,c('lat','lon')], list(input_df$kit_id), FUN = mean) # gemiddelde om per sensor een latlon te krijgen
names(sensor_unique)[names(sensor_unique)=='Group.1'] <-'kit_id'
sensor_unique$selected <-FALSE
sensor_unique$huidig <- FALSE
sensor_unique$groep <- geen_groep
sensor_unique$kleur <- kleur_marker_sensor
sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info

# Voor de multiselect tool: omzetten lat/lon naar spatialpoints
ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

# Voor de knmimarkers: locatie en labels opzetten
knmi_stations <- data.frame("code" = c("knmi_06225", "knmi_06240", "knmi_06260"), "lat" =c(52.4622,52.3156,52.0989), "lon" =c(4.555,4.79028,5.17972))
knmi_stations$naam <- c("IJmuiden", "Schiphol", "De Bilt")
knmi_labels <- as.list(paste("KNMI", knmi_stations$naam, sep = ": "))

# Voor de lmlmarkers: locatie en labels opzetten
lml_stations <- data.frame("code" = c("NL49014","NL49551","NL49572","NL49561","NL10636","NL49573","NL49570","NL49553","NL49012"))
lml_stations$lat <- c(52.3597,52.463,52.4744,52.334,52.105,52.4789,52.4893,52.494,52.39)
lml_stations$lon <- c(4.86621,4.60184,4.6288,4.77401,5.12446,4.57934,4.64053,4.60199,4.88781)

# Maak in de labelling onderscheid tussen de LML en GGD stations
lml_labels <- vector("list", length(lml_stations$code))
lml_labels[grep('NL49', lml_stations$code)] <- "GGD"
lml_labels[grep('NL10', lml_stations$code)] <- "LML"
lml_labels <- as.list(paste(lml_labels, lml_stations$code, sep = ": "))

