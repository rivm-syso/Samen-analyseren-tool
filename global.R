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
library(tidyr) # for pivot_wider
library(shinythemes)
library(shinyWidgets)
library(purrr)
library(sp)
library(devtools)
library(geoshaper)
library(DT) #for downlaod and datatable

# library(profvis)
# profvis(runApp())

## Load Functions ----

# Functies voor het genereren van de input opties voor openair call
source("selectReactiveComponent.R", local = TRUE) 
# Functies voor het genereren van de inhoud van de tabpanels
source("tabPanels.R", local = TRUE) 
# Functies voor het ophalen van data via api
source("Functies_API.R", local = TRUE) 


## Initialise ----
projectnaam <- "Hollandse Luchten"
file <- "HLL_voorbeeld_data.RDS" 

choices <- c( "PM10 - gekalibreerd", "PM2.5 - gekalibreerd","PM10", "PM2.5") #set up choices for shiny app
kleur_cat <- list('#42145f','#ffb612','#a90061','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
kleur_sensor <- "leeg"
kleur_marker_sensor <- "#525252" # default kleur sensor
geen_groep <- "" # default waarde als de sensor niet in een groep zit



icons_stations <- iconList(
  knmi = makeIcon("ionicons_compass.svg", 18, 18),
  # knmi = makeIcon("ionicons_analytics_white.svg", 18, 18),
  # lml = makeIcon("ionicons_analytics_black.svg", 15, 15))
  lml = makeIcon("ionicons_analytics.svg", 15, 15))

input_df <- NULL
sensor_unique <- NULL
sensor_labels <- NULL
input_df_lml <- NULL
input_df_knmi <- NULL

# ## Inlezen van de data ----
# input_df <- readRDS(file)
# 
# ## Default locatie, kleur en label opzetten ----
# input_df$kit_id <- gsub('HLL_hl_', '', input_df$kit_id) #remove HLL-string from input_df for shorter label
# 
# # Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
# sensor_unique <- aggregate(input_df[,c('lat','lon')], list(input_df$kit_id), FUN = mean) # gemiddelde om per sensor een latlon te krijgen
# names(sensor_unique)[names(sensor_unique)=='Group.1'] <-'kit_id'
# sensor_unique$selected <-FALSE
# sensor_unique$huidig <- FALSE
# sensor_unique$groep <- geen_groep
# sensor_unique$kleur <- kleur_marker_sensor
# sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info
# 
# # Voor de multiselect tool: omzetten lat/lon naar spatialpoints
# ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

# Voor de knmimarkers: locatie en labels opzetten
# knmi_stations <- data.frame("code" = c("knmi_06225", "knmi_06240", "knmi_06260"), "lat" =c(52.4622,52.3156,52.0989), "lon" =c(4.555,4.79028,5.17972))
# knmi_stations$naam <- c("IJmuiden", "Schiphol", "De Bilt")
# knmi_labels <- as.list(paste("KNMI", knmi_stations$naam, sep = ": "))

knmi_stations_all <- readRDS('locaties_knmi_all.RDS')
knmi_stations_all$selected <- FALSE
knmi_labels <- as.list(knmi_stations_all$statcode)


# # Voor de lmlmarkers: locatie en labels opzetten
# lml_stations <- data.frame("code" = c("NL49014","NL49551","NL49572","NL49561","NL10636","NL49573","NL49570","NL49553","NL49012"))
# lml_stations$lat <- c(52.3597,52.463,52.4744,52.334,52.105,52.4789,52.4893,52.494,52.39)
# lml_stations$lon <- c(4.86621,4.60184,4.6288,4.77401,5.12446,4.57934,4.64053,4.60199,4.88781)

# Voor de lml stations: ophalen hun naam en locatie en labels opzetten
lml_stations_all <- readRDS('locaties_lml_all.RDS')
lml_stations_all$selected <- FALSE

# Maak in de labelling onderscheid tussen de LML en GGD stations
lml_labels <- vector("list", length(lml_stations_all$statcode))
lml_labels[grep('NL49', lml_stations_all$statcode)] <- "GGD"
lml_labels[grep('NL10', lml_stations_all$statcode)] <- "LML"
lml_labels[grep('NL01', lml_stations_all$statcode)] <- "DCMR"
lml_labels[grep('NL54', lml_stations_all$statcode)] <- "ODRA"
lml_labels[grep('NL50', lml_stations_all$statcode)] <- "Lim"

lml_labels <- as.list(paste(lml_labels, lml_stations_all$statcode, sep = ": "))

## Voor het kiezen van gemeente of rpoject voor het data ophalen van samenmeten API
# hoofdkeuze 
hoofd_choices <- data.frame('namen'=c('project', 'gemeente'), 'labels'=c('Project','Gemeente')) #LET op deze worden gebruikt in server voor if else, dus pas niet zomaar aan.
hoofd_choices = setNames(hoofd_choices$namen,hoofd_choices$labels)

# Overzicht projectnamen
project_choices <- c('Amersfoort', 'Hollandse Luchten')

# Inladen overzicht gemeente code en naam
overzicht_gemeente <- read.csv('./overzicht_code_gemeentes.csv',header=F)
# Sorteer de gemeentes per naam
overzicht_gemeente <- overzicht_gemeente[order(overzicht_gemeente$V2),]
gemeente_choices = setNames(overzicht_gemeente$V1,overzicht_gemeente$V2)
