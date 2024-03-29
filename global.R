## ---------------------------------------------------------
## R Script voor interactieve data-analyse van sensordata, met o.a. R package openair, leaflet en shiny.
## Deze Samen Analyseren Tool bestaat uit meerdere scripts. Dit is het Global.R script.
## Auteur: Henri de Ruiter en Elma Tenner namens het Samen Meten Team, RIVM. 
## Laatste versie: jan 2021 
## Contact: info@samenmeten.nl 
## ---------------------------------------------------------
## Opmerkingen: 
## 
## ---------------------------------------------------------

## Load de packages 
# if (!require("pacman")) install.packages("pacman") # handig packacge dat packages checkt en installeert
# pacman::p_load(tidyr, openair, dplyr, tidyverse, leaflet,leaflet.extras, shiny, shinythemes,shinyWidgets, sp,devtools,geoshaper)
# devtools::install_github("https://github.com/rivm-syso/samanapir")
# devtools::install_github("RedOakStrategic/geoshaper")
# Om het te publiceren op de shinyserver kan je pacman niet gebruiken. Dat werkt namelijk niet.
library(openair) # voor de grafieken
library(shinythemes)
library(shinyWidgets)
library(leaflet) # voor de kaart
library(leaflet.extras)
library(plyr)
library(dplyr)
library(tidyr) # for pivot_wider
library(purrr)
library(sp)
library(devtools)
library(geoshaper)
library(Rmisc)
library(DT) #for download and datatable
library(taRifx)
library(ggplot2)
library(htmltools) # voor inlezen js script
library(samanapir) # voor de api functionaliteiten

#Laat voor de log zien wanneer de tool geopend is.
print("Gebruik van de samen analyseren tool om: ")
print(Sys.time())

## Load Functions ----
# Functies voor het genereren van de inhoud van de tabpanels
source("tabPanelsData.R", local = TRUE)
source("tabPanelsAnalyse.R", local = TRUE)

## Initialise ----
projectnaam <- "Hollandse Luchten"
sensor_file <- "Voorbeeld_sensoren.csv" 
lml_file <- "Voorbeeld_luchtmeetnet.csv" 
knmi_file <- "Voorbeeld_knmi.csv" 

choices <- c( "PM10 - gekalibreerd", "PM2.5 - gekalibreerd","PM10", "PM2.5") #set up choices for shiny app
kleur_cat <- list('#ffb612','#42145f','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
kleur_cat<-rev(kleur_cat) # Dan komen de kleuren wel goed om, namelijk de meest saturated eerst
kleur_sensor <- "leeg"
kleur_marker_sensor <- "#000000" # default kleur sensor grey=#525252 black=000000
kleur_marker_sensor_no_data <- '#b8b8b8'
geen_groep <- "" # default waarde als de sensor niet in een groep zit
lijn_cat <- list('dashed', 'dotted', 'dotdash', 'longdash', 'twodash') # linetype: “blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”. 0123456

icons_stations <- iconList(
  knmi_black = makeIcon("symbol_knmi_black.svg"),
  lml_white = makeIcon("symbol_lml_wijnrood.svg"),
  knmi_white = makeIcon("symbol_knmi_wijnrood.svg"),
  lml_black = makeIcon("symbol_lml_black.svg"),
  knmi_grey = makeIcon("symbol_knmi_grey.svg"),
  lml_grey = makeIcon("symbol_lml_grey.svg"))

icons_stations <- iconList(
  knmi_hasdata = makeIcon("symbol_knmi_black.svg"),
  lml_selected = makeIcon("symbol_lml_wijnrood.svg"),
  knmi_selected = makeIcon("symbol_knmi_wijnrood.svg"),
  lml_hasdata = makeIcon("symbol_lml_black.svg"),
  knmi_nodata = makeIcon("symbol_knmi_grey.svg"),
  lml_nodata = makeIcon("symbol_lml_grey.svg"))

input_df <- NULL
sensor_unique <- NULL
sensor_labels <- NULL
input_df_lml <- NULL
input_df_knmi <- NULL

# Voor de knmimarkers: locatie en labels opzetten
knmi_stations_all <- readRDS('locaties_knmi_all.RDS')
# Er staan meer stations in de samenmetendatabase dan via de API van KNMI op te vragen zijn, deels zijn dus andere bron
# hier alleen die van de API van het KNMI meenemen
nummers_knmi <- c(391,370,331,315,324,375,380,240,286,310,283,280,273,323,249,377,316,313,277,348,308,319,215,278,285,343,225,330,267,269,344,275,235,257,290,350,251,248,279,258,356,209,312,340,260,270,242)
knmi_stations_all$station_number <- as.numeric(gsub('knmi_06', '', knmi_stations_all$statcode))
knmi_stations_all$statcode <- paste0("KNMI", knmi_stations_all$station_number)
# Neem alleen de stations die in de API van het KNMI zitten
knmi_stations_all <- knmi_stations_all[which(knmi_stations_all$station_number %in% nummers_knmi),]
knmi_stations_all$selected <- FALSE
knmi_stations_all$hasdata <- FALSE
knmi_stations_all$name_icon <- 'knmi_nodata'
knmi_labels <- as.list(paste("KNMI", knmi_stations_all$station_number, sep = ": "))

# Voor de lml stations: ophalen hun naam en locatie en labels opzetten
lml_stations_all <- samanapir::GetLMLallstatinfoAPI()
lml_stations_all$selected <- FALSE
lml_stations_all$hasdata <- FALSE
lml_stations_all$name_icon <- 'lml_nodata'
lml_stations_all$kleur <- '#000000'
lml_stations_all$kleur <- '#a90061'
lml_stations_all$lijn <- 'solid'
lml_stations_all$groep <- geen_groep

## Voor het kiezen van gemeente of rpoject voor het data ophalen van samenmeten API
# hoofdkeuze 
hoofd_choices <- data.frame('namen'=c('project', 'gemeente'), 'labels'=c('Project','Gemeente')) #LET op deze worden gebruikt in server voor if else, dus pas niet zomaar aan.
hoofd_choices = setNames(hoofd_choices$namen,hoofd_choices$labels)

# Inladen overzicht projectnaam
overzicht_project <- read.csv('./overzicht_projecten.csv',header=T)
project_choices <- sort(overzicht_project$project)

# Inladen overzicht gemeente code en naam
overzicht_gemeente <- read.csv('./overzicht_code_gemeentes.csv',header=F)
# Sorteer de gemeentes per naam
overzicht_gemeente <- overzicht_gemeente[order(overzicht_gemeente$V2),]
gemeente_choices = setNames(overzicht_gemeente$V1,overzicht_gemeente$V2)

# Voor het selecteren van de component
overzicht_component <- data.frame('component' = c("pm10","pm10_kal","pm25","pm25_kal"), 'label'=c("PM10","PM10 - gekalibreerd","PM2.5" ,"PM2.5 - gekalibreerd" ))
component_choices = setNames(overzicht_component$component, overzicht_component$label)
