library(shiny)
library(shinythemes)
library(DBI)
library(pool)
library(shinycssloaders)
library(rtrim)
library(jsonlite)
library(mongolite)
library(shinyjs)
library(leaflet)
library(sf)
#app_url <<- "http://127.0.0.1:6891"


postgres_database <<- "sftdatabase"
postgres_database_parameters <<- "sftrtrimparams"
postgres_url <<- ""
postgres_user <<- "postgres"
postgres_password <<- "blabla"

mongo_database <<- "ecodata"
mongo_url <<- "mongodb://localhost"
mongo_user <<- "mongo"
mongo_password <<- "blabla"

species_list_api_url <<- "https://lists.biodiversitydata.se/ws/"
species_list_url <<- paste0(species_list_api_url, "speciesListItems/")
species_list_KVP_details <<- "?includeKVP=true&max=1000"
list_id_bird <<- "dr627"
list_id_owl <<- "dr167"
list_id_mammal <<- "dr159"
list_id_amphibian <<- "dr160"

path_project <<- "/home/blabla/rtrim-interface-development"
path_project_extract <<- "/home/blabla/rtrim-interface-development/extract"
url_extract <<- "http://canmoveapp.ekol.lu.se/rtrim-sft-extract/"

project_id_std <<- "xxx-xxx-xxx-xxx"
project_activity_id_std <<- "xxx-xxx-xxx-xxx"

project_id_punkt <<- "xxx-xxx-xxx-xxx"
project_activity_id_winter <<- "xxx-xxx-xxx-xxx"
project_activity_id_summer <<- "xxx-xxx-xxx-xxx"

project_id_iwc <<- "xxx-xxx-xxx-xxx"
project_activity_id_iwc <<- "xxx-xxx-xxx-xxx"