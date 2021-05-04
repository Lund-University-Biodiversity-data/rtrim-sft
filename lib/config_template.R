library(shiny)
library(shinythemes)
library(DBI)
library(pool)
library(shinycssloaders)
library(rtrim)
library(jsonlite)
library(mongolite)

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
species_list_details_url <<- paste0(species_list_api_url, "species/")
list_id_bird <<- "dr158"
list_id_owl <<- "dr167"
list_id_mammal <<- "dr159"
list_id_amphibian <<- "dr160"

path_project <<- "/home/blabla/rtrim-interface-development"
path_project_extract <<- "/home/blabla/rtrim-interface-development/extract"
url_extract <<- "http://canmoveapp.ekol.lu.se/rtrim-sft-extract/"

project_id_std <<- "dsfsfsadfsa"
project_id_summer <<- "dsfsfsadfsa"
project_id_winter <<- "dsfsfsadfsa"
project_id_iwc <<- "dsfsfsadfsa"
