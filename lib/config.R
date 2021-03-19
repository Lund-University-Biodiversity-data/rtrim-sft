library(shiny)
library(shinythemes)
library(DBI)
library(pool)
library(shinycssloaders)
library(rtrim)
library(jsonlite)
library(mongolite)

#app_url <<- "http://127.0.0.1:6891"


postgres_database <<- "sft_20210211"
postgres_url <<- ""
postgres_user <<- "postgres"
postgres_password <<- "tamere"

mongo_database <<- "ecodata"
mongo_url <<- "mongodb://localhost"
mongo_user <<- "postgres"
mongo_password <<- "tamere"

bird_list_id <<- "dr158"