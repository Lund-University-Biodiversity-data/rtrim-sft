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
postgres_url <<- ""
postgres_user <<- "postgres"
postgres_password <<- "blabla"

mongo_database <<- "ecodata"
mongo_url <<- "mongodb://localhost"
mongo_user <<- "mongo"
mongo_password <<- "blabla"

bird_list_id <<- "dr158"