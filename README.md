# Rtrim shinyapp development
This is a project to develop a shinyapp interface for using rtrim to analyse bird trends from Svensk Fågeltaxering (SFT). It includes fetching data from the SFT server, running rtrim analyses and handle the results (some displaying and producing output-files (.xlsx)). 

Files needed should be downloaded to the local computer. Preferably create a Rstudio project (make a project using Rstudio) and add files to this. Files (and folders) needed are:

app.R<br/>
UsefulFunctions.R<br/>
Summarise_Results.R<br/>
www (folder)<br/>
SpeciesWithShorterTimePeriods.xls<br/>

extract (folder, writing right required)<br/>

# R packages required
```
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinycssloaders")
install.packages("pool")
install.packages("rtrim")
install.packages("readxl")
install.packages("writexl")
```

# To use it without ODBC connector, but directly to postgres
```
install.packages("RPostgres")
```
in the app.R file, comment the line:
```
pool <- dbPool(drv = odbc::odbc(), dsn = 'SFT_64', encoding = 'windows-1252')
```
and uncomment/edit these ones, with your configuration
```
#library(RPostgres)
#pool<-dbConnect(RPostgres::Postgres(), dbname = 'sft20201002', user='postgres')
```


# create database rtrim-params
sudo -u postgres psql < rtrim-params.sql


# to install the mongo version and make it run with R version 4
sudo apt-get install libsasl2-dev
install.packages("mongolite")


sudo apt-get install libcurl4-openssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
install.packages("devtools")
devtools::install_github('jbryer/DTedit')
install.packages("rjson")
install.packages("stringr")
install.packages("RCurl")


# change to R version 4 on server (canmove-app)
https://medium.com/@hpgomide/how-to-update-your-r-3-x-to-the-r-4-x-in-your-linux-ubuntu-46e2209409c3

then reinstall packages
install.packages("digest")
install.packages("R6")
install.packages("later")
install.packages("magrittr")
install.packages("promises")
install.packages("httpuv")
install.packages("mime")
install.packages("xtable")
install.packages("lifecycle")
install.packages("jquerylib")
install.packages("shiny")
install.packages("withr")
install.packages("shinythemes")
install.packages("shinycssloaders")

install.packages("pool")
install.packages("rtrim")
install.packages("jsonlite")
install.packages("mongolite")
install.packages("htmlwidgets")
install.packages("xfun")
install.packages("evaluate")
install.packages("knitr")
install.packages("DT")

install.packages("DTedit") ## Don't work =>

sudo apt-get install libcurl4-openssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

install.packages("glue")
install.packages("rappdirs")
install.packages("xml2")
install.packages("stringr")
install.packages("pkgload")
install.packages("rprojroot")
install.packages("prettyunits")
install.packages("xopen")
install.packages("pkgdown")
install.packages("devtools")
devtools::install_github('jbryer/DTedit')
install.packages("rjson")
install.packages("bitops")
install.packages("RCurl")

install.packages("crayon")
install.packages("pkconfig")
install.packages("hms")
install.packages("progress")

install.packages("cellranger")
install.packages("readxl") # ERROR !! progress needed before. solved
install.packages("writexl")

install.packages("bit")
install.packages("bit64")
install.packages("blob")
install.packages("RPostgres")

install.packages("generics")
install.packages("lubridate")
install.packages("crosstalk")
install.packages("yaml")
install.packages("askpass")
install.packages("openssl")
