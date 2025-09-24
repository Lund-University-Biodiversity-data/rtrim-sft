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

install.packages("shinyjs", dependencies=TRUE)
sudo apt-get install libgdal-dev
sudo apt-get install gfortran
install.packages("sf", dependencies=TRUE)
sudo apt-get install libudunits2-dev
install.packages("leaflet", dependencies=TRUE)
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


## Install shiny-server for PROD environment

To install R :
sudo apt-get install r-base r-base-dev
To install shiny-server :
sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
To set shiny-server as service :
sudo apt install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.18.987-amd64.deb
sudo gdebi shiny-server-1.5.20.1002-amd64.deb
sudo systemctl start shiny-server
 => http://server-ip-address:3838

To install the different webapps :
check the path of the apps (default) in the shiny config file : /etc/shiny-server/shiny-server.conf
Usually the apps are at /srv/shiny-server
In this folder, create symbolic links to the apps
like :
sudo ln -s /home/canmoveapp/rtrim-sft sft-rtrim
