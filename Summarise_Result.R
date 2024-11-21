#################################################
#### Code to make xls-files for reports etc. ####
#################################################

#################################################
#### First you should set some parameters ####
#################################################

# OBS! If you have tricky species, like BEFIN in winter, you should run 'Code for specific tasks.R' first.

# What base year do you want?
base <- 2012

# Do you want to use shorter time periods for some species?
# (i.e. use the information in "SpeciesWithShorterTimePeriods.xls")
# If the system(s) you are running does not have such information in the xls-file it does not matter how you specify this.
useShorterPeriods <- TRUE

# What is the "general" name of the output-files (i.e. what you entered as filename för the rdata-file in the app)
# Hade problem en gång, med ett "+" i filnamnet. Bör nog undvikas att använda "+"
# Det första alternativet nedan fungerade för både Trippel och Komb. Har du redan kört alla delprogrammen under olika "general" namn, 
# så gör en kopia av vardera och döp om vardera till, t.ex. "trimOutput_totalstandard_2022-02-01_14_04_18". 

# OBS: Innan nästa steg måste du ta bort sekund-decimalerna på .rdata-filen !!!!

filenames <- 'trimOutput_VinPKT3_2012_2022'
#filenames <- 'trimOutput'
#filenames <- '_Natt20102022_harar_räv_Sverige'  
#filenames <- 'Orebro20112020' 

# What are the name(s) of the "monitoring systems" (tables in SFT) that you want summaries for. What you leave within parenthesis
# are the systems you want to work with. Can be 1 or several systems. Can be 'misc_census'. 
#tables <- c('totalsommar_pkt')
tables <- c('totalvinter_pkt')
#tables <- c('misc_census')
#tables <- c('totalstandard')
#tables <- c('totalvatmark')
#tables <- c('totalstandard', 'totalsommar_pkt', 'totalvinter_pkt')
#tables <- c('totalstandard', 'totalsommar_pkt')
#tables <- c('total_iwc_januari')

# What short (one?) letter kombination should identify the monitoring system(s) in the Index and Convergence columns? Should be
# the same number of factors as in the previous code line. You can leave it empty by ''
shortcolumn <- c('V')
#shortcolumn <- c('T', 'S', 'V')
#shortcolumn <- c('T', 'S')

# What short (one?) letter kombination should identify the monitoring system(s) in tabs? Should also be the same number of factors
# as in the previous code line. You can leave it empty by ''
shorttab <- c('V')
#shorttab <- c('T', 'S')
#shorttab <- c('T', 'S', 'V')

# Which combinations of monitoring systems do you want to produce (trippel, komb etc)?
# Make a list of vectors specifying the indices (in the desired order) of the systems you want to combine.
# The numbers relate to the order of systems assigned above under "tables" 

#combinations <- list(c(3, 2, 1), c(2, 1))  # Denna skapar både Trippel och Komb
#combinations <- list(c(2, 1))
combinations <- NULL

# Do you want single files (trimv201x...) for graph making (each system separately)? For example, do you also want Winter 
# as a separate file? TRUE or FALSE
single <- TRUE

# Do you want "homepage" files (you will get one for each system)? This is the "overview data" file.
homepage <- TRUE

# Do you want swedish- (SE), english- (EN),  or world- (WD) names for species (as specified in EUROLIST)?
# NOTE: Worldnames are not available för all species in EUROLIST
lang <- 'SE' 
# lang <- 'EN'
# lang <- 'WD'

#################################################
#### The code below should just be run (down ####
#### to BUT NOT INCLUDING the section called ####
#### "Reproduce old TRIM output")            ####
#################################################

## Load necessary "stuff"
## Needed especially if you haven't run the app in this session (most of this will already be loaded if you have)
library(DBI)  # Package needed to get eurolistdata from SFT
library(pool) # as above
library(rtrim) # Needed to use functions for extracting info from "trim"-objects
#library(xlsx) # Needed to save data to Excel-files
library(readxl)
library(writexl)
source('UsefulFunctions.R') # A bunch of useful functions

## Get Eurolist data
pool <- dbPool(drv = odbc::odbc(), dsn = 'SFT_64') #, encoding = 'windows-1252')
querysp <- "select art, arthela, latin, englishname, worldname, rank
              from eurolist
              where art<'800'
              order by art"
spdat <- dbGetQuery(pool, querysp)
poolClose(pool)


## Get info on species specific startyear (has been used in the app analyses as well)
if (useShorterPeriods & any(tables%in%c('totalsommar_pkt', 'totalvinter_pkt', 'totalstandard'))){
  # startyr <- read.xlsx('SpeciesWithShorterTimePeriods.xls', sheetName = 'StartYear', encoding = 'UTF-8',
  #                      stringsAsFactors = F)
  startyr <- read_excel('SpeciesWithShorterTimePeriods.xls', sheet = 'StartYear')
  startyr$Delprogram[startyr$Delprogram=='SomPKT'] <- 'totalsommar_pkt'
  startyr$Delprogram[startyr$Delprogram=='Standard'] <- 'totalstandard'
  startyr$Delprogram[startyr$Delprogram=='VinPKT'] <- 'totalvinter_pkt'
} else {
  startyr <- NULL
}


## Read in rdata-files with results
## Finds and read in the latest file produced for each monitoring system (and called trimOutput-something...)
## The findlatestFile-function is found among the UsefulFunctions
files2summarize <- paste(filenames, tables, sep = '_')

reslist <- lapply(files2summarize, function(x) {
  f <- findlatestFile(filename = paste0(x, '_'), dateform = '%Y-%m-%d_%H_%M_%S')
  load(f)
  return(output)
})
names(reslist) <- tables # name the elements of the list according to what system it comes from

## Refine the reslist such that only models that "worked" (produced a trim-object) are kept
worked <- lapply(reslist, function(x) x[sapply(x, function(y) inherits(y$value, 'trim'))])

## Get the combined range of years covered over all systems
yrrange <- range(sapply(worked, function(x) range(sapply(x, function(y) range(y$value$time.id)))))

## Get a list of which species were successfully run in each system
sps_char <- lapply(worked, function(x) names(x))

## The same as above but only species numbers (numeric)
sps_num <- lapply(worked, function(x) as.numeric(gsub('Art_', '', names(x))))

## Get the unique set of species run over all systems
usps_num <- sort(unique(unlist(sps_num)))

#### Extract species-wise results/info necessary to produce all Excel-files (ever!!!) #### 
resultout <- vector(mode='list', length = length(usps_num)) 
names(resultout) <- paste0('Art_', usps_num)
for (us in usps_num){
  us_char <- paste0('Art_', us)
  # trimV <- worked$totalvinter_pkt[[us_char]]$value
  # trimS <- worked$totalsommar_pkt[[us_char]]$value
  # trimSt <- worked$totalstandard[[us_char]]$value
  newbase <- data.frame(tabell = names(worked), base = base)
  if (!is.null(startyr)){
    stysp <- startyr[as.integer(startyr$Art)==us, c('Delprogram', 'StartYear')]
    if (nrow(stysp)>0){
      six <- stysp$Delprogram%in%newbase$tabell & stysp$StartYear>base
      nix <- match(stysp$Delprogram[six], newbase$tabell, nomatch=0)
      newbase$base[nix] <- stysp$StartYear[six]
    }
  }
  templist <- lapply(names(worked),
                     function(x){
                       ExtractRes(worked[[x]][[us_char]]$value, x,
                                  base = newbase$base[newbase$tabell==x],
                                  yrrange = yrrange)
                     })
  names(templist) <- names(worked)
  resultout[[us_char]] <- list(Art = us,
                               SEname = spdat[match(us, as.integer(spdat$art)), 'arthela'],
                               ENname = spdat[match(us, as.integer(spdat$art)), 'englishname'],
                               WDname = spdat[match(us, as.integer(spdat$art)), 'worldname'],
                               Scname = spdat[match(us, as.integer(spdat$art)), 'latin'],
                               Systord = spdat[match(us, as.integer(spdat$art)), 'rank'],
                               Results = templist
  )
}


MakeXlsFile(resultout, colnames = shortcolumn, tabnames = shorttab, specieslist = sps_char, specieslanguage = lang,
            combinations = combinations, single = single, homepage = homepage)















#######################################################################################################################




#### Reproduce old TRIM output ####
## Code to extract the same output information from a "trim"-object (produced by rtrim) as 
## is produced by old TRIM (example 151.out) ####
temp <- trimOutput$Art_151

## General summary
count_summary(temp$data, count_col = 'count', site_col = 'site2', year_col = 'time')
# 2. Time number of values:
length(unique(temp$data$time))

## Total count
sum(totals(temp$value, obs=T)$observed)

## Sites containing more than 10% of the total count
aggtemp <- aggregate(observed~site, data = results(temp$value), sum, na.rm=T)
aggtemp$perc <- 100*(aggtemp$observed/sum(aggtemp$observed))
aggtemp[aggtemp$perc>10,]

## Time point averages
aggtime <-  aggregate(observed~time, data = results(temp$value), length)
aggtime$average <- aggregate(observed~time, data = results(temp$value), mean)$observed
aggtime$index <- aggtime$average/aggtime$average[1]
aggtime

### To reproduce some old TRIM output we need a model 3 version of the analysis
tempm3 <- trim(count~site2 + time, data=temp$data, model=3, overdisp=T, serialcor=T)

## RESULTS FOR MODEL: Effects for Each Time Point 
summary(tempm3)

## WALD-TEST FOR SIGNIFICANCE OF DEVIATIONS FROM LINEAR TREND 
wald(tempm3)

## Linear Trend + Deviations for Each Time
coef(tempm3, 'deviations')

### For the rest of the old TRIM output we can use either model 2 or model 3 version

## Time INDICES,  Base is Time 1998 
index(temp$value, which='both', base=1998)
index(tempm3, which='both', base=1998)
# note, they are not identical but this is due to rounding errors (machine precision)
identical(index(temp$value, which='both', base=1998), index(tempm3, which='both', base=1998))
all.equal(index(temp$value, which='both', base=1998), index(tempm3, which='both', base=1998))

##  TIME TOTALS
totals(temp$value, which = 'both', obs=T)
identical(totals(temp$value, which = 'both', obs=T), totals(tempm3, which = 'both', obs=T)) # no rounding -> identical

## OVERALL SLOPE MODEL
overall(temp$value, 'fitted')
all.equal(overall(temp$value, 'fitted'), overall(tempm3, 'fitted'))

## OVERALL SLOPE IMPUTED
overall(temp$value, 'imputed')
all.equal(overall(temp$value, 'imputed'), overall(tempm3, 'imputed'))
