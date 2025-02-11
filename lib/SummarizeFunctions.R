## Load necessary "stuff"
## Needed especially if you haven't run the app in this session (most of this will already be loaded if you have)
library(DBI)  # Package needed to get eurolistdata from SFT
library(pool) # as above
library(rtrim) # Needed to use functions for extracting info from "trim"-objects
#library(xlsx) # Needed to save data to Excel-files
library(readxl)
library(writexl)



DoSummarizeResult <- function (filenames=NULL, tables=NULL, base=NULL, spdat=NULL, startyr=NULL, homepage=NULL, lang=NULL, params=NULL) {

  shortcolumn <- c()
  shorttab <- c()
  
  for (t in 1:length(tables)) {
    shortcolumn[t] <- tabShorts$short[tabShorts$table == tables[t]]
    shorttab[t] <- tabShorts$short[tabShorts$table == tables[t]]
  }
  
  ## Read in rdata-files with results
  ## Finds and read in the latest file produced for each monitoring system (and called trimOutput-something...)
  ## The findlatestFile-function is found among the UsefulFunctions
  files2summarize <- paste(filenames, tables, sep = '_')

  reslist <- lapply(files2summarize, function(x) {
    f <- findlatestFile(folder=path_project_extract, filename = paste0(x, '_'), dateform = '%Y-%m-%d_%H_%M_%S')
    shiny::validate(
      need(length(f) > 0, "No files were found. Did you save your analysis as a .rdata file? Make sure to check '.rdata-file' in tab 'Analyze data'.")
    )
    load(paste0(path_project_extract,f))
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


  MakeXlsFile(resultout, colnames = shortcolumn, tabnames = shorttab, specieslist = sps_char, specieslanguage = lang, homepage = homepage, params = params)

}