
#### DoQuery-function  ####
## Function creating and running queries to SFT to get all data.
## Optionally saves data to various places
DoQuery <- function (pool=NULL, tab=NULL, spec=NULL, specper=NULL, selyrs=NULL,
                     line=NULL, savedat=NULL, filename = NULL) {
  
  print(paste("start DoQuery", Sys.time()))

  ## countvar talar om vad countkolumnen heter i respektive tabell ## 
  countvar <- switch(tab,
                     totalvinter_pkt = 'ind',
                     totalsommar_pkt = 'ind',
                     totalstandard = ifelse(line, 'lind', 'pkind'),
                     totalvatmark = 'ind',
                     total_iwc_januari = 'antal',
                     total_iwc_september = 'antal',
                     misc_census = 'ind',
                     stop('No applicable table'))
  
  ## Skapar söksträngar för valda arter och period (behövs i queries nedan) ##
  specsel <- paste0('(', paste(spec, collapse = ','), ')')
  persel <- paste0('(', paste(specper, collapse = ','), ')')
  yrsel <- paste0('(', paste(selyrs[1]:selyrs[2], collapse = ','), ')')
  
  
  #### Skapa queries ####
  
  ## Delqueries dvs. SQL-kod för att skapa följande tabeller:                   ##
  ## "-1"      -1 sätts som count för alla kombinationer av rutt, år och        ##
  ##           någonsin sedda arter på varje rutt (periodspecifik för vinter)   ##
  ##                                                                            ##
  ## "0"       0 sätts som count för alla någonsin sedda arter vid varje rutt   ##
  ##           för de år som rutten besökts                                     ##
  ##                                                                            ##
  ## "counts"  De faktiskt räknade antalen för arter som observerats vid varje  ##
  ##           ruttbesök                                                        ##
  ##                                                                            ##
  ## En subquery för varje typ av inventering                                   ##
  
  subqueries <- switch(tab, 
                       totalvinter_pkt = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                           from
                                           (select distinct tot.persnr || '_' || tot.rnr as site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s and to_number(per, '9') in %s)
                                           as ro,
                                           (select distinct tot.yr as time
                                           from
                                           %s as tot
                                           where yr in %s)
                                           as yrs", tab, specsel, persel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                         from
                                         (select tot.persnr || '_' || tot.rnr as site, tot.yr as time 
                                         from
                                         %s as tot
                                         where art='000' and to_number(per, '9') in %s and yr in %s)
                                         as vi,
                                         (select distinct tot.persnr || '_' || tot.rnr as site, tot.art as species
                                         from
                                         %s as tot
                                         where to_number(art, '000') in %s and to_number(per, '9') in %s)
                                         as ro
                                         where vi.site=ro.site", tab, persel, yrsel, tab, specsel, persel)
                         ,
                         tabcount = sprintf("select persnr || '_' || rnr as site, art as species, yr as time, %s as count
                                          from
                                          %s
                                          where to_number(art, '000') in %s and to_number(per, '9') in %s and yr in %s",
                                            countvar, tab, specsel, persel, yrsel)
                         
                       ),
                       totalsommar_pkt = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                           from
                                           (select distinct tot.persnr || '_' || tot.rnr as site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s)
                                           as ro,
                                           (select distinct tot.yr as time
                                           from
                                           %s as tot
                                           where yr in %s)
                                           as yrs", tab, specsel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                         from
                                         (select tot.persnr || '_' || tot.rnr as site, tot.yr as time 
                                         from
                                         %s as tot
                                         where art='000' and yr in %s)
                                         as vi,
                                         (select distinct tot.persnr || '_' || tot.rnr as site, tot.art as species
                                         from
                                         %s as tot
                                         where to_number(art, '000') in %s)
                                         as ro
                                         where vi.site=ro.site", tab, yrsel, tab, specsel)
                         ,
                         tabcount = sprintf("select persnr || '_' || rnr as site, art as species, yr as time, %s as count
                                          from
                                          %s
                                          where to_number(art, '000') in %s and yr in %s", countvar, tab, specsel, yrsel)
                         
                       ),
                       totalstandard = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                           from
                                           (select distinct tot.karta as site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s and %s>0)
                                           as ro,
                                           (select distinct tot.yr as time
                                           from
                                           %s as tot
                                           where yr in %s)
                                           as yrs", tab, specsel, countvar, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                         from
                                         (select tot.karta as site, tot.yr as time 
                                         from
                                         %s as tot
                                         where art='000' and yr in %s)
                                         as vi,
                                         (select distinct tot.karta as site, tot.art as species
                                         from
                                         %s as tot
                                         where to_number(art, '000') in %s and %s>0)
                                         as ro
                                         where vi.site=ro.site", tab, yrsel, tab, specsel, countvar)
                         ,
                         tabcount = sprintf("select karta as site, art as species, yr as time, %s as count
                                          from
                                          %s
                                          where to_number(art, '000') in %s and yr in %s and %s>0",
                                            countvar, tab, specsel, yrsel, countvar)
                       ),
                       totalvatmark = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                            from
                                            (select distinct tot.rutt as site, tot.art as species
                                             from
                                             %s as tot
                                             where to_number(art, '000') in %s)
                                             as ro,
                                             (select distinct tot.yr as time
                                             from
                                             %s as tot
                                             where yr in %s)
                                             as yrs", tab, specsel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                           from
                                           (select tot.rutt as site, tot.yr as time
                                           from
                                           %s as tot
                                           where art = '000' and yr in %s)
                                           as vi,
                                           (select distinct tot.rutt as site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s)
                                           as ro
                                           where vi.site=ro.site", tab, yrsel, tab, specsel)
                         ,
                         tabcount = sprintf("select rutt as site, art as species, yr as time, %s as count
                                            from
                                            %s
                                            where to_number(art, '000') in %s and yr in %s",
                                            countvar, tab, specsel, yrsel)
                       ),
                       total_iwc_januari = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                             from
                                             (select distinct tot.site, tot.art as species
                                             from
                                             %s as tot
                                             where to_number(art, '000') in %s)
                                             as ro,
                                             (select distinct tot.yr as time
                                             from
                                             %s as tot
                                             where yr in %s)
                                             as yrs", tab, specsel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                           from
                                           (select tot.site, tot.yr as time
                                           from
                                           %s as tot
                                           where art='000' and yr in %s)
                                           as vi,
                                           (select distinct tot.site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s)
                                           as ro
                                           where vi.site=ro.site", tab, yrsel, tab, specsel)
                         ,
                         tabcount = sprintf("select site, art as species, yr as time, %s as count
                                            from
                                            %s
                                            where to_number(art, '000') in %s and yr in %s",
                                            countvar, tab, specsel, yrsel)
                           
                         ),
                       total_iwc_september = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                             from
                                             (select distinct tot.site, tot.art as species
                                             from
                                             %s as tot
                                             where to_number(art, '000') in %s)
                                             as ro,
                                             (select distinct tot.yr as time
                                             from
                                             %s as tot
                                             where yr in %s)
                                             as yrs", tab, specsel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                           from
                                           (select tot.site, tot.yr as time
                                           from
                                           %s as tot
                                           where art='000' and yr in %s)
                                           as vi,
                                           (select distinct tot.site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s)
                                           as ro
                                           where vi.site=ro.site", tab, yrsel, tab, specsel)
                         ,
                         tabcount = sprintf("select site, art as species, yr as time, %s as count
                                            from
                                            %s
                                            where to_number(art, '000') in %s and yr in %s",
                                            countvar, tab, specsel, yrsel)
                         
                       ),                       
                       misc_census = list(
                         tabminus1 = sprintf("select ro.site, ro.species, yrs.time, -1 as count
                                            from
                                            (select distinct tot.karta as site, tot.art as species
                                             from
                                             %s as tot
                                             where to_number(art, '000') in %s)
                                             as ro,
                                             (select distinct tot.yr as time
                                             from
                                             %s as tot
                                             where yr in %s)
                                             as yrs", tab, specsel, tab, yrsel)
                         ,
                         tabzero = sprintf("select vi.site, ro.species, vi.time, 0 as count
                                           from
                                           (select tot.karta as site, tot.yr as time
                                           from
                                           %s as tot
                                           where art = '000' and yr in %s)
                                           as vi,
                                           (select distinct tot.karta as site, tot.art as species
                                           from
                                           %s as tot
                                           where to_number(art, '000') in %s)
                                           as ro
                                           where vi.site=ro.site", tab, yrsel, tab, specsel)
                         ,
                         tabcount = sprintf("select karta as site, art as species, yr as time, %s as count
                                            from
                                            %s
                                            where to_number(art, '000') in %s and yr in %s",
                                            countvar, tab, specsel, yrsel)
                       ),
                       stop('No applicable table')
  )
  
  
  
  ## Bygga ihop subqueries till den query som ska skickas till SFT  ##
  ## Tabellerna "-1", "0", "counts" läggs ihop (efter varandra) och ##
  ## sedan tas max över count-kolumnen                              ##
  
  query <- sprintf("select site, species, time, max(count) as count
                 from
                 (%s
                 union all
                 %s
                 union all
                 %s)
                 as allt
                 group by site, time, species
                 order by site, time, species", subqueries$tabminus1, subqueries$tabzero, subqueries$tabcount)
  
  
  #### Körning mot SFT ####
  ## Göra uttaget ##
  dat <- dbGetQuery(pool, query)

  ## Export data
  if (1%in%savedat){
    dat <<- dat
  }
  if (2%in%savedat){
    write.csv(dat, file = paste0('extract/psql_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.csv'),
              row.names = FALSE)
  }
  if (3%in%savedat){
    write.csv2(dat, file = paste0('extract/psql_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '_csv2.csv'),
              row.names = FALSE)
  }
  if (4%in%savedat){
    save(dat, file = paste0('extract/psql_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.rdata'))
  }

  print(paste("fin DoQuery", Sys.time()))

  return(dat)
}

#### RunTRIMmodel ####
## Function that runs rtrim on data on specified species
## Optionally saves results in various places
RunTRIMmodel <- function(dat = NULL, modeltype = NULL, sp_to_run = NULL, 
                         speciesdat = NULL, odisp = NULL, sercor = NULL,
                         changepoints = 'all', autodel = NULL, startyr=NULL,
                         tabell=NULL, saveresult = NULL, filename = NULL){
  ## Fix data to satisfy rtrim ##
  if (any(unique(na.omit(dat$count))==-1)) {
    dat$count[dat$count==-1] <- NA
    } 
  dat$site <- factor(dat$site)
  #dat$site2 <- as.integer(dat$site) # FIXME i nya rtrim kan site vara factor så detta behövs inte (ändra också i modelspecifikationen)
  
  
  ## Get species names ##
  sp_to_run_names <- speciesdat$arthela[match(sp_to_run, as.integer(speciesdat$art))]
  
  ## Prepare output-object ##
  output <- vector(mode = 'list', length = length(sp_to_run))
  names(output) <- paste0('Art_', sp_to_run)

  ## Fix model inputs
  changepoints <- if(modeltype == 2){changepoints} else {integer(0)}
  
  ## Loop through species ##
  withProgress(message = 'Running analysis', value = 0, {
    for (sp in 1:length(sp_to_run)){
      sdat <- droplevels(subset(dat, as.integer(species)==sp_to_run[sp]))
      if (!is.null(startyr) && nrow(startyr)>0){
        if(sp_to_run[sp]%in%as.integer(startyr$Art)){
          sdat <- droplevels(subset(sdat, time >= startyr$StartYear[as.integer(startyr$Art)==sp_to_run[sp]]))
        }
      } 
      output[[sp]] <- tryCatch.W.E(trim(count~site + time,
                                        data = sdat,
                                        model = modeltype, overdisp = odisp, serialcor = sercor,
                                        changepoints = changepoints, autodelete = autodel, max_iter=1000))
      output[[sp]]$data <- sdat
    incProgress(1/length(sp_to_run), detail = paste('now', sp_to_run_names[sp]))
    }
  })
  oix <- order(spdat$rank[match(names(output), paste0('Art_', as.integer(spdat$art)))])
  output <- output[oix]
  
  ## Export results
  if (1%in%saveresult){
    trimOutput <<- output
  }
  if (2%in%saveresult){
    save(output, file = paste0('extract/', filename, '_', tabell, '_', gsub('[ :]', '_', Sys.time()), '.rdata'))
  }
  
  return(output)
}

#### tryCatch.W.E-function ####
## Function to handle (collect) errors and warnings
## Modified from https://stat.ethz.ch/pipermail/r-help/2010-December/262626.html
tryCatch.W.E <- function(expr) 
{ 
  W <- list(message=NULL, call=NULL) 
  w.handler <- function(w){ # warning handler 
    W$message <<- c(W$message, w$message)
    W$call <<- c(W$call, w$call)
    invokeRestart("muffleWarning") 
  } 
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), 
                                   warning = w.handler), 
       warning = W) 
} 

#### AddNSspecies - function ####
## Add southern/northern species (often races) or groups of species to dataset
AddNSspecies <- function(data = NULL, rangedata = NULL, coorddata = NULL){
  if (any(unique(na.omit(data$count))==-1)) {
    data$count[data$count==-1] <- NA
  }
  for (i in 1:length(rangedata$art)){
    #print(i)
    if(rangedata$art[i]=='645' & any(c('243', '244', '245')%in%data$species)){
      spd <- aggregate(count ~ site + time, data = subset(data, species%in%c('243', '244', '245')), sum, na.action = na.pass)
      spd$species <- '645'
      spd <- spd[, c('site', 'species', 'time', 'count')]
    } else if (rangedata$speciesmain[i]%in%data$species){
      if (rangedata$smaller[i]){
        kix <- coorddata$lat < rangedata$Latlimit[i]
        } else {
        kix <- coorddata$lat > rangedata$Latlimit[i]
        }
      rix <- data$site%in%coorddata$site[kix]
      six <- data$species==rangedata$speciesmain[i]
      spd <- subset(data, rix & six)
      if(nrow(spd)>0){
        spd$species <- rangedata$art[i]
        spd <- spd[, c('site', 'species', 'time', 'count')]
      } else {
        spd <- NULL
      }
    } else {
      spd <- NULL
    }
    data <- rbind(data, spd)
  }
  return(data)
}

#### index2overall - replace time-totals in trimobject with index(trimobject) values ####
#  and output overall("newtrimobject") according to hack provided by Patrick Bogaart
index2overall <- function(trimobjs = NULL, base = NULL, startyr = NULL){
  out <- vector(mode = 'list', length = length(trimobjs))
  names(out) <- names(trimobjs)
  for (i in 1:length(trimobjs)){
    mo <- trimobjs[[i]]$value
    spno <- as.integer(gsub('Art_', '', names(trimobjs)[i]))
    if (!is.null(startyr)){
      stysp <- startyr$StartYear[as.integer(startyr$Art)==spno]
      newbase <- ifelse(length(stysp)>0, ifelse(stysp>base, stysp, base), base)
    } else {
      newbase <- base
    }
    idx <- index(mo, "both", base=newbase)                                         # get the index
    mo$tt_mod <- idx$fitted                 # disguise modelled index as time-totals
    mo$var_tt_mod <- diag(idx$se_fit^2) # idem for it's variance (we loose covariance info...)
    mo$tt_imp <- idx$imputed                                      # Idem for imputed
    mo$var_tt_imp <- diag(idx$se_imp^2)
    if (all(is.finite(mo$tt_imp)) & all(is.finite(mo$var_tt_imp))){
      oa_mo <- overall(mo, which = 'imputed')
      attr(oa_mo, 'typetext') <- '(Imputed indices)'
    } else {
      oa_mo <- overall(mo, which = 'fitted')
      attr(oa_mo, 'typetext') <- '(Fitted indices)'
    }
    out[[i]] <- oa_mo
  }
  return(out)
}

#### indexplot - plot-function of indices from rtrim that ####
# a) allows to specify number of columns of plot figures
# b) allows to also produce a pdf-file of the plots
#     and if so the number of rows of figures to display at 
#     each page in the pdf
# c) uses the hack from Patrick Boogart to produce
#     a "overall"version plot of indices which includes a 
#     trend line based on indices
indexplot <- function(trimobjlist = NULL, base = NULL, ncol = NULL, makepdf = TRUE,
                      nrow.pdf = 6, speciesdat = NULL, startyr = NULL, filename = 'extract/TrimGrafer.pdf') {
  nsp <- length(trimobjlist)
  if (nsp<ncol){
    ncol <- nsp
  }
  if ((nsp/ncol) < nrow.pdf){
    nrow.pdf <- ceiling(nsp/ncol)
  }
  pobj <- index2overall(trimobjlist, base = base, startyr = startyr)
  op <- par(mfrow = c(ceiling(nsp/ncol), ncol))
  for (i in 1:nsp) {
      plot.trim.overall_MS(pobj[[i]], ylab = 'Index', cex.lab=1.4)
      abline(h=1, lty=2)
      pname <- speciesdat$arthela[match(gsub('Art_','', names(pobj)[i]), as.numeric(speciesdat$art))]
      title(main = paste(pname, attr(pobj[[i]], 'typetext')), cex.main=1.6)
  }
  par(op)
  if(makepdf) {
    pdf(file = filename, width = 7*ncol, height = 7*nrow.pdf)
    plotlist <- split(seq(length.out = nsp), ceiling(seq(length.out = nsp)/(nrow.pdf*ncol)))
    for (pl in names(plotlist)){
      op <- par(mfrow = c(nrow.pdf, ncol))
      for (p in plotlist[[pl]]){
        plot.trim.overall_MS(pobj[[p]], ylab = 'Index', cex.lab=1.4)
        abline(h=1, lty=2)
        pname <- speciesdat$arthela[match(gsub('Art_','', names(pobj)[p]), as.numeric(speciesdat$art))]
        title(main = paste(pname, attr(pobj[[p]], 'typetext')), cex.main=1.6)
      }
      par(op)
    }
    dev.off()
  }
  
}





#### plot.trim.overall_MS - Version of plot.trim.overall that allows for main and ylab to be specified ####
plot.trim.overall_MS <- function (x, imputed = TRUE, main = attr(X, "title"), ...) 
{
  X <- x
  # title <- if (is.null(list(...)$main)) {
  #   attr(X, "title")
  # }
  # else {
  #   list(...)$main
  # }
  # ylab <- if (is.null(list(...)$ylab)) {
  #   'Count'
  # }
  # else {
  #   list(...)$ylab
  # }
  tpt = X$timept
  J <- X$J
  ydata <- X$tt
  y0 = ydata - X$err
  y1 = ydata + X$err
  trend.line <- NULL
  conf.band <- NULL
  X$type <- "changept"
  if (X$type == "normal") {
    a <- X$coef[[1]][1]
    b <- X$coef[[1]][2]
    x <- seq(1, J, length.out = 100)
    ytrend <- exp(a + b * x)
    xtrend <- seq(min(tpt), max(tpt), len = length(ytrend))
    trendline = cbind(xtrend, ytrend)
    xconf <- c(xtrend, rev(xtrend))
    alpha <- 0.05
    df <- J - 2
    t <- qt((1 - alpha/2), df)
    j = 1:J
    dx2 <- (x - mean(j))^2
    sumdj2 <- sum((j - mean(j))^2)
    dy <- t * sqrt((X$SSR/(J - 2)) * (1/J + dx2/sumdj2))
    ylo <- exp(a + b * x - dy)
    yhi <- exp(a + b * x + dy)
    yconf <- c(ylo, rev(yhi))
    conf.band <- cbind(xconf, yconf)
  }
  else if (X$type == "changept") {
    nsegment = nrow(X$slope)
    for (i in 1:nsegment) {
      a <- X$intercept[i, 3]
      b <- X$slope[i, 3]
      from <- which(tpt == X$slope[i, 1])
      upto <- which(tpt == X$slope[i, 2])
      delta = (upto - from) * 10
      x <- seq(from, upto, length.out = delta)
      ytrend <- exp(a + b * x)
      xtrend <- seq(tpt[from], tpt[upto], length.out = length(ytrend))
      if (i == 1) {
        trendline = cbind(xtrend, ytrend)
      }
      else {
        trendline = rbind(trendline, NA)
        trendline = rbind(trendline, cbind(xtrend, ytrend))
      }
      xconf <- c(xtrend, rev(xtrend))
      alpha <- 0.05
      ntpt <- upto - from + 1
      df <- ntpt - 2
      if (df <= 0) 
        next
      t <- qt((1 - alpha/2), df)
      j = from:upto
      dx2 <- (x - mean(j))^2
      sumdj2 <- sum((j - mean(j))^2)
      SSR = X$SSR[i]
      dy <- t * sqrt((SSR/df) * (1/ntpt + dx2/sumdj2))
      ylo <- exp(a + b * x - dy)
      yhi <- exp(a + b * x + dy)
      yconf <- c(ylo, rev(yhi))
      if (is.null(conf.band)) {
        conf.band <- cbind(xconf, yconf)
      }
      else {
        conf.band = rbind(conf.band, NA)
        conf.band = rbind(conf.band, cbind(xconf, yconf))
      }
    }
    yrange = c(300, 700)
  }
  else stop("Can't happen")
  xrange = range(trendline[, 1], na.rm = TRUE)
  yrange1 = range(range(y0), range(y1), range(trendline[, 2]), 
                  na.rm = TRUE)
  yrange2 = range(range(conf.band[, 2], na.rm = TRUE))
  yrange = range(yrange1, yrange2, na.rm = TRUE)
  ylim = 2 * yrange1[2]
  if (yrange[2] > ylim) 
    yrange[2] = ylim
  yrange <- range(0, yrange)
  cbred <- rgb(228, 26, 28, maxColorValue = 255)
  cbblue <- rgb(55, 126, 184, maxColorValue = 255)
  # plot(xrange, yrange, type = "n", xlab = "Time point", ylab = ylab, 
  #      las = 1, main = title)
  plot(xrange, yrange, type = "n", xlab = "Time point", 
       las = 1, main=main, ...)
    polygon(conf.band, col = gray(0.9), lty = 0)
  lines(trendline, col = cbred, lwd = 3)
  segments(tpt, y0, tpt, y1, lwd = 2.5, col = gray(0.5))
  points(tpt, ydata, col = cbblue, type = "b", pch = 16, lwd = 3, cex=1.5)
}


testplottrim2 <- function (x, imputed = TRUE, base=1, ...) 
{
  X <- x
  title <- if (is.null(list(...)$main)) {
    attr(X, "title")
  }
  else {
    list(...)$main
  }
  tpt = X$timept
  J <- X$J
  if (base[1] %in% tpt) {
    stopifnot(all(base %in% tpt))
    for (i in seq_along(base)) base[i] <- which(base[i] == 
                                                  tpt)
  }
  else if (base < 1 || base > x$nyear) {
    msg <- sprintf("Invalid base year %d. Must be either %d--%d or %d--%d", 
                   base, 1, J, tpt[1], tpt[J])
    stop(msg)
  }
  ydata <- X$tt
  y0 = ydata - X$err
  y1 = ydata + X$err
  ydata <- ydata/X$tt[base]
  y0 <- y0/X$tt[base]
  y1 <- y1/X$tt[base]
  trend.line <- NULL
  conf.band <- NULL
  X$type <- "changept"
  if (X$type == "normal") {
    a <- X$coef[[1]][1]
    b <- X$coef[[1]][2]
    x <- seq(1, J, length.out = 100)
    ytrend <- exp(a + b * x)
    xtrend <- seq(min(tpt), max(tpt), len = length(ytrend))
    trendline = cbind(xtrend, ytrend)
    xconf <- c(xtrend, rev(xtrend))
    alpha <- 0.05
    df <- J - 2
    t <- qt((1 - alpha/2), df)
    j = 1:J
    dx2 <- (x - mean(j))^2
    sumdj2 <- sum((j - mean(j))^2)
    dy <- t * sqrt((X$SSR/(J - 2)) * (1/J + dx2/sumdj2))
    ylo <- exp(a + b * x - dy)
    yhi <- exp(a + b * x + dy)
    yconf <- c(ylo, rev(yhi))
    conf.band <- cbind(xconf, yconf)
  } else if (X$type == "changept") {
    nsegment = nrow(X$slope)
    for (i in 1:nsegment) {
      a <- X$intercept[i, 3]
      b <- X$slope[i, 3]
      from <- which(tpt == X$slope[i, 1])
      upto <- which(tpt == X$slope[i, 2])
      delta = (upto - from) * 10
      x <- seq(from, upto, length.out = delta)
      ytrend <- exp(a + b * x)/X$tt[base]
      xtrend <- seq(tpt[from], tpt[upto], length.out = length(ytrend))
      if (i == 1) {
        trendline = cbind(xtrend, ytrend)
      }
      else {
        trendline = rbind(trendline, NA)
        trendline = rbind(trendline, cbind(xtrend, ytrend))
      }
      xconf <- c(xtrend, rev(xtrend))
      alpha <- 0.05
      ntpt <- upto - from + 1
      df <- ntpt - 2
      if (df <= 0) 
        next
      t <- qt((1 - alpha/2), df)
      j = from:upto
      dx2 <- (x - mean(j))^2
      sumdj2 <- sum((j - mean(j))^2)
      SSR = X$SSR[i]
      dy <- t * sqrt((SSR/df) * (1/ntpt + dx2/sumdj2))
      ylo <- exp(a + b * x - dy)/X$tt[base]
      yhi <- exp(a + b * x + dy)/X$tt[base]
      yconf <- c(ylo, rev(yhi))
      if (is.null(conf.band)) {
        conf.band <- cbind(xconf, yconf)
      }
      else {
        conf.band = rbind(conf.band, NA)
        conf.band = rbind(conf.band, cbind(xconf, yconf))
      }
    }
    yrange = c(300, 700)
  } else stop("Can't happen")
  xrange = range(trendline[, 1], na.rm = TRUE)
  yrange1 = range(range(y0), range(y1), range(trendline[, 2]), 
                  na.rm = TRUE)
  yrange2 = range(range(conf.band[, 2], na.rm = TRUE))
  yrange = range(yrange1, yrange2, na.rm = TRUE)
  ylim = 2 * yrange1[2]
  if (yrange[2] > ylim) 
    yrange[2] = ylim
  yrange <- range(0, yrange)
  cbred <- rgb(228, 26, 28, maxColorValue = 255)
  cbblue <- rgb(55, 126, 184, maxColorValue = 255)
  plot(xrange, yrange, type = "n", xlab = "Time point", ylab = "Index", 
       las = 1, main = title, ...)
  polygon(conf.band, col = gray(0.9), lty = 0)
  lines(trendline, col = cbred, lwd = 3)
  segments(tpt, y0, tpt, y1, lwd = 3, col = gray(0.5))
  points(tpt, ydata, col = cbblue, type = "b", pch = 16, lwd = 3)
}

#### findlatestFile ####
findlatestFile <- function(folder = getwd(), filename, dateform){

  f <- list.files(paste0(folder, '/'))
  f <- f[ grep(paste0(filename, '.*\\.rdata'), f)]
  rgxp <- gsub('%[[:alpha:]]', '[[:digit:]]+', dateform)
  cat('Files found:', f <- f[ grep(paste0(filename, rgxp, '\\.rdata'), f)], '\n')
  pos <- regexpr(rgxp, f)
  dn <- as.double(strptime(regmatches(f, pos), dateform))
  fname <- f[which.max(dn)]
  return(fname)
}

#### ExtractRes ####
ExtractRes <- function(obj, tabell, base = 1998, yrrange){
  ismodel <- inherits(obj, 'trim')
  idx_name <- switch(tabell,
                     totalvinter_pkt = 'IndexV',
                     totalsommar_pkt = 'IndexS',
                     totalstandard = 'IndexT',
                     'Index')
  if (ismodel) {
    oa <- overall(obj)
    oadf <- data.frame(avcount = round(mean(obj$time.totals$observed)),
                       nroute = obj$nsite,
                       slope = round(oa$slope$mul, 4),
                       se_slope = round(oa$slope$se_mul, 4),
                       p_slope = oa$slope$p,
                       sig = ifelse(oa$slope$p<=0.05,
                                    ifelse(oa$slope$p>0.01, '*',
                                           ifelse(oa$slope$p>0.001, '**', '***')), 'NS'),
                       pctslope = round((oa$slope$mul - 1)*100, 1),
                       interpret = oa$slope$meaning,
                       stringsAsFactors = F
                       )
    idx <- index(obj, base = base)
    idx$converge <- obj$converged
    idx$converge_info <- obj$convergence
    idxdf <- merge(data.frame(time = yrrange[1]:yrrange[2]), idx, all.x = T)
  } else {
    oadf <- data.frame(avcount = '-',
                       nroute ='-',
                       slope = '-', 
                       se_slope = '-',
                       p_slote = '-',
                       sig = '-',
                       pctslope = '-',
                       interpret = '_',
                       stringsAsFactors = F)
    idxdf <- data.frame(time = yrrange[1]:yrrange[2], imputed=NA, se_imp = NA, converge = NA, converge_info='')
  }
  out <- list(Overall = oadf,
              Index = cbind(idxdf, indexcol = idx_name))
  return(out)
}


#### MakeXlsFile ####
MakeXlsFile <- function(obj, colnames = NULL, tabnames = NULL, specieslist = NULL, specieslanguage = 'SE',
                        combinations = NULL, single = TRUE, homepage = TRUE){
  nsyst <- unname(sapply(obj[1], function(x) {length(x$Results)}))
  if(nsyst>1 & !is.null(combinations)){
    if (!is.list(combinations)){
      stop('combinations must be a list of numeric vector(s)')
    }
    for (c in 1:length(combinations)) {
      cix <- combinations[[c]]
      combtab <- lapply(obj, function(x){
        spname <- switch(specieslanguage,
                         SE = x$SEname,
                         EN = x$ENname,
                         WD = x$WDname)
        tinf <- data.frame(ArtRad1 = paste(spname, x$Scname, sep = ', '),
                           ArtRad2 = paste0('(',
                                            paste(
                                              sapply(x$Results[cix], function(y) {
                                                paste(y$Overall[, c('avcount', 'nroute', 'pctslope', 'sig')], collapse = ', ')
                                              }),
                                              collapse = '; '),
                                            ')'),
                           Artnamn = spname,
                           ART = x$Systord,
                           Yr = x$Results[[1]]$Index$time,
                           stringsAsFactors = F)
        tindex <- as.data.frame(lapply(x$Results[cix], function(y) round(y$Index$imputed, 4)))
        names(tindex) <- paste0('Index', colnames[cix])
        tconv <- do.call(cbind,
                         lapply(x$Results[cix], function(y) {
                           data.frame(conv=y$Index$converge, inf=y$Index$converge_info, stringsAsFactors = F)
                         }
                         ))
        names(tconv) <- paste0(rep(c('Converge', 'ConvergeInfo'), length(colnames[cix])), rep(colnames[cix], each = 2))
        return(cbind(tinf, tindex, tconv))
      })
      combtab <- do.call(rbind, combtab)
      # write.xlsx2(combtab[order(combtab$ART),],
      #             file = paste0('Trimcombined_', paste(colnames[cix], collapse = '_'), '_', max(combtab$Yr), '_Figurritning.xlsx'),
      #             sheetName='TRIMCOMBO', row.names=FALSE, showNA=FALSE)
      write_xlsx(list(TRIMCOMBO=combtab[order(combtab$ART),]),
                  path = paste0(path_project_extract,'Trimcombined_', paste(colnames[cix], collapse = '_'), '_', max(combtab$Yr), '_Figurritning.xlsx'),
                  format_headers = TRUE)
      
    }
  } 
  if (single) {
    for (i in 1:nsyst){
      singltab <- lapply(specieslist[[i]], function(x){
        spname <- switch(specieslanguage,
                         SE = obj[[x]]$SEname,
                         EN = obj[[x]]$ENname,
                         WD = obj[[x]]$WDname)
        ttab <- data.frame(Arthela = paste(
          paste(spname, obj[[x]]$Scname, sep = ', '),
          paste0('(',
                 paste(obj[[x]]$Results[[i]]$Overall[, c('avcount', 'nroute', 'pctslope', 'sig')],
                       collapse = ', '),
                 ')'),
          sep = ' - '),
          Artnamn = spname,
          ART = obj[[x]]$Systord,
          Yr = obj[[x]]$Results[[i]]$Index$time,
          Index = round(obj[[x]]$Results[[i]]$Index$imputed, 4),
          #Artnamn = obj[[x]]$SEname,
          Converge = obj[[x]]$Results[[i]]$Index$converge,
          ConvergeInfo = obj[[x]]$Results[[i]]$Index$converge_info,
          stringsAsFactors = F)
      })
      singltab <- do.call(rbind, singltab)
      uyrs <- unique(singltab$Yr[apply(singltab[, c('Index', 'Converge', 'ConvergeInfo')], 1, function(a) !all(is.na(a)))])
      # singltab <- subset(singltab, Yr>=min(uyrs) & Yr<=max(uyrs))
      singltab <- list(subset(singltab, Yr>=min(uyrs) & Yr<=max(uyrs))[order(singltab$ART),])
      names(singltab) <- paste0('TRIM', tabnames[i])
      # write.xlsx2(singltab[order(singltab$ART),],
      #             file = paste0('Trim', colnames[i], '_', max(singltab$Yr), '_Figurritning.xlsx'), sheetName=paste0('TRIM', tabnames[i]), row.names=FALSE, showNA=FALSE)
      write_xlsx(singltab,
                  path = paste0(path_project_extract,'Trim', colnames[i], '_', min(uyrs), '-', max(uyrs), '_Figurritning.xlsx'), format_headers = TRUE)
    }
  }
  if (homepage) {
    for (i in 1:nsyst){
      Index <- lapply(specieslist[[i]],
                      function(x){
                        #count <<- count+1
                        d1 <- data.frame(systord = obj[[x]]$Systord,
                                         art = ifelse(obj[[x]]$Art>99,
                                                      obj[[x]]$Art,
                                                      ifelse(obj[[x]]$Art>9,
                                                             paste0('0', obj[[x]]$Art),
                                                             paste0('00', obj[[x]]$Art))),
                                         artnamn = switch(specieslanguage,
                                                          SE = obj[[x]]$SEname,
                                                          EN = obj[[x]]$ENname,
                                                          WD = obj[[x]]$WDname),
                                         stringsAsFactors = F)
                        d2 <- round(obj[[x]]$Results[[i]]$Index[, c('time', 'imputed', 'se_imp')], 4)
                        names(d2) <- c('yr', 'index(imp)', 'se(imp)')
                        d2$konvergerat <- obj[[x]]$Results[[i]]$Index$converge
                        cbind(d1, d2)
                      })
      Index <- do.call(rbind, Index)
      uyrs <- unique(Index$yr[apply(Index[, c('index(imp)', 'se(imp)', 'konvergerat')], 1, function(a) !all(is.na(a)))])
      Index <- subset(Index, yr>=min(uyrs) & yr<=max(uyrs))
      Slopes <- lapply(specieslist[[i]],
                       function(x){
                         naix <- apply(obj[[x]]$Results[[i]]$Index[, c('imputed', 'se_imp', 'converge', 'converge_info')], 1, function(a) !all(is.na(a)))
                         d1 <- data.frame(systord = obj[[x]]$Systord,
                                          art = ifelse(obj[[x]]$Art>99,
                                                       obj[[x]]$Art,
                                                       ifelse(obj[[x]]$Art>9,
                                                              paste0('0', obj[[x]]$Art),
                                                              paste0('00', obj[[x]]$Art))),
                                          arthela = switch(specieslanguage,
                                                           SE = obj[[x]]$SEname,
                                                           EN = obj[[x]]$ENname,
                                                           WD = obj[[x]]$WDname),
                                          stringsAsFactors = F)
                         d2 <- obj[[x]]$Results[[i]]$Overall[, c('avcount', 'nroute', 'slope', 'se_slope', 'pctslope', 'sig', 'interpret')]
                         d2[, c('slope', 'se_slope')] <- round(d2[, c('slope', 'se_slope')], 4)
                         names(d2) <- c('medelind', 'antalrutt', 'slope', 'se', 'd%Year', 'sign', 'tolkning')
                         d2$Period <- paste(
                           range(obj[[x]]$Results[[i]]$Index$time[naix]),
                           collapse = ' - ')
                         d2$konvergerat <- all(obj[[x]]$Results[[i]]$Index$converge, na.rm=T)
                         cbind(d1, d2)
                       })
      Slopes <- do.call(rbind, Slopes)
      Tabell <- lapply(specieslist[[i]],
                       function(x){
                         d1 <- data.frame(Systord = obj[[x]]$Systord,
                                          Art = ifelse(obj[[x]]$Art>99,
                                                       obj[[x]]$Art,
                                                       ifelse(obj[[x]]$Art>9,
                                                              paste0('0', obj[[x]]$Art),
                                                              paste0('00', obj[[x]]$Art))),
                                          Arthela = switch(specieslanguage,
                                                           SE = obj[[x]]$SEname,
                                                           EN = obj[[x]]$ENname,
                                                           WD = obj[[x]]$WDname),
                                          Ind = obj[[x]]$Results[[i]]$Overall$avcount,
                                          Nrutt = obj[[x]]$Results[[i]]$Overall$nroute,
                                          stringsAsFactors = F)
                         d2 <- reshape(cbind(d1, obj[[x]]$Results[[i]]$Index[, c('time', 'imputed')]),
                                       timevar = 'time', idvar = c('Systord', 'Art', 'Arthela', 'Ind', 'Nrutt'), v.names = 'imputed',
                                       times = '', direction = 'wide')
                         d2 <- cbind(d2, obj[[x]]$Results[[i]]$Overall[, c('pctslope', 'sig')])
                         names(d2)[names(d2)%in%c('pctslope', 'sig')] <- c('%Y', 'S')
                         d2$Arthela2 <- obj[[x]]$SEname
                         #d2[, paste0('imputed.', obj[[x]]$Results[[i]]$Index$time[!naix])] <- NULL
                         d2[, grep('imputed.', names(d2))] <- round(d2[, grep('imputed.', names(d2))], 4)
                         #names(d2) <- gsub('imputed.', '', names(d2))
                         return(d2)
                       })
      Tabell <- do.call(rbind, Tabell)
      Tabell[,apply(Tabell, 2, function(a) all(is.na(a)))] <- NULL
      tabn <- max(gsub('imputed.', '', grep('imputed.', names(Tabell), value = T)))
      names(Tabell) <- gsub('imputed.', '', names(Tabell))
      fname <- paste0(path_project_extract,'Trim', colnames[i], '_Tabeller.xlsx')
      hptabs <- list(TABELL = Tabell, Index = Index, Slopes = Slopes)
      # write.xlsx2(Tabell, file = fname, sheetName=paste('TABELL' , tabn), row.names=FALSE, showNA=FALSE)
      # write.xlsx2(Index, file = fname, sheetName="Index", row.names=FALSE, append=TRUE, showNA=FALSE)
      # write.xlsx2(Slopes, file = fname, sheetName="Slopes", row.names=FALSE, append=TRUE, showNA=FALSE)
      write_xlsx(hptabs, path = fname, format_headers = TRUE)
    }
  }
}
