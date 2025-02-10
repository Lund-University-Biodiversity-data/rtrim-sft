
# function to query the mongo database "activity" collection for approved activities for the selected scheme
# input: projectActivityId
# output: string with all approved activityIds for input in subsequent queries
getApprovedActivities <- function (projectActivityId) {
  
  print(paste("start getApprovedActivities", Sys.time()))
  
  mongoConnection  <- mongo(collection = "activity",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())
  
  act <- mongoConnection$aggregate(sprintf('[
	    {"$match": {
	        "projectActivityId" : %s,
	        "verificationStatus" : "approved"
	    }}, 
	    {"$project": {
	        "activityId":1
	    }}
	]', paste0('"', projectActivityId, '"')),
  options = '{"allowDiskUse":true}',
  iterate = TRUE
  )
  
  nbElt <- 0
  aActivities <- array()
  
  while(!is.null(output <- act$one())){
    
    nbElt <- nbElt +1
    
    aActivities[nbElt] <- output$activityId
  }
  
  activities <-  paste('"', aActivities, '"', sep = "", collapse = ", ")
  
  print(paste("end getApprovedActivities", Sys.time()))
  
  return(activities)
}


# function to query mongo database collection "" for earliest and latest year of observation
# input: projectActivityId of selected monitoring scheme
getYearsMongo <- function (projectActivityId) {
  
  activities <- getApprovedActivities(projectActivityId)
  
  print(paste("start getYearsMongo", Sys.time()))
  
  mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())
  
  res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active",
	        "activityId": {"$in" : [%s]}
	    }},
	    {"$project": {
	        "data.surveyDate":1
	    }},
	    {"$unwind": "$data"},
      {"$project": {
	        "dates": "$data.surveyDate"
	    }},
	    {"$group": {
	    	"_id": null,
	    	"yearsUnique": {"$addToSet": "$dates"}
	    }},
	    {"$sort" : {
		    "yearsUnique" : 1
		}}
	    ]', activities),
    options = '{"allowDiskUse":true}',
    iterate = TRUE
  )
  
  listYrs <- c()
  
  while(!is.null(output <- res$one())){
    for (yr in output$yearsUnique) {
      
      listYrs <- c(listYrs, yr)
    }
  }
  
  listYrs <- unique(listYrs)
  Yrsdf <- data.frame(minyr = as.integer(substring(min(listYrs), 1, 4)), maxyr = as.integer(substring(max(listYrs), 1, 4)))
  
  print(paste("end getYearsMongo", Sys.time()))
  
  return(Yrsdf)
}


# function to query mongo database collection "output" for species' scientific names
# input: projectActivityId of selected monitoring scheme
getUniquesSpeciesFromScheme <- function (projectActivityId) {
  
  activities <- getApprovedActivities(projectActivityId)
  
  print(paste("start getUniquesSpeciesFromScheme", Sys.time()))
  
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active",
	        "activityId": {"$in" : [%s]}
	    }},
	    {"$project": {
	        "data.observations":1
	    }},
	    {"$unwind": "$data"},
	    {"$project": {
	        "obs": "$data.observations"
	    }},
	    {"$unwind": "$obs"},
	    {"$project": {
	        "ssn": "$obs.species.scientificName"
	    }},
	    {"$group": {
	    	"_id": null, 
	    	"speciesUnique": {"$addToSet": "$ssn"}
	    }},
	    {"$sort" : {
		    "speciesUnique" : 1
		}}
	    ]', activities),
		options = '{"allowDiskUse":true}',
		iterate = TRUE
	)

	listSsn <- c()
	
	while(!is.null(output <- res$one())){
		for (ssn in output$speciesUnique) {
			# http://www.endmemo.com/r/gsub.php

			# if digits in the end 
			# remove all the final parenthesis=> removes all the 
			ssn <- str_replace(ssn, " \\([:alnum:]+\\, [:digit:]+\\)", "")
			# remove the final word
			ssn <- str_replace(ssn, " [:alnum:]+\\, [:digit:]+", "")

			listSsn <- c(listSsn, ssn)
		}
	}

	listSsn <- unique(listSsn)
	vSn <- data.frame(name=listSsn)

	#while(!is.null(output <- res$one())){
	#	ssn <- output$speciesUnique

	#	r <- unlist(strsplit(toString(ssn), ","))
	#	vSn <- data.frame(name=r[c(TRUE)])
	#}

	print(paste("end getUniquesSpeciesFromScheme", Sys.time()))
	
	return(vSn)
}


getListsFromAla <- function (poolParams) {

	print("delete species_from_ala")
	delete <- paste0("DELETE FROM species_from_ala;")
	resD <- dbSendQuery(poolParams, delete)
	dbClearResult(resD)

	vArt <- vector()
	vName <- vector()
	vArthela <- vector()
	vEnglishname <- vector()
	vWorldname <- vector()
	vRank <- vector()
	vGuid <- vector()

	iS2 <- 0

	for (iList in 1:3) {

		if (iList==1) {
			animal_list <- list_id_mammal
			print("MAMMALS LIST")
		}
		else if (iList==2) {
			animal_list <- list_id_bird
			print("BIRDS LIST")
		}
		else if (iList==3) {
			animal_list <- list_id_amphibian
			print("AMPHIBIANS LIST")
		}
		# owls already included in birds
		#else if (iList==4) {
		#	animal_list <- list_id_owl
		#	print("OWLS LIST")
		#}

		dataurl <- getURL(paste0(species_list_url, animal_list, species_list_KVP_details), .encoding = 'UTF-8')

		print(paste("URL to scan:", paste0(species_list_url, animal_list, species_list_KVP_details)))

		data_json_species = fromJSON(dataurl)
		#data_json_species = fromJSON(file=paste0(species_list_url, bird_list_id))
		print(paste("Elements found :",length(data_json_species)))
		
		nbElt <-length(data_json_species)
		iS <-1

		while(iS <= nbElt){

			species<-data.frame()

		  	if (!is.null(data_json_species[[iS]]$lsid)) {
		  		lsid <- data_json_species[[iS]]$lsid
		  	}
		  	else {
		  		lsid <- 0
		  	}

			  if (!is.null(data_json_species[[iS]]$commonName)) {
			    worldname <- data_json_species[[iS]]$commonName
			  }
			  else {
			    worldname <- ""
			  }
			
		    nbKeys <- length(data_json_species[[iS]]$kvpValues)

		    iKey <- 1
		    continue <- TRUE
		    art <- ""
		    arthela <- ""
		    englishname <- ""
		    #worldname <- ""
		    rank <- 0

		    while(continue && iKey <= nbKeys){



		      if (data_json_species[[iS]]$kvpValues[[iKey]]$key == "art") {
		        art <- data_json_species[[iS]]$kvpValues[[iKey]]$value
		      }

		      if (data_json_species[[iS]]$kvpValues[[iKey]]$key == "arthela") {
		        arthela <- data_json_species[[iS]]$kvpValues[[iKey]]$value
		      }

		      if (data_json_species[[iS]]$kvpValues[[iKey]]$key == "englishname") {
		        englishname <-data_json_species[[iS]]$kvpValues[[iKey]]$value
		      }

		      #if (data_json_species[[iS]]$kvpValues[[iKey]]$key == "worldname") {
		      #  worldname <- data_json_species[[iS]]$kvpValues[[iKey]]$value
		      #}

		      if (data_json_species[[iS]]$kvpValues[[iKey]]$key == "rank") {
		        rank <- data_json_species[[iS]]$kvpValues[[iKey]]$value
		      }

		      iKey <- iKey+1
		    }

		    iS2 <- iS2 + 1
		    vArt[iS2] <- str_pad(art, 3, side="left", pad="0")
		    vName[iS2] <- data_json_species[[iS]]$name
		    vArthela[iS2] <- arthela
		    vEnglishname[iS2] <- englishname
		    vWorldname[iS2] <- worldname
		    vRank[iS2] <- rank
		    vGuid[iS2] <- lsid
		    #print(paste("art :", art))

		  	insert <- paste0("INSERT INTO species_from_ala (species_id, species_sw_name, species_latin_name, species_en_name, species_worldname, species_rank, species_guid)",
		  			"VALUES (",
		  			"'", str_pad(art, 3, side="left", pad="0"), "', ",
	  				"'", arthela, "',  ",
	  				"'", str_replace(data_json_species[[iS]]$name, "'", "''"), "',  ",
	  				"'", str_replace_all(englishname, "'", "''"), "',  ",
	  				"'", str_replace_all(worldname, "'", "''"), "',  ",
	  				"", rank, ",  ",
	  				"", lsid, "  ",
		  			")")
		  	resQ <- dbSendQuery(poolParams, insert)
		  	dbClearResult(resQ)

		  	iS <- iS + 1
		}

	}


	print(paste("Elements added to table :", length(vArt)))
	dfspecies <- data.frame(vArt, vArthela, vName, vEnglishname, vWorldname, vRank, vGuid)
	colnames(dfspecies) <- c("art", "arthela", "latin", "englishname", "worldname", "rank", "guid")
	#print(dfspecies)
	print(paste("Success!"))
	print(("Please reload the app to use the new species list!"))

	return(dfspecies)
}

# get species names for misc census
getSpeciesNames <- function (pool, species) {
  querysp <- sprintf("select species_id as art, species_sw_name as arthela, species_latin_name as latin, species_en_name as englishname, species_worldname as worldname, species_rank as rank, species_guid as guid
              from species_from_ala
              where to_number(species_id, '000') in (%s)
              order by art", species)
  spdat <<- dbGetQuery(pool, querysp)
  
  return(spdat)
}

getLimitNorthSouth <- function (pool) {
	query <- 'select id, species_id as "art", species_id_main as "speciesmain", species_sw_name as "arthela", species_latin_name as "latin", species_en_name as "englishname", latitude_limit as "Latitudgräns"
              from species_limit_north_south
              order by art'
	limitns <<- dbGetQuery(pool, query)

	return(limitns)
}

getStartYear <- function (pool) {
	query <- 'select id, species_id as "Art", species_sw_name as "Arthela", scheme as "Delprogram", year as "StartYear", comment as "Extra"
              from species_start_year
              order by scheme, species_id '
	startyr <<- dbGetQuery(pool, query)

	return(startyr)
}

getSpeciesData  <- function (pool) {
	querysp <- "select art, arthela, latin, englishname, worldname, rank
              from eurolist
              order by art"
	spdat <<- dbGetQuery(pool, querysp)

	return(spdat)	
}

getSpeciesDataParams<- function (pool) {

	querysp <- "select species_id as art, species_sw_name as arthela, species_latin_name as latin, species_en_name as englishname, species_worldname as worldname, species_rank as rank, species_guid as guid
              from species_from_ala
              order by art"
	spdat <<- dbGetQuery(pool, querysp)

	return(spdat)	
}

getSites <- function(pool) {
	queryrc <- "select karta as site, mitt_wgs84_lon as lon, mitt_wgs84_lat as lat
            from 
            standardrutter_koordinater
            union all
            select pk.site, tp.lon, tp.lat
            from
            (select persnr || '_' || rnr as site, kartatx
            from punktrutter) as pk,
            (select kartatx, wgs84_lon as lon, wgs84_lat as lat
            from
            koordinater_mittpunkt_topokartan) as tp
            where pk.kartatx=tp.kartatx"
	rcdat <<- dbGetQuery(pool, queryrc)

	return(rcdat)

}

getSitesMongo <- function (projectId) {

	# MISSING ALL THE PK SITES

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
	  fields = '{"adminProperties.internalSiteId":1, "extent.geometry.decimalLongitude":1, "extent.geometry.decimalLatitude":1}'
	)

	nbElt <- 0
	vSite <- vector()
	vLon <- vector()
	vLat <- vector()

	while(!is.null(x <- res$one())){
		nbElt <- nbElt +1
		
		vSite[nbElt] <- x$adminProperties$internalSiteId
		
		vLon[nbElt] <- x$extent$geometry$decimalLongitude
		
		vLat[nbElt] <- x$extent$geometry$decimalLatitude
	}

	result <- data.frame(vSite, vLon, vLat)
	colnames(result) <- c("site", "lon", "lat")

	return(result)
}


getMatchSpecies <- function (poolParams, speciesSel = "all") {

	specsel <- paste0('(', paste(speciesSel, collapse = ','), ')')

	if (speciesSel == "all") {
		querysp <- sprintf("select species_id as art, species_latin_name as latin
	          from species_from_ala
	          order by art")
	}
	else {
		querysp <- sprintf("select species_id as art, species_latin_name as latin
	          from species_from_ala
	          where to_number(species_id, '000') in %s
	          order by art", specsel)
	}

	species <- dbGetQuery(poolParams, querysp)

	nbSp <- nrow(species)
	iSp <- 1
	spMatch <- array()

	while (iSp<=nbSp) {
		spMatch[[species$latin[iSp]]] <- species$art[iSp]
		iSp <- iSp+1
	}

	return(spMatch)
}

getMatchSpeciesSN <- function (poolParams, speciesSel) {

	specsel <- paste0('(', paste(speciesSel, collapse = ','), ')')

	querysp <- sprintf("select species_latin_name as sn, species_latin_name as latin
          from species_from_ala
          where to_number(species_id, '000') in %s
          order by sn", specsel)

	species <- dbGetQuery(poolParams, querysp)

	speciesFinal <- paste('"', species$sn, '"', sep = "", collapse = ',')

	return(speciesFinal)
}


getListBirdsUrl <- function (listId, speciesSel) {


	url <- paste0(species_list_url, listId)
	btc <- jsonlite::fromJSON(url)

	speciesNames <- array()
	speciesNames <- btc$name

	speciesSN <-  paste('"', speciesNames, '"', sep = "", collapse = ", ")

	return(speciesSN)

}


getMatchSitesMongo <- function (projectId) {

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	#if (projectId == project_id_std) {
	#	res <- mongoConnection$iterate(
	#	  query = sprintf('{"status":"active", "karta":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
	#	  fields = '{"karta":1, "siteId":1}'
	#	)
	#}
	#else if (projectId == project_id_punkt) {
	#	res <- mongoConnection$iterate(
	#	  query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
	#	  fields = '{"adminProperties.internalSiteId":1, "siteId":1}'
	#	)
	#}

	res <- mongoConnection$iterate(
		  query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
		  fields = '{"adminProperties.internalSiteId":1, "siteId":1}'
		)


	nbElt <- 0
	vSite <- vector()
	vLat <- vector()

	sites <- array()

	while(!is.null(x <- res$one())){

		#if (projectId == project_id_std) {
		#	sites[[toString(x$siteId)]] <- x$karta
		#}
		#else if (projectId == project_id_punkt) {
		#	sites[[toString(x$siteId)]] <- x$adminProperties$internalSiteId
		#}

		sites[[toString(x$siteId)]] <- x$adminProperties$internalSiteId

	}

	return(sites)
}

getBiotopSites <- function(pool) {
	queryregSt <- "select karta, namn, lsk, lan, fjall104, fjall142
             from
             standardrutter_biotoper
             order by karta"
	regStdat <<- dbGetQuery(pool, queryregSt)

	colnames(regStdat) <- c('internalSiteId', 'namn', 'lsk', 'lan', 'fjall104', 'fjall142')

	return(regStdat)

}



getBiotopSitesMongo <- function (projectId) {

	# MISSING ALL THE PK SITES

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
	  fields = '{"adminProperties.internalSiteId":1, "commonName":1, "name":1, "adminProperties.lan":1, "adminProperties.lsk":1, "adminProperties.fjall104":1, "adminProperties.fjall142":1}'
	)

	nbElt <- 0
	vKarta <- vector()
	vName <- vector()
	vCommonName <- vector()
	vLan <- vector()
	vLsk <- vector()
	vF104 <- vector()
	vF142 <- vector()

	while(!is.null(x <- res$one())){
		nbElt <- nbElt +1

		vKarta[nbElt] <- x$adminProperties$internalSiteId
		vName[nbElt] <- x$name
		if (!is.null(x$commonName)) vCommonName[nbElt] <- x$commonName
		else vCommonName[nbElt] <- "-"
		if (!is.null(x$adminProperties$lan)) vLan[nbElt] <- x$adminProperties$lan
		else vLan[nbElt] <- ""
		if (!is.null(x$adminProperties$lsk)) vLsk[nbElt] <- x$adminProperties$lsk
		else vLsk[nbElt] <- ""
		if (!is.null(x$adminProperties$fjall104)) vF104[nbElt] <- x$adminProperties$fjall104
		else vF104[nbElt] <- FALSE
		if (!is.null(x$adminProperties$fjall142)) vF142[nbElt] <- x$adminProperties$fjall142
		else vF142[nbElt] <- FALSE
	}

#	result <- array(c(vKarta, vCommonName, vLsk, vLan, vF104, vF142), dim=c(nbElt, 6, 1), dimnames=list(c(),c("karta", "namn", "lsk", "lan", "fjall104", "fjall142")))
	result <- data.frame(vKarta, vCommonName, vLsk, vLan, vF104, vF142)
	colnames(result) <- c("internalSiteId", "namn", "lsk", "lan", "fjall104", "fjall142")

	return(result)

}


getIWCData <- function (pool) {
	queryregIWC <- "select site, lokalnamn, ki, ev, lan
             from
             iwc_koordinater
             order by site"
	regIWCdat <<- dbGetQuery(pool, queryregIWC)

	return(regIWCdat)
}

getIWCDataMongo <- function (projectId) {

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
	  fields = '{"adminProperties.internalSiteId":1, "commonName":1, "name":1, "adminProperties.ki":1, "adminProperties.ev":1, "adminProperties.lan":1}'
	)

	nbElt <- 0
	vSite <- vector()
	vName <- vector()
	vCommonName <- vector()
	vKi <- vector()
	vEv <- vector()
	vLan <- vector()

	while(!is.null(x <- res$one())){
		nbElt <- nbElt +1

		vSite[nbElt] <- x$adminProperties$internalSiteId
		vName[nbElt] <- x$name
		if (!is.null(x$commonName)) vCommonName[nbElt] <- x$commonName
		else vCommonName[nbElt] <- "-"
		if (!is.null(x$adminProperties$ki)) vKi[nbElt] <- x$adminProperties$ki
		else vKi[nbElt] <- ""
		if (!is.null(x$adminProperties$ev)) vEv[nbElt] <- x$adminProperties$ev
		else vEv[nbElt] <- ""
		if (!is.null(x$adminProperties$lan)) vLan[nbElt] <- x$adminProperties$lan
		else vLan[nbElt] <- ""
	}

	result <- data.frame(vSite, vName, vKi, vEv, vLan)
	colnames(result) <- c("site", "lokalnamn", "ki", "ev", "lan")

	return(result)
}

# get site data from psql database for punktrutter schemes
# for use in filtering the data by county (län)
getPKTData <- function (pool) {
  queryregPKT <- "select persnr || '_' || rnr as site, ruttnamn as lokalnamn, lan
                  from
                  punktrutter
                  order by site"
  regPKTdat <<- dbGetQuery(pool, queryregPKT)
  
  # replace the county names with the respective codes
  # where no county is assigned to a site, leave empty
  for(iLan in 1:nrow(regPKTdat)) {
    if (nchar(regPKTdat$lan[iLan]) > 2) {
      regPKTdat$lan[iLan] <- counties$code[counties$name == regPKTdat$lan[iLan]]
    }
  }
  
  return(regPKTdat)
}

# get site data from psql database for kustfagel
# for use in filtering the data by county (län)
getKustData <- function (pool) {
  queryregKust <- "select distinct ruta as site, ruta as lokalnamn, left(ruta, -4) as lan
                  from
                  totalkustfagel200
                  order by site"
  regKustdat <<- dbGetQuery(pool, queryregKust)
  
  return(regKustdat)
}

# get site data from mongoDB for schemes Sommarpunktrutter and Vinterpunktrutter and Kustfagel
# based on their project ID in the database
# output: data frame used for filtering data by county (län)
getPKTDataMongo <- function (projectId) {
  
  mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())
  
  res <- mongoConnection$iterate(
    query = sprintf('{"status":"active", "adminProperties.internalSiteId":{"$exists":1}, "projects":%s}', paste0('"', projectId, '"')), 
    fields = '{"adminProperties.internalSiteId":1, "name":1, "adminProperties.lan":1}'
  )
  
  nbElt <- 0
  vSite <- vector()
  vName <- vector()
  vLan <- vector()
  
  while(!is.null(x <- res$one())){
    nbElt <- nbElt +1
    
    vSite[nbElt] <- x$adminProperties$internalSiteId
    vName[nbElt] <- x$name
    if (!is.null(x$adminProperties$lan)) vLan[nbElt] <- x$adminProperties$lan
    else vLan[nbElt] <- ""
  }
  
  result <- data.frame(vSite, vName, vLan)
  colnames(result) <- c("site", "lokalnamn", "lan")
  
  # some sites have the county code listed, most the county name
  # replace the codes with the respective county name
  # where no county is assigned to a site, leave empty
  for(iLan in 1:nrow(result)) {
    if (nchar(result$lan[iLan]) > 2) {
      result$lan[iLan] <- counties$code[counties$name == result$lan[iLan]]
    }
  }
  
  return(result)
}


getTabMinus1Mongo <- function (projectActivityId, species, speciesSN, sites, years, linepoint, selectedPeriod, activities) {

	print(paste("start getTabMinus1Mongo ", Sys.time()))
  
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	fieldCount <- "individualCount"
	checkPeriod <- ""
	condTotSupZero <- ""
	if (projectActivityId == project_activity_id_std) {
		fieldCount <- paste0(linepoint, "Count")

		condTotSupZero <- sprintf('%s : {"$gt":0},', paste0('"', fieldCount, '"'))
	}
	else if (projectActivityId == project_activity_id_winter) {
		checkPeriod <- paste0('"data.period" : {"$in" : [', selectedPeriod,']},')
	}
	else if (projectActivityId == project_activity_id_iwc) {
		checkPeriod <- paste0('"data.period" : ', selectedPeriod,',')
	}

	res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
			%s
	        "status" : "active",
	        "activityId": {"$in" : [%s]}
	    }},
	    {"$project": {
	        "data.location":1,
	        "data.observations":1
	    }},
	    {"$unwind": "$data"},
	    {"$project": {
	        "site": "$data.location",
	        "obs": "$data.observations"
	    }},
	    {"$unwind": "$obs"},
	    {"$project": {
	        "site": 1,
	        %s: %s,
	        "ssn": "$obs.species.scientificName"
	    }},
	    {"$match": {
	        %s
	        "ssn" : {"$in" : [%s]}
	    }},
	    {"$group": {
	        "_id" : { "site" : "$site", "ssn" : "$ssn" },
	        "site" : { "$first" : "$site" },
	        "ssn" : { "$first" : "$ssn" }
	    }}
	]', checkPeriod, activities, paste0('"', fieldCount, '"'), paste0('"',"$obs.", fieldCount, '"'), condTotSupZero, speciesSN),
	options = '{"allowDiskUse":true}',
	iterate = TRUE
	)

	nbElt <- 0

	vKartaMatch <- vector()
	vArtMatch <- vector()
	vCount <- vector()

	while(!is.null(output <- res$one())){

		nbElt <- nbElt +1

		vKartaMatch[nbElt] <- sites[[toString(output$site)]]
		vArtMatch[nbElt] <- species[[toString(output$ssn)]]
		vCount[nbElt] <- -1

	}

	result <- data.frame(vKartaMatch, vArtMatch, vCount)


	mergeR <- merge(result, seq(years[1],years[2]))
	colnames(mergeR) <- c("site", "species", "count", "time")

	print(paste("fin getTabMinus1Mongo ", Sys.time()))

	return(mergeR)


}





getTabZeroMongo <- function (projectActivityId, species, speciesSN, sites, years, linepoint, selectedPeriod, activities) {

	print(paste("start getTabZeroMongo ", Sys.time()))

	fieldCount <- "individualCount"
	checkPeriod <- ""
	condTotSupZero <- ""
	or <- createOrEventDateCriteria(years)

	if (projectActivityId == project_activity_id_std) {
		fieldCount <- paste0(linepoint, "Count")
		
		condTotSupZero <- sprintf('%s : {"$gt":0},', paste0('"', fieldCount, '"'))
	}
	else if (projectActivityId == project_activity_id_winter) {
		checkPeriod <- paste0('"data.period" : {"$in" : [', selectedPeriod,']},')
		or <- createOrEventDateCriteria(years, vinter=TRUE)
		listMonthVinterPreviousYr <- c('01', '02', '03', '04')
	}
	else if (projectActivityId == project_activity_id_iwc) {
		checkPeriod <- paste0('"data.period" : ', selectedPeriod,',')
		# important double quotes !!
		if (selectedPeriod == '"Januari"') {
			or <- createOrEventDateCriteria(years, iwcjanuari=TRUE)
		}
		else if (selectedPeriod == '"September"') {
			or <- createOrEventDateCriteria(years, iwcseptember=TRUE)
		}
	}

	
  # query for years in which each site was surveyed
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res1 <- mongoConnection$aggregate(sprintf('[
		  {"$match": {
			    %s
	        "status" : "active",
	        "activityId": {"$in" : [%s]},
	        %s
	    }},
	    {"$project": {
	        "data.location":1,
	        "data.surveyDate":1
	    }}
	]', checkPeriod, activities, or),
	options = '{"allowDiskUse":true}',
	iterate = TRUE
	)

	nbElt <- 0
	#vKarta <- vector()
	vKartaMatch <- vector()
	vYear <- vector()


	while(!is.null(output <- res1$one())){

		nbElt <- nbElt +1

	  #vKarta[nbElt] <- toString(output$data$location)
	  vKartaMatch[nbElt] <- sites[[toString(output$data$location)]]

		# for the winter protocols, some months have to be lined to the year before
		if (projectActivityId == project_activity_id_winter) {
			month <- substr(output$data$surveyDate, 6 , 7)
			if (month %in% listMonthVinterPreviousYr) {
				yr <- substr(output$data$surveyDate, 1 , 4)
				vYear[nbElt] <- paste0(strtoi(yr)-1)
			}

			else {
				vYear[nbElt] <- substr(output$data$surveyDate, 1 , 4)
			}
		}
		# for iwcjanuari, december belongs to the year after
		else if (projectActivityId == project_activity_id_iwc) {
			month <- substr(output$data$surveyDate, 6 , 7)
			if (month == "12") {
				yr <- substr(output$data$surveyDate, 1 , 4)
				vYear[nbElt] <- paste0(strtoi(yr)+1)
			}

			else {
				vYear[nbElt] <- substr(output$data$surveyDate, 1 , 4)
			}
		}
		else {
			vYear[nbElt] <- substr(output$data$surveyDate, 1 , 4)
		}

	}


	result1 <- data.frame(vKartaMatch,  vYear)
	colnames(result1) <- c("site", "time")
	result1 <- unique(result1)

  # query for species observed at the sites
	res2 <- mongoConnection$aggregate(sprintf('[
  		{"$match": {
  			  %s
	        "status" : "active",
	        "activityId": {"$in" : [%s]}
	    }},
	    {"$project": {
	        "data.location":1,
	        "data.observations":1
	    }},
	    {"$unwind": "$data"},
	    {"$project": {
	        "site": "$data.location",
	        "obs": "$data.observations"
	    }},
	    {"$unwind": "$obs"},
	    {"$project": {
	        "site": 1,
	        %s: %s,
	        "ssn": "$obs.species.scientificName"
	    }},
	    {"$match": {
	        %s
	        "ssn" : {"$in" : [%s]}
	    }},
	    {"$group": {
	        "_id" : { "site" : "$site", "ssn" : "$ssn" },
	        "site" : { "$first" : "$site" },
	        "ssn" : { "$first" : "$ssn" }
	    }}
	]', checkPeriod, activities, paste0('"', fieldCount, '"'), paste0('"',"$obs.", fieldCount, '"'), condTotSupZero, speciesSN),
	options = '{"allowDiskUse":true}',
	iterate = TRUE
	)

	nbElt <- 0
	vKartaMatch <- vector()
	vArtMatch <- vector()

	while(!is.null(output <- res2$one())){

		nbElt <- nbElt +1

		vKartaMatch[nbElt] <- sites[[toString(output$site)]]
		vArtMatch[nbElt] <- species[[toString(output$ssn)]]

	}

	result2 <- data.frame(vKartaMatch, vArtMatch)
	colnames(result2) <- c("site", "species")

	# remove doublons
	result2 <- unique(result2)

	resultMerge <- merge(x= result1, y=result2)

	# add a 0 in last column
	resultFinal <- data.frame(resultMerge, 0)
	colnames(resultFinal) <- c("site", "time", "species", "count")

	print(paste("end getTabZeroMongo ", Sys.time()))

	return(resultFinal)


}


createOrEventDateCriteria <- function (years, vinter= FALSE, iwcjanuari=FALSE, iwcseptember=FALSE) {

	# for the winter routes, the winter starts on september of year Y, until april of year Y+1 !
	if (vinter) {
		or <- '"$or" : [ '
		for (iYear in years[1]:tail(years, n=1)){ 
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-09", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-10", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-11", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-12", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',(iYear+1),'-01", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',(iYear+1),'-02", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',(iYear+1),'-03", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',(iYear+1),'-04", "$options" : "i" } }, ', sep="")
		}

		# repeat the same at the end of the OR to deal with the comma
		or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-09", "$options" : "i" } }] ', sep="")


	}
	# for the iwcjanuari routes, januari could be december before, until mars  !
	else if (iwcjanuari) {
		or <- '"$or" : [ '
		for (iYear in years[1]:tail(years, n=1)){ 
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',(iYear-1),'-12", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-01", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-02", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-03", "$options" : "i" } }, ', sep="")
		}

		# repeat the same at the end of the OR to deal with the comma
		or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-01", "$options" : "i" } }] ', sep="")

	}
	# for the iwcseptember routes, september could be juni to oktober  !
	else if (iwcseptember) {
		or <- '"$or" : [ '
		for (iYear in years[1]:tail(years, n=1)){ 
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-06", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-09", "$options" : "i" } }, ', sep="")
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-10", "$options" : "i" } }, ', sep="")
		}

		# repeat the same at the end of the OR to deal with the comma
		or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'-09", "$options" : "i" } }] ', sep="")

	}
	else {
		or <- '"$or" : [ '
		for (iYear in years[1]:tail(years, n=1)){ 
			or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'", "$options" : "i" } }, ', sep="")
		}

		# repeat the same at the end of the OR to deal with the comma
	    or <- paste (or, '{"data.surveyDate": { "$regex" : "', iYear, '", "$options" : "i" } }]', sep="")
	}

    return(or)
}

getTabCountMongo <- function (projectActivityId, species, speciesSN, sites, years, linepoint, selectedPeriod, activities) {

	print(paste("start getTabCountMongo ", Sys.time()))
  
  # query for count data i.e. number of individuals observed
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	fieldCount <- "individualCount"
	checkPeriod <- ""
	condTotSupZero <- ""
	or <- createOrEventDateCriteria(years)

	if (projectActivityId == project_activity_id_std) {
		fieldCount <- paste0(linepoint, "Count")

		condTotSupZero <- sprintf('%s : {"$gt":0},', paste0('"', fieldCount, '"'))
	}
	else if (projectActivityId == project_activity_id_winter) {
		checkPeriod <- paste0('"data.period" : {"$in" : [', selectedPeriod,']},')
		or <- createOrEventDateCriteria(years, vinter=TRUE)
		listMonthVinterPreviousYr <- c('01', '02', '03', '04')
	}
	else if (projectActivityId == project_activity_id_iwc) {
		checkPeriod <- paste0('"data.period" : ', selectedPeriod,',')
		# important double quotes !!
		if (selectedPeriod == '"Januari"') {
			or <- createOrEventDateCriteria(years, iwcjanuari=TRUE)
		}
		else if (selectedPeriod == '"September"') {
			or <- createOrEventDateCriteria(years, iwcseptember=TRUE)
		}

	}


	res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
			%s
	        "status" : "active",
	        "activityId": {"$in" : [%s]},
	        %s 
	    }},
	    {"$project": {
	        "data.location":1,
	        "data.surveyDate":1,
		    "data.observations":1
	    }},
	    {"$unwind": "$data"},
	    {"$project": {
	        "site": "$data.location",
	        "obs": "$data.observations",
	        "surveydate": "$data.surveyDate"
	    }},
	    {"$unwind": "$obs"},
	    {"$project": {
	        "site": 1,
	        %s: %s,
	        "ssn": "$obs.species.scientificName",
	        "surveydate": 1
	    }},
	    {"$match": {
	        %s
	        "ssn" : {"$in" : [%s]}
	    }}
	]', checkPeriod, activities, or, paste0('"', fieldCount, '"'), paste0('"',"$obs.", fieldCount, '"'), condTotSupZero, speciesSN),
	options = '{"allowDiskUse":true}',
	iterate = TRUE
	)


# { "data.location":"922298f6-eab3-4303-993e-d27b8b91a5a0" }
        
	nbElt <- 0
	#vKarta <- vector()
	vKartaMatch <- vector()
	#vArt <- vector()
	vArtMatch <- vector()
	vYear <- vector()
	vCount <- vector()

	while(!is.null(output <- res$one())){

		nbElt <- nbElt +1

		#vKarta[nbElt] <- toString(output$data$location)
		vKartaMatch[nbElt] <- sites[[toString(output$site)]]
		#vArt[nbElt] <- obs$species$scientificName
		vArtMatch[nbElt] <- species[[toString(output$ssn)]]

		#vYear[nbElt] <- substr(output$surveydate, 1 , 4)
		if (projectActivityId == project_activity_id_winter) {
			month <- substr(output$surveydate, 6 , 7)
			if (month %in% listMonthVinterPreviousYr) {
				yr <- substr(output$surveydate, 1 , 4)
				vYear[nbElt] <- paste0(strtoi(yr)-1)
			}

			else {
				vYear[nbElt] <- substr(output$surveydate, 1 , 4)
			}
		}
		
		# for iwcjanuari, december belongs to the year after
		else if (projectActivityId == project_activity_id_iwc) {
		  month <- substr(output$surveydate, 6 , 7)
		  if (month == "12") {
		    yr <- substr(output$surveydate, 1 , 4)
		    vYear[nbElt] <- paste0(strtoi(yr)+1)
		  }

		  else {
		    vYear[nbElt] <- substr(output$surveydate, 1 , 4)
		  }
		}
		
		else {
			vYear[nbElt] <- substr(output$surveydate, 1 , 4)

		}


		if (projectActivityId == project_activity_id_std) {

			if (linepoint == "point") {
				vCount[nbElt] <- as.integer(output$pointCount)
			}
			else {
				vCount[nbElt] <- as.integer(output$lineCount)
			}

		}
		else {
			vCount[nbElt] <- as.integer(output$individualCount)
		}


	}

	result <- data.frame(vKartaMatch, vArtMatch, vYear, vCount)
	colnames(result) <- c("site", "species", "time", "count")

	# error message in case no data was found
	shiny::validate(
	  need(nrow(result) > 0, "No data found for the specified request.")
	)

	# aggregation was moved out of this function to app.R
	
	print(paste("end getTabCountMongo ", Sys.time()))

	return(result)


}

applySpecificCorrections <- function (fullData, correctionsArt) {


	print(paste("start applySpecificCorrections ", Sys.time()))

	fullDataFinal <- fullData

	

	if (correctionsArt$s043 == TRUE) {
		# VinPKT 
		# Running RÖGLA (Red Kite, 043) in winter
		# by changing the very highest values per route (50+) 
		# to  a lower number (now 40, well 30)
		print(cat("fixing", length(fullDataFinal["count"][(fullDataFinal$count>30 & fullDataFinal$species=="043"),]), "row(s) for correctionsArt043"))
		fullDataFinal["count"][(fullDataFinal$count>30 & fullDataFinal$species=="043"),] <- 30
	}

	if (correctionsArt$s242 == TRUE) {

		year242 = c(1984, 1986, 1994)

		# VinPKT 
		# Running TABIT (Pine Grosbeak, 242) in winter
		# by actively removing three changepoints (1984, 1986 and 1994). The years 1984 and 1994 would actually be removed anyway
		# by autodelete (0 observations in these years), but 1986 has 1 bird seen, so 1986 must be removed in this way.
		print(cat("fixing", length(fullDataFinal["count"][(fullDataFinal$count>0 & fullDataFinal$species=="242" & fullDataFinal$time %in% year242),]), "row(s) for correctionsArt242"))
		fullDataFinal["count"][(fullDataFinal$count>0 & fullDataFinal$species=="242" & fullDataFinal$time %in% year242),] <- 0

	}

	if (correctionsArt$s248 == TRUE) {

		# VinPKT 
		# Running BEFIN (Brambling, 248) in winter
		# by changing the very highest values per route (50000+) 
		# to  a lower number (now 50000)
		print(cat("fixing", length(fullDataFinal["count"][(fullDataFinal$count>50000 & fullDataFinal$species=="248"),]), "row(s) for correctionsArt248"))
		fullDataFinal["count"][(fullDataFinal$count>50000 & fullDataFinal$species=="248"),] <- 50000

	}


	print(paste("end applySpecificCorrections ", Sys.time()))
	
	return(fullDataFinal)
}


mergeTabs <- function (minus1, zeros, stdcount) {


	merge1 <- merge(x= minus1, y=zeros, by=c("site", "species","time"), all.x=TRUE)

	#write.csv(merge1, file = 'test_merge1.csv', row.names = FALSE)

	merge2 <- merge(x= merge1, y=stdcount, by=c("site", "species","time"), all.x=TRUE)

	#write.csv(merge2, file = 'test_merge2.csv', row.names = FALSE)

	final <- data.frame (merge2$site, merge2$species, merge2$time, pmax(merge2$count.x, merge2$count.y, merge2$count, na.rm= TRUE))
	colnames(final) <- c("site", "species", "time", "count")

	return(final)
}

getCountData <- function (projectActivityId, speciesMatch, speciesMatchSN, sitesMatchMongo, yearsSel, linepoint, selectedPeriod, correctionsArt) {
  
  withProgress(message = 'Running query', value = 0, {
  
    start1 <- Sys.time()
    # query of "activity" collection
    activities <- getApprovedActivities(projectActivityId)
    # queries of "output" collection
  	minus1 <- getTabMinus1Mongo(projectActivityId, species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel, linepoint = linepoint, selectedPeriod = selectedPeriod, activities = activities)
    #write.csv(minus1, file = 'minus.csv', row.names = FALSE)
  	incProgress(1/4, detail = paste('step 1 #minus1'))
  	
  	start2 <- Sys.time()
  	zeros <- getTabZeroMongo(projectActivityId, species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel, linepoint = linepoint, selectedPeriod = selectedPeriod, activities = activities)
    #write.csv(zeros, file = 'zeros.csv', row.names = FALSE)
  	incProgress(1/4, detail = paste('step 2 #zeros'))
  
  	start3 <- Sys.time()
  	stdcount <- getTabCountMongo(projectActivityId, species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel, linepoint = linepoint, selectedPeriod = selectedPeriod, activities = activities)
  	incProgress(1/4, detail = paste('step 3 #count'))
  	
  	start4 <- Sys.time()
  	stdcount <- applySpecificCorrections(stdcount, correctionsArt)
  
  	print(paste("before final merge :", Sys.time()))
  
  	dataMerge <- mergeTabs(minus1 = minus1, zeros = zeros, stdcount = stdcount)
  	incProgress(1/4, detail = paste('step 4 #merge'))
    end4 <- Sys.time()
    
  	#write.csv(dataMerge, file = 'test_result.csv', row.names = FALSE)
  	print(paste("before return :", Sys.time()))
  	
  	times <<- data.frame(step = c('step 1 #minus1', 'step 2 #zeros', 'step 3 #count', 'step 4 #merge'), 
  	                     duration = c(diff(c(start1, start2)), diff(c(start2, start3)), diff(c(start3, start4)), diff(c(start4, end4))))
  
  	})

	return(dataMerge)
}


exportSaveData <- function (dataMerge, savedat, filename, tab = "totalstandard") {

	## Export data
	if (1%in%savedat){
		dataMerge <<- dataMerge
	}
	if (2%in%savedat){
		write.csv(dataMerge, file = paste0(path_project_extract, 'mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.csv'),	          row.names = FALSE)
	}
	if (3%in%savedat){
		write.csv2(dataMerge, file = paste0(path_project_extract, 'mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.csv'),	          row.names = FALSE)
	}
	if (4%in%savedat){
		save(dataMerge, file = paste0(path_project_extract, 'mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.rdata'))
	}

	print(paste("after export :", Sys.time()))

	return(dataMerge)
}