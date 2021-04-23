
getListsFromAla <- function (poolParams) {

	print("delete species_from_ala")
	delete <- paste0("DELETE FROM species_from_ala;")
	resD <- dbSendQuery(poolParams, delete)
	dbClearResult(resD)

	print(paste("URL to scan:",paste0(species_list_url, bird_list_id)))

	dataurl <- getURL(paste0(species_list_url, bird_list_id))
	data_json_species = fromJSON(dataurl)
	#data_json_species = fromJSON(file=paste0(species_list_url, bird_list_id))
	print(paste("Elements found :",length(data_json_species)))
	nbElt <-length(data_json_species)
	iS <-1

	vArt <- vector()
	vName <- vector()
	vArthela <- vector()
	vEnglishname <- vector()
	vWorldname <- vector()
	vRank <- vector()
	vGuid <- vector()

	iS2 <- 0

	while(iS < nbElt){

	  species<-data.frame()
	  #print(data_json_species[[iS]]$lsid)
	  if (!is.null(data_json_species[[iS]]$lsid)) {

	    #print(paste0(iS, "url : ",species_list_details_url, data_json_species[[iS]]$lsid))

	    data_json_species_details = fromJSON(file=paste0(species_list_details_url, data_json_species[[iS]]$lsid))

	    nbKeys <- length(data_json_species_details[[1]]$kvpValues)

	    iKey <- 1
	    continue <- TRUE
	    art <- ""
	    arthela <- ""
	    englishname <- ""
	    worldname <- ""
	    rank <- NULL

	    while(continue && iKey <= nbKeys){

	      if (data_json_species_details[[1]]$kvpValues[[iKey]]$key == "art") {
	        art <- data_json_species_details[[1]]$kvpValues[[iKey]]$value
	      }

	      if (data_json_species_details[[1]]$kvpValues[[iKey]]$key == "arthela") {
	        arthela <- data_json_species_details[[1]]$kvpValues[[iKey]]$value
	      }

	      if (data_json_species_details[[1]]$kvpValues[[iKey]]$key == "englishname") {
	        englishname <-data_json_species_details[[1]]$kvpValues[[iKey]]$value
	      }

	      if (data_json_species_details[[1]]$kvpValues[[iKey]]$key == "worldname") {
	        worldname <- data_json_species_details[[1]]$kvpValues[[iKey]]$value
	      }

	      if (data_json_species_details[[1]]$kvpValues[[iKey]]$key == "rank") {
	        rank <- data_json_species_details[[1]]$kvpValues[[iKey]]$value
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
	    vGuid[iS2] <- data_json_species[[iS]]$lsid
	    #print(paste("art :", art))

#('art', 'latin', 'arthela', 'englishname', 'worldname', 'rank', 'guid')
	  	insert <- paste0("INSERT INTO species_from_ala (species_id, species_sw_name, species_latin_name, species_en_name, species_worldname, species_rank, species_guid)",
	  			"VALUES (",
	  			"'", str_pad(art, 3, side="left", pad="0"), "', ",
  				"'", arthela, "',  ",
  				"'", data_json_species[[iS]]$name, "',  ",
  				"'", str_replace(englishname, "'", "''"), "',  ",
  				"'", str_replace(worldname, "'", "''"), "',  ",
  				"", rank, ",  ",
  				"", data_json_species[[iS]]$lsid, "  ",
	  			")")
	  	resQ <- dbSendQuery(poolParams, insert)
	  	dbClearResult(resQ)
	  }
	  iS <- iS + 1
	}

	print(paste("Elements added to table :", length(vArt)))
	dfspecies <- data.frame(vArt, vArthela, vName, vEnglishname, vWorldname, vRank, vGuid)
	colnames(dfspecies) <- c("art", "arthela", "latin", "englishname", "worldname", "rank", "guid")
	#print(dfspecies)
	print(paste("Success!"))
	print(("Please reload the app to use the new species list!"))

	return(dfspecies)
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
	queryrc <- "select karta as site, mitt_wgs84_lat as lat
            from 
            standardrutter_koordinater
            union all
            select pk.site, tp.lat
            from
            (select persnr || '_' || rnr as site, kartatx
            from punktrutter) as pk,
            (select kartatx, wgs84_lat as lat
            from
            koordinater_mittpunkt_topokartan) as tp
            where pk.kartatx=tp.kartatx"
	rcdat <<- dbGetQuery(pool, queryrc)

	return(rcdat)

}

getSitesMongo <- function () {

	# MISSING ALL THE PK SITES

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = '{"karta":{"$exists":1}, "projects":"89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c"}', 
	  fields = '{"karta":1, "extent.geometry.decimalLatitude":1}'
	)

	nbElt <- 0
	vSite <- vector()
	vLat <- vector()

	while(!is.null(x <- res$one())){
		nbElt <- nbElt +1

		vSite[nbElt] <- x$karta
		vLat[nbElt] <- x$extent$geometry$decimalLatitude
	}


	#result <- array(c(vSite, vLat), dim=c(nbElt, 2, 1), dimnames=list(c(),c("site", "lat")))
	result <- data.frame(vSite, vLat)
	colnames(result) <- c("site", "lat")

	return(result)

	# db.site.find({karta:{$exists:1}, projects:"89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c"}, {karta:1, "extent.geometry.decimalLongitude":1}).
}

getMatchSpecies <- function (poolParams, speciesSel) {

	specsel <- paste0('(', paste(speciesSel, collapse = ','), ')')

	querysp <- sprintf("select species_id as art, species_latin_name as latin
          from species_from_ala
          where to_number(species_id, '000') in %s
          order by art", specsel)
	
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


	url <- paste("https://lists.bioatlas.se/ws/speciesListItems/", listId, sep="")
	btc <- jsonlite::fromJSON(url)

	speciesNames <- array()
	speciesNames <- btc$name

	speciesSN <-  paste('"', speciesNames, '"', sep = "", collapse = ", ")

	return(speciesSN)

}


getMatchSitesMongo <- function () {

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = '{"karta":{"$exists":1}, "projects":"89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c"}', 
	  fields = '{"karta":1, "siteId":1}'
	)

	nbElt <- 0
	vSite <- vector()
	vLat <- vector()

	sites <- array()

	while(!is.null(x <- res$one())){
		sites[[toString(x$siteId)]] <- x$karta
	}

	return(sites)
}

getBiotopSites <- function(pool) {
	queryregSt <- "select karta, namn, lsk, lan, fjall104, fjall142
             from
             standardrutter_biotoper
             order by karta"
	regStdat <<- dbGetQuery(pool, queryregSt)

	return(regStdat)

}



getBiotopSitesMongo <- function () {

	# MISSING ALL THE PK SITES

	mongoConnection  <- mongo(collection = "site",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$iterate(
	  query = '{"karta":{"$exists":1}, "projects":"89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c"}', 
	  fields = '{"karta":1, "commonName":1, "name":1, "LAN":1, "LSK":1, "Fjall104":1, "Fjall142":1}'
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

		vKarta[nbElt] <- x$karta
		vName[nbElt] <- x$name
		vCommonName[nbElt] <- x$commonName
		if (exists("x$LAN")) vLan[nbElt] <- x$LAN
		else vLan[nbElt] <- "a"
		if (exists("x$LSK")) vLsk[nbElt] <- x$LSK
		else vLsk[nbElt] <- "b"
		if (exists("x$Fjall104")) vF104[nbElt] <- x$Fjall104
		else vF104[nbElt] <- FALSE
		if (exists("x$Fjall104")) vF142[nbElt] <- x$Fjall142
		else vF142[nbElt] <- FALSE
	}


#	result <- array(c(vKarta, vCommonName, vLsk, vLan, vF104, vF142), dim=c(nbElt, 6, 1), dimnames=list(c(),c("karta", "namn", "lsk", "lan", "fjall104", "fjall142")))
	result <- data.frame(vKarta, vCommonName, vLsk, vLan, vF104, vF142)
	colnames(result) <- c("karta", "namn", "lsk", "lan", "fjall104", "fjall142")

	return(result)

}


getIWCData <- function (pool) {
	queryregIWC <- "select site, lokalnamn, ki, ev
             from
             iwc_koordinater
             order by site"
	regIWCdat <<- dbGetQuery(pool, queryregIWC)

	return(regIWCdat)
}


getTabMinus1Mongo <- function (species, speciesSN, sites, years) {

	print(paste("start getTabMinus1Mongo ", Sys.time()))
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active"
	    }},
	    {"$lookup": {
          "from": "activity",
          "localField": "activityId",
          "foreignField": "activityId",
          "as": "actID"
        }},
        {"$unwind": "$activityId"},
	    {"$match": {
	        "actID.projectId" : "89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c",
	        "actID.verificationStatus" : "approved"
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
	        "linecount": "$obs.lineCount",
	        "ssn": "$obs.species.scientificName"
	    }},
	    {"$match": {
	        "linecount" : {"$gt":0},
	        "ssn" : {"$in" : [%s]}
	    }},
	    {"$group": {
	        "_id" : { "site" : "$site", "ssn" : "$ssn" },
	        "site" : { "$first" : "$site" },
	        "ssn" : { "$first" : "$ssn" }
	    }}
	]', speciesSN),
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





getTabZeroMongo <- function (species, speciesSN, sites, years) {

	print(paste("start getTabZeroMongo ", Sys.time()))

	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	or <- createOrEventDateCriteria(years)

	res1 <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active",
	        %s 
	    }},
	    {"$lookup": {
          "from": "activity",
          "localField": "activityId",
          "foreignField": "activityId",
          "as": "actID"
        }},
	    {"$match": {
	        "actID.projectId" : "89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c",
	        "actID.verificationStatus" : "approved"
	    }},
	    {"$project": {
	        "data.location":1,
	        "data.surveyDate":1
	    }}
	]', or),
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
		vYear[nbElt] <- substr(output$data$surveyDate, 1 , 4)

	}

	result1 <- data.frame(vKartaMatch,  vYear)
	colnames(result1) <- c("site", "time")

	res2 <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active"
	    }},
	    {"$lookup": {
          "from": "activity",
          "localField": "activityId",
          "foreignField": "activityId",
          "as": "actID"
        }},
	    {"$match": {
	        "actID.projectId" : "89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c",
	        "actID.verificationStatus" : "approved"
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
	        "linecount": "$obs.lineCount",
	        "ssn": "$obs.species.scientificName"
	    }},
	    {"$match": {
	        "linecount" : {"$gt":0},
	        "ssn" : {"$in" : [%s]}
	    }},
	    {"$group": {
	        "_id" : { "site" : "$site", "ssn" : "$ssn" },
	        "site" : { "$first" : "$site" },
	        "ssn" : { "$first" : "$ssn" }
	    }}
	]', speciesSN),
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


createOrEventDateCriteria <- function (years) {
	or <- '"$or" : [ '
	for (iYear in years[1]:tail(years, n=1)){ 
		or <- paste (or, '{"data.surveyDate": { "$regex" : "',iYear,'", "$options" : "i" } }, ', sep="")
	}

	# repeat the same at the end of the OR to deal with the comma
    or <- paste (or, '{"data.surveyDate": { "$regex" : "', iYear, '", "$options" : "i" } }]', sep="")

    return(or)
}

getTabStandardCountMongo <- function (species, speciesSN, sites, years) {

	print(paste("start getTabStandardCountMongo ", Sys.time()))
	mongoConnection  <- mongo(collection = "output",db = mongo_database,url = mongo_url,verbose = FALSE,options = ssl_options())

	
	or <- createOrEventDateCriteria(years)

	res <- mongoConnection$aggregate(sprintf('[
		{"$match": {
	        "status" : "active",
	        %s 
	    }},
	    {"$lookup": {
          "from": "activity",
          "localField": "activityId",
          "foreignField": "activityId",
          "as": "actID"
        }},
	    {"$match": {
	        "actID.projectId" : "89383d0f-9735-4fe7-8eb4-8b2e9e9b7b5c",
	        "actID.verificationStatus" : "approved"
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
	        "linecount": "$obs.lineCount",
	        "ssn": "$obs.species.scientificName",
	        "surveydate": 1
	    }},
	    {"$match": {
	        "linecount" : {"$gt":0},
	        "ssn" : {"$in" : [%s]}
	    }}
	]', or, speciesSN),
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
		vYear[nbElt] <- substr(output$surveydate, 1 , 4)
		vCount[nbElt] <- output$linecount


	}

	result <- data.frame(vKartaMatch, vArtMatch, vYear, vCount)
	colnames(result) <- c("site", "species", "time", "count")

	resRemoveDuplicate=unique(result)

	print(paste("end getTabStandardCountMongo ", Sys.time()))

	return(resRemoveDuplicate)


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

getTotalStandardData <- function (speciesMatch, speciesMatchSN, sitesMatchMongo, yearsSel) {

	minus1 <- getTabMinus1Mongo(species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel)
	zeros <- getTabZeroMongo(species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel)
	stdcount <- getTabStandardCountMongo(species = speciesMatch, speciesSN = speciesMatchSN, sites = sitesMatchMongo, years = yearsSel)

	print(paste("before final merge :", Sys.time()))

	dataMerge <- mergeTabs(minus1 = minus1, zeros = zeros, stdcount = stdcount)

	#write.csv(dataMerge, file = 'test_result.csv', row.names = FALSE)
	print(paste("before return :", Sys.time()))

	return(dataMerge)
}


exportSaveData <- function (dataMerge, savedat, filename, tab = "totalstandard") {

	## Export data
	if (1%in%savedat){
		dataMerge <<- dataMerge
	}
	if (2%in%savedat){
		write.csv(dataMerge, file = paste0('extract/mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.csv'),	          row.names = FALSE)
	}
	if (3%in%savedat){
		write.csv2(dataMerge, file = paste0('extract/mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.csv'),	          row.names = FALSE)
	}
	if (4%in%savedat){
		save(dataMerge, file = paste0('extract/mongo_', filename, '_', tab, '_', gsub('[ :]', '_', Sys.time()), '.rdata'))
	}

	print(paste("before return :", Sys.time()))

	return(dataMerge)
}