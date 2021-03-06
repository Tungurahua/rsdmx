# E.Blondel - 2013/06/10
#=======================

SDMXDataSet <- function(xmlObj){
	sdmx <- SDMX(xmlObj);
	new("SDMXDataSet",
		SDMX(xmlObj)
		)		
}

#methods
as.data.frame.SDMXDataSet <- function(x, ...){
	xmlObj <- x@xmlObj;
  dataset <- NULL
  
  sdmxVersion <- getVersion(SDMXSchema(xmlObj))
  VERSION.21 <- sdmxVersion == "2.1"
  
  #namespace
  nsDefs.df <-as.data.frame(
    do.call("rbind",
            lapply(xmlNamespaceDefinitions(xmlObj, simplify = F),
                   function(x){c(x$id, x$uri)})),
    stringAsFactors = FALSE
  )
  colnames(nsDefs.df) <- c("id","uri")
  nsDefs.df$id <- as.character(nsDefs.df$id)
  nsDefs.df$uri <- as.character(nsDefs.df$uri)
  ns <- c(ns = nsDefs.df$uri[grep("generic", nsDefs.df$uri[grep("metadata", nsDefs.df$uri, invert = TRUE)])])
  if(length(ns) == 0){
    #in case no ns found, try to find specific namespace
    ns.df <- nsDefs.df[regexpr("http://www.SDMX.org", nsDefs.df$uri, "match.length") == -1
                       & regexpr("http://www.w3.org", nsDefs.df$uri, "match.length") == -1,]
    ns <- ns.df$uri
    if(length(ns) > 1){
      warning("More than one target dataset namespace found!")
      ns <- ns[1L]
    }
    hasAuthorityNS <- TRUE
    authorityId <- nsDefs.df[nsDefs.df$uri == ns,]$id
  }
	
  
  if(type.SDMXType(xmlObj) %in% c("SDMXGenericData","SDMXMessageGroup")){
    
    #series
    seriesXML <- getNodeSet(xmlObj, "//ns:Series", namespaces = ns)
    seriesNb <- length(seriesXML)
    if(seriesNb == 0) return(NULL);
    
    conceptId <- "concept"
    if(VERSION.21) conceptId <- "id"
    
  	#serie keys
  	keysXML <- getNodeSet(xmlDoc(getNodeSet(xmlObj,
                                            "//ns:SeriesKey",
                                            namespaces = ns)[[1]]),
                          "//ns:Value",
                          namespaces = ns)
  	keysNames <- unique(sapply(keysXML, function(x) xmlGetAttr(x, conceptId)))
    
    #serie observation attributes
    obsAttrsNames <- NULL    
    obsAttrsXML <- getNodeSet(xmlObj,
                              "//ns:Obs/ns:Attributes/ns:Value",
                              namespaces = ns)
    if(length(obsAttrsXML) > 0){
      obsAttrsNames <- unique(sapply(obsAttrsXML,function(x){
        xmlGetAttr(x, conceptId)
      }))
    }
    
    #output structure
    serieNames <- c(keysNames, "Time", "ObsValue")
    if(!is.null(obsAttrsNames)) serieNames <- c(serieNames, obsAttrsNames)
  	
    #obs parser function
    parseObs <- function(obs){
      
      obsXML <- xmlDoc(obs)
      
      #time
      timeElement <- "Time"
      if(VERSION.21) timeElement <- "ObsDimension"
      obsTimeXML <- getNodeSet(obsXML,
                               paste("//ns:",timeElement,sep=""),
                               namespaces=ns)[[1]]
      obsTime <- NA
      if(!VERSION.21){
        obsTime <- xmlValue(obsTimeXML)
      } else {
        obsTime <- xmlGetAttr(obsTimeXML,"value")
      }
      obsTime <- as.data.frame(obsTime)
      
      #value
      obsValueXML <- getNodeSet(obsXML,
                                "//ns:ObsValue",
                                namespaces = ns)[[1]]
      obsValue <- as.numeric(xmlGetAttr(obsValueXML, "value"))
      obsValue <- as.data.frame(obsValue)
      
      #attributes
      obsAttrs.df <- NULL
      if(!is.null(obsAttrsNames)){
        obsAttrsXML <- getNodeSet(obsXML,
                                  "//ns:Attributes/ns:Value",
                                  namespaces = ns)
        if(length(obsAttrsXML) > 0){
          obsAttrsValues <- sapply(obsAttrsXML, function(x){
            sapply(obsAttrsNames, function(t){
              if((xmlGetAttr(x, conceptId) == t)){
                as.character(xmlGetAttr(x, "value"))
              }
            })
          })
          obsAttrs.df <- as.data.frame(t(obsAttrsValues), stringAsFactors = FALSE)
          for(i in 1:ncol(obsAttrs.df)){
            if(any(obsAttrs.df[,i] == "NA")){
              obsAttrs.df[,i][obsAttrs.df[,i] == "NA"] <- NA
            }
            if(any(obsAttrs.df[,i] == "NULL")){
              obsAttrs.df[,i][obsAttrs.df[,i] == "NULL"] <- NA
            }
          }
        }
      }
      
      #output
      obsR <- cbind(obsTime, obsValue)
      if(!is.null(obsAttrs.df)) obsR <- cbind(obsR, obsAttrs.df)
      return(obsR)
    }
    
  	#function to parse a Serie
  	parseSerie <- function(x){
  		
  		# Single serie XMLInternalNode converted into a XMLInternalDocument
  		serieXML <- xmlDoc(x)
      
      #parseobs
      obssXML <- getNodeSet(serieXML, "//ns:Series/ns:Obs", namespaces = ns)

      #apply obsParser
      obsdf <- NULL
      if(length(obssXML) > 0){
        obsdf <- do.call("rbind.fill",lapply(obssXML, function(x) parseObs(x)))
      }
        
  		#Key values
  		#SeriesKey (concept attributes/values) are duplicated according to the
      #number of Time observations
  		keyValuesXML <- getNodeSet(serieXML,
                                 "//ns:SeriesKey/ns:Value",
                                 namespaces = ns)
  		keyValues <- sapply(keyValuesXML, function(x){
        as.character(xmlGetAttr(x, "value"))
      })
  		keydf <- structure(keyValues, .Names = keysNames) 
  		keydf <- as.data.frame(lapply(keydf, as.character), stringsAsFactors=FALSE)
  		if(!is.null(obsdf)){
        keydf <- keydf[rep(row.names(keydf), nrow(obsdf)),]
  		  row.names(keydf) <- 1:nrow(obsdf)
  		}
        
  		#single Serie as DataFrame
      serie <- keydf
      if(!is.null(obsdf)){
        serie <- cbind(serie, obsdf)
      }
      
      #convert factor columns
      if("obsTime" %in% colnames(serie)){
        serie[,"obsTime"] <- as.character(serie[,"obsTime"])
      }
      if(!is.null(obsAttrsNames) & !is.null(obsdf)){
        for(i in 1:length(colnames(obsdf))){
          serie[,colnames(obsdf)[i]] <- as.character(serie[,colnames(obsdf)[i]])
        }
  	  }
  		return(serie)
  	}
  	
  	#converting SDMX series to a DataFrame R object
  	dataset <- do.call("rbind.fill", lapply(seriesXML, function(x){
      serie <- parseSerie(x)
    }))
    
    dataset$obsValue <- as.numeric(dataset$obsValue)
  
  }else if(type.SDMXType(xmlObj) == "SDMXCompactData"){
    if(hasAuthorityNS){
      
      seriesXML <- getNodeSet(xmlObj, paste("//",authorityId,":Series",sep=""))
      
      #function to parse a Serie
      parseSerie <- function(x){
        
        #obs values
        obsValueXML <- xmlChildren(x)
        obsValue <- as.data.frame(
                      do.call("rbind", lapply(obsValueXML, function(t){
                        xmlAttrs(t)
                      })),
                      stringAsFactors = FALSE,
                      row.names = 1:length(obsValueXML),
                      stringAsFactors = FALSE)
        
        #key values
        keydf <- as.data.frame(t(as.data.frame(xmlAttrs(x), stringAsFactors = FALSE)), stringAsFactors = FALSE)
        if(nrow(obsValue) > 0){
          keydf <- keydf[rep(row.names(keydf), nrow(obsValue)),]
          row.names(keydf) <- 1:nrow(obsValue)
        }
          
        #single Serie as DataFrame
        if(nrow(obsValue) > 0){  
          serie <- cbind(keydf, obsValue, row.names = 1:nrow(obsValue))
        }else{
          #manage absence data
          serie <- keydf
        }
        return(serie)
      }
      
      #converting SDMX series to a DataFrame R object
      dataset <- do.call("rbind.fill", lapply(seriesXML, function(x){serie <- parseSerie(x) }))
      
    }else{
      #to see how to deal with this case
      stop("Unsupported CompactData parser for generic SDMX namespace")
    }
  }

  if(any(as.character(dataset$obsValue) == "NaN", na.rm = TRUE)){
    dataset[as.character(dataset$obsValue) == "NaN",]$obsValue <- NA
  }
	if(!is.null(dataset)) row.names(dataset) <- 1:nrow(dataset)
  
	# output
	return(dataset)
}

setAs("SDMXDataSet", "data.frame", function(from) as.data.frame.SDMXDataSet(from));
