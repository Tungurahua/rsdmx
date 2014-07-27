library(XML)
library(RCurl)
library(plyr)

# REST resource for DSD of nama_gdp_c
# downloading, parsing XML an setting root
file <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/cdh_e_fos/..PC.FOS1.BE/?startperiod=2005&endPeriod=2013"
content <- getURL(file, httpheader = list('User-Agent' = 'R-Agent'))
root <- xmlRoot(xmlInternalTreeParse(content, useInternalNodes = TRUE))

# get Nodeset of Series and its length
n_Series <- getNodeSet(root,"//generic:Series")
nn_Series <- length(n_Series)

# get the list of value attributes
Codes <- unique(xpathSApply(root,"//generic:SeriesKey/generic:Value",xmlGetAttr,"id"))
Attributes  <- unique(xpathSApply(root,"//generic:Attributes/generic:Value",xmlGetAttr,"id"))



# namespace
ns <- xmlNamespaceDefinitions(root, simplify = TRUE)

## Get the number observations per Series
obs_per_ser <- sapply(seq(nn_Series),
       function(i){
         xpathApply(root, 
                    path = paste0("count(//generic:Series[",i,"]/generic:Obs)"),xmlValue)})



## Series Keys
df1 <- as.data.frame(sapply(Codes, function(i){xpathSApply(root, paste0(".//generic:Value[@id='",i,"']") , xmlGetAttr, "value", namespaces=ns)}))
df1 <- df1[rep(seq_len(nrow(df1)), time=obs_per_ser),]


## time and value columns
df2 <- data.frame(
  time  = xpathSApply(root, ".//generic:ObsDimension" , xmlGetAttr, "value", namespaces=ns),
  value = xpathSApply(root, ".//generic:ObsValue" , xmlGetAttr, "value", namespaces=ns))
  

## Attributes
df3 <- data.frame(
  Att1 <- xpathSApply(root, "//generic:Attributes/generic:Value[@id='OBS_STATUS']",
                      function(x){xmlGetAttr(x, "value", default="Hicks")}),
  
  Att2 <- xpathSApply(root, "//generic:Attributes/generic:Value[@id='OBS_FLAG']", 
                      function(x){xmlGetAttr(x, "value")})
  
  length(getNodeSet(root, "//generic:Attributes/generic:Value[@id='OBS_STATUS']"))
   
  
  Att2
cbind(df1,df2)



