read_eurostat <- function(flowRef, key, start, end){

# Prepare REST Call from function arguments
file <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/",flowRef,"/",key,"/?startperiod=",start,"&endPeriod=",end)
  
# downloading, parsing XML an setting root
content <- getURL(file, httpheader = list('User-Agent' = 'R-Agent'))
root <- xmlRoot(xmlInternalTreeParse(content, useInternalNodes = TRUE))

# get Nodeset of Series and its length
n_Series <- getNodeSet(root,"//generic:Series")
nn_Series <- length(n_Series)

# get the list of value attributes
# [TODO] Attributes are still hard-coded below
Codes <- unique(xpathSApply(root,"//generic:SeriesKey/generic:Value",xmlGetAttr,"id"))
Attributes  <- unique(xpathSApply(root,"//generic:Attributes/generic:Value",xmlGetAttr,"id"))


# namespace
ns <- xmlNamespaceDefinitions(root, simplify = TRUE)

## Get the number observations per Series
obs_per_ser <- sapply(seq(nn_Series),
       function(i){
         xpathApply(root, 
                    path = paste0("count(//generic:Series[",i,"]/generic:Obs)"),xmlValue)})



## Series Keys. These need to be replicated since multiple observations (years) can be 
df1 <- as.data.frame(sapply(Codes, function(i){xpathSApply(root, paste0(".//generic:Value[@id='",i,"']") , xmlGetAttr, "value", namespaces=ns)}))
df1 <- df1[rep(seq_len(nrow(df1)), time=obs_per_ser),]


## Values and Attributes
df2 <- do.call(rbind, lapply(getNodeSet(root,"//generic:Obs"), function(x) {
  data.frame(
    dimension=xmlGetNodeAttr(x, "./generic:ObsDimension","value",NA,ns),
    value=xmlGetNodeAttr(x, "./generic:ObsValue","value",NA,ns),
    status=xmlGetNodeAttr(x, "./generic:Attributes/generic:Value[@id='OBS_STATUS']","value",NA,ns),
    flag=xmlGetNodeAttr(x, "./generic:Attributes/generic:Value[@id='OBS_FLAG']","value",NA,ns)
  )
}))


# Combine the dfs
df <- cbind(df1,df2)
return(df)
}

#read_eurostat(flowRef = "cdh_e_fos", 
#key = "..PC.FOS1.BE",
#start = 2005,
#end =   2011)

