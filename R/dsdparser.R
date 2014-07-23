library(XML)
library(RCurl)
library(rsdmx)

## creating arguments for testing purpose

# url to REST resource
file <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_nama_gdp_c"

# downloading and parsing XML
rsdmxAgent <- paste("rsdmx/",as.character(packageVersion("rsdmx")),sep="")
content <- getURL(
  file,
  httpheader = list('User-Agent' = rsdmxAgent)
)

## if "Internal" is used, you need to specify the Root
doc <- xmlInternalTreeParse(content, useInternalNodes = TRUE)
root <- xmlRoot(doc)



# xmlApply, xmlSApply


## get Nodeset of Codelists
nodes <- getNodeSet(root,"//str:Codelist")
sapply(nodes, xmlGetAttr,"id")
sapply(nodes, xmlChildren)


xmlGetAttr(xmlChildren(nodes[[1]]),"id")

## extract only the Codelist Names
codelist <- xpathApply(root,"//str:Codelist",xmlGetAttr,"id")


# First Node Codelist
codelist1 <- xpathApply(root,path = ("//str:Codelist[1]/str:Code"),xmlGetAttr,"id")
names1 <- xpathApply(root,path = ("//str:Codelist[1]/str:Code/com:Name"),xmlValue)

# All Nodes Codelist
nn <- length(nodes)

# Get List of all Codes
codelistAll <- lapply(seq(nn),function(i){
  xpathSApply(root,paste0("//str:Codelist[",i,"]/str:Code"),xmlGetAttr, "id")
})

# Get List of all Names
namelistAll <- lapply(seq(nn),function(i){
  xpathSApply(root,paste0("//str:Codelist[",i,"]/str:Code/com:Name"),xmlValue)
})

alldfList <-lapply(seq(nn),function(i) data.frame(codes=codelistAll[[i]],names=namelistAll[[i]]))


names(alldfList)  <- sapply(nodes, xmlGetAttr,"id")

 