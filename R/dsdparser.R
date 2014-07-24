library(XML);library(RCurl)

# REST resource for DSD of nama_gdp_c
# downloading, parsing XML an setting root
file <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_nama_gdp_c"
content <- getURL(file, httpheader = list('User-Agent' = 'R-Agent'))
root <- xmlRoot(xmlInternalTreeParse(content, useInternalNodes = TRUE))

# get Nodeset of Codelists and its length
nodes <- getNodeSet(root,"//str:Codelist")
nn <- length(nodes)

# Create nested List of all Codes and Names
codelistAll <- lapply(seq(nn),function(i){
  xpathSApply(root,paste0("//str:Codelist[",i,"]/str:Code"),xmlGetAttr, "id")
})

namelistAll <- lapply(seq(nn),function(i){
  xpathSApply(root,paste0("//str:Codelist[",i,"]/str:Code/com:Name"),xmlValue)
})

# Create a list of dataframes from the nested lists
alldfList <-lapply(seq(nn),function(i) data.frame(codes=codelistAll[[i]],names=namelistAll[[i]]))

# Name the list items like the nodes
names(alldfList)  <- sapply(nodes, xmlGetAttr,"id")

