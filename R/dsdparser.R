library(XML);library(RCurl)

# REST resource for DSD of nama_gdp_c
# downloading, parsing XML an setting root
file <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_nama_gdp_c"
content <- getURL(file, httpheader = list('User-Agent' = 'R-Agent'))
root <- xmlRoot(xmlInternalTreeParse(content, useInternalNodes = TRUE))

# get Namespaces
ns <- xmlNamespaceDefinitions(root, simplify = TRUE)

# get Nodeset of Codelists and its length
nodes <- getNodeSet(root,"//str:Codelist")


alldfList <- lapply(nodes, function(x){ data.frame(
  codes= xpathSApply(x, ".//str:Code" , xmlGetAttr, "id", namespaces=ns),
  names= xpathSApply(x, ".//str:Code" , xmlValue, namespaces=ns) )})

names(alldfList)  <- sapply(nodes, xmlGetAttr,"id")

alldfList
