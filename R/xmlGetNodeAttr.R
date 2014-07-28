

xmlGetNodeAttr <- function(n, xp, attr,default=NA,ns) {
  nds<-getNodeSet(n, xp, namespaces=ns)
  if(length(nds)<1) {
    return(default)
  } else {
    sapply(nds, xmlGetAttr, attr, default)
  }
}