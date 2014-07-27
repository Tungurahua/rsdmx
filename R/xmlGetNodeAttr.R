

xmlGetNodeAttr <- function(n, xp, attr,ns,default=NA) {
  nds<-getNodeSet(n, xp, namespaces=ns)
  if(length(nds)<1) {
    return(default)
  } else {
    sapply(nds, xmlGetAttr, attr, default)
  }
}