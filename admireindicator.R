library(dplyr)
library(gasub)
library(statnet)
library(RSiena)


fw4<-filter(cclsm_w4)
fw2<-filter(cclsm_w2)
acl4<-filter(aclsm2)
acl2<-filter(aclsm4)



acl2<-filter(acl2,  from!=to)
acl4<-filter(acl4, from!=to)
fw2<-filter(fw2,  from!=to)
fw4<-filter(fw4,  from!=to)

acl2<-acl2[order(acl2[,2], acl2[,11] ),]
acl4<-acl4[order(acl4[,2], acl4[,11] ),]
fw2<-fw2[order(fw2[,1], fw2[,2] ),]
fw4<-fw4[order(fw4[,1], fw4[,2] ),]

acl4<-acl4[!duplicated(acl4),c(2,11)]
acl2<-acl2[!duplicated(acl2),c(2,11)]
fw2<-fw2[!duplicated(fw2),]
fw4<-fw4[!duplicated(fw4),]

acl2<-filter(acl2, from %in% iid &to %in% iid & from!=to)
acl4<-filter(acl4, from %in% iid &to %in% iid & from!=to)

ccl2<-graph_from_data_frame(fw2, directed = TRUE)
ccl4<-graph_from_data_frame(fw4, directed = TRUE)
acl2<-graph_from_data_frame(acl2, directed = TRUE)
acl4<-graph_from_data_frame(acl4, directed = TRUE)


w2data
w4data
