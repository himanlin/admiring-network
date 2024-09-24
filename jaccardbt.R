library(dplyr)
library(gasub)
library(statnet)
library(RSiena)

jac2<-c()
jas2<-c()
jcs2<-c()
jac<-c()
jas<-c()
jcs<-c()


for (i in class){
  class_id<-filter(stu_id, school==i)
  class_id<-class_id[order(class_id[,1]),]
  id<-class_id$id
  
  fw4<-filter(cclsm_w4, school==i)
  fw2<-filter(cclsm_w2, school==i)
  acl4<-filter(aclsm2, school==i)
  acl2<-filter(aclsm4, school==i)
  scl2<-filter(sclsm2, school==i)
  scl4<-filter(sclsm4, school==i)
  
  fw2<-fw2[,c(1,2)]
  fw4<-fw4[,c(1,2)]
  scl4<-scl4[,c(3,14)]
  scl2<-scl2[,c(3,14)]
  acl2<-acl2[,c(2,11)]
  acl4<-acl4[,c(2,11)]
  
  attric <- attr %>% filter(school == i)
  
  
  scl2<-filter(scl2, from %in% id &to %in% id & from!=to)
  scl4<-filter(scl4, from %in% id &to %in% id & from!=to)
  acl2<-filter(acl2, from %in% id &to %in% id & from!=to)
  acl4<-filter(acl4, from %in% id &to %in% id & from!=to)
  fw2<-filter(fw2, from %in% id &to %in% id & from!=to)
  fw4<-filter(fw4, from %in% id &to %in% id & from!=to)
  scl2<-scl2[order(scl2[,1], scl2[,2] ),]
  scl4<-scl4[order(scl4[,1], scl4[,2] ),]
  acl2<-acl2[order(acl2[,1], acl2[,2] ),]
  acl4<-acl4[order(acl4[,1], acl4[,2] ),]
  fw2<-fw2[order(fw2[,1], fw2[,2] ),]
  fw4<-fw4[order(fw4[,1], fw4[,2] ),]
  
  acl4<-acl4[!duplicated(acl4),]
  acl2<-acl2[!duplicated(acl2),]
  scl2<-scl2[!duplicated(scl2),]
  scl4<-scl4[!duplicated(scl4),]
  fw2<-fw2[!duplicated(fw2),]
  fw4<-fw4[!duplicated(fw4),]
  

  jac<-c(jac,JaccardSimilarity(graph_from_data_frame(fw2, directed = TRUE),
                               graph_from_data_frame(acl2, directed = TRUE), type = "edge"))
  jas<-c(jas,JaccardSimilarity(graph_from_data_frame(scl2, directed = TRUE),
                               graph_from_data_frame(acl2, directed = TRUE), type = "edge"))
  jcs<-c(jcs,JaccardSimilarity(graph_from_data_frame(fw2, directed = TRUE),
                               graph_from_data_frame(scl2, directed = TRUE), type = "edge"))
  jac2<-c(jac2,JaccardSimilarity(graph_from_data_frame(fw4, directed = TRUE),
                                 graph_from_data_frame(acl4, directed = TRUE), type = "edge"))
  jas2<-c(jas2,JaccardSimilarity(graph_from_data_frame(scl4, directed = TRUE),
                                 graph_from_data_frame(acl4, directed = TRUE), type = "edge"))
  jcs2<-c(jcs2,JaccardSimilarity(graph_from_data_frame(fw4, directed = TRUE),
                                 graph_from_data_frame(scl4, directed = TRUE), type = "edge"))
}  