library(dplyr)
library(gasub)
library(statnet)
library(RSiena)
acl4<-filter(acl_weight,time==2)
acl2<-filter(acl_weight,time==2)
acl4<-filter(acl_weight,time==4)
acl2<-filter(acl_weight,time==2)
acl2<-filter(acl2,  from!=to)
acl4<-filter(acl4, from!=to)
View(acl2)
acl2<-acl2[order(acl2[,1], acl2[,4] ),]
acl4<-acl4[order(acl4[,1], acl4[,4] ),]
acl4<-acl4[!duplicated(acl4),c(1,4)]
acl2<-acl2[!duplicated(acl2),c(1,4)]
acl2<-filter(acl2, from %in% iid &to %in% iid & from!=to)
acl4<-filter(acl4, from %in% iid &to %in% iid & from!=to)
acl2<-graph_from_data_frame(acl2, directed = TRUE)
acl4<-filter(acl_weight,time==4)
acl2<-filter(acl_weight,time==2)
acl2<-filter(acl2, from %in% iid &to %in% iid & from!=to)
acl4<-filter(acl4, from %in% iid &to %in% iid & from!=to)
acl4<-acl4[!duplicated(acl4),c(1,4,2)]
acl2<-acl2[!duplicated(acl2),c(1,4,2)]
acl2<-acl2[order(acl2[,1], acl2[,4] ),]
acl2<-acl2[order(acl2[,1], acl2[,2] ),]
acl4<-acl4[order(acl4[,1], acl4[,2] ),]

acl4pagerank<-c()
acl2pagerank<-c()

for (i in class){
aclc2<-filter(acl2,acl2$from%/%100==i)
aclc4<-filter(acl4,acl4$from%/%100==i)

a2<-graph_from_data_frame(aclc2, directed = TRUE)
a4<-graph_from_data_frame(aclc4, directed = TRUE)
a4 <- set_edge_attr(a4, "weight", value= aclc4$wt)
a2 <- set_edge_attr(a2, "weight", value= aclc2$wt)

acl4pagerank<-c(acl4pagerank,page_rank(a4)$vector)
acl2pagerank<-c(acl2pagerank,page_rank(a2)$vector)


}


aclc2<-filter(acl2,acl2$from%/%100==37)
a2<-graph_from_data_frame(aclc2, directed = TRUE)
a2 <- set_edge_attr(a2, "weight", value= aclc2$wt)
V(a2)$pager<-page_rank(a2)$vector
V(a2)$inde<-degree(a2,mode='in')
gend32<-data.frame(id=aclw2$from,gender=aclw2$gender.x)
gend32<-gend32[!duplicated(gend32),]
gend32<-filter(gend32,id%/%100==37)
V(a2)$color<-gend32$gender
plot(a2, layout=layout_with_fr,vertex.label=NA, edge.arrow.size=0.2,edge.width=(E(a2)$weight/3),vertex.size=V(a2)$inde/1.5+10)
