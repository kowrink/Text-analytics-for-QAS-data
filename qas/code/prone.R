library(tidyverse)
pop_data=read.csv("erp-lga-qld.csv",stringsAsFactors = FALSE)

pop_data=pop_data %>%
  unnest_tokens(word,X)
pop_data1=filter(pop_data,nchar(word)>1)
#pop_data1=pop_data1[!duplicated(pop_data1$X2018p), ]
names(pop_data1)=c("population","locality")
pop_pos=merge(postcode1,pop_data1,by="locality")
postcode_fre=read.csv("postcode_fre.csv")
names(postcode_fre)=c("id","postcode","fre")
fre_pop=merge(postcode_fre,pop_pos,by="postcode")


fre_pop=fre_pop[!duplicated(fre_pop$postcode), ]
result=aggregate(fre_pop$fre,by=list(Category=fre_pop$locality), FUN=sum)
names(result)=c("locality","frequency")
result=merge(result,pop_data1,by="locality")
result=aggregate(result$frequency,by=list(Category=result$population), FUN=sum)
names(result)=c("population","frequency")
result=merge(result,pop_data1,by="population")
write.csv(result,file="frequency_population1.csv")
result=read.csv("frequency_population1.csv",stringsAsFactors = FALSE)
result$frequency=as.numeric(result$frequency)
result$population=as.numeric(gsub(",","",result$population))
result$rate=result$frequency/result$population
pop_pos$population=as.numeric(gsub(",","",pop_pos$population))
pos_rate=merge(result,pop_pos,by="population")
write.csv(pos_rate,file="pos_rate.csv")


pos_pop=read.csv("population.csv")
names(pos_pop)=c("postcode","locality")
fre_pop=merge(postcode_fre,pos_pop,by="postcode")
fre_pop$rate=fre_pop$fre/fre_pop$locality
write.csv(fre_pop,file="excatsuburbfre.csv")


highaccpost=filter(fre_pop,rate>0.05)$postcode
highaccpost=highaccpost[-2]
sensdata=subset(my_data, Postcode %in% highaccpost)


sensi=data.frame(id=sensdata$`EARF Number`,txt=paste(sensdata$Postcode,sensdata$`Case Comments`,sensdata$`Street Name`,sensdata$`Final Assessment Code`),stringsAsFactors = FALSE)
sensi <- sensi%>%
  group_by(id) %>%
  ungroup()
sensi=sensi %>%
  unnest_tokens(word, txt)

sensi <- sensi %>%
  anti_join(stop_words)
sensicor<- sensi %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, id, sort = TRUE)
sensicor= sensicor %>%
  filter (as.numeric(item1)>1000 & correlation> 0.05) %>%
  group_by(item1)


names(sensicor) = c("postcode","locality","correlation")

sensi=merge(sensicor,postcode1,by = c("postcode","locality"))

names(sensi)=c("item1","locality","correlation")
names(sensicor)=c("item1","locality","correlation")
sensi=anti_join(sensicor,sensi,by="item1")
write.csv(sensi,file="sensitiveareacor.csv")
sensi=filter(sensi,nchar(locality)>3)
lar_pop=filter(pos_pop,locality>800)
names(lar_pop)=c("item1","pop")
sensi_lar=merge(lar_pop,sensi, by = "item1")
write.csv(sensi_lar,file="sensihighpopeareacor.csv")
