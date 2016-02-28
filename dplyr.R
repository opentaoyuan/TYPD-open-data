install.packages("dplyr")
library(dplyr)

data <- read.csv("traffic.csv",fileEncoding="big5")

type <- data %>%
  filter(year == 104) %>%
  group_by(type) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

vtype <- data %>%
  filter(year == 104) %>%
  group_by(vehicle_type) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

method <- data %>%
  filter(year == 104) %>%
  group_by(method) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))


write.csv(type,file = "type.csv",row.names = F,fileEncoding="utf-8")
write.csv(vtype,file = "vtype.csv",row.names = F,fileEncoding="utf-8")
write.csv(method,file = "method.csv",row.names = F,fileEncoding="utf-8")

type_month <- data %>%
  filter(year == 104) %>%
  group_by(month,type) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

vtype_month <- data %>%
  filter(year == 104) %>%
  group_by(month,vehicle_type) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

method_month <- data %>%
  filter(year == 104) %>%
  group_by(month,method) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))


write.csv(type_month,file = "type_month.csv",row.names = F,fileEncoding="utf-8")
write.csv(vtype_month,file = "vtype_month.csv",row.names = F,fileEncoding="utf-8")
write.csv(method_month,file = "method_month.csv",row.names = F,fileEncoding="utf-8")

ggplot(type[1:10,],aes(x=type,y =count,fill = type))+
  geom_bar(stat="identity")

ggplot(vtype,aes(x=vehicle_type,y =count,fill = vehicle_type))+
  geom_bar(stat="identity")

ggplot(method,aes(x=method,y =count,fill = method))+
  geom_bar(stat="identity")
  
for(i in 1:12){
  X <- type_month %>%
    filter(month == i)
  bar[[i]]<- ggplot(X[1:10,],aes(x=type,y =count,fill = type))+
    geom_bar(stat="identity")
}
  bar[[1]] +  ggtitle(paste(1,"月交通違規舉發"))
  bar[[2]] +  ggtitle(paste(2,"月交通違規舉發"))
  bar[[3]] +  ggtitle(paste(3,"月交通違規舉發"))
  bar[[4]] +  ggtitle(paste(4,"月交通違規舉發"))
  bar[[5]] +  ggtitle(paste(5,"月交通違規舉發"))
  bar[[6]] +  ggtitle(paste(6,"月交通違規舉發"))
  bar[[8]] +  ggtitle(paste(8,"月交通違規舉發"))
  bar[[9]] +  ggtitle(paste(9,"月交通違規舉發"))
  bar[[10]] +  ggtitle(paste(10,"月交通違規舉發"))
  bar[[11]] +  ggtitle(paste(11,"月交通違規舉發"))
  bar[[12]] +  ggtitle(paste(12,"月交通違規舉發"))
