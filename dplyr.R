install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

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
  geom_bar(stat="identity") +
  ggtitle("104年交通違規舉發件數")

ggplot(vtype,aes(x=vehicle_type,y =count,fill = vehicle_type))+
  geom_bar(stat="identity") +
  ggtitle("104年交通違規舉發車種別件數")

ggplot(method,aes(x=method,y =count,fill = method))+
  geom_bar(stat="identity") +
  ggtitle("104年交通違規舉發方式別件數")

bar <- list() 
for(i in 1:12){
  X <- type_month %>%
    filter(month == i)
  bar[[i]]<- ggplot(X[1:10,],aes(x=type,y =count,fill = type))+
    geom_bar(stat="identity",width=.5) +
    ggtitle(paste("104年",i,"月交通違規舉發"))
}
bar[[1]] 
bar[[2]] 
bar[[3]] 
bar[[4]]
bar[[5]] 
bar[[6]]
bar[[8]]
bar[[9]]
bar[[10]]
bar[[11]]
bar[[12]]
