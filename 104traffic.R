install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

data <- read.csv("traffic.csv",fileEncoding="big5",stringsAsFactors = F)

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

ggplot(type[1:5,],aes(x=type,y =count,fill = type))+
  geom_bar(stat="identity",width=.5,alpha = 0.8) +
  geom_text(aes(label = count),size = 3.5) +
  ggtitle("104年交通違規舉發件數")

ggplot(vtype,aes(x=vehicle_type,y =count,fill = vehicle_type))+
  geom_bar(stat="identity",width=.5,alpha = 0.8) +
  geom_text(aes(label = count),size = 3.5) +
  ggtitle("104年交通違規舉發車種別件數")

ggplot(method,aes(x=method,y =count,fill = method))+
  geom_bar(stat="identity",width=.5,alpha = 0.8) +
  geom_text(aes(label = count),size = 3.5) +
  ggtitle("104年交通違規舉發方式別件數")

top5type <- type[1:5,1]#取出數量前五的種類
top5 <- data.frame()#產生空的框架
for(i in 1:5){
  X <- type_month %>%
    filter(type == as.character(top5type[i,]))
  top5 <- rbind(top5,X)
}#重新取出數量前五種類的資料

bar <- list()#產生空的list
for(i in 1:12){
  X <- ungroup(top5) %>%
    filter(month == i)
  bar[[i]]<- ggplot(X,aes(x=type,y =count,fill = type))+
    geom_bar(stat="identity",width=.5,alpha = 0.8) +
    geom_text(aes(label = count),size = 3.5) +
    ggtitle(paste("104年",i,"月交通違規舉發"))
}#產生1至12月的bars
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
#bars全部畫出

ggplot(top5,aes(x=type,y =count,fill = type))+
  geom_boxplot(alpha = 0.7) +
  ggtitle("104年交通違規盒鬚圖")
#畫出盒鬚圖

trendline <- list() 
for(i in 1:5){
   X <- ungroup(top5) %>%
    filter(type == as.character(top5type[i,]))
  trendline[[i]] <- ggplot(X,aes(x=month,y =count))+
    geom_text(aes(label = count),size = 3.5) +
    geom_line(colour = "darkred",size = 1,alpha=0.8) +
    scale_x_continuous(breaks = 1:12) +
    ggtitle(paste("104年",as.character(top5type[i,]),"趨勢線"))
}#產生前5種類的trendllines
trendline[[1]] 
trendline[[2]] 
trendline[[3]] 
trendline[[4]] 
trendline[[5]] 
#trendllines全部畫出
