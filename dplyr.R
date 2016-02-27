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
