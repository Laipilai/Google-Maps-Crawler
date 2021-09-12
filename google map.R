### Google Maps 爬蟲分析:以台北市為例 ### 

# 下載套件
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringi)
library(stringr)
library(magrittr)
library(hablar)
library(rgdal)

setwd("C:/Users/user/Desktop/爬蟲/google map example")

# 讀檔
# 特店資料
# store = read.csv("台新特店.csv", encoding = 'utf-8')
# 0730資料
res_0730 = read_excel("0730_google map_res750(台北市).xls") # 3562間
coffee_0730 = read_excel("0730_google map_咖啡廳750(台北市).xls") # 2148間
bakery_0730 = read_excel("0730_google map_麵包店750(台北市).xls") # 895間
bar_0730 = read_excel("0730_google map_酒吧750(台北市).xls") # 535間
# 0803資料
res_0803 = read_excel("0803_google map_res750(台北市).xls") # 3470間
coffee_0803 = read_excel("0803_google map_咖啡廳750(台北市).xls") # 2144間
bakery_0803 = read_excel("0803_google map_麵包店750(台北市).xls") # 858間
bar_0803 = read_excel("0803_google map_酒吧750(台北市).xls") # 589間
# 0806資料
res_0806 = read_excel("0806_google map_res750(台北市).xls") # 3538間
coffee_0806 = read_excel("0806_google map_咖啡廳750(台北市).xls") # 2153間
bakery_0806 = read_excel("0806_google map_麵包店750(台北市).xls") # 821間
bar_0806 = read_excel("0806_google map_酒吧750(台北市).xls") # 590間
# 0810資料
res_0810 = read_excel("0810_google map_res750(台北市).xls") # 3578間
coffee_0810 = read_excel("0810_google map_咖啡廳750(台北市).xls") # 2164間
bakery_0810 = read_excel("0810_google map_麵包店750(台北市).xls") # 893間
bar_0810 = read_excel("0810_google map_酒吧750(台北市).xls") # 624間
# 0813資料
res_0813 = read_excel("0813_google map_res750(台北市).xls") # 3555間
coffee_0813 = read_excel("0813_google map_咖啡廳750(台北市).xls") # 2236間
bakery_0813 = read_excel("0813_google map_麵包店750(台北市).xls") # 875間
bar_0813 = read_excel("0813_google map_酒吧750(台北市).xls") # 606間
# 0820資料
res_0820 = read_excel("0820_google map_res750(台北市).xls") # 3555間
coffee_0820 = read_excel("0820_google map_咖啡廳750(台北市).xls") # 2249間
bakery_0820 = read_excel("0820_google map_麵包店750(台北市).xls") # 929間
bar_0820 = read_excel("0820_google map_酒吧750(台北市).xls") # 633間

### ------------------------資料前處理---------------------------
# 貼上日期標籤
res_0730$date = ymd("2020-07-30")
coffee_0730$date = ymd("2020-07-30")
bakery_0730$date = ymd("2020-07-30")
bar_0730$date = ymd("2020-07-30")
res_0803$date = ymd("2020-08-03")
coffee_0803$date = ymd("2020-08-03")
bakery_0803$date = ymd("2020-08-03")
bar_0803$date = ymd("2020-08-03")
res_0806$date = ymd("2020-08-06")
coffee_0806$date = ymd("2020-08-06")
bakery_0806$date = ymd("2020-08-06")
bar_0806$date = ymd("2020-08-06")
res_0810$date = ymd("2020-08-10")
coffee_0810$date = ymd("2020-08-10")
bakery_0810$date = ymd("2020-08-10")
bar_0810$date = ymd("2020-08-10")
res_0813$date = ymd("2020-08-13")
coffee_0813$date = ymd("2020-08-13")
bakery_0813$date = ymd("2020-08-13")
bar_0813$date = ymd("2020-08-13")
res_0820$date = ymd("2020-08-20")
coffee_0820$date = ymd("2020-08-20")
bakery_0820$date = ymd("2020-08-20")
bar_0820$date = ymd("2020-08-20")

# 分種類合併每期資料
res = bind_rows(res_0730, res_0803, res_0806, res_0810, res_0813, res_0820)
coffee = bind_rows(coffee_0730, coffee_0803, coffee_0806, coffee_0810, coffee_0813, coffee_0820)
bakery = bind_rows(bakery_0730, bakery_0803, bakery_0806, bakery_0810, bakery_0813, bakery_0820)
bar = bind_rows(bar_0730, bar_0803, bar_0806, bar_0810, bar_0813, bar_0820)

# 刪除歇業中的店家
res = res[-which(res$permanently_closed == T), ]
coffee = coffee[-which(coffee$permanently_closed == T), ]
bakery = bakery[-which(bakery$permanently_closed == T), ]
bar = bar[-which(bar$permanently_closed == T), ]

# 刪除重複的店家
coffee = coffee %>%
  filter(!place_id %in% res$place_id) # 刪除res有的店名

bakery = bakery %>%
  filter(!place_id %in% res$place_id) %>% # 刪除res有的店名
  filter(!place_id %in% coffee$place_id ) # 刪除coffee有的店名

bar = bar %>%
  filter(!place_id %in% res$place_id) %>% # 刪除res有的店名
  filter(!place_id %in% coffee$place_id) %>% # 刪除coffee有的店名
  filter(!place_id %in% bakery$place_id) # 刪除bar有的店名

# 資料合併
data = bind_rows(res_0730, coffee_0730, bar_0730, bakery_0730)

# 刪除不必要的變量
rm(res, coffee, bakery, bar, res_0730, res_0803, res_0806, res_0810, res_0813, res_0820, coffee_0730, coffee_0803, coffee_0806, coffee_0810, coffee_0813, coffee_0820, bar_0730, bar_0803, bar_0806, bar_0810, bar_0813, bar_0820, bakery_0730, bakery_0803, bakery_0806, bakery_0810, bakery_0813, bakery_0820)

# 挑選重要欄位、留下包含"台北市"、"區"、刪除"区"資料、更改欄位名稱
data = data %>% 
  select(name,formatted_address,place_id,types,date,price_level,rating,user_ratings_total) %>%
  filter(grepl("台北市", formatted_address) & grepl("區", formatted_address) & !grepl("区", formatted_address)) %>%
  rename(region = formatted_address) %>%
  rename(user = user_ratings_total)
data$region = gsub("台灣中山區", "台灣", data$region) # 特殊地址處理

# 將地址轉換為台北市區別
temp = c("中正區","大同區","中山區","松山區","大安區","萬華區","信義區","士林區","北投區","內湖區","南港區","文山區")
data$region = sapply(data$region, stri_extract_all, fixed = temp) %>% # 從region欄位抓出所有符合temp的字
  apply(., 2, function(x) {temp[which(!is.na(x))]}) %>% # 抓出不是NA值的region欄位
  as.character()


### ------------------------重複資料處理與新增欄位---------------------------

# 挑選資料重複的店家(重複place_id)，只留最新的兩筆資料，新增欄位名稱
a = data %>% 
  group_by(place_id) %>% 
  mutate(index = n()) %>% 
  filter(index > 1) %>% #只留有重複的店家
  select(-index) %>%
  slice(c(1,n())) %>% # 取最新跟最舊的資料
  arrange(date) %>%
  mutate(rating_lag = lag(rating, n = 1),
         user_lag = lag(user, n = 1),
         rating_dif = rating - rating_lag,# 計算每期rating差異
         user_dif = user - user_lag) %>% # 計算每期user差異
  ungroup()

# # 資料全部店家(重複保留最新者)
# b = data[!duplicated(data$place_id, fromLast = TRUE),]


### -------------------------檢查是否與行內資料重複-------------------------

# 新增欄位名稱:special(是否為台新特店)
# a = a %>% 
#   mutate(special1 = ifelse(name %in% store$CNTR_COMP_NM, 1, 0),
#          special2 = ifelse(name %in% store$MRCH_NM, 1, 0),
#          special = special1 + special2)
# a$special[which(a$special == 2)] == 1
# a = a %>% select(-c(special1,special2))
# 
# # 匯出特店名單比對資料(auto)
# write.csv(a,"special store_auto.csv")
# 
# # 匯入人工篩選(excel)的特店名單比對資料(man)
# a = read.csv("special store_man.csv", encoding = 'utf-8')
# 
# # for (i in nrow(a)) {
# #   a$name[i] = agrep(a$name[i], z$MRCH_NM, value = T)
# # }
# 
# store$MRCH_NM = removePunctuation(store$MRCH_NM, ucp = T) %>% # 刪除全形標點符號
#   removePunctuation() %>% #刪除半形標點符號
#   {gsub("[[:punct:][:blank:]]+","",.)}
# user_top$name = removePunctuation(user_top$name, ucp = T) %>%
#   removePunctuation() %>%
#   {gsub("[[:punct:][:blank:]]+","",.)}
# # 名單模糊比對
# x= sapply(1:nrow(user_top), function(x) agrep(user_top$name[x], store$MRCH_NM, ignore.case = F, value = T, max = 2.5))
# 
# # y = lapply(star_top$name, agrep, store$MRCH_NM)
# # agrep("你好", c("H","你好窩"), ignore.case = F, value = T)

### -------------------------各大榜單資訊-------------------------

# 1. 最熱門:評分數量最高(user)
user = a %>%
  subset(!is.na(user)) %>%
  group_by(place_id) %>%
  slice_max(order_by = date, n = 1) %>% # 看最新那天的成長幅度
  arrange(-user) %>%
  ungroup() %>%
  select(-place_id)

# 查看前20名
user_top = user %>%
  head(20)

write.csv(user_top, "user_top_2.csv")


# 2. 星等高:評論數(user)前30%，分數(rating)最高的前50間
star = a %>%
  # filter(!name %in% user_top$name) %>% # 刪除排序一店名
  subset(!is.na(user) | !is.na(rating)) %>%
  group_by(place_id) %>%
  slice_max(order_by = date, n = 1) %>% # 看最新那天的成長幅度
  arrange(-user) %>%
  ungroup() %>%
  head(nrow(user)*0.3) %>%
  arrange(-rating) %>%
  select(-place_id)

# 查看前50名，貼上"最熱門"標籤
star_top = star %>%
  head(20) %>%
  mutate(label_1 = ifelse(name %in% user_top$name, "最熱門", ""))

# 同時是最熱門與星等高有X間
table(star_top$label_1)

write.csv(star_top, "star_top_2.csv")


# 3. 崛起之王:評分數量快速上升(user_dif)
user_dif = a %>%
  # filter(!name %in% c(user_top$name, user_dif_top$name)) %>% # 刪除排序一二店名
  subset(!is.na(user_dif)) %>%
  select(-place_id) %>%
  arrange(-user_dif)

# 查看前50名，貼上"最熱門"與"星等高"標籤
user_dif_top = user_dif %>%
  head(20) %>%
  mutate(label_1 = ifelse(name %in% user_top$name, "最熱門", "")) %>%
  mutate(label_2 = ifelse(name %in% star_top$name, "星等高", ""))

write.csv(user_dif_top, "user_dif_top_2.csv")


# 崛起之王榜單上，最熱門有0間，星等高有0間，同時是最熱門與星等高有0間
table(user_dif$label_1,user_dif$label_2)

# 4. 進步獎:評論數(user)前30%，評分分數(rating_dif)進步最多，同分比評論數(user)
progress = a %>%
  # filter((!name %in% c(user_top$name, user_dif_top$name, star_top$name))%>% # 刪除排序一二三店名
  subset(!is.na(user) | !is.na(rating_dif))%>%
  select(-place_id) %>%
  arrange(-user) %>%
  head(nrow(user)*0.3) %>%
  arrange(-rating_dif, -user)

# 查看前50名，貼上"進步獎"標籤，並補上"最熱門"與"崛起之王"與"星等高"標籤
progress_top = progress %>%
  head(20) %>%
  mutate(label_1 = ifelse(name %in% user_top$name, "最熱門", "")) %>%
  mutate(label_2 = ifelse(name %in% star_top$name, "星等高", "")) %>%
  mutate(label_3 = ifelse(name %in% user_dif_top$name, "崛起之王", ""))

# 在進步獎榜單中
sum(progress_top$label_1 == '最熱門')
sum(progress_top$label_2 == '星等高')
sum(progress_top$label_3 == '崛起之王')
table(progress_top$label_1, progress_top$label_2, progress_top$label_3)

write.csv(progress_top, "progress_top_2.csv")

### ------------------------資料統計---------------------------

# 台北市區域分布

# # 匯入地理資料，中文亂碼轉換
# twn = readOGR("C:/Users/user/Desktop/爬蟲/google map example", layer="TOWN_MOI_1090727", encoding="UTF-8")
# 
# twn@data$COUNTYNAME = iconv(twn@data$COUNTYNAME, from = "UTF-8", to="UTF-8")
# twn@data$TOWNNAME = iconv(twn@data$TOWNNAME, from = "UTF-8", to="UTF-8")
# 
# # 篩選臺北市地理資料
# twn.taipei = twn[which(twn@data$COUNTYNAME == "臺北市"), ]
# 
# count = as.data.frame(table(b$region)) #統計各地區人數
# count$Freq = as.character(count$Freq)
# twn.taipei@data = merge(twn.taipei@data, count, by.x = "TOWNNAME", by.y = "Var1", sort=FALSE)
# 
# qtm(twn.taipei,fill="Freq", text = "TOWNNAME", text.size = 0.7, fill.title = "台北市各地區店家數量",sort = T, fill.palette="Blues")+

# ggplot(a, aes(x = as.factor(region))) +
#   geom_bar() + 
#   geom_text(stat = "count", aes(label = (..count..)), vjust = -1, color=I("black"), size = 5) +
#   labs(title = "台北市各地區店家數量")


# 1.全部店家(重複保留最新者)之評分數量(user)分布

# 按照評論數量多寡(user)分10組，最後都算第10組
for(i in c(1:nrow(user))){
  user[i,"rank"] = (i-1) %/% (nrow(user) %/% 10) + 1
}
user$rank[which(user$rank == 11)] = 10

# 全部店家評論數量(user)之分組表格
user_form = user %>%
  group_by(rank) %>%
  mutate(max = max(user),
         min = min(user),
         seq = n())%>%
  filter (!duplicated(rank)) %>%
  select(rank,seq,min,max)

# 2.評論數(user)排名前30%之評分(rating)分布圖
ggplot(star, aes(x = rating)) +
  geom_bar(fill = "steelblue3") + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -1, color=I("black"), size = 5) +
  labs(title = "評論數(user)排名前30%之評分(rating)分布圖")

# 評論數(user)排名前30%之分數最高的前50間店之評分(rating)分布圖
# ggplot(star_top, aes(x = rating)) +
#   geom_bar() + 
#   geom_text(stat = "count", aes(label = (..count..)), vjust = -1, color=I("black"), size = 5) +
#   labs(title = "星等高之評分分數(rating)分布圖")

# 3.資料重複的店家之評分數量差異(user_dif)分布

# 按照評分數量差異多寡(user_dif)分10組，最後都算第10組
for(i in c(1:nrow(user_dif))){
  user_dif[i,"rank"] = (i-1) %/% (nrow(user_dif) %/% 10) + 1
}
user_dif$rank[which(user_dif$rank == 11)] = 10

# 資料重複的店家之評分數量差異(user_dif)之分組表格
user_dif_form = user_dif %>%
  group_by(rank) %>%
  mutate(max = max(user_dif),
         min = min(user_dif),
         seq = n())%>%
  filter (!duplicated(rank)) %>%
  select(rank,seq,min,max)

# 4.評論數量(user)排名前30%，先比評論分數差異(rating_dif)，再比評分數量(user)之分布圖
progress$rating_dif = round(progress$rating_dif,1)
ggplot(progress, aes(x = rating_dif)) +
  geom_bar(fill = "steelblue3") + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -1, color=I("black"), size = 5)+
  labs(title = "評論數量(user)排名前30%之評論分數差異(rating_dif)分布圖")

# 評論數量(user)排名前30%，先比評論分數差異(rating_dif)，再比評分數量(user)之前50間分布圖
# progress_top$rating_dif = round(progress_top$rating_dif,1)
# ggplot(progress_top, aes(x = rating_dif)) +
#   geom_bar() + 
#   geom_text(stat = "count", aes(label = (..count..)), vjust = -1, color=I("black"), size = 5)+
#   labs(title = "進步獎之評分數量差異(rating_dif)分布圖")
