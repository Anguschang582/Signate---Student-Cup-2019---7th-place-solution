
library(tidyverse)
library(lightgbm)
library(e1071)
library(moments)
library(entropy)
library(MLmetrics)

set.seed(7777)
options(scipen = 99)
Sys.setlocale("LC_CTYPE", locale="Japanese")


train <- read_csv("train.csv" ) 
test <- read_csv("test.csv")
sample_sub <- read_csv("sample_submit.csv", col_names = F)


# wrong target
add <- add
train <- train[-5776,]

y <- train$賃料
train$賃料 <- NULL
tem <- train %>% bind_rows(test) 


# custom metric for log-transformed
custom_metric <- function(preds, d0) {
  labels <- getinfo(d0, "label")
  score <- as.numeric(RMSE(expm1(preds),expm1(labels)))
  return(list(name = "custom rmse (log_trans) score", value = score, higher_better = FALSE))
}

# Some function
get_parking <- function(data){
  
  data <- data %>% separate(駐車場, into = paste0("a",1:20,sep=""), sep="\t") 
  data <- data[,paste0("a",1:20)] 
  
  a <- rep(NA, nrow(data)) ; b <- rep(NA, nrow(data)) ; c <- rep(NA, nrow(data))
  
  for(i in 1:19){
    a[which(as.vector(unlist(data[,i] == "駐車場")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "駐車場"))),i+1]))
    b[which(as.vector(unlist(data[,i] == "駐輪場")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "駐輪場"))),i+1]))
    c[which(as.vector(unlist(data[,i] == "バイク置き場")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "バイク置き場"))),i+1]))
  }
  return(data.frame(parking_car = a, parking_motor = b, parking_bike = c))
}
# ------------------------------------------------------------------------------------------------------

get_around <- function(data){
  
  data <- data %>% mutate(around = str_remove_all(周辺環境,'m') ) %>% separate(around, into = paste0("a",1:20), sep="([\\】\\\t])") 
  data <- data[,paste0("a",1:20)] 
  
  a=b=c=d=e=f=g=h=i1=j=k=l=m=n=o=p=q=r=rep(NA, nrow(data)) 
  
  for(i in seq(19,1,-2)){
    a[which(as.vector(unlist(data[,i] == "【幼稚園・保育園")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【幼稚園・保育園"))),i+1]))
    b[which(as.vector(unlist(data[,i] == "【コンビニ ")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【コンビニ "))),i+1]))
    c[which(as.vector(unlist(data[,i] == "【ドラッグストア")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【ドラッグストア"))),i+1]))
    d[which(as.vector(unlist(data[,i] == "【公園")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【公園"))),i+1]))
    e[which(as.vector(unlist(data[,i] == "【小学校")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【小学校"))),i+1]))
    f[which(as.vector(unlist(data[,i] == "【大学")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【大学"))),i+1]))
    g[which(as.vector(unlist(data[,i] == "【図書館")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【図書館"))),i+1]))
    h[which(as.vector(unlist(data[,i] == "【総合病院")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【総合病院"))),i+1]))
    i1[which(as.vector(unlist(data[,i] == "【レンタルビデオ")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【レンタルビデオ"))),i+1]))
    j[which(as.vector(unlist(data[,i] == "【病院")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【病院"))),i+1]))  
    # --- 11 ---
    k[which(as.vector(unlist(data[,i] == "【郵便局")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【郵便局"))),i+1]))
    l[which(as.vector(unlist(data[,i] == "【スーパー")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【スーパー"))),i+1]))
    m[which(as.vector(unlist(data[,i] == "【デパート")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【デパート"))),i+1]))
    n[which(as.vector(unlist(data[,i] == "【飲食店")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【飲食店"))),i+1]))
    o[which(as.vector(unlist(data[,i] == "【学校")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【学校"))),i+1]))
    p[which(as.vector(unlist(data[,i] == "【銀行")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【銀行"))),i+1]))
    q[which(as.vector(unlist(data[,i] == "【クリーニング")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【クリーニング"))),i+1]))
    r[which(as.vector(unlist(data[,i] == "【月極駐車場")))] <- as.vector(unlist(data[which(as.vector(unlist(data[,i] == "【月極駐車場"))),i+1]))
  }
  tmp <- data.frame(a,b,c,d,e,f,g,h,i1,j,k,l,m,n,o,p,q,r)
  colnames(tmp) <- paste0("dist_around_",1:ncol(tmp))
  for(ncol in 1:ncol(tmp)){ tmp[,ncol] <- as.numeric(as.character(unlist(tmp[,ncol]))) }
  return(tmp)
}

# ------------------------------------------------------------------------------------------------------

get_year <- function(data){
  data[which(grepl("まで", data$contract_time)), ] <- (
    (  as.numeric(unlist(lapply(str_extract_all(data[which(grepl("ヶ月", data$contract_time)), ]$contract_time , "[0-9]+"), 
                                FUN = function(x){head(x,1)}))) *12  ) + 
      (  as.numeric(unlist(lapply(str_extract_all(data[which(grepl("ヶ月", data$contract_time)), ]$contract_time , "[0-9]+"),
                                  FUN = function(x){tail(x,1)})))      ) - (2018*12 + 6)    ) / 12
  
  data <- data %>% separate(contract_time, into=c("yr","mon"),"年") 
  data[which(grepl("月", data$yr)),]$mon <- data[which(grepl("月", data$yr)),]$yr   
  data[which(grepl("月", data$yr)),]$yr <- NA
  data[which(grepl("月", data$mon)),]$mon <- as.numeric(unlist( str_extract_all( data[which(grepl("月", data$mon)),]$mon  , "[0-9]+") )) / 12
  data[which(grepl("間", data$mon)),]$mon <- NA
  data$yr <- as.numeric(data$yr) ; data$mon <- as.numeric(data$mon)
  
  data$contract_time <- apply(data[,c("yr","mon")], 1,function(x){sum(x,na.rm = T)})                                 
  
  return(data.frame(contract_time = data$contract_time))
}     

# ------------------------------------------------------------------------------------------------------

# check unique function

check_unique <- function(data,feature_name){
  
  # data <- train ; feature_name <- "周辺環境"
  tmp <- data[,feature_name] ; colnames(tmp) <- "feature"
  tmp <- tmp %>% separate(feature , into = paste0("a",1:30)) 
  tmp <- tmp[,paste0("a",1:30)]  
  a <- vector()
  for(i in 1:30){a <- c(a,as.vector(unlist(tmp[,i])))}
  return(table(a))
  
}

# Quick example  
#check_unique(tem,"室???設備")

# ------------------------------------------------------------------------------------------------------
# kmeans for lat and lon
get_kmeans <- function(address){
  set.seed(666666)
  add1 <- address
  add1[is.na(add1)] <- 0
  kmean_data <- data.frame(
    kmean3 = kmeans(add1,3)$cluster,
    kmean5 = kmeans(add1,5)$cluster,
    kmean7 = kmeans(add1,7)$cluster,
    kmean11 = kmeans(add1,11)$cluster,
    kmean15 = kmeans(add1,15)$cluster,
    kmean21 = kmeans(add1,21)$cluster,
    kmean30 = kmeans(add1,30)$cluster,
    kmean50 = kmeans(add1,50)$cluster,
    kmean70 = kmeans(add1,70)$cluster
  )
  return(kmean_data)
}


dist_to_clus <- function(address){
  set.seed(666666)
  add1 <- address
  add1[is.na(add1)] <- 0
  df_dist <- data.frame(0)
  
  for(i in c(3,5,7,11,15,21,30,50,70)){
    km <- kmeans(add1,i)
    cen <- data.frame(km$centers) ; cen$clus = 1:i
    tmp <- data.frame(add1, clus = km$cluster)
    tmp <- left_join(tmp, cen, by = "clus")
    dist <- sqrt(rowSums( (tmp[,1:2] - tmp[,4:5])^2 ))
    df_dist <- data.frame(df_dist,dist) 
  }
  df_dist <- df_dist[,-1]
  colnames(df_dist) <- paste0("dist_to_cluster_", c(3,5,7,11,15,21,30,50,70), sep="" )
  return(df_dist)
}


# -------------------------------------------- main script -------------------------------------------- #

# wrong data
tem[c(20232,20428),"築年数"] <- NA
tem[39953,]$間取り <- "1R" 

tem <- tem %>%
  mutate(txt1 = paste(所在地, 間取り, 築年数, 所在階, sep = " "),
         txt2 = paste(所在地, アクセス, 間取り, 築年数, 方角, 所在階, `バス・トイレ`, キッチン,
                         `放送・通信`, 室内設備, 駐車場, 周辺環境, sep = " "),
         
  )

txt1 <- tem$txt1
txt2 <- tem$txt2

tem <- tem %>%
  mutate(txt1_capE = str_count(txt1, "[A-Z]"),
         txt1_pun = str_count(txt1, "[[:punct:]]"),
         txt1_dig = str_count(txt1, "[[:digit:]]"),
         txt1_word_count = str_count(txt1),
         txt1_kata_zen = str_count(txt1,"([ァ-ン])"),
         txt1_hira = str_count(txt1,"([ｧ-ﾝﾞﾟ])"),
         txt1_kanji = str_count(txt1,"([一-龯])"),
         txt1_kata.and.hira = str_count(txt1,"([ぁ-んァ-ン])"),
         
         
         txt2_capE = str_count(txt2, "[A-Z]"),
         txt2_pun = str_count(txt2, "[[:punct:]]"),
         txt2_dig = str_count(txt2, "[[:digit:]]"),
         txt2_word_count = str_count(txt2),
         txt2_kata_zen = str_count(txt2,"([ァ-ン])"),
         txt2_hira = str_count(txt2,"([ｧ-ﾝﾞﾟ])"),
         txt2_kanji = str_count(txt2,"([一-龯])"),
         txt2_kata.and.hira = str_count(txt2,"([ぁ-んァ-ン])")
  ) %>%
  mutate(location = 所在地) %>%
  separate(所在地, into = c("a","b"), sep = "丁目") %>%
  separate(a, into = c("city","c"), sep = "都" ) %>%
  separate(c, into = c("area","sub_area"), sep = "区") %>%
  mutate(sub_area = str_remove_all(sub_area,'[0-9]+'),
         sub_area = str_remove_all(sub_area,'[:punct:]+')
  ) %>%
  select(-b,-city) %>%
  separate(アクセス, into = c("train_line1","train_station1","c","space",
                          "train_line2","train_station2","c1","space2",
                          "train_line3","train_station3","c2"), sep="\t"
  ) %>%
  mutate(dist1 = as.numeric(unlist(lapply(str_extract(c,"[0-9]+"), FUN = function(x){sum(as.numeric(x))} )) ),
         dist2 = as.numeric(unlist(lapply(str_extract_all(c1,"[0-9]+"), FUN = function(x){sum(as.numeric(x))} )) ),
         dist3 = as.numeric(unlist(lapply(str_extract_all(c2,"[0-9]+"), FUN = function(x){sum(as.numeric(x))} )) )
  ) %>% 
  mutate(room_type_normal = as.numeric(str_extract(間取り,"[0-9]+")),
         room_type_L = ifelse(grepl("L",間取り), 1, 0 ),
         room_type_D = ifelse(grepl("D",間取り), 1, 0 ),
         room_type_K = ifelse(grepl("K",間取り), 1, 0 ),
         room_type_S = ifelse(grepl("S",間取り), 1, 0 )
  ) %>%
  select(-space,-space2,-c,-c1,-c2) %>%
  separate(築年数, into = c("year","month"), sep="年") %>%
  mutate(year = as.numeric(ifelse(year == "新築", 0, year)),
         month = as.numeric(str_remove_all(month, "ヶ月")),
         building_time = year*12 + month
  ) %>% 
  select(-year, -month) %>%
  mutate(square_meter = as.numeric(str_remove(面積, "m2"))
  ) %>% 
  select(-面積) %>%
  separate(所在階, into = c("a","b"), sep="／") %>%
  mutate(floor = as.numeric(str_extract( ifelse(is.na(b), NA, a), "[0-9]+" )),
         num_floor_of_building = as.numeric(str_extract( ifelse(is.na(b), NA, b), "[0-9]+" )),
         single_house_flag = ifelse(is.na(b),1,0)
  ) %>%
  select(-a,-b) %>%
  mutate(bath_toilet_1 = ifelse(grepl("専用トイレ",`バス・トイレ`), 1, 0 ),
         bath_toilet_2 = ifelse(grepl("浴室乾燥機",`バス・トイレ`), 1, 0 ),
         bath_toilet_3 = ifelse(grepl("共同トイレ",`バス・トイレ`), 1, 0 ),
         bath_toilet_4 = ifelse(grepl("シャワー",`バス・トイレ`), 1, 0 ),
         bath_toilet_5 = ifelse(grepl("温水洗浄便座",`バス・トイレ`), 1, 0 ),
         bath_toilet_6 = ifelse(grepl("専用バス",`バス・トイレ`), 1, 0 ),
         bath_toilet_7 = ifelse(grepl("洗面台独立",`バス・トイレ`), 1, 0 ),
         bath_toilet_8 = ifelse(grepl("追焚機能",`バス・トイレ`), 1, 0 ),
         bath_toilet_9 = ifelse(grepl("バス",`バス・トイレ`), 1, 0 ),
         bath_toilet_10 = ifelse(grepl("バスなし",`バス・トイレ`), 1, 0 ),
         bath_toilet_11 = ifelse(grepl("共同バス",`バス・トイレ`), 1, 0 )
  ) %>%
  select(-`バス・トイレ`) %>%
  mutate(num_stove = ifelse(grepl("コンロ1口",キッチン),1, 
                            ifelse(grepl("コンロ2口",キッチン),2, 
                                   ifelse(grepl("コンロ3口",キッチン),3,
                                          ifelse(grepl("コンロ4口以上",キッチン),4, NA
                                          )))),
         kitchen_1 = ifelse(grepl("IHコンロ",キッチン), 1, 0),
         kitchen_2 = ifelse(grepl("給湯",キッチン), 1, 0),
         kitchen_3 = ifelse(grepl("ガスコンロ",キッチン), 1, 0),
         kitchen_4 = ifelse(grepl("電気コンロ",キッチン), 1, 0),
         kitchen_5 = ifelse(grepl("システムキッチン",キッチン), 1, 0),
         kitchen_6 = ifelse(grepl("独立キッチン",キッチン), 1, 0),
         kitchen_7 = ifelse(grepl("カウンターキッチン",キッチン), 1, 0),
         kitchen_8 = ifelse(grepl("冷蔵庫あり",キッチン), 1, 0)
  ) %>%
  select(-キッチン) %>%
  mutate(internet_and_radio_1 = ifelse(grepl("有線放送",`放送・通信`), 1, 0),
         internet_and_radio_2 = ifelse(grepl("インターネット使用料無料",`放送・通信`), 1, 0),
         internet_and_radio_3 = ifelse(grepl("BSアンテナ",`放送・通信`), 1, 0),
         internet_and_radio_4 = ifelse(grepl("CSアンテナ",`放送・通信`), 1, 0),
         internet_and_radio_5 = ifelse(grepl("高速インターネット",`放送・通信`), 1, 0),
         internet_and_radio_6 = ifelse(grepl("CATV",`放送・通信`), 1, 0),
         internet_and_radio_7 = ifelse(grepl("光ファイバー",`放送・通信`), 1, 0),
         internet_and_radio_8 = ifelse(grepl("インターネット対応",`放送・通信`), 1, 0)
  ) %>%
  select(-`放送・通信`) %>%
  mutate(setting_inside_1 = ifelse(grepl("ルーフバルコニー",室内設備), 1, 0),
         setting_inside_2 = ifelse(grepl("水道その他",室内設備), 1, 0),
         setting_inside_3 = ifelse(grepl("24時間換気システム",室内設備), 1, 0),
         setting_inside_4 = ifelse(grepl("フローリング",室内設備), 1, 0),
         setting_inside_5 = ifelse(grepl("エレベーター",室内設備), 1, 0),
         setting_inside_6 = ifelse(grepl("公営水道",室内設備), 1, 0),
         setting_inside_7 = ifelse(grepl("下水",室内設備), 1, 0),
         setting_inside_8 = ifelse(grepl("ウォークインクローゼット",室内設備), 1, 0),
         setting_inside_9 = ifelse(grepl("ロフト付き",室内設備), 1, 0),
         setting_inside_10 = ifelse(grepl("出窓",室内設備), 1, 0),
         setting_inside_11 = ifelse(grepl("シューズボックス",室内設備), 1, 0), 
         setting_inside_12 = ifelse(grepl("プロパンガス",室内設備), 1, 0),
         setting_inside_13 = ifelse(grepl("エアコン付",室内設備), 1, 0),
         setting_inside_14 = ifelse(grepl("クッションフロア",室内設備), 1, 0),
         setting_inside_15 = ifelse(grepl("タイル張り",室内設備), 1, 0),
         setting_inside_16 = ifelse(grepl("2面採光",室内設備), 1, 0),
         setting_inside_17 = ifelse(grepl("室内洗濯機置場",室内設備), 1, 0),
         setting_inside_18 = ifelse(grepl("室外洗濯機置場",室内設備), 1, 0),
         setting_inside_19 = ifelse(grepl("床暖房",室内設備), 1, 0),
         setting_inside_20 = ifelse(grepl("床下収納",室内設備), 1, 0),
         setting_inside_21 = ifelse(grepl("石油暖房",室内設備), 1, 0),
         setting_inside_22 = ifelse(grepl("洗濯機置場なし",室内設備), 1, 0),
         setting_inside_23 = ifelse(grepl("都市ガス",室内設備), 1, 0),
         setting_inside_24 = ifelse(grepl("敷地内ごみ置き場",室内設備), 1, 0),
         setting_inside_25 = ifelse(grepl("冷房",室内設備), 1, 0),
         setting_inside_26 = ifelse(grepl("ガス暖房",室内設備), 1, 0),
         setting_inside_27 = ifelse(grepl("バルコニー",室内設備), 1, 0)
  ) %>%
  select(-室内設備) %>%
  mutate(spec_contract_flag = ifelse(grepl("定期借家",契約期間), 1, 0),
         time_limit_flag = ifelse(grepl("まで",契約期間), 1, 0)
  ) %>%
  separate(契約期間, into=c("contract_time","b"),sep="\t") %>%
  select(-b) %>%
  select(-txt1,-txt2,-id)


tem <- data.frame(tem, get_parking(tem), get_around(tem), get_year(tem), get_kmeans(add[,-3]), dist_to_clus(add[,-3]))
tem <- tem %>% select(-contract_time,-駐車場,-周辺環境) %>% bind_cols(geo1) %>%
  mutate(room_total = apply( tem[,c("room_type_normal","room_type_L","room_type_D","room_type_K","room_type_S")], 1, 
                             function(x){sum(x,na.rm = T)}  ),
         bath_total = apply( tem[,paste0("bath_toilet_",c(1:2,4:8))], 1, function(x){sum(x,na.rm = T)}   ),
         square_meter_per_room = square_meter / room_total ,
         square_meter_per_room_v2 = square_meter / room_type_normal ,
         est_price = square_meter * near_500m_mean_h31_landprice
         
  )

colnames(tem)[9:11] <- c("room_type","direction_facing","type_constructing")


