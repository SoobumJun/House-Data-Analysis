#### package ####

install.packages("ggmap")
install.packages("ggplot2")
install.packages("readxl")
install.packages("data.table")
install.packages("DT")

install.packages("psych")
install.packages("Hmisc")

install.packages("agricolae")
install.packages("rvest")

install.packages("corrplot")
install.packages("lm.beta")
install.packages("mlbench")


library(ggplot2)
library(ggmap)
library(readxl)
library(data.table)
library(DT)

library(psych)
library(Hmisc)

library(agricolae)
library(rvest)
library(corrplot)
library(lm.beta)
library(mlbench)

rm(list=ls())

# 작업공간 설정
getwd()
setwd("D:/R")

# 데이터 읽어오기
kc_house <- readxl::read_excel(path      = "D:/KC_house_data.xlsx",
                               sheet     = 1,
                               col_names = TRUE)

#### data summary ####
str(kc_house)
head(kc_house)
summary(kc_house)

#### 미국집값을 시계열 자료로 보여주기 ####
head(sort(date_numeric))                    # 20140502 ~ (31+28+31+30+2 = 122) 365-122=243
head(sort(date_numeric, decreasing = TRUE)) # ~ 20150527 (31+28+31+30+27= 147) 365-147=218
365/2
house_price_ts <- ts(kc_house$price, start = c(2014, 182), end = c(2015, 182), frequency = 365)
plot(house_price_ts, ylab = "Kingcounty House Price", xlab = "Year.Month",
     xlim = c(2014.5, 2015.5))
# 이건 의미없어보임
house_price_ts <- ts(iqr_price, frequency = 365)
plot(house_price_ts, ylab = "Kingcounty House Price", xlab = "Year.Month")

par(mfrow = c(1,2))
acf(house_price_ts) # 파란색 점선 밑에 그래프가 있어야 유의한것이다!
pacf(house_price_ts) 
ndiffs(house_price_ts) # 0

# 아주 강력한 function! auto.arima
houseBest <- auto.arima(x = house_price_ts)
houseBest   

par(mfrow = c(1,1))
forecast(houseBest, h = 5) -> houseforecast # 80%, 95% 신뢰구간이 같이 나옴
plot(houseforecast)







#### 정규성 검정 ####

# by(A, B, shapiro.test) # B에 있는 모든 집단의 A값에 대해 정규성 검정을 한다
A <- by(kc_house$price, kc_house$zipcode, shapiro.test)

## p-value > 0.05인 zipcode group

###
#   kc_house$zipcode: 98002
# 
#       Shapiro-Wilk normality test
# 
# data:  dd[x, ]
# W = 0.99639, p-value = 0.9243
###
# kc_house$zipcode: 98108
# 
#       Shapiro-Wilk normality test
# 
# data:  dd[x, ]
# W = 0.99176, p-value = 0.3707
###




#### all_data correlation ####
# 변수들 간에 상관관계 -> 모든 변수를 numeric으로 만들어야 가능!

# date 변수를 numeric으로 바꾸기
date_numeric <- substr(kc_house$date, 1, 8) # 20141124 형태로 연도날짜 문자부분만 추출
is.numeric(date_numeric)      # FALSE

date_numeric <- as.numeric(date_numeric)
is.numeric(date_numeric)      # TRUE

kc_house$date <- date_numeric # 기존의 date를 새로운 numeric date로 대체
View(kc_house)

house_cor <- cor(kc_house)    # 모든 변수들의 서로서로 간의 상관관계를 계산 

#
library(psych)
house_cortest <- psych::corr.test(kc_house)
DT::datatable(house_cortest)
View(house_cortest)

library(Hmisc)
Hmisc::rcorr(as.matrix(attitude))

round(house_cor, 2)           # 소숫점 둘째자리까지 round
pairs(house_cor, 
      pch = 19, 
      bg  = c("red", "green", "blue"))  # 행렬모양 산점도
corrplot(house_cor)
# 상관원계수가 클수록 크기가 크고 색깔이 진하다
# 양수면 파란색, 음수면 붉은색
corrplot(house_cor, method = "number")    # 수와 색깔로 표현
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(house_cor, 
         method = "color",      # 색깔로 표현
         col    = col(200),     # 색상 200개 선정
         type   = "lower",      # 왼쪽 아래 행렬만 표기
         order  = "hclust",     # 유사한 상관계수끼리 군집화
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black",      # 변수명 색깔  
         tl.srt = 45,           # 변수명 45도 기울임
         diag   = FALSE)            # 대각행렬 제외


# 종속변수 : price
summary(kc_house$price)
#   Min. 1st Qu.  Median   Mean  3rd Qu.   Max. 
# 75000  321950  450000  540088  645000 7700000 

# high, low 25% house price
high_25_price <- kc_house[kc_house$price >= 645000, "price"]  # list
low_25_price  <- kc_house[kc_house$price <= 321950, "price"]  # list

# IQR(Q1~Q3) house price
iqr_price <- kc_house[(kc_house$price<=645000) & (kc_house$price>=321950), "price"] # list

# list -> numeric 변환하기
as.numeric(iqr_price)  # 안돼!
iqr_price_numeric <- iqr_price[[1]]   # 첫번째 row의 모든 숫자를 double로 바꾸어줌!
typeof(iqr_price_numeric) # double

high_25_price[[1]]
low_25_price[[1]]

hist(iqr_price_numeric)
boxplot(iqr_price_numeric)

# 상관관계가 높은 변수들의 산점도 ( > 0.5)
# sqft_lot15    - sqft_lot      : 0.72
# bathrooms     - floors        : 0.50
# bathrooms     - yr_built      : 0.51  #
# bathrooms     - bedrooms      : 0.52
# price         - bathrooms     : 0.53  #
# sqft_living15 - bathrooms     : 0.57
# sqft_living15 - price         : 0.59
# grade         - bathrooms     : 0.66  #
# grade         - price         : 0.67  #
# grade         - sqft_living15 : 0.71  #
# sqft_living   - bedrooms      : 0.58
# sqft_living   - bathrooms     : 0.75
# sqft_living   - price         : 0.70
# sqft_living   - sqft_living15 : 0.76
# sqft_living   - grade         : 0.76  #
# sqft_above    - floors        : 0.52
# sqft_above    - bathrooms     : 0.69
# sqft_above    - price         : 0.61
# sqft_above    - sqft_living15 : 0.73
# sqft_above    - grade         : 0.76  #
# sqft_above    - sqft_living   : 0.88
# zipcode       - long          : -0.56 #


# bathrooms     - yr_built      : 0.51  #
# price         - bathrooms     : 0.53  #
# grade         - bathrooms     : 0.66  #
# grade         - price         : 0.67  #
# grade         - sqft_living15 : 0.71  #
# sqft_living   - grade         : 0.76  #
# sqft_above    - grade         : 0.76  #
# zipcode       - long          : -0.56 #

#
plot(kc_house$bathrooms, kc_house$yr_built)  # 최근에 지어진 집일수록 화장실수가 많다
plot(kc_house$price, kc_house$bathrooms)     # 가격이 높을수록 화장실수가 많다
plot(kc_house$grade, kc_house$bathrooms)     # grade가 높을수록 화장실 수가 많다
plot(kc_house$grade, kc_house$price)         # grade가 높을수록 가격이 높다
plot(kc_house$grade, kc_house$sqft_living15) # 2015년 living면적이 높을수록 grade가 높다
plot(kc_house$sqft_living, kc_house$grade)   # living 면적이 높을수록 grade가 높다
plot(kc_house$sqft_above, kc_house$grade)    # 지상층 면적이 높을수록 grade가 높다
plot(kc_house$zipcode, kc_house$long)        # zipcode가 높을수록 대체로 경도가 낮다



#### google map ####

# 집의 위치를 구글지도에 뿌리기
summary(kc_house$long)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -122.5  -122.3  -122.2  -122.2  -122.1  -121.3 

summary(kc_house$lat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 47.16   47.47   47.57   47.56   47.68   47.78 

# high 25% price house "location"
high_25_loc <- kc_house[kc_house$price >= 645000, c("long", "lat")]
#  low 25% price house "location"
low_25_loc  <- kc_house[kc_house$price <= 321950, c("long", "lat")]

house_map <- get_googlemap(center = c(lon = -122.1, lat = 47.5),
                           zoom = 10) %>% ggmap
house_map + ggplot2::geom_point(data = low_25_loc,
                                aes(x = long, y = lat),
                                colour="red") + ggplot2::geom_point(
                                  data = high_25_loc,
                                  aes(x = long, y = lat),
                                  colour="orange")



#### data anlaysis ####

# kc_house data : data.frame -> data.table로 변환
as.data.table(kc_house)
View(kc_house)


#### 1. 주택이 오래된 정도 - price : 상관관계 ####
# 주택이 지어진 정도는 가격과 상관관계가 없다고 볼 수 있다
summary(kc_house$yr_built)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1900    1951    1975    1971    1997    2015

house_age = 2017 - kc_house$yr_built
summary(house_age)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   20.00   42.00   45.99   66.00  117.00 

cor(house_age, kc_house$price)  # -0.054
plot(house_age, kc_house$price)


#### 2. renovate의 여부에 따른 price 차이 : 양측 가설검정, 상관관계 ####
# renovate 된 group의 집값이 renovate 안된 group의 집값보다 높다
renovate_house <- kc_house[kc_house$yr_renovated != 0, "price"] # renovate 된 집들의 가격
not_renovate_house <- kc_house[kc_house$yr_renovated == 0, "price"] # renovate 안된 집들의 가격

t.test(renovate_house[[1]], not_renovate_house[[1]])
# p-value < 2.2e-16 이므로 두그룹의 평균은 같지 않다.(H0 기각)
#     mean of x   mean of y 
#     760379.0    530360.8 


#### 3. zipcode(98001~98119) group 별 price 차이 : ANOVA ___________진행중 ####
#   지도에 뿌려보고 group으로 묶을 수 있는지

# zipcode group (1~10, 11~20, 21~30, 31~40, 41~50)
zipcode_1_10_loc  <- kc_house[(kc_house$zipcode >= 98001)&(kc_house$zipcode <= 98010), c("long", "lat")]
zipcode_11_20_loc <- kc_house[(kc_house$zipcode >= 98011)&(kc_house$zipcode <= 98020), c("long", "lat")]
zipcode_21_30_loc <- kc_house[(kc_house$zipcode >= 98021)&(kc_house$zipcode <= 98030), c("long", "lat")]
zipcode_31_40_loc <- kc_house[(kc_house$zipcode >= 98031)&(kc_house$zipcode <= 98040), c("long", "lat")]
zipcode_41_50_loc <- kc_house[(kc_house$zipcode >= 98041)&(kc_house$zipcode <= 98050), c("long", "lat")]

house_map <- get_googlemap(center = c(lon = -122.1, lat = 47.5),
                           zoom = 10) %>% ggmap

house_map + ggplot2::geom_point(data = zipcode_1_10_loc,
                                aes(x = long, y = lat),
                                colour="red") + 
  ggplot2::geom_point(data = zipcode_11_20_loc,
                      aes(x = long, y = lat),
                      colour="orange") + 
  ggplot2::geom_point(data = zipcode_21_30_loc,
                      aes(x = long, y = lat),
                      colour="yellow") + 
  ggplot2::geom_point(data = zipcode_31_40_loc,
                      aes(x = long, y = lat),
                      colour="green") + 
  ggplot2::geom_point(data = zipcode_41_50_loc,
                      aes(x = long, y = lat),
                      colour="blue")

# zipcode group (1로끝나는애들, 2로끝나는애들)
zipcode__1_loc <- kc_house[kc_house$zipcode == grep("1$", kc_house$zipcode, value = TRUE), c("long", "lat")]
zipcode__2_loc <- kc_house[kc_house$zipcode == grep("2$", kc_house$zipcode, value = TRUE), c("long", "lat")]
zipcode__3_loc <- kc_house[kc_house$zipcode == grep("3$", kc_house$zipcode, value = TRUE), c("long", "lat")]

house_map + ggplot2::geom_point(data = zipcode__1_loc,
                                aes(x = long, y = lat),
                                colour="red") + 
  ggplot2::geom_point(data = zipcode__2_loc,
                      aes(x = long, y = lat),
                      colour="orange") + 
  ggplot2::geom_point(data = zipcode__3_loc,
                      aes(x = long, y = lat),
                      colour="yellow")

# zipcode >= 98100 group plot
plot(kc_house$zipcode, kc_house$lat)
par(new = T)
plot(kc_house$zipcode, kc_house$long, col="red")

zipcode_98001_loc<- kc_house[kc_house$zipcode <= 98100, c("long", "lat")]
zipcode_98100_loc<- kc_house[kc_house$zipcode >= 98100, c("long", "lat")]

house_map + ggplot2::geom_point(data = zipcode_98001_loc,
                                aes(x = long, y = lat),
                                colour="red") + 
  ggplot2::geom_point(data = zipcode_98100_loc,
                      aes(x = long, y = lat),
                      colour="orange")

# 해안가 집의 zipcode
sort(table(kc_house[kc_house$waterfront == 1, "zipcode"]), decreasing = TRUE)
# 70,166,40,75,198

# price 상위 25% 이상인 집의 zipcode
sort(table(kc_house[kc_house$price >= 645000, "zipcode"]), decreasing = TRUE)
# 6,4,75,40,52,33,74



#### 3-1. zipcode 별로 학교수 차이가 가격에 영향을 미치나?_______진행중 ####
kc_house <- readxl::read_excel(path      = "kc_house_data.xlsx",
                               sheet     = 1,
                               col_names = TRUE)

zipcode_school <- readxl::read_excel(path      = "zipcode_school.xlsx",
                                     sheet     = 1,
                                     col_names = TRUE)
str(zipcode_school)
head(zipcode_school)
kc_house_DT <- as.data.table(kc_house)
# 원데이터 kc_house와 zipcode_school 데이터를 join 해서 kc_house에 넣어줌
kc_house <- merge(kc_house, zipcode_school, by = "zipcode", all = TRUE)
View(kc_house)

# 학교수 내림차순으로 zipcode 정렬
x <- kc_house[order(kc_house$school_to, decreasing = TRUE) , c("zipcode","school_to")]
unique(x) # 중복값 제거

# price 내림차순으로 zipcode 정렬
kc_house[order(kc_house$price, decreasing = TRUE) , c("price","zipcode")]

# zipcode 별 price의 평균을 group화
kc_house_DT <- as.data.table(kc_house)
zipcode_price_group <- kc_house_DT[ , list(n = .N,
                                           Mean = mean(price)),
                                    by = list(zipcode)]

zipcode_price_group <- kc_house_DT[ , list(n = .N,
                                           Mean = mean(price)),
                                    by = list(zipcode, 
                                              school_el, school_mi,
                                              school_hi, school_to)]
head(zipcode_price_group)
cor(zipcode_price_group[,2][[1]], zipcode_price_group[,7][[1]]) # el - price  # -0.2
cor(zipcode_price_group[,3][[1]], zipcode_price_group[,7][[1]]) # mi - price  # -0.2
cor(zipcode_price_group[,4][[1]], zipcode_price_group[,7][[1]]) # hi - price  # -0.3
cor(zipcode_price_group[,5][[1]], zipcode_price_group[,7][[1]]) # to - price  # -0.3

cor.test(zipcode_price_group[,2][[1]], zipcode_price_group[,7][[1]]) # el - price  # -0.2
cor.test(zipcode_price_group[,3][[1]], zipcode_price_group[,7][[1]]) # mi - price  # -0.2
cor.test(zipcode_price_group[,4][[1]], zipcode_price_group[,7][[1]]) # hi - price  # -0.3
cor.test(zipcode_price_group[,5][[1]], zipcode_price_group[,7][[1]]) # to - price  # -0.3

zip_group_i <- for(i in 1:70){
  print(zipcode_price_group[i,])
}





#### 4. 크기에 대한 변수의 group에 대한 price 차이 : 상관관계 분석 ####
# bedrooms, bathrooms, floors 데이터 모델링 하기

cor(kc_house$bedrooms, kc_house$price)    # 0.308
cor(kc_house$bathrooms, kc_house$price)   # 0.525
cor(kc_house$floors, kc_house$price)      # 0.257
cor(kc_house$sqft_living, kc_house$price) # 0.702
cor(kc_house$sqft_lot, kc_house$price)    # 0.090  # 제외!

0.308 + 0.525 + 0.257 + 0.702 = 1.792
kc_house$room_newnum = kc_house$bedrooms*0.308 + kc_house$bathrooms*0.525 + kc_house$floors*0.257 + kc_house$sqft_living*0.702
# kc_house$room_rum_2 = kc_house$bedrooms*0.308/1.792 + kc_house$bathrooms*0.525/1.792 + kc_house$floors*0.257/1.792 + kc_house$sqft_living*0.702/1.792

cor(kc_house$room_newnum, kc_house$price)  # 0.702

#
# names(kc_house)
# RgFit <- lm(Y ~ X, data = R1)
# # (Intercept)     bedrooms    bathrooms  sqft_living     sqft_lot       floors  
# #   8.066e+04   -5.953e+04    6.958e+03    3.143e+02   -3.788e-01   -1.758e+03 
# C <- -5.953e+04 + 6.958e+03 + 3.143e+02 + -3.788e-01 + -1.758e+03
# c <- c(-5.953e+04/C, 6.958e+03/C, 3.143e+02/C, -3.788e-01/C, -1.758e+03/C)
# c[1]+c[2]+c[3]+c[4]+c[5]
# 
# reg_rum <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors, data = kc_house)
# summary(reg_rum)
# 
# kc_house$bedrooms
# kc_house$bathrooms
# kc_house$sqft_living
# kc_house$sqft_lot
# kc_house$floors



#### 5. waterfront 1, 0 에 따른 price 차이 : 양측 가설검정 ####
# 해안가(=1)group의 price  >  해안가가 아닌(=0) group의 price
waterfront_T <- kc_house[kc_house$waterfront == 1, c("long", "lat")]
waterfront_F <- kc_house[kc_house$waterfront == 0, c("long", "lat")]

house_map <- get_googlemap(center = c(lon = -122.1, lat = 47.5), zoom = 10) %>% ggmap
house_map + ggplot2::geom_point(data = waterfront_T,
                                aes(x = long, y = lat),
                                colour="red") + ggplot2::geom_point(
                                  data = waterfront_F,                   
                                  aes(x = long, y = lat),
                                  colour="orange")
waterfront_T_price <- kc_house[kc_house$waterfront == 1, "price"]
waterfront_F_price <- kc_house[kc_house$waterfront == 0, "price"]
summary(waterfront_T_price[[1]])
summary(waterfront_F_price[[1]])
t.test(waterfront_T_price[[1]], high_25_price[[1]])
boxplot(waterfront_T_price[[1]], high_25_price[[1]])

#### 6. 팔린날짜(봄/여름/가을/겨울) 별 price 차이 : ANOVA ####

# ANOVA 
# Groups, Treatments and means
#     a     1(봄)      552600 
#     ab     2(여름)    546800 
#     bc     3(가을)     530800 
#     c     4(겨울)     519200 

# t.test p-value
#       여름     가을       겨울
# 봄    0.370    0.001865   4.94e-06
# 여름  0.02386  0.0001824
# 가을  0.1365

# date "20141013T000000" -> data_numeric "20141013"
head(date_numeric)

# 새로운 변수 생성
kc_house$date_numeric = date_numeric

# 집이 팔린 날짜가
# 봄(3,4,5월) / 여름(6,7,8월) / 가을(9,10,11월) / 겨울(12,1,2월) 별로 price 그룹화
spr_price <- kc_house[grep("....03..|....04..|....05..", date_numeric), "price"]
sum_price <- kc_house[grep("....06..|....07..|....08..", date_numeric), "price"]
fal_price <- kc_house[grep("....09..|....10..|....11..", date_numeric), "price"]
win_price <- kc_house[grep("....12..|....01..|....02..", date_numeric), "price"]

mean(spr_price[[1]]) # 552603.2
mean(sum_price[[1]]) # 546782.0
mean(fal_price[[1]]) # 530846.5
mean(win_price[[1]]) # 519221.2

length(spr_price[[1]]) + length(sum_price[[1]]) + length(fal_price[[1]]) + length(win_price[[1]]) 
# 21613  "전체를 다 가져왔는지 확인" OK

seson_price <- c(spr_price[[1]], sum_price[[1]], fal_price[[1]], win_price[[1]])

group <- c(rep(1, length(spr_price[[1]])),
           rep(2, length(sum_price[[1]])),
           rep(3, length(fal_price[[1]])),
           rep(4, length(win_price[[1]])))
length(group) # 21613

cbind(seson_price, group)
boxplot(seson_price ~ group)
describe.by(seson_price, group)  # 그룹별 기술통계량 계산
ANO_R<-aov(seson_price ~ group)
anova(ANO_R)
scheffe.test(ANO_R, "group", alpha = 0.05, console = TRUE)
LSD.test(ANO_R, "group", alpha = 0.05, console = TRUE)
duncan.test(ANO_R, "group", alpha = 0.05, console = TRUE)

t.test(fal_price[[1]], win_price[[1]])





#### 7. 집 보러온 횟수 - price : ANOVA ####
# 집 보러온 횟수가 높은 집일수록 집값이 높다
# 0회 < 1회=2회 < 3회 < 4회
view0_price <- kc_house[kc_house$view == 0,"price"]
length(view0_price[[1]]) # 19489

view1_price <- kc_house[kc_house$view == 1,"price"]
length(view1_price[[1]]) # 332

view2_price <- kc_house[kc_house$view == 2,"price"]
length(view2_price[[1]]) # 963

view3_price <- kc_house[kc_house$view == 3,"price"]
length(view3_price[[1]]) # 510

view4_price <- kc_house[kc_house$view == 4,"price"]
length(view4_price[[1]]) # 319

mean(view0_price[[1]])    #  496564.2
mean(view1_price[[1]])    #  812280.8
mean(view2_price[[1]])    #  792400.9
mean(view3_price[[1]])    #  971965.3
mean(view4_price[[1]])    # 1463711

view_price <- c(view0_price[[1]], view1_price[[1]], view2_price[[1]], view3_price[[1]], view4_price[[1]])

length(view_price)  # 21613
length(group)       # 21613

group <- c(rep(0, 19489),
           rep(1, 332),
           rep(2, 963),
           rep(3, 510),
           rep(4, 319))
cbind(view_price, group)
boxplot(view_price ~ group)
describe.by(view_price, group)  # 그룹별 기술통계량 계산  # mad
ANO_R <- aov(view_price ~ group)
anova(ANO_R)
library(agricolae)
scheffe.test(ANO_R, "group", alpha = 0.05, console = TRUE)
LSD.test(ANO_R, "group", alpha = 0.05, console = TRUE)
duncan.test(ANO_R, "group", alpha = 0.05, console = TRUE)


#### 8. 지하층/지상층 면적과 price와의 관계 : 비율검정, 상관분석_____진행중 ####




#### grade, condition 등을 factor로 변환하기_____진행중 ####


#### condition - grade 어떤 차이가 있는지_____진행중 ####

# 미국 서브프라임 모기지 사태(2007.04) 때 지어진 집의 가격 - 전체 price 차이가 있을까?
b_2006_price <- kc_house[kc_house$yr_built == 2006, "price"][[1]]
mean(b_2006_price) # 630880.1  
b_2008_price <- kc_house[kc_house$yr_built == 2008, "price"][[1]]
mean(b_20078_price) # 641903.8 
t.test(b_2006_price, b_2008_price) # p-value = 0.7205 "유의미한 차이가 없다"

b_2007_price <- kc_house[kc_house$yr_built >= 2007, "price"][[1]]
mean(b_2007_price) # 619978.6
a_2007_price <- kc_house[kc_house$yr_built < 2007, "price"][[1]]
mean(a_2007_price) # 530781.8
t.test(b_2007_price, a_2007_price) # p-value < 2.2e-16

b_2008_price <- kc_house[kc_house$yr_built >= 2008, "price"][[1]]
mean(b_2007_price) # 621107.1   
a_2008_price <- kc_house[kc_house$yr_built < 2008, "price"][[1]]
mean(a_2008_price) # 532557.8
t.test(b_2008_price, a_2008_price) # p-value < 2.2e-16

b_79_price <- kc_house[(kc_house$yr_built >= 2007)&(kc_house$yr_built <= 2008), "price"][[1]]
mean(b_79_price) # 627596.4   
e_79_price <- kc_house[(kc_house$yr_built < 2007)|(kc_house$yr_built > 2008), "price"][[1]]
mean(e_79_price) # 536794.3
t.test(b_79_price, e_79_price) # p-value = 2.932e-08
