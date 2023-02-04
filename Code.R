#BUSINESS TASK: DETERMINE THE BEST FRUITS OR VEGETABLES A FARMER IN UK CAN PLANT USING THE PRICES OF FRUITS IN 2017 - 2019.
##########################################
#THE DATA USED IN THIS ANALYSIS WAS GOTTEN FROM KAGGLE.COM
##########################################
#STEP 1: LOAD DATA
fruits<-read_csv("fruits.csv")
#inspect the data
head(fruits)
str(fruits)
summary(fruits)
colnames(fruits)
#STEP 2 DATA PREPARATION
#AS SEASONS MAY AFFECT PRICES,I CREATED A COLUMN FOR MONTH SO ANALYSIS CAN BE DONE BY SEASONS
fruits$month<-format(as.Date(fruits$date),"%m")
fruits$year<-format(as.Date(fruits$date),"%Y")
#NOW THAT WE HAVE CREATED A COLUMN FOR MONTH, WE CREATE ANOTHER COLUMN FOR SEASONS TO GROUP THE MONTHS INTO SEASONS.
fruits$season<- sapply(fruits$month, switch,"03" = 'Spring', "04" = 'Spring',"05"='Spring',"06"='Summer',"07"='Summer',
"08"='Summer',"09"='Autumn',"10"='Autumn',"11"='Autumn',"12"='Winter',"01"='Winter',"02"='Winter')
#STEP 3 DATA ANALYSIS
#CALCULATE THE AVERAGE FRUIT PRICE
Average_fruit_price<- fruits%>% group_by(item) %>%
summarise(average_price=mean(price),.groups='drop')
###FIND HIGHEST SELLING FRUIT PER YEAR USING AVERAGE
#CREATE DATAFRAMES FOR THE DIFFERENT YEARS FILTER
fruit_2017<-fruits%>%
filter(year == 2017)
fruit_2018<-fruits%>%
filter(year == 2018)
fruit_2019<-fruits%>%
filter(year == 2019)
###################################
##CALCULATE AVERAGE
Average_price_2017<- fruit_2017%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
Average_price_2018<- fruit_2018%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
Average_price_2019<- fruit_2019%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
####FIND HIGHEST SELLING FRUIT PER SEASON USING AVERAGE
#CREATE DATAFRAMES FOR DIFFERNT SEASONS
spring<-fruits%>%
filter(season == "Spring")
summer<-fruits%>%
filter(season == "Summer")
autumn<-fruits%>%
filter(season == "Autumn")
winter<-fruits%>%
filter(season == "Winter")
#####CALCULATE AVERAGE PER SEASON
Average_price_spring<- spring%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
Average_price_summer<- summer%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
Average_price_autumn<- autumn%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
Average_price_winter<- winter%>% group_by(item)%>%
summarise(average_price=mean(price),.groups='drop')
#STEP 4 DATA VISUALIZATION
##########################
#PLOT GRAPH TO SHOW HIGHEST PRICED FRUITS/VEGETABLES BY SEASON,YEAR AND OVER THE PAST FIVE YEARS
#HIGHEST PRICED FRUITS/VEGETABLES FROM 2017-2019
p<- Average_fruit_price%>%
filter(average_price > 5) %>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits 2017 - 2019")+ theme(plot.title = element_text(hjust=0.5))
q<-Average_price_2017%>%
filter(average_price > 3)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits 2017")+ theme(plot.title = element_text(hjust=0.5))
r<-Average_price_2018%>%
filter(average_price > 5)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits 2018")+ theme(plot.title = element_text(hjust=0.5)) 
s<-Average_price_2019%>%
filter(average_price > 5)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits 2019")+ theme(plot.title = element_text(hjust=0.5))
########HIGHEST PRICED FRUITS PER SEASON
v<- Average_price_summer%>%
filter(average_price > 5)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits in the summer")+ theme(plot.title = element_text(hjust=0.5))
w<- Average_price_winter%>%
filter(average_price > 3)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits in the winter")+ theme(plot.title = element_text(hjust=0.5))
x<- Average_price_spring%>%
filter(average_price > 5)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits in the spring")+ theme(plot.title = element_text(hjust=0.5))
y<- Average_price_autumn%>%
filter(average_price > 5)%>%
ggplot(aes(x =item, y=average_price,fill=item))+ geom_bar(stat="identity")+ ggtitle("Highest priced fruits in the autumn")+ theme(plot.title = element_text(hjust=0.5))
#NOW TO SAVE THE PLOTS AND GET FURTHER INSIGHTS FROM THE DATA.
#TO NAVIGATE THE DATA INSIGHTS
#use ls() to retrieve all datasets
##KEY###################VALUE#######
##p#######Highest priced fruits 2017 -2019
##q#######Highest priced fruits 2017
##r#######Highest priced fruits 2018
##s#######Highest priced fruits 2019
##v#######Highest priced fruits in the summer
##w#######Highest priced fruits in the winter
##x#######Highest priced fruits in the spring
##y#######Highest priced fruits in the autumn
