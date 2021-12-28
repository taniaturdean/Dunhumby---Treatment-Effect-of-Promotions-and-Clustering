# import libraries
install.packages("ggridges")
install.packages("ggmap")
install.packages("dygraphs")
install.packages("viridis")
install.packages("maps")
install.packages("scatterplot3d") # Install
library(readxl)
library(ggplot2)
library(dplyr)
library(ggridges)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(viridis)
library(broom)
library(maps)
library(bayesm)
library(scatterplot3d)

#2.2 DATA PRE-PROCCESING

#check for duplicates in stores ids
duplicated(stores$STORE_ID)

#select only upc and category from products
products<-products[,c(1,4)]

#merge the data
trans_prod <- left_join(transactions,products,by=c('UPC'='UPC'))
table(trans_prod$CATEGORY)

# merge transactions and products to stores
trans_p_s <- left_join(trans_prod,stores[!duplicated(stores$STORE_ID),],by =c('STORE_NUM'='STORE_ID'))


# we select cold cereal, has the most transactions
trans_p_s_new<-trans_p_s[trans_p_s$CATEGORY == 'COLD CEREAL', ] 

#select the columns we need
cities<-uscities
cities<-cities[,c(1,3,7,8)]

# see city names in cities and stores
unique(stores[c("ADDRESS_CITY_NAME")])
unique(cities[c("city")])


#write cities in cities with capital letterscities$city<-toupper(cities$city)
cities$city<-toupper(cities$city)


# merged cities to our data
test_c <- left_join(trans_p_s_new,cities,by =c('ADDRESS_CITY_NAME'='city','ADDRESS_STATE_PROV_CODE'='state_id'))


#missing values

#see which columns have nas
map(test_c, ~sum(is.na(.)))


#replace with 0 for parking space because it means it doesn exist
test_c <- mutate_at(test_c, c("PARKING_SPACE_QTY"), ~replace(., is.na(.), 0))



#fill missing nas for price = base price (2 nas) because they had no promotion

test_c$PRICE <- ifelse(is.na(test_c$PRICE), test_c$BASE_PRICE, test_c$PRICE)

#remove 10 rows - nas for base_price
test_c<- test_c[!is.na(test_c$BASE_PRICE),]

#extract rows with na
nas<-test_c[rowSums(is.na(test_c)) > 0, ]   

#see which cities are missing
unique(nas[c("ADDRESS_CITY_NAME")])

#add rows for KINGWOOD, CYPRESS, WOODLANDS TX,  in cities (we googled their coordinates and population) - added 3 rows manually 
#note - import uscities_new
cities_new<- uscities_new

#select columns
cities_new<-cities_new[,c(1,3,7,8)]

#write cities in cities with capital letters
cities_new$city<-toupper(cities_new$city)

#rename  st marys, west chester in cities
cities_new$city[cities_new$city == "ST. MARYS"] <- "SAINT MARYS" 
cities_new$city[cities_new$city == "OLDE WEST CHESTER"] <- "WEST CHESTER" 

# merged cities to our data
test_c <- left_join(trans_p_s_new,cities_new,by =c('ADDRESS_CITY_NAME'='city','ADDRESS_STATE_PROV_CODE'='state_id'))

#replace with 0 for parking space because it means it doesn exist
test_c <- mutate_at(test_c, c("PARKING_SPACE_QTY"), ~replace(., is.na(.), 0))


#fill missing nas for price = base price (2 nas) because they had no promotion
test_c$PRICE <- ifelse(is.na(test_c$PRICE), test_c$BASE_PRICE, test_c$PRICE)

#remove 10 rows - nas for base_price
test_c<- test_c[!is.na(test_c$BASE_PRICE),]

#see which columns have nas - 0 nas now
map(test_c, ~sum(is.na(.)))


#created a new column - has parking or not
test_c$parking_y_n <- test_c$PARKING_SPACE_QTY

test_c$parking_y_n[test_c$parking_y_n >0] <- "yes"
test_c$parking_y_n[test_c$parking_y_n ==0] <- "no"

final<-test_c

#4.1. PRICE DISCRIMINATION

# creating a dataframe holding the number of observations per state
sample_size_state <- final %>%
  group_by(ADDRESS_STATE_PROV_CODE) %>% 
  summarize(num=n())

# plotting violin with boxplot and mean - distribution of price per state
final %>%
  left_join(sample_size_state) %>%
  mutate(x_axis = paste0(ADDRESS_STATE_PROV_CODE, "\n", "n = ", num)) %>%
  ggplot(aes(x=x_axis, y=PRICE, fill=ADDRESS_STATE_PROV_CODE)) + 
  geom_violin(alpha=0.8) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, 
               color="#164d96", fill="#164d96") +
  theme(
    legend.position="none",
  ) +
  coord_flip() +
  labs(title="Distribution of Price by State", 
       x="STATE",y="PRICE OF COLD CEREAL")

#plotting ridgeline prices per state
ggplot(final, aes(x = PRICE, y = ADDRESS_STATE_PROV_CODE, fill = ADDRESS_STATE_PROV_CODE)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  xlab("Price") +
  ylab("State")

##### plotting prices per city
final %>%
  ggplot(aes(x=PRICE, fill=ADDRESS_STATE_PROV_CODE)) + 
  geom_density() +
  facet_wrap("ADDRESS_CITY_NAME", scales="free_x") +
  labs(title="Distribution of Price by City", 
       x="PRICE", fill="STATE")

###Creating a df with a single UPC - 3000006340

UPC_Single <- subset(final,test_c$UPC==3000006340)
View(UPC_Single)

##### creating a df that holds the number of observations per state
sample_size_state <- UPC_Single %>%
  group_by(ADDRESS_STATE_PROV_CODE) %>% 
  summarize(num=n())

##### plotting violin boxplot - distribution of price of a single UPC per state - 3000006340
UPC_Single %>%
  left_join(sample_size_state) %>%
  mutate(x_axis = paste0(ADDRESS_STATE_PROV_CODE, "\n", "n = ", num)) %>%
  ggplot(aes(x=x_axis, y=PRICE, fill=ADDRESS_STATE_PROV_CODE)) + 
  geom_violin(alpha=0.8) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, 
               color="#164d96", fill="#164d96") +
  theme(
    legend.position="none",
  ) +
  coord_flip() +
  labs(title="Distribution of Price of a Single UPC by State", 
       x="STATE",y="PRICE OF COLD CEREAL")

##### plotting prices per city Single UPC
UPC_Single %>%
  ggplot(aes(x=PRICE, fill=ADDRESS_STATE_PROV_CODE)) + 
  geom_density() +
  facet_wrap("ADDRESS_CITY_NAME", scales="free_x") +
  labs(title="Distribution of Price of a Single UPC by City", 
       x="PRICE", fill="STATE")

#PED

#find whether units have value of 0, if yes, which row(s) is it
which(final$UNITS == 0)
length(which(final$UNITS == 0))

#find whether price have value of 0, if yes, which row(s) is it
which(final$PRICE == 0)
length(which(final$PRICE == 0))

#remove the row 123247 as there cant be log(0) in the lm
final_ped <- final[-123247,]

#double check if the dataset still have 0 value for units or not
which(final_ped$UNITS == 0)
length(which(final_ped$UNITS == 0))

#you need to have at least version 3.6.3 of R for this part

#by states
STATE = group_by(final_ped, ADDRESS_STATE_PROV_CODE) %>%
  do(PED = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(STATE, PED) 
STATE_PED<-tidy(STATE, PED) 
STATE_P<-filter(STATE_PED, term=="log(PRICE)")

#by cities
CITIES = group_by(final_ped, ADDRESS_CITY_NAME) %>%
  do(PED_cities = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(CITIES, PED_cities) 
CITIES_PED<-tidy(CITIES, PED_cities) 
CITIES_P<-filter(CITIES_PED, term=="log(PRICE)")

#states PED by single UPC
STATE_PED_UPC = group_by(UPC_Single, ADDRESS_STATE_PROV_CODE) %>%
  do(PED = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(STATE_PED_UPC, PED) 
STATE_PED_UPC_6340<-tidy(STATE_PED_UPC, PED) 
STATE_P_UPC<-filter(STATE_PED_UPC_6340, term=="log(PRICE)")

#cities PED by single UPC
CITIES_PED_UPC = group_by(UPC_Single, ADDRESS_CITY_NAME) %>%
  do(PED_cities = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(CITIES_PED_UPC, PED_cities) 
CITIES_P_UPC<-tidy(CITIES_PED_UPC, PED_cities) 

#4.2 PROMOTION

#DISPLAY

#display dataset (feature and trp 0)
display<-test_c[test_c$FEATURE==0&test_c$TPR_ONLY==0,]
display = display[,!(names(display) %in% c("parking_y_n"))]

#treatment effect of display on sales and revenue

table<-tribble(
  ~sales_rev, ~treatment_effect,
  "sales volume", mean(display[display$DISPLAY==1,]$UNITS)-mean(display[display$DISPLAY==0,]$UNITS),
  "revenue", mean(display[display$DISPLAY==1,]$SPEND)-mean(display[display$DISPLAY==0,]$SPEND)
)

table  %>%
  ggplot(aes(x=sales_rev, y=treatment_effect)) + 
  geom_col()+
  ggtitle("Display")+
  xlab(" ")

#TPR

#trp dataset (feature and display 0)
trp<-test_c[test_c$FEATURE==0&test_c$DISPLAY==0,]

#treatment effect of tpr on sales and revenue

table<-tribble(
  ~sales_rev, ~treatment_effect,
  "sales volume", mean(trp[trp$TPR_ONLY==1,]$UNITS)-mean(trp[trp$TPR_ONLY==0,]$UNITS),
  "revenue", mean(trp[trp$TPR_ONLY==1,]$SPEND)-mean(trp[trp$TPR_ONLY==0,]$SPEND)
)

table  %>%
  ggplot(aes(x=sales_rev, y=treatment_effect)) + 
  geom_col()+
  ggtitle("TPR only")+
  xlab(" ")

#FEATURE

#feature dataset (trp and display 0)
feature<-test_c[test_c$TPR_ONLY==0&test_c$DISPLAY==0,]

#count number of rows with/without feature
table(feature$FEATURE)

#treatment effect of feature on sales and revenue

table<-tribble(
  ~sales_rev, ~treatment_effect,
  "sales volume", mean(feature[feature$FEATURE==1,]$UNITS)-mean(feature[feature$FEATURE==0,]$UNITS),
  "revenue", mean(feature[feature$FEATURE==1,]$SPEND)-mean(feature[feature$FEATURE==0,]$SPEND)
)

table  %>%
  ggplot(aes(x=sales_rev, y=treatment_effect)) + 
  geom_col()+
  ggtitle("Feature")+
  xlab(" ")

#STORE PRICE TIER
#treatment effect by seg_value_name


tab<- feature %>%
  group_by(FEATURE, SEG_VALUE_NAME) %>%
  summarise(UNITS = mean(UNITS),SPEND=mean(SPEND)) %>%
  group_by(SEG_VALUE_NAME) %>%
  summarise(treat_eff_sales = UNITS[FEATURE==1]-UNITS[FEATURE==0],
            treat_eff_rev=SPEND[FEATURE==1]-SPEND[FEATURE==0]) 


df2 <- rbind(
  data.frame("Type_of_store"=tab$SEG_VALUE_NAME, "treatment_effect" = tab$treat_eff_sales, "type"="sales_volume"),
  data.frame("Type_of_store"=tab$SEG_VALUE_NAME, "treatment_effect" = tab$treat_eff_rev, "type"="revenue")
)


# Grouped

ggplot(df2, aes(fill=type, y=treatment_effect, x=Type_of_store)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Treatment Effect by Type of Store")

#STATE

tab<- feature %>%
  group_by(FEATURE, ADDRESS_STATE_PROV_CODE) %>%
  summarise(UNITS = mean(UNITS),SPEND=mean(SPEND)) %>%
  group_by(ADDRESS_STATE_PROV_CODE) %>%
  summarise(treat_eff_sales = UNITS[FEATURE==1]-UNITS[FEATURE==0],
            treatment_effect_revenue=SPEND[FEATURE==1]-SPEND[FEATURE==0]) 

df2 <- rbind(
  data.frame("State"=tab$ADDRESS_STATE_PROV_CODE, "treatment_effect" = tab$treat_eff_sales, "type"="sales_volume"),
  data.frame("State"=tab$ADDRESS_STATE_PROV_CODE, "treatment_effect" = tab$treatment_effect_revenue, "type"="revenue")
)


# Grouped

ggplot(df2, aes(fill=type, y=treatment_effect, x=State)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Treatment Effect by State")


# Map of the US states:
# load United States state map data
MainStates <- map_data("state")

#plot all states with ggplot2, using black borders and light blue fill
ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )


#rename states in our table
tab$ADDRESS_STATE_PROV_CODE[tab$ADDRESS_STATE_PROV_CODE =="IN"]<- "indiana"
tab$ADDRESS_STATE_PROV_CODE[tab$ADDRESS_STATE_PROV_CODE =="KY"]<- "kentucky"
tab$ADDRESS_STATE_PROV_CODE[tab$ADDRESS_STATE_PROV_CODE =="OH"]<- "ohio"
tab$ADDRESS_STATE_PROV_CODE[tab$ADDRESS_STATE_PROV_CODE =="TX"]<- "texas"
names(tab)[1]<-"region"


#MERGE states with treat eff
MergedStates <- left_join(MainStates, tab, by = "region")


# Create a Choropleth map of the United States
p <- ggplot()+ggtitle("U.S. Map - Treatment Effect on Revenue based on Geographical Position")
p <- p + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = treatment_effect_revenue), 
                       color="white", size = 0.2) 
p


# cities not texas
not_texas <-feature[!feature$ADDRESS_STATE_PROV_CODE=="TX",]

#CITY
city<-
  not_texas %>%
  group_by(FEATURE, ADDRESS_CITY_NAME) %>%
  summarise(SPEND=mean(SPEND)) %>%
  group_by(ADDRESS_CITY_NAME) %>%
  summarise(treatment_effect_revenue=SPEND[FEATURE==1]-SPEND[FEATURE==0]) 


#decresing te

city$ADDRESS_CITY_NAME <- factor(city$ADDRESS_CITY_NAME,                                    # Factor levels in decreasing order
                                 levels = city$ADDRESS_CITY_NAME[order(city$treatment_effect_revenue, decreasing = FALSE)])


ggplot(city,
       aes(x=ADDRESS_CITY_NAME, y=treatment_effect_revenue) )+ 
  geom_bar(width=0.5,stat="identity")+
  xlab("City")+
  coord_flip()+
  ggtitle("Treatment Effect on Revenue for cities in Indiana, Ohio, Kentucky")

#cities map
cities_coord<-cities_new[cities_new$state_id %in% c("IN","KY","OH"),]    
city <- left_join(city,cities_coord,by =c('ADDRESS_CITY_NAME'='city'))



ggplot(city, aes(x=lng, y=lat, color=treatment_effect_revenue)) + 
  geom_point(size=3) +
  ggtitle("U.S. Map - Treatment Effect on Revenue based on Geographical Position for cities in Indiana, Ohio, Kentucky")

#CLUSTERING

#clustering on lat/lng 

unique(test_c$ADDRESS_CITY_NAME) #51 cities

km10 = kmeans(feature[,c("lng","lat")], 10)
km10$centers

feature$cluster<- km10[["cluster"]]
feature$cluster<- as.character(feature$cluster)

ggplot(feature, aes(x=lng, y=lat, color=cluster)) + 
  geom_point(size=1) +
  ggtitle("Clusters 2D")

#treatment effect by cluster
feature %>%
  group_by(FEATURE, cluster) %>%
  summarise(SPEND = mean(SPEND)) %>%
  group_by(cluster) %>%
  summarise(Treatment_Effect_Revenue = SPEND[FEATURE==1]-SPEND[FEATURE==0]) %>%
  ggplot(aes(x=cluster, y=Treatment_Effect_Revenue)) + 
  geom_col()+
  ggtitle("Treatment Effect on Revenue for each 2D Cluster")+
  xlab("Clusters2D")


#count rows in cluster 1 (best cluster)
nrow(feature[feature$cluster == "1",])

#cities in cluster 7
cit<- feature[feature$cluster== 7 ,]
unique(cit[c("ADDRESS_CITY_NAME")])

#clustering by lat,long of cities, seg_value

big_cluster<-feature[,c("lng","lat","SEG_VALUE_NAME")]

#encoding seg_value_name

big_cluster$SEG_VALUE_NAME[big_cluster$SEG_VALUE_NAME =="VALUE"] <- 1
big_cluster$SEG_VALUE_NAME[big_cluster$SEG_VALUE_NAME =="MAINSTREAM"] <- 2
big_cluster$SEG_VALUE_NAME[big_cluster$SEG_VALUE_NAME =="UPSCALE"] <- 3

big_cluster$SEG_VALUE_NAME<- as.numeric(big_cluster$SEG_VALUE_NAME)

#we have to re-scale the 4 variables now
big_cluster_sc <- scale(big_cluster)

#create 6 clusters
km6 = kmeans(big_cluster_sc, 6)
km6$centers

feature$cluster_big<- km6[["cluster"]]
feature$cluster_big<- as.character(feature$cluster_big) 

#3d plot with clusters

# Basic 3d graphics
#make seg numerical

feature$SEG_VALUE_NAME[feature$SEG_VALUE_NAME =="VALUE"] <- 1
feature$SEG_VALUE_NAME[feature$SEG_VALUE_NAME =="MAINSTREAM"] <- 2
feature$SEG_VALUE_NAME[feature$SEG_VALUE_NAME =="UPSCALE"] <- 3

feature$SEG_VALUE_NAME<- as.numeric(feature$SEG_VALUE_NAME)

colors<- c("#999999", "#E69F00", "#56B4E9", '#E30B5D', '#679267','#000000')
colors <- colors[as.numeric(feature$cluster_big)]


#Source the function addgrids
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

s3d <- scatterplot3d(feature[,c("lng","lat","SEG_VALUE_NAME")], pch = 16, grid=FALSE,box=FALSE,color=colors,main="Clusters 3D")
#Add grids
addgrids3d(feature[,c("lng","lat","SEG_VALUE_NAME")], grid = c("xy", "xz", "yz"))
legend( "center",legend = c("1","2","3","4","5","6"),
        col =  c("#999999", "#E69F00", "#56B4E9", '#E30B5D', '#679267','#000000'), pch = 16)


#treatment effect by cluster
feature %>%
  group_by(FEATURE, cluster_big) %>%
  summarise(SPEND = mean(SPEND)) %>%
  group_by(cluster_big) %>%
  summarise(Treatment_Effect_Revenue = SPEND[FEATURE==1]-SPEND[FEATURE==0]) %>%
  ggplot(aes(x=cluster_big, y=Treatment_Effect_Revenue)) + 
  geom_col()+
  ggtitle("Treatment Effect on Revenue for each 3D Cluster")+
  xlab("Clusters3D")

#cities in cluster 6
cit<- feature[feature$cluster_big== 6 ,]
unique(cit[c("ADDRESS_CITY_NAME")])

#IMPLICATIONS

#treatment effect for 6 marketing strategies

table<-tribble(
  ~sales_revenue, ~treatment_effect, ~strategy, 
  "sales_volume", mean(feature[feature$FEATURE==1,]$UNITS)-  mean(feature[feature$FEATURE==0,]$UNITS), "NoTargeting",
  "revenue", mean(feature[feature$FEATURE==1,]$SPEND)-  mean(feature[feature$FEATURE==0,]$SPEND), "NoTargeting",
  "sales_volume", mean(feature[feature$FEATURE==1&feature$SEG_VALUE_NAME==1,]$UNITS)-  mean(feature[feature$FEATURE==0&feature$SEG_VALUE_NAME==1,]$UNITS), "MAINSTREAM",
  "revenue", mean(feature[feature$FEATURE==1&feature$SEG_VALUE_NAME==1,]$SPEND)-  mean(feature[feature$FEATURE==0&feature$SEG_VALUE_NAME==1,]$SPEND), "MAINSTREAM",
  "sales_volume", mean(feature[feature$FEATURE==1&feature$ADDRESS_STATE_PROV_CODE=="IN",]$UNITS)-  mean(feature[feature$FEATURE==0&feature$ADDRESS_STATE_PROV_CODE=="IN",]$UNITS), "IN",
  "revenue", mean(feature[feature$FEATURE==1&feature$ADDRESS_STATE_PROV_CODE=="IN",]$SPEND)-  mean(feature[feature$FEATURE==0&feature$ADDRESS_STATE_PROV_CODE=="IN",]$SPEND), "IN",
  "sales_volume", mean(feature[feature$FEATURE==1&feature$ADDRESS_CITY_NAME %in% c("SAINT MARYS","LEBANON"),]$UNITS)-  mean(feature[feature$FEATURE==0&feature$ADDRESS_CITY_NAME %in% c("SAINT MARYS","LEBANON"),]$UNITS), "STMARYS_LEBANON",
  "revenue", mean(feature[feature$FEATURE==1&feature$ADDRESS_CITY_NAME %in% c("SAINT MARYS","LEBANON"),]$SPEND)-  mean(feature[feature$FEATURE==0&feature$ADDRESS_CITY_NAME %in% c("SAINT MARYS","LEBANON"),]$SPEND), "STMARYS_LEBANON",
  "sales_volume", mean(feature[feature$FEATURE==1&feature$cluster== 7 ,]$UNITS)-  mean(feature[feature$FEATURE==0&feature$cluster== 7 ,]$UNITS), "cluster7_2D",
  "revenue", mean(feature[feature$FEATURE==1&feature$cluster== 7 ,]$SPEND)-  mean(feature[feature$FEATURE==0&feature$cluster== 7 ,]$SPEND), "cluster7_2D",
  "sales_volume", mean(feature[feature$FEATURE==1&feature$cluster_big== 6 ,]$UNITS)-  mean(feature[feature$FEATURE==0&feature$cluster_big== 6 ,]$UNITS), "cluster6_3D",
  "revenue", mean(feature[feature$FEATURE==1&feature$cluster_big== 6 ,]$SPEND)-  mean(feature[feature$FEATURE==0&feature$cluster_big== 6 ,]$SPEND), "cluster6_3D",
)

ggplot(table, aes(fill=sales_revenue, y=treatment_effect, x=strategy)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Treatment Effect on Sales Volume and Revenue for 5 targeting strategies + no strategy")


#5.3 INTERACTION EFFECT BETWEEN ALL 3 TYPES OF PROMOTION

#display and feature dataset (tpr 0)
test_c = test_c[,!(names(test_c) %in% c("parking_y_n"))]
d_f <- test_c[test_c$TPR_ONLY==0,]

#treatment effect of feature on sales and revenue
table<-tribble(
  ~sales_rev, ~treatment_effect,
  "sales volume", mean(d_f[d_f$DISPLAY==1&d_f$FEATURE==1,]$UNITS)-mean(d_f[d_f$DISPLAY==0 & d_f$FEATURE==0,]$UNITS),
  "revenue", mean(d_f[d_f$DISPLAY==1 & d_f$FEATURE==1,]$SPEND)-mean(d_f[d_f$DISPLAY==0&d_f$FEATURE==0,]$SPEND)
)

table  %>%
  ggplot(aes(x=sales_rev, y=treatment_effect)) + 
  geom_col()+
  ggtitle("Display_Feature")+
  xlab(" ")
