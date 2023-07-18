#### MDAV_2_SEM_Pražáková ####

#### LIBRARIES ####
library(dplyr) 
library(plyr) 
library(ggplot2) 
library(gridExtra)
library(gapminder)
library(magrittr) 
library(scales)
library(hms)
library(tidyverse)
library(aweek)


#### DATA UPLOAD ####
oct <- read.csv("/users/lucie/downloads/archive/2019-Oct.csv", stringsAsFactors=FALSE)
nov <- read.csv("/users/lucie/downloads/archive/2019-Nov.csv", stringsAsFactors=FALSE)
dec <- read.csv("/users/lucie/downloads/archive/2019-Dec.csv", stringsAsFactors=FALSE)
jan <- read.csv("/users/lucie/downloads/archive/2020-Jan.csv", stringsAsFactors=FALSE)
feb <- read.csv("/users/lucie/downloads/archive/2020-Feb.csv", stringsAsFactors=FALSE)

oct$month <- "2019/10"
nov$month <- "2019/11"
dec$month <- "2019/12"
jan$month <- "2020/01"
feb$month <- "2020/02"

# Sloučení do jednoho table
table <- bind_rows(oct, nov, dec, jan, feb) # spojení jednotlivých souborů do jednoho
View(table)



#### ANALYSIS ####
summary(table) # základní statistiky
str(table)
sapply(table,class) # typy proměnných
sapply(table, function(x) sum(is.na(x))) # kontrola NA hodnot




#### PRICE ####
# Graf veškerých cen - lze vidět záporné hodnoty
prices <- table(table$price)
barplot(prices, main="All prices",
        xlab="Prices")  

# Kladné ceny
table_prices <- table[(table$price>0),] # kladné ceny
positive_prices <- nrow(table_prices) # počet řádků s kladnou hodnotou ceny

# Graf kladných cen
prices <- table(table_prices$price)
barplot(prices, main="Positive prices",
        xlab="Prices")  

# Záporné a nulové ceny
negative_prices <- table[(table$price<0),] # záporné ceny
zero_prices <- table[(table$price==0.00),] # nulové ceny

nrow(negative_prices) # počet řádků s zápornou 
nrow(zero_prices) # počet řádků s nulovou hodnotou proměnné cena

# Odstranění záporných a nulových hodnot u cen
table <- table_prices # přepis původního table
# View(table) # table pouze s nenulovými a nezápornými hodnotami





#### EVENT_TIME ####
# Rozdělení event_time na event_time(čas) a event_date(datum)
time1 <- strptime(table$event_time, "%Y-%m-%d %H:%M:%S")
event_time <- format(time1,"%H:%M:%S")
event_date <- format(time1,"%Y-%m-%d")

table$event_time <- as_hms(event_time) # přidání času do souboru
table$event_date <- as.Date(event_date) # přidání data do souboru
table$hours <- substr(table$event_time, 0, 2) # pridání hodin
table$day_of_week <- weekdays(as.Date(table$event_date,'%d-%m-%Y')) # přidání hodnot dnů v týdnu
table$week_number <- as.aweek(table$event_date) # přidání čísla týdne
table$week_number <- substr(table$week_number, 7, 8) # osekání pouze na číslo týdne

View(table)


# Dny s nejvíce nákupy
top_days <- table %>%
  select("event_date", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(event_date) %>%
  arrange(event_date) %>% tally()
top_days <- as.data.frame(top_days)
top_days_final <- top_days[order(-top_days$n),]
colnames(top_days_final) <- c("Date", "Purchase")
head(top_days_final,10)


# Hodiny s nejvíce nákupy
top_hours <- table %>%
  select("hours", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(hours) %>%
  arrange(hours) %>% tally()
top_hours <- as.data.frame(top_hours)
top_hours_final <- top_hours[order(-top_hours$n),]
colnames(top_hours_final) <- c("Hours", "Purchase")
head(top_hours_final,10)


# Dny v týdnu s nejvíce nákupy
top_daysow <- table %>%
  select("day_of_week", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(day_of_week) %>%
  arrange(day_of_week) %>% tally()
top_daysow <- as.data.frame(top_daysow)
top_daysow_final <- top_daysow[order(-top_daysow$n),]
colnames(top_daysow_final) <- c("Day of week", "Purchase")
head(top_daysow_final,7)



# Chart of purchase history - hourly
table_purchase2 <- table %>% 
  select(event_type, hours) %>%
  filter(event_type == "purchase") %>%
  group_by(hours) %>%
  arrange(hours) %>%
  tally()
View(table_purchase2)

# create data
hours <- table_purchase2$hours
purchase <- table_purchase2$n
data2 <- data.frame(hours,purchase)
#as.matrix(data2$hours)

# Plot
data2 <- data.frame(
  hours=table_purchase2$hours,
  number_of_purchases=table_purchase2$n
)

ggplot(data2, aes(hours, number_of_purchases)) + 
  geom_bar(stat = "identity")





## Chart of purchase history - daily
table_purchase0 <- table %>% 
  select(event_type, event_date) %>%
  filter(event_type == "purchase") %>%
  group_by(event_date) %>%
  arrange(event_date) %>%
  tally()
View(table_purchase0)

# create data
date <- table_purchase0$event_date
purchase <- table_purchase0$n
data <- data.frame(date,purchase)

# Plot
ggplot(data, aes(x=date, y=purchase)) +
  geom_line()
plot(date, purchase)





## Chart of purchase history - weekly
table_purchase1 <- table %>% 
  select(event_type, week_number) %>%
  filter(event_type == "purchase") %>%
  group_by(week_number) %>%
  arrange(week_number) %>%
  tally()
View(table_purchase1)

# create data
week_number <- table_purchase1$week_number
purchase <- table_purchase1$n
data1 <- data.frame(week, purchase)

# Plot
plot(week_number, purchase)





# Zobrazení dnů v týdnu
## Chart of purchase history - hourly
table_purchase3 <- table %>% 
  select(event_type, day_of_week) %>%
  filter(event_type == "purchase") %>%
  group_by(day_of_week) %>%
  arrange(day_of_week) %>%
  tally()
View(table_purchase3)

# create data
day_of_week <- table_purchase3$day_of_week
purchase <- table_purchase3$n
data3 <- data.frame(day_of_week,purchase)

# Plot
plot(data3,type = "o")

data3 <- data.frame(
  day_of_week=table_purchase3$day_of_week,  
  number_of_purchases=table_purchase3$n
)

ggplot(data3, aes(day_of_week, number_of_purchases)) + 
  geom_bar(stat = "identity")







#### EVENT_TYPE ####

# Počet hodnot proměnné view, cart, remove_from_cart a purchase
table_view <- table %>%
  filter(event_type == "view")
nrow(table_view)

table_cart <- table %>%
  filter(event_type == "cart")
nrow(table_cart) 

table_rfc <- table %>%
  filter(event_type == "remove_from_cart")
nrow(table_rfc) 

table_purchase <- table %>%
  filter(event_type == "purchase")
nrow(table_purchase) 


# Procento odstranění z košíku z celkových zobrazení
nrow(table_rfc) / nrow(table_view) 
# Procento vložení do košíku z celkových zobrazení
nrow(table_cart) / nrow(table_view) 
# Procento nákupů z celkových zobrazení
nrow(table_purchase) / nrow(table_view) 
# Procento nákupů z celkových vložení do košíku
nrow(table_purchase) / nrow(table_cart) 


# Výskyt proměnné event_type v souboru
# TABLE: Zobrazení event_type v grafu
table_event_type <- table(table$event_type)
barplot(table_event_type, main="Overall - event type",
        xlab="event_type")

table_event_type <- data.frame(table_event_type)
colnames(table_event_type)<- c("Event Type","Freq")
table_event_type

# OCT: Zobrazení event_type v grafu
oct_event_type <- table(oct$event_type)
barplot(oct_event_type, main="October - event type",
        xlab="event_type")

# NOV: Zobrazení event_type v grafu
nov_event_type <- table(nov$event_type)
barplot(nov_event_type, main="November - event type",
        xlab="event_type")

# DEC: Zobrazení event_type v grafu
dec_event_type <- table(dec$event_type)
barplot(dec_event_type, main="December - event type",
        xlab="event_type")

# JAN: Zobrazení event_type v grafu
jan_event_type <- table(jan$event_type)
barplot(jan_event_type, main="January - event type",
        xlab="event_type")

# FEB: Zobrazení event_type v grafu
feb_event_type <- table(feb$event_type)
barplot(feb_event_type, main="February - event type",
        xlab="event_type")


all_events_months <- data.frame(oct_event_type, nov_event_type, dec_event_type, jan_event_type,feb_event_type)
colnames(all_events_months) <- c("Event type","October",
                                 "Event type","November",
                                 "Event type","December",
                                 "Event type","January",
                                 "Event type","February")
all_events_months <- all_events_months[,-c(3,5,7,9)]
all_events_months 


## History of all events
event_type_oct <- as.data.frame(table(oct$event_type))
names(event_type_oct) <- c("Event_type", "Freq")
event_type_oct$Month <- "2019/10"
event_type_nov <- as.data.frame(table(nov$event_type))
names(event_type_nov) <- c("Event_type", "Freq")
event_type_nov$Month <- "2019/11"
event_type_dec <- as.data.frame(table(dec$event_type))
names(event_type_dec) <- c("Event_type", "Freq")
event_type_dec$Month <- "2019/12"
event_type_jan <- as.data.frame(table(jan$event_type))
names(event_type_jan) <- c("Event_type", "Freq")
event_type_jan$Month <- "2020/01"
event_type_feb <- as.data.frame(table(feb$event_type))
names(event_type_feb) <- c("Event_type", "Freq")
event_type_feb$Month <- "2020/02"
overall_event_type <- rbind(event_type_feb, event_type_jan, event_type_dec, event_type_nov, event_type_oct)

options(repr.plot.width = 8, repr.plot.height = 5)
p <- ggplot(data = overall_event_type, mapping = aes(x = Month, y = Freq)) + geom_line(mapping = aes(color = Event_type, group = Event_type), lwd = 1.5) + geom_point(mapping = aes(color = Event_type, size = 0.5), show.legend = FALSE) + theme_linedraw() + ggtitle("History of all events") + xlab("Months") + ylab("Total Count") + scale_color_manual(values=c("Blue","Black","Dark Blue","Light Blue"))
p







#### PRODUCT_ID ####
# Nejvíce odebírané produkty z košíku
removed_products <- table %>%
  select("product_id", "event_type") %>% 
  filter(event_type == 'remove_from_cart') %>% 
  group_by(product_id) %>% 
  arrange(product_id) %>% 
  tally()
removed_products <- as.data.frame(removed_products)
removed_products_final <- removed_products[order(-removed_products$n),]
colnames(removed_products_final) <- c("Product", "n")
head(removed_products_final,10)


# Nejvíce prodávající se produkty
table_products <- table %>%
  select("product_id", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(product_id) %>% 
  arrange(product_id) %>% 
  tally()
table_products <- as.data.frame(table_products)
table_products_final <- table_products[order(-table_products$n),]
colnames(table_products_final) <- c("Product", "n")
head(table_products_final,10)






#### CATEGORY_ID ####
hist(table$category_id) # zobrazení v histogramu

# oprava category_id na character
table$category_id <- as.character(table$category_id) # pro table

# pro jednotlivé měsíce
oct$category_id <- as.character(oct$category_id) # oct
nov$category_id <- as.character(nov$category_id) # nov
dec$category_id <- as.character(dec$category_id) # dec
jan$category_id <- as.character(jan$category_id) # jan
feb$category_id <- as.character(feb$category_id) # feb


# Nejvíce prodávající se category_id
table_category <- c("category_id", "event_type")
table_category <- table %>%
  select("category_id", "event_type") %>%
  filter(event_type == 'purchase') %>%
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
table_category <- as.data.frame(table_category)
table_category_final <- table_category[order(-table_category$n),]
table_category <- c("category_id", "event_type")
head(table_category_final,10)


# Best categories in OCT
category_oct <- c("event_type","category_id")
category_oct <- oct %>%
  select("event_type","category_id") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
category_oct <- as.data.frame(category_oct)
category_oct_final <- category_oct[order(-category_oct$n),]
purchase_oct <- head(category_oct_final,12)


# Best categories in NOV
category_nov <- c("event_type","category_id")
category_nov <- nov %>%
  select("event_type","category_id") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
category_nov <- as.data.frame(category_nov)
category_nov_final <- category_nov[order(-category_nov$n),]
purchase_nov <- head(category_nov_final,12)

# Best categories in DEC
category_dec <- c("event_type","category_id")
category_dec <- dec %>%
  select("event_type","category_id") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
category_dec <- as.data.frame(category_dec)
category_dec_final <- category_dec[order(-category_dec$n),]
purchase_dec <- head(category_dec_final,12)


# Best categories in JAN
category_jan <- c("event_type","category_id")
category_jan <- nov %>%
  select("event_type","category_id") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
category_jan <- as.data.frame(category_jan)
category_jan_final <- category_jan[order(-category_jan$n),]
purchase_jan <- head(category_jan_final,12)

# Best categories in FEB
category_feb <- c("event_type","category_id")
category_feb <- nov %>%
  select("event_type","category_id") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(category_id) %>%
  arrange(category_id) %>% tally()
category_feb <- as.data.frame(category_feb)
category_feb_final <- category_feb[order(-category_feb$n),]
purchase_feb <- head(category_feb_final,12)


#spojení do grafu
names(purchase_oct) <- c("category_id", "purchase")
purchase_oct$month <- "2019/10"

names(purchase_nov) <- c("category_id", "purchase")
purchase_nov$month <- "2019/11"

names(purchase_dec) <- c("category_id", "purchase")
purchase_dec$month <- "2019/12"

names(purchase_jan) <- c("category_id", "purchase")
purchase_jan$month <- "2020/01"

names(purchase_feb) <- c("category_id", "purchase")
purchase_feb$month <- "2020/02"

categories <- bind_rows(purchase_oct, purchase_nov, purchase_dec, purchase_jan, purchase_feb)
categories

options(repr.plot.width = 8, repr.plot.height = 5)
p <- ggplot(data = categories, mapping = aes(x = month, y = purchase)) + geom_line(mapping = aes(color = category_id, group = category_id), lwd = 1.5) + geom_point(mapping = aes(color = category_id, size = 0.5), show.legend = FALSE) + theme_linedraw() + ggtitle("Purchase/categories") + xlab("Months") + ylab("Total Count") 
p






#### BRAND ####

# Nejvíce prodávající se brandy
table_brand <- table %>%
  select("brand", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(brand) %>%
  arrange(brand) %>% tally()
table_brand <- as.data.frame(table_brand)
table_brand_final <- table_brand[order(-table_brand$n),]
head(table_brand_final,10)







#### USER_ID ####
# Users october
users_oct <- oct %>%
  select(user_id) %>%
  group_by(user_id) %>%
  tally()
#users_oct  
uu_oct <- nrow(users_oct)

# Users november
users_nov <- nov %>%
  select(user_id) %>%
  group_by(user_id) %>%
  tally()
#users_nov  
uu_nov <- nrow(users_nov)

# Users december
users_dec <- dec %>%
  select(user_id) %>%
  group_by(user_id) %>%
  tally()
#users_dec  
uu_dec <- nrow(users_dec)

# Users january
users_jan <- jan %>%
  select(user_id) %>%
  group_by(user_id) %>%
  tally()
#users_jan  
uu_jan <- nrow(users_jan)

# Users february
users_feb <- feb %>%
  select(user_id) %>%
  group_by(user_id) %>%
  tally()
#users_jan  
uu_feb <- nrow(users_feb)


# Unique users through months
unique_users <- data.frame(uu_oct, uu_nov, uu_dec, uu_jan, uu_feb)
colnames(unique_users) <- c("October", "November", "December","January", "February")
unique_users 

unique_users <- c(uu_oct, uu_nov, uu_dec, uu_jan, uu_feb)
barplot(unique_users,
        main = "Unique users through months",
        xlab = "Months",
        ylab = "Unique users",
        names.arg = c("October", "November", "December","January", "February"),
        horiz = FALSE)



# Nejvíce nakupující zákazníci
table_users <- table %>%
  select("user_id", "event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(user_id) %>% #arrange(user_id) %>% 
  tally()
table_users <- as.data.frame(table_users)
table_users_final <- table_users[order(-table_users$n),]
colnames(table_users_final) <- c("user_id", "purchases")
head(table_users_final,10)

# Zákaznící, kteří uskutečnili nejdražší nákupy
table_users2 <- table %>%
  select("user_id","price","event_type") %>% 
  filter(event_type == 'purchase') %>% 
  group_by(user_id) %>% arrange(price) 
table_users2
table_users2 <- as.data.frame(table_users2)
table_users2_final <- table_users2[order(-table_users2$price),]
colnames(table_users2_final) <- c("user_id", "price","event_type")
head(table_users2_final,10)







#### USER_SESSION ####
user_sess <- table %>% filter(user_id=="150318419") # otestování libovolné user_id
user_sess # vidíme sessions navázané k user_id






#### NEW METRICS ####

####  CONVERSION RATE ####

# Conversion rate - monthly
users_monthly<- table %>% 
  select(month, user_id) %>%
  group_by(month) %>%
  arrange(month) %>%
  tally()
#View(users_monthly)

purchases_monthly <- table %>% 
  select(month, event_type) %>%
  filter(event_type == "purchase") %>%
  group_by(month) %>%
  arrange(month) %>%
  tally()
#View(purchases_monthly)

cr_monthly <- merge(users_monthly,purchases_monthly, by="month") # sloučení do jedné tabulky
cr_monthly <- cr_monthly %>% mutate( cr = (n.x)/(n.y)) # výpočet conversion rate
View(cr_monthly)

# Chart
cr_monthly <- data.frame(
  month=cr_monthly$month,
  conversion_rate=cr_monthly$cr)

p1a <- ggplot(cr_monthly, aes(month, conversion_rate)) + geom_bar(stat = "identity")+  ggtitle("Conversion rate - monthly") + xlab("Months") + ylab("Conversion rate (in %)")
p1a

p1b <- ggplot(cr_monthly, aes(month,conversion_rate,group = 1)) + geom_line() +  ggtitle("Conversion rate - monthly") + xlab("Months") + ylab("Conversion rate (in %)")
p1b




# # Conversion Rate - weekly
# users_weekly <- table %>% 
#   select(week_number, user_id) %>%
#   group_by(week_number) %>%
#   arrange(week_number) %>%
#   tally()
# #View(users_weekly)
# 
# purchases_weekly <- table %>% 
#   select(week_number, event_type) %>%
#   filter(event_type == "purchase") %>%
#   group_by(week_number) %>%
#   arrange(week_number) %>%
#   tally()
# #View(purchases_weekly)
# 
# cr_weekly<- merge(users_weekly, purchases_weekly, by="week_number")
# cr_weekly <- cr_weekly %>% mutate( cr = (n.x)/(n.y))
# #View(cr_weekly)
# 
# # Chart
# cr_weekly <- data.frame(
#   weeks=cr_weekly$hours,
#   conversion_rate=cr_weekly$cr)
# 
# p2 <- ggplot(cr_weekly, aes(weeks, conversion_rate)) + geom_bar(stat = "identity")+  ggtitle("Conversion rate - weekly") + xlab("Weeks") + ylab("Conversion rate (in %)")
# p2




# Conversion Rate - daily
users_daily <- table %>% 
  select(event_date, user_id) %>%
  group_by(event_date) %>%
  arrange(event_date) %>%
  tally()
#View(users_hourly)

purchases_daily <- table %>% 
  select(event_date, event_type) %>%
  filter(event_type == "purchase") %>%
  group_by(event_date) %>%
  arrange(event_date) %>%
  tally()
#View(purchases_hourly)

cr_daily <- merge(users_daily,purchases_daily, by="event_date") # sloučení do jedné tabulky
cr_daily <- cr_daily %>% mutate(cr = (n.x)/(n.y)) # výpočet conversion rate
#View(cr_hourly)

# Plot
cr_daily <- data.frame(
  days=cr_daily$event_date,
  conversion_rate=cr_daily$cr)

p3 <- ggplot(cr_daily, aes(days, conversion_rate)) + geom_bar(stat = "identity")+  ggtitle("Conversion rate - daily") + xlab("Days") + ylab("Conversion rate (in %)")
p3




# Conversion Rate - hourly (for one month - December)
users_hourly <- table %>% 
  select(month, hours, user_id) %>%
  filter(month == "2019/12") %>%
  group_by(hours) %>%
  arrange(hours) %>%
  tally()
#View(users_hourly)

purchases_hourly <- table %>% 
  select(month, hours, event_type) %>%
  filter(event_type == "purchase") %>%
  filter(month == "2019/12") %>%
  group_by(hours) %>%
  arrange(hours) %>%
  tally()
#View(purchases_hourly)

cr_hourly <- merge(users_hourly,purchases_hourly, by="hours") # sloučení do jedné tabulky
cr_hourly <- cr_hourly %>% mutate(cr = (n.x)/(n.y)) # výpočet conversion rate
#View(cr_hourly)

# Plot
cr_hourly <- data.frame(
  hours=cr_hourly$hours,
  conversion_rate=cr_hourly$cr)

p4 <- ggplot(cr_hourly, aes(hours, conversion_rate)) + geom_bar(stat = "identity")+  ggtitle("Conversion rate - hourly (December)") + xlab("Hours") + ylab("Conversion rate (in %)")
p4







#### CART ABANDONMENT RATE ####
# Cart abandonment rate - monthly
cart_monthly <- table %>% 
  select(month, event_type) %>%
  filter(event_type == "cart") %>%
  group_by(month) %>%
  arrange(month) %>%
  tally()
#View(cart_monthly)

rfc_monthly <- table %>% 
  select(month, event_type) %>%
  filter(event_type == "remove_from_cart") %>%
  group_by(month) %>%
  arrange(month) %>%
  tally()
#View(purchases_monthly)

car_monthly <- merge(cart_monthly,rfc_monthly, by="month") # sloučení do jedné tabulky
car_monthly <- car_monthly %>% mutate( car = (n.x)/(n.y)) # výpočet conversion rate
#View(cr_monthly)

# Chart
car_monthly <- data.frame(
  month=car_monthly$month,
  cart_aband_rate=car_monthly$car)

p5 <- ggplot(car_monthly, aes(month, cart_aband_rate)) + geom_bar(stat = "identity")+  ggtitle("Cart abandonment rate - monthly") + xlab("Month") + ylab("Cart abandonment rate (in %)")
p5

p1b <- ggplot(car_monthly, aes(month,cart_aband_rate,group = 1)) + geom_line() +  ggtitle("Cart abandonment rate - monthly") + xlab("Month") + ylab("Cart abandonment rate (in %)")
p1b


# Cart abandonment rate - daily
cart_daily <- table %>% 
  select(event_date, event_type) %>%
  filter(event_type == "cart") %>%
  group_by(event_date) %>%
  arrange(event_date) %>%
  tally()
#View(cart_monthly)

rfc_daily <- table %>% 
  select(event_date, event_type) %>%
  filter(event_type == "remove_from_cart") %>%
  group_by(event_date) %>%
  arrange(event_date) %>%
  tally()
#View(purchases_monthly)

car_daily <- merge(cart_daily,rfc_daily, by="event_date") # sloučení do jedné tabulky
car_daily <- car_daily %>% mutate( car = (n.x)/(n.y)) # výpočet conversion rate
#View(cr_monthly)

# Chart
car_daily <- data.frame(
  date=car_daily$event_date,
  cart_aband_rate=car_daily$car)

p6 <- ggplot(car_daily, aes(date, cart_aband_rate)) + geom_bar(stat = "identity")+  ggtitle("Cart abandonment rate - daily") + xlab("Date") + ylab("Cart abandonment rate (in %)")
p6



# Cart abandonment rate - hourly (for December)
cart_hourly <- table %>% 
  select(month, hours, event_type) %>%
  filter(month == "2019/12") %>%
  filter(event_type == "cart") %>%
  group_by(hours) %>%
  arrange(hours) %>%
  tally()
#View(users_hourly)

rfc_hourly <- table %>% 
  select(month, hours, event_type) %>%
  filter(event_type == "remove_from_cart") %>%
  filter(month == "2019/12") %>%
  group_by(hours) %>%
  arrange(hours) %>%
  tally()
#View(purchases_hourly)
#View(purchases_monthly)

car_hourly <- merge(cart_hourly,rfc_hourly, by="hours") # sloučení do jedné tabulky
car_hourly <- car_hourly %>% mutate( car = (n.x)/(n.y)) # výpočet conversion rate
#View(cr_monthly)

# Chart
car_hourly <- data.frame(
  hours=car_hourly$hours,
  cart_aband_rate=car_hourly$car)

p7 <- ggplot(car_hourly, aes(hours, cart_aband_rate)) + geom_bar(stat = "identity")+  ggtitle("Cart abandonment rate - hourly (December)") + xlab("Hours") + ylab("Cart abandonment rate (in %)")
p7


