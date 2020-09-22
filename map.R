require("chron")
library(sp)
library(maptools)
library(ggplot2)
library(stringr)
library(devtools)
library(choroplethrZip)
library(choroplethr)
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv")
dat1<- read.csv(text = x)
colnames(dat1)
region<-dat1$MODIFIED_ZCTA

data(df_pop_zip)
data(zip.regions)
nyzip<-zip.regions[which(zip.regions$county.fips.numeric==36005|zip.regions$county.fips.numeric==36047|
                           zip.regions$county.fips.numeric==36061|zip.regions$county.fips.numeric==36081|
                           zip.regions$county.fips.numeric==36085),]
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
dat3<-merge(dat2,nyzip,by="region",all.y=TRUE)
dat3[is.na(dat3)] <- 0
region<-as.character(dat3$region)
value<-dat3$value
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
df1<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb1.csv",header = TRUE)
tb1<-merge(df1,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb1[is.na(tb1)] <- 0
region<-as.character(tb1$Var1)
value<-tb1$Freq
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 8,
               title       = "2019 New York City Telemedicine Calls",
               legend      = "Numbers")

df2<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb2.csv",header = TRUE)
tb2<-merge(df2,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb2[is.na(tb2)] <- 0
region<-as.character(tb2$Var1)
value<-tb2$Freq
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2019 New York City All Visits",
               legend      = "Numbers")

df3<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb3.csv",header = TRUE)
tb3<-merge(df3,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb3[is.na(tb3)] <- 0

region<-as.character(tb3$Var1)
value<-tb3$Freq
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Telemedicine Calls",
               legend      = "Numbers")

df4<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb4.csv",header = TRUE)
tb4<-merge(df4,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb4[is.na(tb4)] <- 0
region<-as.character(tb4$Var1)
value<-tb4$Freq
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City All Visits",
               legend      = "Numbers")

df5<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb5.csv",header = TRUE)
tb5<-merge(df5,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb5[is.na(tb5)] <- 0
region<-as.character(tb5$Var1)
value<-tb5$Freq
df<-data.frame(region,value)
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Covid Diagnosis in NYU",
               legend      = "Numbers")

df6<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/tb6.csv",header = TRUE)
tb6<-merge(df6,nyzip,by.x="Var1",by.y="region",all.y=TRUE)
tb6[is.na(tb6)] <- 0
covid_percent<-tb6$Freq/tb3$Freq

region<-as.character(tb6$Var1)
value<-covid_percent
df<-data.frame(region,value)
df[is.na(df)] <- 0

df <- unique( df[ , 1:2 ] )

zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Percentage of Call Covid-Related",
               legend      = "Numbers")




##Social demographic information##
df_ses<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/update0527/nyc_zip_code.csv",header = TRUE)
df_ses$zip<-substr(df_ses$NAME,nchar(df_ses$NAME)-4,nchar(df_ses$NAME))
typeof(df_ses$zip)
typeof(nyzip$region)
tb_ses<-merge(df_ses,nyzip,by.x="zip",by.y="region",all.y=TRUE)

region<-as.character(tb_ses$zip)
value<-as.numeric(tb_ses$Percent_black)
tb6[is.na(tb6)] <- 0
df<-data.frame(region,value)
df[is.na(df)] <- 0
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 7,
               title       = "2020 New York City Percentage Black",
               legend      = "Numbers")

region<-as.character(tb_ses$zip)
value<--as.numeric(tb_ses$median_household_income)
df<-data.frame(region,value)
df[is.na(df)] <- 0
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 7,
               title       = "2020 New York City Median Household Income",
               legend      = "Numbers")

## Percent Covid diagnosed##
df5<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/update0527/tb5.csv",header = TRUE)
tb5<-merge(df5,nyzip,by.x="Var1",by.y="region",all.y=TRUE)

df7<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/update0527/tb7.csv",header = TRUE)
tb7<-merge(df7,nyzip,by.x="Var1",by.y="region",all.y=TRUE)

df6<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/update0527/tb6.csv",header = TRUE)
tb6<-merge(df6,nyzip,by.x="Var1",by.y="region",all.y=TRUE)

df3<-read.csv(file="/Users/yuan/OneDrive/NYU/Research/Telemedicine/update0527/tb3.csv",header = TRUE)
tb3<-merge(df3,nyzip,by.x="Var1",by.y="region",all.y=TRUE)

region<-as.character(tb3$Var1)
value<-tb6$Freq/tb3$Freq
df<-data.frame(region,value)
df[is.na(df)] <- 0
df <- unique( df[ , 1:2 ] )
zip_choropleth(df,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Percentage of Covid-Call",
               legend      = "Numbers")

try<-merge(dat1,nyzip,by.x="MODIFIED_ZCTA",by.y="region",all.y=TRUE)

value<-try$COVID_CASE_COUNT
region<-try$MODIFIED_ZCTA
dat2<-data.frame(region,value)

dat2 <- unique(dat2[ , 1:2 ] )
dat2[is.na(dat2)] <- 0

zip_choropleth(dat2,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Covid Cases Distribution by Jun 3",
               legend      = "Numbers")

value<-try$COVID_DEATH_COUNT
region<-try$MODIFIED_ZCTA
dat2<-data.frame(region,value)

dat2 <- unique(dat2[ , 1:2 ] )
dat2[is.na(dat2)] <- 0

zip_choropleth(dat2,
               county_zoom = nyc_fips,num_colors = 9,
               title       = "2020 New York City Covid Deaths Distribution by Jun 3",
               legend      = "Numbers")
