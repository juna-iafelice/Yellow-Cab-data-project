# Yellow-Cab-data-project


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(modelr)
library(tidyverse)
library(hms)
library(lubridate)
library(devtools)
library(ggmap)
library(lubridate)
library(viridis)
library(jsonlite)
library(randomForest)

memory.limit(size=10e7)
### Load files in r, use pathway below with PC Anthony Saitta. 
January_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-01.csv")
February_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-02.csv")
March_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-03.csv")
April_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-04.csv")
May_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-05.csv")
June_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-06.csv")
July_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-07.csv")
August_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-08.csv")
September_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-09.csv")
October_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-10.csv")
November_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-11.csv")
December_2018 <- 
  read_csv(file = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-12.csv")

## Join csv's by r_bind
nycTaxiData2018 <- rbind(sample_n(January_2018, 
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(February_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(March_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(April_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(May_2018, 
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(June_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(July_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(August_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(September_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(October_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(November_2018,
                                  size=1000000,
                                  replace=TRUE),
                         sample_n(December_2018,
                                  size=1000000,
                                  replace=TRUE))

### Join Tables with Geographic Location
LocationID <- 
  read_csv(file = "https://raw.githubusercontent.com/anthonysaitta/taxiMapJoin/master/TaxiLocation.csv")

to_lat_long <- function(address,apiKey="AIzaSyB9dDoopjdcRZig0m_0zVNgFAojLpa_HAU",url="https://maps.googleapis.com/maps/api/geocode/json?address=") {
  cleanAddress <- str_replace_all(address,"/"," ")
  cleanAddress <- str_replace_all(cleanAddress," ","+")
  geocode_url <- paste(url,cleanAddress, "&key=" , apiKey, sep="")
  print(geocode_url)
  result = fromJSON(str_conv(geocode_url,"UTF-8"))
  list_result <- list(latitude= result$results$geometry$location$lat, longitude =result$results$geometry$location$lng)
  print(list_result)
  return(list_result)
}
### Registering Google API key for accessing maps

register_google(key="AIzaSyB9dDoopjdcRZig0m_0zVNgFAojLpa_HAU")
geocodes <- mutate_geocode(LocationID,taxiName)
geocodes <- tibble(geocodes)
write_csv(geocodes,"TaxiLocation.csv")


nycTaxiData2018 <- inner_join(nycTaxiData2018,
                              LocationID,
                              by=c("PULocationID"="LocationID"))
nycTaxiData2018 <- inner_join(nycTaxiData2018,
                              LocationID,
                              by=c("DOLocationID"="LocationID"))

### Create tibble from rbound table
taxiData2018 <- tibble(
  TPEP_Provider = nycTaxiData2018$VendorID,
  Pickup_Date = ymd(as.Date(nycTaxiData2018$tpep_pickup_datetime)),
  Pickup_Time = as_hms(nycTaxiData2018$tpep_pickup_datetime),
  Dropoff_Date = ymd(as.Date(nycTaxiData2018$tpep_dropoff_datetime)),
  Dropoff_Time = as_hms(nycTaxiData2018$tpep_dropoff_datetime),
  Number_of_Passengers = nycTaxiData2018$VendorID,
  Trip_Distance_Miles = nycTaxiData2018$trip_distance,
  Pickup_Location_ID = nycTaxiData2018$PULocationID,
  PU_Borough = nycTaxiData2018$Borough.x,
  PU_Neighbourhood = nycTaxiData2018$taxiName.x,
  PU_xID = nycTaxiData2018$xID.x,
  PU_Lat = nycTaxiData2018$latitude.x,
  PU_Long = nycTaxiData2018$longitude.x,
  Dropoff_Location_ID = nycTaxiData2018$DOLocationID,
  DO_Borough = nycTaxiData2018$Borough.y,
  DO_Neighbourhood = nycTaxiData2018$taxiName.y,
  DO_xID = nycTaxiData2018$xID.y,
  DO_Lat = nycTaxiData2018$latitude.y,
  DO_Long = nycTaxiData2018$longitude.y,
  Payment_Type_ID = nycTaxiData2018$payment_type, 
  Fare_Amount = nycTaxiData2018$fare_amount, 
  Extra_Charges = nycTaxiData2018$extra, 
  MTA_Tax = nycTaxiData2018$mta_tax,
  Improvement_Surcharge = nycTaxiData2018$improvement_surcharge, 
  Tip_Amount = nycTaxiData2018$tip_amount, 
  Tolls_Amount = nycTaxiData2018$tolls_amount, 
  Total_Charge_to_Passengers = nycTaxiData2018$total_amount
)

### Clean data to remove outliers
taxiDataClean <- 
  taxiData2018 %>% 
  select(1:27) %>%
  filter(Trip_Distance_Miles>0&
           Trip_Distance_Miles<50&
           Pickup_Date>=as.Date("2018-01-01")& 
           Pickup_Date<=as.Date("2018-12-31")&
           Dropoff_Time-Pickup_Time>0) %>%
  mutate(Trip_Duration=(Dropoff_Time-Pickup_Time)/60, na.rm=TRUE) %>%
  select(1:28)

```

## Purpose of the Project

  Yellow Cabs have become synonymous with New York City.  Though struggling with the onslaught on new entrants from shared ride services, New York Yellow Cabs remain an immensely convenient and accessible means of transportation in a city of 8 million inhabitants.  This project will examine the raw data associated with Yellow Cabs in New York from the perspective of a cab driver.  Given the 130 million rows of data, how can a cab driver parse usable information without getting lost in a forest of data?  This project seeks to provide cab driver with useable information to maximize their tipping profits and maximize their days to provide the highest profitability possible in a city inundated with competition. 
  
## Overview of Data 

  The data on Yellow Cab trips was obtained from https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page, a New York City organization dedicated to tracking cab data for various purposes that are outlined in the above link.  For more granular analysis, the data is divided into the 12 months of the year.  After combining all the data sets, the Taxi Zone Lookup Table was added to the combined data file to provide borough and neighborhood information for each pickup and dropoff location.  Without the inner joined table, the pickup and dropoff locations would be simple numeric identifiers.  
  After this inner join, the taxiCabData2018 variable was created, but 130 million rows were far too onerous for the group's systems to work with in a quick and reliable manner.  Use sample_n of 1,000,000 rows from each month, a 12 million row data set entitled nycTaxiData2018 was created.  To further enhance usability further, this new dataset of 12 million was converted into a tibble, with the datetime columns separated into date and time (by seconds).  After the tibble creation, work began on the cleaning of the data. 

  
## Data Dictionary for Columns
  To better understand the data set, it’s important to follow the data dictionary and variables the group decided were important to the success of the project.  Accordingly, the following link: https://www1.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf details the column names and their descriptions.  Nothing was edited from the source material except for the following: 
  tpep_pick_datetime and tpep_dropoff_datetime were changed divided into Pickup_Date/Pickup_Time and Dropoff_Date/Dropoff_Time.  The _date variables are formatted as dates in format of yyyymmdd.  The times are in hms format in seconds, which were converted into minutes for the purposes of analysis.  In addition, Pickup_Location_ID and Dropoff_Location_ID were divided into PULocationID, PU_Borough, PU_Neighbourhood, and PU_ServiceZone.  The Borough and Neighborhood provide character variables with the name of the boroughs and neighborhoods in NYC. 

```{r taxiData2018}
summary(taxiDataClean)
```

## Data Cleaning 
    The data set was cleaned in the following ways:
    1.  All trips that had negative time values were excluded from the dataset.  (i.e. Dropoff_Time-Pickup_Time must be greater than 0)
    2.  The Pickup_Date must be between 2018-01-01 and 2018-12-31
    3.  Trip length is over 0 miles.  
    4.  Trip length is less than 32 miles (The distance between Newark and LaGuardia Airports)
  
  The resulting data set, taxiDataClean, contained 11,785,260 out of 12,000,000 original rows of data.  This equates to a loss of 1.8% of the data. 


```{r taxiDataClean}
summary(taxiDataClean)
```

## Testing Data for Centrality
  In addition to the cleaning, an additional column was added for Average Trip Length by pickup date to visualize the distribution.  The following graphs below some tests.  When filtering the data for average trip length and summarizing the columns by day (356 days), a historgram is produced that illustrates a normal distribution with a mean slighly higher than the median, indicating a positive skewing of the data.  This is illustrated when outliers are included in the attached box plot, below the histogram, which shows the effect of outliers on the greater majority of data points.  


```{r test, include=FALSE}
### Test Case
AverageTripLength <- 
  taxiDataClean %>% 
  group_by(Pickup_Date) %>%
  filter(Pickup_Date<=as.Date("2018-12-31")) %>%
  summarize(avgTripLength = mean(Trip_Distance_Miles), na.rm=TRUE)%>%
  filter(avgTripLength > 0 & avgTripLength < 32) %>%
  select(Pickup_Date, (avgTripLength))
```

```{r histogram1}
### Test Plot
ggplot(data=AverageTripLength, aes(x=avgTripLength))+
  geom_vline(aes(xintercept = mean(avgTripLength)), linetype = 2)+
  geom_histogram(bins=20, fill='red', color="green", alpha=0.5)+
  geom_density(col=200)

```

```{r boxplot}
### Test Plot
boxplot(taxiDataClean$Total_Charge_to_Passengers)

```

Now that we have a clean data set to work with, we can begin to analyze the data in an effort to draw conclusions which would help our prospective Yellow Cab driver in their efforts to maximize their pay.  Cab Drivers are compensated as a function of total fare, with the addition of Tips on top, which are variable. Fortunately Tip data is included in the data set, though it's unavailable for cash transactions as well as cash tips on credit card transactions.  In addition to the total Tip Amount, Tips as a percentage of Fare, as well as Tips as a function of Time will be added to the DataFrame for analysis.  Due to the lack of Tip data on cash transaction, only credit card transactions will be considered for this portion of the analysis.  The resulting dataframe is 8,048,865 observations, after additionally removing outliers.
```{r taxiTipData, include=False}
### add new data
taxiTipData <- taxiDataClean %>%
  mutate(Fare_Pre_Tip = Total_Charge_to_Passengers - Tip_Amount) %>%
  filter(Payment_Type_ID==1) %>%
  mutate(Tip_Percent = (Tip_Amount / Fare_Pre_Tip)*100) %>%
  mutate(Tip_Per_Hour =60 * Tip_Amount / as.integer(Trip_Duration)) %>%
  filter(  Tip_Percent<100&
           Tip_Percent>=0) %>%
  na.omit()
```
```{r histogram2}
ggplot(data=taxiTipData, aes(x=Tip_Percent))+
  geom_vline(aes(xintercept = mean(Tip_Percent)), linetype = 2)+
  geom_histogram(bins=40, fill='blue', color="green", alpha=0.5)+
  geom_density(col=100)
```


The resulting histogram is not altogether surprising, the card readers used to process transactions come with pre-programmed tip amounts, with 20% being one of the defaults.  The average Tip Percentage falling slightly below the preset of 20% is probably in part due to individuals either not tipping at all, or electing to tip in cash in which case we can't know the true tip amount in that instance.  We are additionally able to see that the most common Tip Amount is roughly $5 in addition to being the most common tip amount.  
```{r histogram3}
ggplot(data=taxiTipData, aes(x=Tip_Amount))+
  geom_vline(aes(xintercept = mean(Tip_Amount)), linetype = 2)+
  geom_histogram(bins=40, fill='blue', color="green", alpha=0.5)+
  geom_density(col=100)
```

This provides a cab driver with a baseline of what they should hope to obtain for any given trip, but by identifying routes, pickup locations and where to pickup in order to have the best chance of taking a trip to dropoff locations we can identify potential ways to outperform the average.  The resulting dataframe provides 3877 observations of pairings of Pickup and Dropoff with average and volume statistics.  Trips which occured in too limited a quantity were excluded, as a trip which occurs once per year is not useful for planning where to focus your driving if you are attempting to maximize tips.  Maybe you get lucky and land the single occurence trip and get excellent tips for that day but the low volume means that trip and the resulting tip are likely extremely infrequent.  As a result, a minimum number of trips of 150 was selected, as this represents trips which should occur with some degree of frequency even if it's not daily.


```{r bestTipRoute}
## Best Tip Routes
bestTipRoute <-  taxiTipData %>%
  filter(Tip_Amount != 0, Pickup_Location_ID < 264, Dropoff_Location_ID < 264) %>%
  group_by(PU_Borough, PU_Neighbourhood, DO_Borough, DO_Neighbourhood) %>%
  summarise(avgTipPercentage = mean(Tip_Amount/Fare_Pre_Tip*100), AvgTipAmount = mean(Tip_Amount), PU_DO_Count = n())%>%
  filter(PU_DO_Count > 150) %>%
  arrange(desc(avgTipPercentage))

head(bestTipRoute, n=10)
```

When sorting by the largest tip percentage, a few common Neighborhoods pop up - The Financial District in particular -  as well as both Airports in Queens.  Those familiar with NYC will also be quick to point out that all of these trips are quite close, either within the same neighborhood or into the adjacent neighborhood.  Additionally, while all the tip amounts are above average, the actual Tip Amount is relatively small.  It is feasible that with enough volume, this could be a valid strategy for maximizing daily tips by trying to mainly operate out of these locations - the financial district in particular featuring three times in the top 10 for tip percentage seems a likely place to begin if this is the chosen strategy.  The volume of trips however, indicated by PU_DU_Count are relatively low given the size of our data set.

What if we were to however, only consider the pairings where the average tip Percentage is greater than the sample average and look at the volume of trips pairings.  This still should yield higher than average tips, however we can identify neighborhoods with much larger volume numbers and identify a location with more frequent trips if a volume strategy is sought.
```{r bestTipRoute2}
sample_AvgTipPct <- mean(bestTipRoute$avgTipPercentage)
bestTipRoute2 <-  taxiTipData %>%
  filter(Tip_Amount != 0, Pickup_Location_ID < 264, Dropoff_Location_ID < 264) %>%
  group_by(PU_Borough, PU_Neighbourhood, DO_Borough, DO_Neighbourhood) %>%
  summarise(avgTipPercentage = mean(Tip_Amount/Fare_Pre_Tip*100), AvgTipAmount = mean(Tip_Amount), PU_DO_Count = n())%>%
  filter(avgTipPercentage>sample_AvgTipPct) %>%
  arrange(desc(PU_DO_Count))

head(bestTipRoute2, n=10)
```
In this instance, we see lower average Tip Percentage, but substantially higher volume, while still earning above average tips by percent.  Additionally, these locations are geographically distinct from what we saw in our previous case, though they have the same phenomenon of predominantly representing relatively short trips within one or two neighborhoods.  If a cab driver were to focus on using a high volume strategy with frequent short trips earning above average tips, it seems that the neighborhoods surrounding central park would be a good place to start.


Zooming in Manhattan

In the provided dataset we have taxi trips from all over New York, but the bulk of the trips are to and from Manhattan, so we are focusing only on the trips initiated there by using longitude and latitude measurements. 

### Geting image map for NYC
nyc <- c(lon = -74.0059, lat = 40.7128)
nyc_map <- get_map(location = nyc, zoom = 11)

 







### Best Pickup routes by tip
We are adding another column named tipPercantage by calculating tip Amount over Total Charge to passengers. In this way we finding the best pick up routes by tip. We are dropping missing values as cash tip, or NaN values. We filtered the data by day, time, area/Taxi Name, and Borough. As we can see the report below the highest tip percentage is on the weekends, mostly on Fridays and usually late at night after 21:00 PM.   Also the highest Tip percentage is on areas as La Guardia airport, JFK airport, Murray Hill, South Ozone Park which correspond to Manhattan and Queens boroughs. 

taxiPickup <- taxiDataClean %>%
  select(Pickup_Date,Pickup_Time,Pickup_Location_ID,
         Number_of_Passengers,Payment_Type_ID,Fare_Amount,
         Extra_Charges,MTA_Tax,Tip_Amount,Tolls_Amount,
         Total_Charge_to_Passengers,Trip_Duration)%>%
  mutate(hour = hour(Pickup_Time), 
         wday = wday(Pickup_Date, label = TRUE), 
         month = month(Pickup_Date, label = TRUE),
         tipPercentage= Tip_Amount/Total_Charge_to_Passengers * 100)

taxiPickup <- 
  inner_join(taxiPickup,LocationID,by=c("Pickup_Location_ID"="LocationID"))%>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))%>%
arrange(desc(tipPercentage))
taxiPickup <- drop_na(taxiPickup)

 












Where does the journey begin?
We are drawing a map by using ggmap packages to visualize where in Manhattan are the highest tipped journeys/routes by pick up location. As we see down the plot the best tip routes are in upper east, upper west and downtown in Manhattan. As we go far from there more north or in Queens and Brooklyn the tip percentage starts to decline.

### Ploting pickup journeys on the ggmap
ggmap(nyc_map, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxiPickup, aes( longitude,latitude), bins = 60, alpha = 0.6) +
  labs(x = 'Longitutde', y = 'Latitude', fill = 'Journey')

 



Random Forests find the hot spots by tip
After preparing the data in the cloud with AWS we trained random forests with deep trees to predict the best Tip pickup routes density. The total number of trained data is 11579733 of 23 variables. Our approach is to predict tip pick up density by tip percentage, as latitude, longitude which will give us Boroughs and areas, and by hours, days and months. The number of trees used for this random forest regression is 80 and sample size of 10000 data and number of variables tried at each split is 1. As you can see below when we call fitted_forest we get a mean of squared residual of 68.38 and % Var explained is 1.85.

# Fitting a random forest
fitted_forest <- randomForest(formula = tipPercentage ~ latitude + longitude + hour + wday + month,
                              data = taxiPickup,
                              ntree = 80, sampsize = 10000)

# Printing the fitted_forest object
fitted_forest
> fitted_forest

Call:
 randomForest(formula = tipPercentage ~ latitude + longitude +      hour + wday + month, data = taxiPickup, ntree = 80, sampsize = 10000) 
               Type of random forest: regression
                     Number of trees: 80
No. of variables tried at each split: 1

          Mean of squared residuals: 68.38087
                    % Var explained: 1.85

importance(fitted_forest)
importance(fitted_forest)
          IncNodePurity
latitude       30947.76
longitude      31651.46
hour           32089.06
wday           18836.41
month          22852.34



Using this Random Forest regression, we are getting the best predicted tip routes by pick up. We are adding one more column as pred_tip as you can see below. We are predicting the best tip routes by date, Pickup time, area/taxiName, borough and tip Percentage. As we can see in the table below the best predicted tip routes are in the beginning and end of year as Jan and Oct. The best week is the third week of the month. The best hours are the afternoon hours. Also, the best areas are upper west side, upper east side, union square, midtown and Laguardia corresponding to Manhattan and Queens boroughs.
taxiPickup$pred_tip <- fitted_forest$predicted
taxiPickup <- taxiPickup %>%
  arrange(desc(pred_tip))
taxi_pickup_name <- taxiPickup %>%
  select(Pickup_Date,Pickup_Time,taxiName,Borough,tipPercentage,pred_tip)


 

## Ploting predicted tip percentage by pick up
ggmap(nyc_map, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxiPickup, aes(x = longitude, y = latitude, z = pred_tip), bins = 60, alpha = 0.6, fun = mean) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Predicted Mean Tip Percentage')



 










