library(modelr)
library(tidyverse)
library(hms)
library(devtools)
library(ggmap)
library(lubridate)
library(viridis)
library(jsonlite)
library(randomForest)

memory.limit(size=10e7)

### Load files in r, use pathway below with PC user name.
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
# nycTaxiData2018 <- rbind(sample_n(January_2018,
#                                   size=1000000,
#                                   replace=TRUE))

### Join Tables with Geographic Location

LocationID <- 
  read_csv(file = "C:\\Users\\junaz\\Desktop\\STA 9750\\YCAB\\TaxiLocation.csv")


### Registering Google API key for accessing maps


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
  PU_ServiceZone = nycTaxiData2018$mapName.x,
  Dropoff_Location_ID = nycTaxiData2018$DOLocationID,
  DO_Borough = nycTaxiData2018$Borough.y,
  DO_Neighbourhood = nycTaxiData2018$taxiName.y,
  DO_ServiceZone = nycTaxiData2018$mapName.y,
  Rate_Code_ID = nycTaxiData2018$RatecodeID, 
  Memory_Store_Flag = nycTaxiData2018$store_and_fwd_flag, 
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
  select(1:25) %>%
  filter(Trip_Distance_Miles>0&
           Trip_Distance_Miles<50&
           Pickup_Date>=as.Date("2018-01-01")& 
           Pickup_Date<=as.Date("2018-12-31")&
           Dropoff_Time-Pickup_Time>0) %>%
  mutate(Trip_Duration=(Dropoff_Time-Pickup_Time)/60, na.rm=TRUE) %>%
  select(1:26)

### Resulting data set loses 1.08% of data from original due to cleaning.        

### Test Case
AverageDistanceJan <- 
  taxiDataClean %>% 
  group_by(Pickup_Date) %>%
  filter(Pickup_Date<=as.Date("2018-12-31")) %>%
  summarize(avgTripLength = mean(Trip_Distance_Miles), na.rm=TRUE)%>%
  filter(avgTripLength > 0 | avgTripLength < 10) %>%
  select(Pickup_Date, (avgTripLength))

### Test Plot
ggplot(data=AverageDistanceJan, aes(x=avgTripLength))+
  geom_vline(aes(xintercept = mean(avgTripLength)), linetype = 2)+
  geom_histogram(bins=20, fill='red', color="green", alpha=0.5)+
  geom_density(col=100)

### Geting image map for NYC
nyc <- c(lon = -74.0059, lat = 40.7128)
nyc_map <- get_map(location = nyc, zoom = 11)

### Best Pickup routes by tip
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
         latitude = as.numeric(latitude))
taxiPickup <- drop_na(taxiPickup)
### Ploting pickup journeys on the ggmap
ggmap(nyc_map, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxiPickup, aes( longitude,latitude), bins = 60, alpha = 0.6) +
  labs(x = 'Longitutde', y = 'Latitude', fill = 'Journey')



# Fitting a random forest
fitted_forest <- randomForest(formula = tipPercentage ~ latitude + longitude + hour + wday + month,
                              data = taxiPickup,
                              ntree = 80, sampsize = 10000)

# Printing the fitted_forest object
fitted_forest


taxiPickup$pred_tip <- fitted_forest$predicted
## We can filter and plot the data as we want, for example by weekday, hour, month, distance etc

# taxiPickupSaturday<- taxiPickup %>%
#   filter(wday == "Sat")

ggmap(nyc_map, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxiPickup, aes(x = longitude, y = latitude, z = pred_tip), bins = 60, alpha = 0.6, fun = mean) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Predicted Mean Tip Percentage')


##Old prediction

bestTipRoute <-
  taxiDataClean %>%
  filter(taxiDataClean$Tip_Amount != 0) %>%
  group_by(Dropoff_Location_ID) %>%
  summarise(avgTipPercentage = mean(Tip_Amount/Total_Charge_to_Passengers*100))%>%
  arrange(desc(avgTipPercentage))

bestTipRoute <- inner_join(bestTipRoute,
                           LocationID,
                           by=c("Dropoff_Location_ID"="LocationID"))


ggplot(data=bestTipRoute, aes(x=Dropoff_Location_ID, y=avgTipPercentage))+
  geom_bar(stat = "identity") +
  ylim(16,25)

