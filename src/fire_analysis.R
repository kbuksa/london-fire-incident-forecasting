
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ---
# Load libraries
library(sf) # Spatial data
library(ggplot2) # Visualisation
library(tmap) # Spatial mapping
library(spdep) # Spatial analysis
library(forecast) # Time series
library(stats) # ARIMA
library(tseries) # Stationarity testing

# Load STARIMA package
source("../data/starima_package.R")

# Data prep 
# Load LFB Incident reports dataset
london_fire <- read.csv("../data/london-fire-incident/LFB Incident data from 2009 - 2017.csv")

# Select columns to keep
wanted_cols <- c("DateOfCall", "CalYear", "HourOfCall",
                 "IncidentGroup", "StopCodeDescription",
                 "PropertyCategory", "PropertyType",
                 "IncGeo_BoroughName",
                 "Easting_rounded", "Northing_rounded")
london_fire <- london_fire[, wanted_cols]

# Set borough name column to lower for compatibility with shapefile
london_fire$IncGeo_BoroughName <- tolower(london_fire$IncGeo_BoroughName)

# Convert data column to correct data type
london_fire$DateOfCall <- as.Date(as.character(london_fire$DateOfCall), format = "%d-%b-%y")

# Create additional useful time columns for months and month-year
london_fire$Month <- format(london_fire$DateOfCall, "%B")
london_fire$MonthYear <- format(london_fire$DateOfCall, "%Y-%m")

# Convert Easting & Northing to numeric, then to proper spatial data type
london_fire$Easting_rounded <- as.numeric(london_fire$Easting_rounded)
london_fire$Northing_rounded <- as.numeric(london_fire$Northing_rounded)
london_fire_sf <- st_as_sf(london_fire, coords = c("Easting_rounded", "Northing_rounded"), crs = 27700)


#---
# EDA
# Get information of dataset
summary(london_fire_sf)
dim(london_fire_sf)

# https://www.spsanderson.com/steveondata/posts/2024-12-03/#counting-missing-values-in-each-column
# Get count of missing values in each row
colSums(is.na(london_fire_sf)) # All columns are complete

# Count incidents by year & type
# https://www.statology.org/table-function-in-r/ to use table to get group frequencies
incident_type_count <- as.data.frame(table(london_fire_sf$CalYear, london_fire_sf$IncidentGroup))
colnames(incident_type_count) <- c("Year", "IncidentGroup", "Count")

# Stacked bar plot of yearly incident counts by type
ggplot(incident_type_count, aes(x = Year, y = Count, fill = IncidentGroup, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  labs(title = "London Incident Counts per Incident Type by Year (2009 - 2017)", 
       x = "Year", y = "Total Incidents Count", fill = "Incident Type")+
  theme(plot.title = element_text(hjust = 0.5))

# Filter for only fire incidents
london_fire_sf <- london_fire_sf[london_fire_sf$IncidentGroup == "Fire",]

# Analyse on monthly basis
monthly_fire_count <- as.data.frame(table(london_fire_sf$Month))
colnames(monthly_fire_count) <- c("Month", "Count")
monthly_fire_count$Month <- factor(monthly_fire_count$Month, levels = month.name)

# Plot monthly incidents
ggplot(monthly_fire_count, aes(x = Month, y = Count))+
  geom_bar(stat = "identity")+
  labs(title = "London Fire Incidents by Month (2009 - 2017)", 
       x = "Month", y = "Fire Incidents Count")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Spatial mapping
# Load borough .shp
london_borough_shp <- st_read("../data/london-borough-shp/London_Boroughs.gpkg")
london_borough_shp$name <- tolower(london_borough_shp$name)

# Set CRS to OSGB
st_crs(london_borough_shp) = 27700

# Show total fire count for each borough
borough_total_fires <- as.data.frame(table(london_fire_sf$IncGeo_BoroughName))
colnames(borough_total_fires) <- c("Borough", "TotalFires")

# Merge total fires for boroughs with shapefile
borough_shp_fires <- merge(x=london_borough_shp, y=borough_total_fires, by.x="name", by.y="Borough", all.x=T)

# Plot total fires
tm_shape(borough_shp_fires)+
  tm_polygons(col = "TotalFires")+
  tm_layout(legend.outside = TRUE)

# Time-series prep
# Collect fire counts by each borough for each month-year
borough_monthly <- as.data.frame(table(london_fire_sf$IncGeo_BoroughName, london_fire_sf$MonthYear))
colnames(borough_monthly) <- c("Borough", "MonthYear", "FireCount")
borough_monthly$MonthYear <- as.Date(paste0(borough_monthly$MonthYear, "-01"))

# Create a sequence of dates for every 6 months (Jan and Jul each year)
# https://stackoverflow.com/questions/48939652/scale-x-date-makes-plot-begins-at-april-not-january-in-ggplot
year_breaks <- seq(as.Date("2009-01-01"), as.Date("2017-12-01"), by = "12 months")

# Plot monthly fire counts for each borough
# https://www.datacamp.com/tutorial/facets-ggplot-r to make multiple line graphs for different groups
ggplot(borough_monthly, aes(x = MonthYear, y = FireCount, group = Borough))+
  geom_line(color="blue")+
  facet_wrap(~Borough, scales ="fixed", ncol = 4, strip.position = "top")+ # Fixed columns show counts equally for boroughs
  scale_x_date(breaks = year_breaks, date_labels = "%Y-%m") + # Show every 6 months
  labs(title = "Monthly Fire Counts Boroughs (2009-2017)",
       x = "Month-Year", y = "Fire Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Reshape data into matrix & convert
# https://stackoverflow.com/questions/5890584/how-to-reshape-data-from-long-to-wide-format
borough_monthly_wide <- reshape(borough_monthly, idvar = "Borough", timevar = "MonthYear", direction = "wide")
colnames(borough_monthly_wide) <- gsub("FireCount\\.", "", colnames(borough_monthly_wide)) # Remove all "FireCount." name instances
rownames(borough_monthly_wide) <- borough_monthly_wide$Borough
borough_fire_matrix <- data.matrix(borough_monthly_wide[, -1])

# Get borough stats
borough_mean <- rowMeans(borough_fire_matrix)
borough_sd <- apply(borough_fire_matrix, 1, sd)
borough_iqr <- apply(borough_fire_matrix, 1, IQR)
borough_cv <- (borough_sd/borough_mean) * 100 # CV = (SD/Mean) * 100

# Combine into a dataframe
# Result: 
# Tower Hamlets: Consistent high incidents
# Kingston: Consistent low incidents, stable (contrast to first)
# Hillingdon: Middle ground, has quite clear seasonal spikes & drops
borough_stats <- data.frame(
  Mean = borough_mean,
  SD = borough_sd,
  IQR = borough_iqr,
  CV = borough_cv
)


# Heatmap - used example from practical
heatmap(borough_fire_matrix,Rowv=NA,Colv=NA, 
        col=heat.colors(256),scale="row", margins=c(5,3),
        xlab="Month-Year",ylab="Borough", 
        cexCol=0.7, cexRow = 0.64)

# Global Spatial Autocorrelation
weight <- nb2listw(poly2nb(london_borough_shp))
borough_fire_avg <- rowMeans(borough_fire_matrix)
moran.test(x=borough_fire_avg, listw=weight)
moran.mc(x=borough_fire_avg, listw=weight, nsim=9999)

moran.plot(borough_fire_avg, weight, xlab="Borough average Fire Count", ylab="Spatailly lagged Borough Fire Count")


# Time Series prep (Hillingdon)

# Augmented Dickey-Fuller Test to check for stationarity
# Dickey-Fuller = -6.6981, Lag order = 4, p-value = 0.01
# Strong stationarity due to p<0.05
# d = 0
adf.test(borough_fire_matrix["hillingdon", ])

# Seasonal differencing test for 12-month lags
# Dickey-Fuller = -5.5112, Lag order = 4, p-value = 0.01
# D = 0
adf.test(diff(borough_fire_matrix["hillingdon", ], lag = 12))

#ACF
acf(borough_fire_matrix["hillingdon", ], lag.max=18, main="ACF 18 lags, Hillingdon")

# PACF
pacf(borough_fire_matrix["hillingdon", ], lag.max=18, main="PACF 18 lags, Hillingdon")

# Seasonal Differencing 
hillingdon_s_diff <- diff(borough_fire_matrix["hillingdon", ], lag=12, differences=1)
acf(hillingdon_s_diff, lag.max=18, xlab="Lag", ylab="ACF", main="18 lags Differenced autocorrelation plot, Hillingdon")
pacf(hillingdon_s_diff, lag.max=18, xlab="Lag", ylab="PACF", main="18 lags Differenced Partial Autocorrelation plot, Hillingdon")

# STARIMA DO ON ALL BOROUGHS

# Time Series prep (Tower Hamlets)

# Augmented Dickey-Fuller Test to check for stationarity
# Dickey-Fuller = -4.7018, Lag order = 4, p-value = 0.01
# Strong stationarity due to p<0.05
# d = 0
adf.test(borough_fire_matrix["tower hamlets", ])

# Seasonal differencing test for 12-month lags
# Dickey-Fuller = -2.9513, Lag order = 4, p-value = 0.1834
# Strong non-stationarity due to p>0.05

# D = 1
adf.test(diff(borough_fire_matrix["tower hamlets", ], lag = 12))

# ACF
acf(borough_fire_matrix["tower hamlets", ], lag.max=18, main="ACF 18 lags, Tower Hamlets")

# PACF
pacf(borough_fire_matrix["tower hamlets", ], lag.max=18, main="PACF 18 lags, Tower Hamlets")

# Diff ACF/PACF
th_s_diff <- diff(borough_fire_matrix["tower hamlets", ], lag=12, differences=1)
acf(th_s_diff, lag.max=18, xlab="Lag", ylab="ACF", main="18 lags Differenced autocorrelation plot, Tower Hamlets")
pacf(th_s_diff, lag.max=18, xlab="Lag", ylab="PACF", main="18 lags Differenced Partial Autocorrelation plot, Tower Hamlets")



# Time Series prep (Kingston Upon Thames)

# Augmented Dickey-Fuller Test to check for stationarity
# Dickey-Fuller = -4.8079, Lag order = 4, p-value = 0.01
# Strong stationarity due to p<0.05
# d = 0
adf.test(borough_fire_matrix["kingston upon thames", ])

# Seasonal differencing test for 12-month lags
# Dickey-Fuller = -4.6564, Lag order = 4, p-value = 0.01
# D = 0
adf.test(diff(borough_fire_matrix["kingston upon thames", ], lag = 12))

# ACF
#q=1
acf(borough_fire_matrix["kingston upon thames", ], lag.max=18, main="ACF 18 lags, Kingston Upon Thames")

# PACF
pacf(borough_fire_matrix["kingston upon thames", ], lag.max=18, main="PACF 18 lags, Kingston Upon Thames")

# Diff ACF/PACF
kt_s_diff <- diff(borough_fire_matrix["kingston upon thames", ], lag=12, differences=1)
acf(kt_s_diff, lag.max=18, xlab="Lag", ylab="ACF", main="18 lags Differenced autocorrelation plot, Kingston Upon Thames")
pacf(kt_s_diff, lag.max=18, xlab="Lag", ylab="PACF", main="18 lags Differenced Partial Autocorrelation plot, Kingston Upon Thames")


# ARIMA predictions
# Define 2017 data for test prediction
test_dates <- seq(as.Date("2017-01-01"), as.Date("2017-12-01"), by = "month")
# Set training to be from 01.2009 - 12.2016
train_end <- 96
# Set training to be for all 2017 months
test_start <- 97
test_end <- 108

# Hillingdon
#ARIMA(p,d,q) p = PACF, q = ACF, 
# Training set 01-2009 to 12-2016 , test set 01-2017 to 12-2017 (last 12 months)
hill.fit.ar <- arima(borough_fire_matrix["hillingdon", 1:train_end],order=c(1,0,0),seasonal=list(order=c(1,0,1),period=12))
hill.fit.ar

hill.pre.ar<-predict(hill.fit.ar, n.ahead=12)
matplot(test_dates,cbind(borough_fire_matrix["hillingdon", test_start:test_end],hill.pre.ar$pred),type="l",main="Hillingdon ARIMA(1,0,0)(1,0,1)12", xlab="Month", ylab="Fire Counts")

# Tower Hamlets
#ARIMA(p,d,q) p = PACF, q = ACF, 
# Training set 01-2009 to 12-2016 , test set 01-2017 to 12-2017 (last 12 months)
th.fit.ar <- arima(borough_fire_matrix["tower hamlets", 1:train_end],order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12))
th.fit.ar

th.pre.ar<-predict(th.fit.ar, n.ahead=12)
matplot(test_dates,cbind(borough_fire_matrix["tower hamlets", test_start:test_end],th.pre.ar$pred),type="l",main="Tower Hamlets ARIMA(1,0,1)(1,1,1)12", xlab="Month", ylab="Fire Counts")

# Kingston Upon Thames
# ARIMA(p,d,q) p = PACF, q = ACF, 
# Training set 01-2009 to 12-2016 , test set 01-2017 to 12-2017 (last 12 months)
kt.fit.ar <- arima(borough_fire_matrix["kingston upon thames", 1:train_end],order=c(1,0,2),seasonal=list(order=c(1,0,1),period=12))
kt.fit.ar

kt.pre.ar<-predict(kt.fit.ar, n.ahead=12)
matplot(test_dates,cbind(borough_fire_matrix["kingston upon thames", test_start:test_end],kt.pre.ar$pred),type="l",main="Kingston Upon Thames ARIMA(1,0,2)(1,0,1)12", xlab="Month", ylab="Fire Counts")

tsdiag(hill.fit.ar)
tsdiag(th.fit.ar)
tsdiag(kt.fit.ar)

# RMSE
NRMSE_hill_fit <- NRMSE(res=hill.fit.ar$residuals, obs=borough_fire_matrix["hillingdon", 1:train_end])
NRMSE_th_fit <- NRMSE(res=th.fit.ar$residuals, obs=borough_fire_matrix["tower hamlets", 1:train_end])
NRMSE_kt_fit <- NRMSE(res=kt.fit.ar$residuals, obs=borough_fire_matrix["kingston upon thames", 1:train_end])
NRMSE_hill_fit
NRMSE_th_fit
NRMSE_kt_fit

# auto.arima
fit.auto.ar.th <- auto.arima(borough_fire_matrix["tower hamlets",1:train_end], seasonal = T)
fit.auto.ar.th

pre.auto.ar.th<-predict(fit.auto.ar.th, n.ahead=12)
matplot(1:12,cbind(borough_fire_matrix["tower hamlets", test_start:test_end],pre.auto.ar.th$pred),type="l",main="", xlab="Month", ylab="Fire Counts Auto ARIMA Tower Hamlets")

fit.auto.ar.kt <- auto.arima(borough_fire_matrix["kingston upon thames", 1:train_end], seasonal = T)
fit.auto.ar.kt

pre.auto.ar.kt<-predict(fit.auto.ar.kt, n.ahead=12)
matplot(1:12,cbind(borough_fire_matrix["kingston upon thames", test_start:test_end],pre.auto.ar.kt$pred),type="l",main="", xlab="Month", ylab="Fire Counts Auto ARIMA Kingston")

# STARIMA
weight_matrix <- listw2mat(weight)

col_matrix <- t(borough_fire_matrix)

stacf(col_matrix, weight_matrix, 24)
stpacf(col_matrix, weight_matrix, 24)

col_matrix.diff <- diff(col_matrix,lag=12,differences=1)
stacf(col_matrix.diff, weight_matrix, 24)
stpacf(col_matrix.diff, weight_matrix, 24)

W_fit<-list(w1=weight_matrix) # Create a list of spatial weight matrices, zero not needed
fit.star <- starima_fit(Z=col_matrix[1:train_end,],W=W_fit,p=0,d=12,q=4)

# Diagnostic Checking
stacf(fit.star$RES,weight_matrix,24)


pre.star <- starima_pre(col_matrix[(test_start-12-1+1):test_end,],model=fit.star)
matplot(test_dates,cbind(col_matrix[test_start:test_end,1],pre.star$PRE[,1]),type="l", main="STARIMA(0,1,4)12", ylab="Fire Count")

fit.star$NRMSE
