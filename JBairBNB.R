#########################################################################################################################################
# Last Date Modified: 6/21/20
# Author: Jasmine Barrera
# Description: Exploratory Data Analysis

# New York City Airbnb Dataset Analysis (Kaggle)
# Questions:
# 1) What are the top ten most frequent neighbourhoods?
# 2) How does listing PRICE vary by room type, neighbourhood group, number of reviews per month, and availability?
# 3) How does ROOM TYPE vary by neighbourhood group, availability, and number of reviews per month?
# 4) How do different LOCATIONS (latitude, longitude) and/or neighbourhood groups 
#     vary by listing price, room type, and availability?
#########################################################################################################################################


###################
# INSTALL PACKAGES
###################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(grid)
library(gridBase)
library(ggmap)
library(leaflet)

##################
# INSTALL DATASET
##################
setwd("C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Airbnb")
data <- read.csv("AB_NYC_2019.csv", header = TRUE)

###############
# DATA SUMMARY
###############
## Basic summary of data 
dim(data) # 48895 rows x 16 columns
str(data)
summary(data)
View(data)

## Basic Missing Value Examination
colnames(data)[ apply(data, 2, anyNA) ] # "reviews_per_month" column has missing values
sum(is.na(data$reviews_per_month)) # 10052 missing values

missing_data <- data[rowSums(is.na(data)) > 0,]
missing_data
# It appears that whenever the number of reviews is zero, the reviews per month was recorded as NA.
# Now need to change reviews per month so that whenever number of reviews is zero, reviews per month is also zero.

data$reviews_per_month[data$number_of_reviews == 0] <- 0
sum(is.na(data$reviews_per_month)) # confirm no missing values

############################
# Exploratory Data Analysis
############################
## Univariate Analysis of Qualitative Columns of Interest
############################################################################################################
# (1) The ten neighbourhoods with the most listings.
diff_room_types <- table(data$room_type)
neighb_groups <- table(data$neighbourhood_group)
top_ten_neighb <- sort(table(data[, "neighbourhood"]), decreasing = TRUE)[1:10] # Counts of ten most frequent neighbourhoods

diff_room_types
neighb_groups
top_ten_neighb

# Barplot of Room Type:
barplot(diff_room_types, main = "Airbnb: Barplot of Room Type", xlab = "Room Type", ylab = "Count", col = "lightpink",
        ylim=range(pretty(c(0, diff_room_types))))
# Entire home/apt and private room are the most common, in that order.

# Barplot of Neighbourhood Group:
barplot(neighb_groups, main = "Airbnb: Barplot of Neighbourhood Group", xlab = "Neighbourhood Group", ylab = "Count",
    col = "lightpink", ylim=range(pretty(c(0, neighb_groups))))
# Manhattan and Brooklyn had the most listings, in that order, followed by Queens.

# Barplot of Top 10 Most Frequent Neighbourhoods:
midpts <- barplot(top_ten_neighb, main = "Airbnb: Barplot of 10 Most Frequent Neighbourhoods", ylab = "Count",
    col = "lightpink", names.arg = "", ylim=range(pretty(c(0, top_ten_neighb))))
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(names(top_ten_neighb),
          x = unit(midpts, "native"), y=unit(-0.5, "lines"),
          just="right", rot=50, gp = gpar(fontsize = 9))

# Interpretation: 
# The top ten most frequent neighbourhoods in NYC have a listing frequency 
# ranging from about 1500 to 4000.

############################################################################################################
## Univariate Analysis of Quantitative Columns of Interest
summary(data$price)
summary(data$reviews_per_month)
summary(data$availability_365)

# Histogram of Price:
ggplot(data, aes(x=price)) + geom_histogram(binwidth = 25, color="black", fill="lightpink") + 
  coord_cartesian(ylim=c(0,10500)) + 
  labs(x = "Price ($)", y = "Count", title = "Airbnb: Histogram of Price($)") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# The price was highly skewed to the right, with the majority of listings have a price ranging from $0 to $1,000.

# Hisogram of Price ranging from $0 to $1,000:
ggplot(data, aes(x=price)) + geom_histogram(binwidth = 25, color="black", fill="lightpink") + 
  coord_cartesian(xlim=c(0,1000), ylim=c(0,9500)) + 
  labs(x = "Price ($)", y = "Count", title = "Airbnb: Histogram of Price($) up to $1000") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# Histogram of # of Reviews Per Month:
ggplot(data, aes(x=reviews_per_month)) + geom_histogram(breaks = seq(1, 60, by=0.5), binwidth = 25, color="black", fill="lightpink") + 
  coord_cartesian(ylim=c(0,5000)) +
  labs(x = "Reviews/Month", y = "Count", title = "Airbnb: Histogram of Reviews/Month") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# The reviews/month was highly skewed to the right, with the majority of listings have less than 20 reviews per month.

# Histogram of # of Reviews Per Month up to 20:
ggplot(data, aes(x=reviews_per_month)) + geom_histogram(breaks = seq(1, 20, by=0.5), color="black", fill="lightpink") + 
  coord_cartesian(xlim=c(0,20), ylim=c(0,4500)) + 
  labs(x = "Reviews/Month", y = "Count", title = "Airbnb: Histogram of Reviews/Month up to 20") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# Histogram of Availability 365:
ggplot(data, aes(x=availability_365)) + geom_histogram(binwidth = 25, color="black", fill="lightpink") + 
  labs(x = "# of Nights Available", y = "Count", title = "Airbnb: Histogram of Availability") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# The majority of properties are available for about 0 to 50 days. The rest of the availability distribution appears uniform.

# Histogram of Availability up to 50:
ggplot(data, aes(x=availability_365)) + geom_histogram(breaks = seq(1, 50, by=4), color="black", fill="lightpink") + 
  coord_cartesian(xlim=c(0,50), ylim=c(0,1500)) +
  labs(x = "# of Nights Available", y = "Count", title = "Airbnb: Histogram of Availability up to 50") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

## Bivariate Analysis
############################################################################################################
# (2) Price vs. room type, neighbourhood group, number of reviews per month, and availability.

# Boxplot of price vs. room type:
ggplot(data, aes(x=room_type, y=price)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Room Type", y = "Price($)", title = "Airbnb: Boxplot of Price ($) vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Median prices for each room type:
rooms <- levels(data$room_type)
for (i in seq(1,length(rooms),1)) {
  print(paste("Median price ($) for", rooms[i], ":", median(data$price[data$room_type == rooms[i]])))
}
# Median price ($) for Entire home/apt : 160
# Median price ($) for Private room : 70
# Median price ($) for Shared room : 45
# To get a better look at the majority of the data (IQR), the price limit could be set to $1,000.

# Boxplot of price up to $1000 vs. room type:
ggplot(data, aes(x=room_type, y=price)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  coord_cartesian(ylim=c(0,1000)) +
  labs(x = "Room Type", y = "Price($)", title = "Airbnb: Boxplot of Price ($) up to $1000 vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# The price of a listing for an entire home/apt was slighlty skewed to the right and had a median of $160, much higher than the 
# other two room types. The median reservaion prices were slightly different for the private room and shared room types, 
# $70 and $45 respectively, but the price for a shared room appeared slightly skewed to the right while the price for a private room
# appeared symmetric. Note that a majority of the outliers past the fourth quartile group made the price data even more skewed 
# to the right, with the entire home/apt room type having the largest range of outliers from about $130 to $10,000. 

# Boxplot of price vs. neighbourhood group:
ggplot(data, aes(x=neighbourhood_group, y=price)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Neighbourhood Group", y = "Price($)", title = "Airbnb: Boxplot of Price ($) vs. Neighbourhood Group") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Median prices for each neighbourhood group:
neighb_groups <- levels(data$neighbourhood_group)
for (i in seq(1,length(neighb_groups),1)) {
  print(paste("Median price ($) for", neighb_groups[i], ":", median(data$price[data$neighbourhood_group == neighb_groups[i]])))
}
# Median price ($) for Bronx : 65
# Median price ($) for Brooklyn : 90
# Median price ($) for Manhattan : 150
# Median price ($) for Queens : 75
# Median price ($) for Staten Island : 75
# To get a better look at the majority of the data (IQR), the price limit could be set to $1,000.

# Boxplot of price up to $1000 vs. neighbourhood group:
ggplot(data, aes(x=neighbourhood_group, y=price)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  coord_cartesian(ylim=c(0,1000)) +
  labs(x = "Neighbourhood Group", y = "Price($)", title = "Airbnb: Boxplot of Price ($) up to $1000 vs. Neighbourhood Group") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# The price of a listing for Manhattan was symmetric and had a median of $150, much higher than the other neighbourhood groups.
# Brooklyn had the second highest median price of $90, but was slightly skewed to the right and had many outliers ranging from about
# $270 to $2,500 with some even up to $10,000. Queens and Staten Island both appeared symmetric and had the same median price of $75,
# however Queens had many more outliers ranging from about $200 to $2,500, with one even up to $10,000, while Staten Island had an 
# outlier of about $5,000 while most outliers ranged from $200 to $1,250. The Bronx had the lowest median price of $65 and appeared
# slightly skewed to the right, with most outliers ranging from about $200 to $1,000, with a single outlier at $2,500.

# Scatterplot of price vs. number of reviews per month:
plot(data$reviews_per_month, data$price, xlab="Reviews/Month", ylab="Price($)",
     main="Airbnb: Scatterplot of Price ($) vs. Reviews/Month")
# Besides the outliers of about 30 and 60 reviews/month for a low priced listing, there were mostly 0 to 20 reviews/month with
# most priced below $2,000.

# Scatterplot of price up to $1000 vs. number of reviews per month up to 20:
plot(data$reviews_per_month, data$price, xlab="Reviews/Month", ylab="Price($)", 
     main="Airbnb: Scatterplot of Price ($) up to $1000 vs. Reviews/Month up to 20", xlim=c(0,20), ylim=c(0,1000))

# The listings with the most reviews tended to have a price lower than $200. There were few reviews per month for
# listings with higher prices. Generally, the number of reviews per month increased with decreasing price, but there was
# still a large number of listings with few reviews and a low price.

# Scatterplot of price vs. availability 365:
plot(data$availability_365, data$price, xlab="# of Nights Available", ylab="Price($)",
     main="Airbnb: Scatterplot of Price ($) vs. Availability")
# There were many listings with a price of about $1,000 that had a # of nights available ranging from 0 to 365. There appeared
# to be spikes in price for 0 and 365 nights (0 and 12 months), ranging from $0 to $6,500 with the outlier of $10,000. There were also 
# spikes in price for the 90 and 190 nights (3 and 6 months), ranging from $0 to about $4,000, with even more expensive outliers.

# Scatterplot of price up to $1000 vs. availability up to 50:
plot(data$availability_365, data$price, xlab="# of Nights Available", ylab="Price($)",
     main="Airbnb: Scatterplot of Price ($) up to $1000 vs. Availability up to 50", xlim=c(0,50), ylim=c(0,1000))
# There appeared to be many high priced reserverations with 0 nights available. Recommend further exploration...

# Scatterplot of price up to $1000 vs. availability from 365:
plot(data$availability_365, data$price, xlab="# of Nights Available", ylab="Price($)",
     main="Airbnb: Scatterplot of Price ($) up to $1000 vs. Availability from 300", xlim=c(300,365), ylim=c(0,1000))
# There appeared to be many high priced reserverations with 365 nights available.

# Interpretation:
# Listings that were an entire room/apt, in Manhattan, and available 0, 90, 190, or 365 nights had the highest prices.
# Listings that had many reviews per month tended to have a lower price.
# Listings with a price less than $200 tended to have uniform availability for 0 to 365 nights. 
############################################################################################################
# (3) Room type vs. neighbourhood group, number of reviews per month, and availability.

# Stacked barplot of room type vs. neigbhourhood group:
ggplot(data, aes(x=neighbourhood_group, fill=room_type)) + geom_bar(position="stack") +
  labs(x = "Neighbourhood Group", y = "Count", title = "Airbnb: Stacked Barplot of Room Type and Neighbourhood Group", fill="Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# The majority of Manhattan listings were an entire home/apt, followed by private room. A very small amount were shared rooms.
# The majority of Brooklyn listings were private rooms, closely followed by entire homes/apts, and also with very few shared rooms.
# The majority of Queens listings were private rooms, followed by entire homes/apts and with very few shared rooms.
# The majority of Bronx listings were private rooms, closely followed by entire homes/apts and with hardly any shared rooms.
# There appeared to be an equal number of listings of entire homes/apts and private rooms in Staten Island, with no shared rooms.

# Boxplot of room type vs. number of reviews per month:
ggplot(data, aes(x=room_type, y=reviews_per_month)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Room Type", y = "# of Reviews/Month", title = "Airbnb: Boxplot of # of Reviews/Month vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Median number of reviews for each room type:
rooms <- levels(data$room_type)
for (i in seq(1,length(rooms),1)) {
  print(paste("Median # of reviews/month for", rooms[i], ":", median(data$reviews_per_month[data$room_type == rooms[i]])))
}
# Median # of reviews/month for Entire home/apt : 0.35
# Median # of reviews/month for Private room : 0.4
# Median # of reviews/month for Shared room : 0.405
# To get a better look at the majority of the data (IQR), the reviews/month limit could be set to 20. 

# Boxplot of room type vs. number of reviews per month up to 20:
ggplot(data, aes(x=room_type, y=reviews_per_month)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  coord_cartesian(ylim=c(0,20)) +
  labs(x = "Room Type", y = "# of Reviews/Month", title = "Airbnb: Boxplot of # of Reviews/Month vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# All room types had about the same median number of reivews per month (about 0.4 per month). The private room type had a single 
# outlier of 60 and another outlier around 30 reviews/month. 

# Boxplot of room type vs. number of reviews:
ggplot(data, aes(x=room_type, y=number_of_reviews)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Room Type", y = "# of Reviews", title = "Airbnb: Boxplot of # of Reviews vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Median number of reviews for each room type:
rooms <- levels(data$room_type)
for (i in seq(1,length(rooms),1)) {
  print(paste("Median # of reviews for", rooms[i], ":", median(data$number_of_reviews[data$room_type == rooms[i]])))
}
# Median # of reviews for Entire home/apt : 5
# Median # of reviews for Private room : 5
# Median # of reviews for Shared room : 4

# Listings for all room types had a similar median value and were skewed to the right. There were many more outliers for
# private room types, with some outliers near 600 reviews, followed by entire home/apt with some outliers near 500.
# Shared rooms had a single outlier around 450. 

# Boxplot of room type vs. availability 365:
ggplot(data, aes(x=room_type, y=availability_365)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Room Type", y = "# of Nights", title = "Airbnb: Boxplot of Availability vs. Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Median availability for each room type:
rooms <- levels(data$room_type)
for (i in seq(1,length(rooms),1)) {
  print(paste("Median # Nights Available for", rooms[i], ":", median(data$availability_365[data$room_type == rooms[i]])))
}

# Median # Nights Available for Entire home/apt : 42
# Median # Nights Available for Private room : 45
# Median # Nights Available for Shared room : 90
# Shared rooms had the largest spread (range from 0 to about 350 nights), with a median of 90 nights available.
# Entire home/apt and private room types were similar in spread (0 to about 200 nights), with entire home/apt having a slightly
# wider range, and also had similar medians.

# Interpretation:
# Most of Manhattan listings were entire homes/apts. Most listings for the other neighbourhood groups were private rooms, followed by
# entire home/apts. All the neighbourhood groups had hardly any or no listings with shared rooms, with Manhattan having the most,
# followed by Brooklyn, Queens, and Bronx.

# Listings with entire house/apt had the lowest number of reviews per month, but had a median of 5 reviews total. Listings with
# shared rooms had the highest number of reviews per month, but had a median of 4 reviews total. 

# Shared rooms had the highest number of nights available, while entire homes/apts had the least number of nights available. 


############################################################################################################
## Location/Multivariate Analysis
############################################################################################################
# (4) Location (latitude, longitude) and/or neighbourhood groups vs. price, room type, and availability.

height <- max(data$latitude) - min(data$latitude)
width <- max(data$longitude) - min(data$longitude)
borders <- c(bottom  = min(data$latitude)  - 0.1 * height,
             top     = max(data$latitude)  + 0.1 * height,
             left    = min(data$longitude) - 0.1 * width,
             right   = max(data$longitude) + 0.1 * width)

# Map showing different neighbourhood groups:
map_group <- get_stamenmap(borders, zoom = 11, maptype = "terrain", color="color")
ggmap(map_group) + geom_point(data = data, mapping = aes(x = longitude, y = latitude, color = neighbourhood_group)) +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb: Map of NYC with Neighbourhood Groups", color="Neighbourhood Group") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))

# Map showing room type:
map_room <- get_stamenmap(borders, zoom = 11, maptype = "terrain-lines", color="color")
ggmap(map_room) + geom_point(data = data, mapping = aes(x = longitude, y = latitude, color = room_type), size=3, alpha=0.75) +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb: Map of NYC with Room Type", color="Room Type") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# The southern part of Manhattan appears to have more listings with the entire homes/apts room type.
# The southern part of Staten Island appears to mostly have listings with the entire room/apt type.

# Map showing availability > 0:
avail_filter <- filter(data, availability_365 > 0)
map_avail <- get_stamenmap(borders, zoom = 11, maptype = "terrain-lines", color="color")
ggmap(map_avail) + geom_point(data = avail_filter, mapping = aes(x = longitude, y = latitude, color = availability_365), size=3, alpha=0.75) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb: Map of NYC with Availability > 0", color="Availability") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# Excluding listings where availability is zero, it appears that there is a number of listings in Manhattan and Brooklyn that
# are available for only a few nights of the year. 

# Map showing price for listings w/ price < $1,000, avaialability > 0, private room type
filter1 <- filter(data, price < 1000, availability_365 > 0, room_type == "Private room")

map_filter1 <- get_stamenmap(borders, zoom = 11, maptype = "terrain-lines", color="color")
ggmap(map_filter1) + geom_point(data = filter1, mapping = aes(x = longitude, y = latitude, color=price), alpha=0.75, size=2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb: Map of NYC with Price($) < $1,000 and Availability > 0 \nand Private Room Type", 
       color="Price($)") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# With the given filters, there appears to more expensive private room listings on the southern part of Manhattan and in Brooklyn
# than in other areas.

# Map showing price in Manhattan listings w/price < $1,000, availability >0, private room type:
manh_data <- filter(data, neighbourhood_group == "Manhattan", room_type == "Private room", price < 1000, availability_365 > 0)

height_manh <- max(data$latitude[data$neighbourhood_group == "Manhattan"]) - min(data$latitude[data$neighbourhood_group == "Manhattan"])
width_manh <- max(data$longitude[data$neighbourhood_group == "Manhattan"]) - min(data$longitude[data$neighbourhood_group == "Manhattan"])
borders_manh <- c(bottom  = min(data$latitude[data$neighbourhood_group == "Manhattan"])  - 0.1 * height,
                  top     = max(data$latitude[data$neighbourhood_group == "Manhattan"])  + 0.1 * height,
                  left    = min(data$longitude[data$neighbourhood_group == "Manhattan"]) - 0.1 * width,
                  right   = max(data$longitude[data$neighbourhood_group == "Manhattan"]) + 0.1 * width)

map_manh_price <- get_stamenmap(borders_manh, zoom = 11, maptype = "terrain-lines", color="color")
ggmap(map_manh_price) + geom_point(data = manh_data, mapping = aes(x = longitude, y = latitude, color = price), size=2, alpha=0.75) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb: Map of NYC with Price($) < $1,000 and Availability > 0 \nand Private Room Type in Manhattan",
       color="Price($)") +
  theme(plot.title = element_text(face = "bold", size = (15), hjust = 0.5))
# With the given filters, there appears to be more expensive private rooms in the southern part of Manhattan.
# Note: this map is a zoomed in version of the one previously plotted.

######## More advanced map visuals:
# Attractions (neighbourhood, place, lat, lon)
# Manhattan, Central Park, 40.785091, -73.968285
# Manhattan, Times Square, 40.758896, -73.985130
# Bronx, Bronx Zoo, 40.852905, -73.872971
# Bronx, Yankees Stadium, 40.829659, -73.926186
# Brooklyn, Coney Island, 40.57788, -73.99403
# Queens, Citi Field, 40.75673, -73.84597
# Staten Island, Historic Richmond Town, 40.57129, -74.14581
# Queens, Outlier1, 40.76810, -73.91651
# Brooklyn, Outlier2, 40.73260, -73.95739
# Manhattan, Outlier3, 40.77213, -73.98665

attractions_df <- data.frame(borough=c("Manhattan", "Manhattan", "Bronx", "Bronx", "Brooklyn", "Queens", "Staten Island", "Queens", "Brooklyn", "Manhattan"),
  place=c("Central Park", "Times Square", "Bronx Zoo", "Yankee Stadium", "Coney Island", "Citi Field", "Historic Richmond Town", "Outlier1", "Outlier2", "Outlier3"),
  lat=c(40.785091, 40.758896, 40.852905, 40.829659, 40.57788, 40.75673, 40.57129, 40.76810, 40.73260, 40.77213),
  lon=c(-73.968285, -73.985130, -73.872971, -73.926186, -73.99403, -73.84597, -74.14581, -73.91651, -73.95739, -73.98665))
attractions_df

# Further inspection of three outliers:
max_outliers <- filter(data, data$price == 10000)
max_outliers

# Map of attractions and max_outliers:
map_attract <- leaflet(attractions_df) %>%
  addTiles() %>%
  addMarkers(lng = ~lon[1:7], lat=~lat[1:7], label = ~paste(as.character(place[1:7]), "in", as.character(borough[1:7]))) %>%
  addCircleMarkers(lng = ~lon[8:10], lat=~lat[8:10], color="red" ,stroke=TRUE, fillOpacity = 0.5,
                   label = ~paste(as.character(place[8:10]), "in", as.character(borough[8:10])))
map_attract
# The seven attractions in NYC are marked on the map, as well as the three outliers (the first outlier is hidden behind the Times
# Square marker).

# Map of all listings in NYC:
map_listings <- leaflet(data) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat=~latitude, 
             clusterOptions = markerClusterOptions())
map_listings
# There are many more listings in Manhattan and Brooklyn, than the other neighbourhood groups.
############################################################################################################

##############
# cONCLUSION
##############

# Recommendations for the host:
# Generally, it is recommended to have a listing price below $200 to increase the number of reservations (# of reviews/month);
# however, properties near attractions don't tend to follow this rule.
# Availability could be based on the median # of nights available for each room type:
# Median # Nights Available for Entire home/apt : 42
# Median # Nights Available for Private room : 45
# Median # Nights Available for Shared room : 90
# Properties near attractions tend to have less availability and higher prices.

# Recommendations for the guest:
# It is recommended to look for listings with more availability to find a cheaper price.
# Listings located in Bronx, Queens and Staten Island tend to have a cheaper median price:
# Median price ($) for Bronx : 65
# Median price ($) for Brooklyn : 90
# Median price ($) for Manhattan : 150
# Median price ($) for Queens : 75
# Median price ($) for Staten Island : 75
# Private rooms and shared rooms tend to have a cheaper median price than the entire home/apt:
# Median price ($) for Entire home/apt : 160
# Median price ($) for Private room : 70
# Median price ($) for Shared room : 45
# Again, properties near attractions tend to have less availability and higher prices.


######################
# Citations/References
######################
citation(package="ggmap")
