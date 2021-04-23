library(ggplot2)
library(readr)

# If you want to see plots of 

classdata <- read_csv("classdata.csv", col_types = cols(CARBONATE = col_number(), 
                                                        DESC = col_character(), DIST_RD_M = col_number(), 
                                                        GEN_HARDNESS = col_number(), LATITUDE = col_number(), 
                                                        LONGITUDE = col_number(), NITRATE = col_number(), 
                                                        NITRITE = col_number(), PH = col_number(), 
                                                        TEMP = col_number()))

# Let's start by looking at the distribution of the temperatures measured. If you see a Warning Message in red saying 
# there are rows removed for non-finite values, that's fine -- those are just the missing data (NA)
ggplot(data=classdata, aes(x=TEMP)) + geom_histogram() +
  labs(title="Temperature histogram plot",x="Temperature (C)", y = "Count")
ggplot(classdata, aes(x=NITRATE)) + geom_histogram() +
  labs(title="Nitrate histogram plot",x="Nitrate", y = "Count")
ggplot(classdata, aes(x=NITRITE)) + geom_histogram() +
  labs(title="Nitrate histogram plot",x="Nitrite", y = "Count")
ggplot(classdata, aes(x=PH)) + geom_histogram() +
  labs(title="pH histogram plot",x="pH", y = "Count")
ggplot(classdata, aes(x=GEN_HARDNESS)) + geom_histogram() +
  labs(title="Hardness histogram plot",x="Hardness", y = "Count")
ggplot(classdata, aes(x=DIST_RD_M)) + geom_histogram() +
  labs(title="Distance from Road histogram plot",x="Distance (m)", y = "Count")
ggplot(classdata, aes(x=LATITUDE)) + geom_histogram() +
  labs(title="Latitude histogram plot",x="Lat (degrees)", y = "Count")
ggplot(classdata, aes(x=LONGITUDE)) + geom_histogram() +
  labs(title="Longitude histogram plot",x="Lat (degrees)", y = "Count")

# We can also check the distribution of the variables for the Duck Pond only.
# First we need to separate out the Duck Pond data into a new dataframe (table)
duckpond <- classdata[ which(classdata$LOCATION=='Duck Pond'), ]

# Now we can run the same plots that we ran above
ggplot(duckpond, aes(x=TEMP)) + geom_histogram() +
  labs(title="Temperature histogram plot for Duck Pond only",x="Temperature (C)", y = "Count")
ggplot(duckpond, aes(x=NITRATE)) + geom_histogram() +
  labs(title="Nitrate histogram plot for Duck Pond only",x="Nitrate", y = "Count")
ggplot(duckpond, aes(x=NITRITE)) + geom_histogram() +
  labs(title="Nitrate histogram plot for Duck Pond only",x="Nitrite", y = "Count")
ggplot(duckpond, aes(x=PH)) + geom_histogram() +
  labs(title="pH histogram plot for Duck Pond only",x="Ph", y = "Count")
ggplot(duckpond, aes(x=GEN_HARDNESS)) + geom_histogram() +
  labs(title="Hardness histogram plot for Duck Pond only",x="Hardness", y = "Count")
ggplot(duckpond, aes(x=DIST_RD_M)) + geom_histogram() +
  labs(title="Distance from Road histogram plot for Duck Pond only",x="Distance (m)", y = "Count")
ggplot(duckpond, aes(x=LATITUDE)) + geom_histogram() +
  labs(title="Latitude histogram plot for Duck Pond",x="Lat (degrees)", y = "Count")
ggplot(duckpond, aes(x=LONGITUDE)) + geom_histogram() +
  labs(title="Longitude histogram plot",x="Long (degrees)", y = "Count")