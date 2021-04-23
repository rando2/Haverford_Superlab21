# Load libraries that you need (you need to make sure to download them with set_up_environment.R first)
library(ggplot2)
library(readr)
library(ggmap)
library(dplyr)
library(maps)

# Load the data from the CSV file into R as the dataframe (table) "data"
# I opened the data in Excel & cleaned it up the data by removing extra characters (e.g., units)
# converting feet to meters, and converting GPS coordinates to lat/long using https://www.gps-coordinates.net/
# I used blank cells to represent missing data or data that could not be compared to other data in that column
# (for example, entries recording how far the site was from the Nature Trail rather than the road)
# I also averaged all measurements that were provided with a range (e.g., nitrate 0-5 would be 2.5)

classdata <- read_csv("classdata.csv", col_types = cols(CARBONATE = col_number(), 
                                                        DESC = col_character(), DIST_RD_M = col_number(), 
                                                        GEN_HARDNESS = col_number(), LATITUDE = col_number(), 
                                                        LONGITUDE = col_number(), NITRATE = col_number(), 
                                                        NITRITE = col_number(), PH = col_number(), 
                                                        TEMP = col_number()))

# Let's start by looking at the distribution of the temperatures measured. If you see a Warning Message in red saying 
# there are rows removed for non-finite values, that's fine -- those are just the missing data (NA)
ggplot(classdata, aes(x=PH)) + geom_histogram() + # tells it which data to plot and how to plot it
  labs(title="pH histogram plot",x="pH", y = "Count") +  # name the labels
  theme(plot.title = element_text(hjust = 0.5)) # center the title
ggplot(classdata, aes(x=GEN_HARDNESS)) + geom_histogram() +
  labs(title="Hardness histogram plot",x="Hardness", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# We can also look whether there were differences based on the distinct from the road using a linear regression
summary(lm(NITRITE+NITRATE+PH+CARBONATE ~ DIST_RD_M, classdata))

# Let's plot the locations of sampling sites that were recorded
PA <- map_data("state") %>% filter(region=="pennsylvania")
ggplot(PA, aes(long, lat)) +  geom_polygon() + 
  geom_point(data = classdata, mapping = aes(x = LONGITUDE, y = LATITUDE), colour = "yellow") +
  labs(title="Phase I Sample Collection Sites",x="Longitude", y = "Latitude") + 
  theme(plot.title = element_text(hjust = 0.5)) # + xlim(-77, -74.5) + ylim(39.6, 40.5) to zoom in

# What is our extreme datapoint?
classdata[which(classdata$LONGITUDE == min(classdata$LONGITUDE, na.rm=TRUE)),]
# View at https://www.gps-coordinates.net/gps-coordinates-converter using 40.05416, -75.53586

# We can also check the distribution of the variables for the Duck Pond only.
# First we need to separate out the Duck Pond data into a new dataframe (table)
duckpond <- classdata[ which(classdata$LOCATION=='Duck Pond'), ]

# Do the Duck Pond measurements differ between the two different lab sections?
# First, let's add the data about which day the samples were collected
classday <- read_csv("classday.csv")
duckpond <- merge(duckpond, classday, by="INITIAL")

# Let's look at some of the data divided between Monday and Tuesday
ggplot(duckpond, aes(x=PH)) + geom_histogram(aes(fill=DATE)) +
  labs(title="pH histogram plot for Duck Pond only",x="Ph", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(duckpond, aes(x=GEN_HARDNESS)) + geom_histogram(aes(fill=DATE)) +
  labs(title="Hardness histogram plot for Duck Pond only",x="Hardness", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(duckpond, aes(x=TEMP)) + geom_histogram(aes(fill=DATE)) +
  labs(title="Temperature histogram plot for Duck Pond only",x="Hardness", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Now let's split the data out by which day it was taken
monday_data <- duckpond[which(duckpond$DATE == "Monday"),]
tuesday_data <- duckpond[which(duckpond$DATE == "Tuesday"),]

# And use a t-test to see whether there are significant differences between the two days
t.test(monday_data$PH, tuesday_data$PH, alternative = "two.sided")
t.test(monday_data$GEN_HARDNESS, tuesday_data$GEN_HARDNESS, alternative = "two.sided")
t.test(monday_data$TEMP, tuesday_data$TEMP, alternative = "two.sided")

# Finally, let's analyze overall variation among the sites using principle components analysis
# For this, we need to drop any variables that are non-numeric (no words allowed)
pca_data <- classdata[,c(3:8, 10:12),] # This tells it to select all rows and columns 3-8 & 10-12

# We also need to drop rows that are missing data
pca_data <- pca_data[complete.cases(pca_data),]

# Now we can run the PCA
pca <- prcomp(pca_data) 

# Let's look at the overall results
summary(pca)

# Now let's plot the two PCs that explain the most variance (over 93%!)
ggplot(pcs, aes(x=PC1, y=PC2)) + geom_point()

# What happens if we add in information about the sites?
# We can take the PC values out of the summary and recombine them with our location information
pcs <- data.frame(pca$x)
pcs["LOCATION"] <- classdata[complete.cases(classdata),2]
pcs["DUCKPOND"] <- pcs$LOCATION == "Duck Pond"
pcs["DESC"] <- classdata[complete.cases(classdata),9]

# Now let's plot the data again, this time by whether it was sampled at the Duck Pond and the type of body of water
ggplot(pcs, aes(x=PC1, y=PC2)) + geom_point(aes(color=DUCKPOND)) +
  labs(title="Principle Components Analysis of Phase I Data Sites",x="PC1", y = "PC2") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Duckpond?")

ggplot(pcs, aes(x=PC1, y=PC2)) + geom_point(aes(color=DESC)) +
  labs(title="Principle Components Analysis of Phase I Data Sites",x="PC1", y = "PC2") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Body of Water")

# What is this pond that is off by itself at the extreme of PC2?
pcs[which(pcs$PC2 == max(pcs$PC2, na.rm=TRUE)),]
clean_classdata <- classdata[complete.cases(classdata),]
clean_classdata[32,]

# Why was Naomi's BMC pond sample so different?
pca$rotation[,2]
# Which variables are contributing?