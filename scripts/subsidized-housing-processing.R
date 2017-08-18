library(dplyr)
library(datapkg)
library(readxl)

##################################################################
#
# Processing Script for Subsidized Housing
# Created by Jenna Daly
# On 08/18/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xls")

sub_housing <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read_excel(paste0(path_to_raw, "/", raw_data[i]), sheet=1, skip=0)
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")))
  current_file$Year <- get_year
  fileCols <- names(current_file)
  #Select pertinent columns
  town <- fileCols[grep("Town", fileCols, ignore.case = T)]
  total_assisted <- fileCols[grepl("Total", fileCols, ignore.case = T) & grepl("Assisted", fileCols, ignore.case = T)]
  names(current_file)[names(current_file) == total_assisted] <- "Value"
  names(current_file)[names(current_file) == town] <- "Town"
  current_file <- current_file %>% select(Town, Value, Year)
  sub_housing <- rbind(sub_housing, current_file)
}

#Trim WS in town names, remove blank rows
sub_housing$Town <- trimws(sub_housing$Town) 
sub_housing <- sub_housing[!is.na(sub_housing$Town),]

#Merge in town FIPS (remove Total rows)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
town_fips <- (town_fips_dp$data[[1]])

sub_housing_fips <- merge(sub_housing, town_fips, by = "Town", all.y=T)

#Remove CT
sub_housing_fips <- sub_housing_fips[sub_housing_fips$Town != "Connecticut",]

## Aggregate county values
# first, take a copy of the dataset where FIPS are only the first 5 digits of town FIPS
county <- sub_housing_fips
county$FIPS <- substr(county$FIPS, 0, 5)

# Aggregate sum of values by county fips codes and year
county <- county %>% 
  group_by(FIPS, Year) %>% 
  summarise(Value = sum(Value))

#Merge in county FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
county_fips <- (county_fips_dp$data[[1]])

#Assign county name based on FIPS
county_with_fips <- merge(county, county_fips, by = "FIPS", all = T)

#Rename county column and remove CT
names(county_with_fips)[names(county_with_fips) == "County"] <- "Town"
county_with_fips <- county_with_fips[county_with_fips$Town != "Connecticut",]

# Aggregate state values
state <- sub_housing_fips %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value, na.rm=T))

state <- state[!is.na(state$Year),]

state$Town <- "Connecticut"
state$FIPS <- "09"

#Combine town, county, and state
sub_housing_fips <- rbind(sub_housing_fips, county_with_fips, state)

# add columns, rename Town/County
sub_housing_fips$`Measure Type` <- "Number"
sub_housing_fips$Variable <- "Total Assisted Units"
names(sub_housing_fips)[names(sub_housing_fips) == "Town"] <- "Town/County"

#Order and sort columns
sub_housing_fips <- sub_housing_fips %>% 
  select(`Town/County`, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(`Town/County`, Year)

# write to file
write.table(
  sub_housing_fips,
  file.path(getwd(), "data", "subsidized-housing_2016.csv"),
  sep = ",",
  row.names = F
)
