library(dplyr)
library(datapkg)
library(readxl)

##################################################################
#
# Processing Script for Subsidized Housing by Type
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
  total <- fileCols[grepl("census", fileCols, ignore.case = T)]
  total_assisted <- fileCols[grepl("Total", fileCols, ignore.case = T) & grepl("Assisted", fileCols, ignore.case = T)]
  govt <- fileCols[grepl("Govern", fileCols, ignore.case = T)]
  chfa <- fileCols[grepl("CHFA", fileCols, ignore.case = T)]
  deed <- fileCols[grepl("restrict", fileCols, ignore.case = T)]
  percent <- fileCols[grepl("percent", fileCols, ignore.case = T)]
  rent <-  fileCols[grepl("rental", fileCols, ignore.case = T)]
  #Checks if rental column is present, if not, populates it with NAs
    if (identical(rent, character(0))) {
    current_file$rental <- NA
    rent = "rental"
  }
  #Rename columns
  names(current_file)[names(current_file) == town] <- "Town"
  names(current_file)[names(current_file) == total] <- "Total Housing Units"
  names(current_file)[names(current_file) == total_assisted] <- "Total Assisted"
  names(current_file)[names(current_file) == govt] <- "Government Assisted"
  names(current_file)[names(current_file) == chfa] <- "CHFA/USDA Mortgages"
  names(current_file)[names(current_file) == deed] <- "Deed Restrictions"
  names(current_file)[names(current_file) == rent] <- "Tenant Rental Assistance"
  names(current_file)[names(current_file) == percent] <- "Total Assisted Percent"
  current_file <- current_file %>% select(Town, Year, `Total Housing Units`, `Government Assisted`, `CHFA/USDA Mortgages`, 
                                          `Deed Restrictions`, `Tenant Rental Assistance`, 
                                          `Total Assisted`, `Total Assisted Percent`)
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
county$Town <- NULL
county$`Total Assisted Percent` <- NA

# Aggregate sum of values by county fips codes and year
county_agg <- unique(county %>% 
  group_by(FIPS, Year) %>% 
  mutate(`Total Housing Units` = sum(`Total Housing Units`, na.rm=T),
         `Government Assisted` = sum(as.numeric(`Government Assisted`), na.rm=T),
         `CHFA/USDA Mortgages` = sum(as.numeric(`CHFA/USDA Mortgages`), na.rm=T),
         `Deed Restrictions` = sum(`Deed Restrictions`, na.rm=T),
         `Tenant Rental Assistance` = sum(`Tenant Rental Assistance`, na.rm=T),
         `Total Assisted` = sum(`Total Assisted`, na.rm=T), 
         `Total Assisted Percent` = (`Total Assisted` / `Total Housing Units`)))

#Merge in county FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
county_fips <- (county_fips_dp$data[[1]])

#Assign county name based on FIPS
county_with_fips <- merge(county_agg, county_fips, by = "FIPS", all = T)

#Rename county column and remove CT
names(county_with_fips)[names(county_with_fips) == "County"] <- "Town"
county_with_fips <- county_with_fips[county_with_fips$Town != "Connecticut",]

# Aggregate state values
state <- sub_housing_fips
state$Town <- NULL
state$FIPS <- NULL
state$`Total Assisted Percent` <- NA

state_agg <- unique(state %>% 
  group_by(Year) %>% 
  mutate(`Total Housing Units` = sum(`Total Housing Units`, na.rm=T),
         `Government Assisted` = sum(as.numeric(`Government Assisted`), na.rm=T),
         `CHFA/USDA Mortgages` = sum(as.numeric(`CHFA/USDA Mortgages`), na.rm=T),
         `Deed Restrictions` = sum(`Deed Restrictions`, na.rm=T),
         `Tenant Rental Assistance` = sum(`Tenant Rental Assistance`, na.rm=T),
         `Total Assisted` = sum(`Total Assisted`, na.rm=T), 
         `Total Assisted Percent` = (`Total Assisted` / `Total Housing Units`)))  

state_agg$Town <- "Connecticut"
state_agg$FIPS <- "09"

state_agg <- as.data.frame(state_agg)

#Combine town, county, and state
sub_housing_fips <- rbind(sub_housing_fips, county_with_fips, state_agg)

#round percent column
sub_housing_fips$`Total Assisted Percent` <- round((as.numeric(sub_housing_fips$`Total Assisted Percent`))*100, 2)

#Convert to long format
cols_to_stack <- c("Total Housing Units", 
                   "Government Assisted", 
                   "CHFA/USDA Mortgages", 
                   "Deed Restrictions",
                   "Tenant Rental Assistance", 
                   "Total Assisted", 
                   "Total Assisted Percent")

long_row_count = nrow(sub_housing_fips) * length(cols_to_stack)

sub_housing_fips_long <- reshape(sub_housing_fips,
                                 varying = cols_to_stack,
                                 v.names = "Value",
                                 timevar = "Variable",
                                 times = cols_to_stack,
                                 new.row.names = 1:long_row_count,
                                 direction = "long"
)

sub_housing_fips_long$id <- NULL

sub_housing_fips_long <- as.data.frame(sub_housing_fips_long)
sub_housing_fips_long$Value <- as.numeric(sub_housing_fips_long$Value)


# add columns, rename Town/County
sub_housing_fips_long$`Measure Type` <- "Number"
sub_housing_fips_long$`Measure Type`[which(sub_housing_fips_long$Variable %in% c("Total Assisted Percent"))] <- "Percent" 
sub_housing_fips_long$Variable <- gsub(" Percent", "", sub_housing_fips_long$Variable)
names(sub_housing_fips_long)[names(sub_housing_fips_long) == "Town"] <- "Town/County"

#Assign factors for sorting
sub_housing_fips_long$Variable <- factor(sub_housing_fips_long$Variable, levels =  c("Total Housing Units", 
                   "Government Assisted", 
                   "CHFA/USDA Mortgages", 
                   "Deed Restrictions",
                   "Tenant Rental Assistance", 
                   "Total Assisted", "Total Assisted"))


#Order and sort columns
sub_housing_fips_long <- sub_housing_fips_long %>% 
  select(`Town/County`, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(`Town/County`, Variable, Year)

#Set NAs to 0
sub_housing_fips_long$Value[is.na(sub_housing_fips_long$Value)] <- 0

# write to file
write.table(
  sub_housing_fips_long,
  file.path(getwd(), "data", "subsidized-housing-by-type_2019.csv"),
  sep = ",",
  row.names = F
)
