# This files prepares the data for Bergamo Q3 - as simple R file

library(readr)
library(tidyverse)
library(stringr)
library(tidyr)
library(stargazer)
library(Hmisc)
library(DescTools)
library(ggplot2)
library(extrafont)
library(ggpubr)
library(janitor)
library(forcats)
library(ggcorrplot)
library(fastDummies)

options(digits = 3)

# Define folders and source functions

setwd("C:/Users/Castelli/Desktop/WU_exams/CEU_WiSe25/Machine_Learning/Assignment2/PART2")

source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/theme_bg.R")
source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/da_helper_functions.R")

# read db
BGQ3_db <- read_csv("BGQ3_raw.csv",na = c("N/A", "NA") )

names(BGQ3_db)
length(unique(BGQ3_db$id)) # unique ID for each observation

# drop some un-meaningful cols: e.g. scraping ids, url, descriptions, some host information etc.
BGQ3_db <- BGQ3_db[!(names(BGQ3_db) %in% c('listing_url', 'scrape_id', 'source', 'description','neighborhood_overview',"host_name","host_location", 'picture_url','host_url', 'host_about','host_thumbnail_url', 'has_availability', 'host_picture_url','host_listings_count','host_total_listings_count','host_has_profile_pic', 'latitude','longitude', 'calendar_updated','calendar_last_scraped') ) ]
BGQ3_db <- BGQ3_db[!(names(BGQ3_db) %like% c('calculated_host_listings%') ) ]


# delete additional cols - this time quickly done by column index
BGQ3_db <- BGQ3_db[,-c(2,11, 12, 27:32, 38:40, 49:50)]
names(BGQ3_db)

# DATA CLEANING ----
## Dep Var: price ---- 

BGQ3_db$price_n <- substr(BGQ3_db$price, 2,nchar(BGQ3_db$price))
BGQ3_db$price_n <- gsub(",", "", BGQ3_db$price_n)
BGQ3_db$price_n <- as.numeric(BGQ3_db$price_n)
unique(BGQ3_db$price[is.na(BGQ3_db$price_n)])

print('Nr missing prices')
NROW(which(is.na(BGQ3_db$price_n))) 

# Drop missing prices
BGQ3_db <- BGQ3_db[!(is.na(BGQ3_db$price_n)),]
# and compute the log scale for comparison (later)
BGQ3_db$price_ln <- log(BGQ3_db$price_n)

## Host information ----

print("Nr. properties per host")
dim(BGQ3_db)[1]/length(unique(BGQ3_db$host_id)) # accom. /host
# some have multiple properties
head(BGQ3_db)
# change host rates into numeric
BGQ3_db$host_acceptance_rate <- gsub('%','', BGQ3_db$host_acceptance_rate )
BGQ3_db$host_acceptance_rate <- as.numeric(BGQ3_db$host_acceptance_rate)

BGQ3_db$host_response_rate <- gsub('%','', BGQ3_db$host_response_rate )
BGQ3_db$host_response_rate <- as.numeric(BGQ3_db$host_response_rate)

cols <- which(names(BGQ3_db) == "host_response_time"):which(names(BGQ3_db) == "host_is_superhost")

print("NAs by variable")
for (col in cols) {
  
  na_nr = sum(is.na(BGQ3_db[[col]]) )
  
  if (sum(is.na(BGQ3_db[[col]])) > 0) {
    
    print(names(BGQ3_db)[col])
    print(na_nr)
    
  }
}

# Deal with Missings 

# replace response rate with median
BGQ3_db$d_miss_resp_rate <- 0
BGQ3_db$d_miss_resp_rate[is.na(BGQ3_db$host_response_rate)] <- 1
value_resp_rate = median(BGQ3_db$host_response_rate, na.rm = T)
BGQ3_db$host_response_rate[is.na(BGQ3_db$host_response_rate)] <- value_resp_rate

# replace categorical host_response_time & host_is_superhost with mode
table(BGQ3_db$host_response_time) # mode: within an hour 

BGQ3_db$d_miss_resp_time <- 0
BGQ3_db$d_miss_resp_time[is.na(BGQ3_db$host_response_time)] <- 1
BGQ3_db$host_response_time[is.na(BGQ3_db$host_response_time)] <- 'within an hour'

# create numerical variable host_resptime_n
BGQ3_db$host_resptime_n <- NA
BGQ3_db$host_resptime_n[BGQ3_db$host_response_time == "within an hour"]<- 1
BGQ3_db$host_resptime_n[BGQ3_db$host_response_time == "within a few hours"]<- 2
BGQ3_db$host_resptime_n[BGQ3_db$host_response_time == "within a day" ]<- 3
BGQ3_db$host_resptime_n[BGQ3_db$host_response_time == "a few days or more"]<- 4
summary(BGQ3_db$host_resptime_n)

# superhost - recode
which(is.na(BGQ3_db$host_is_superhost)) # FALSE
sum(length(which(is.na(BGQ3_db$host_is_superhost))))  

BGQ3_db$d_miss_superhost <- 0
BGQ3_db$d_miss_superhost[which(is.na(BGQ3_db$host_is_superhost))] <- 1
BGQ3_db$host_is_superhost[which(is.na(BGQ3_db$host_is_superhost))] <- 'FALSE'


## Property ----
### Location (Neighborhood) ----

# first, keep only BnBs within the city boundaries
# cols: "neighbourhood" "neighbourhood_cleansed" "neighbourhood_group_cleansed"   
which(is.na(BGQ3_db$neighbourhood_cleansed)) # start from this column 

# drop places lying outside the metro-line system (by vars: neighbourhood and neighbourhood_group_cleansed)
sort(unique(BGQ3_db$neighbourhood_cleansed))
# drop neighbourhoods outside the metro system 
BGQ3_db = BGQ3_db[(BGQ3_db$neighbourhood_cleansed %like%  c('Bergamo%') ),]

# in the analysis, we'll keep only neighbourhood_cleansed, so let's drop the other vars
BGQ3_db <- BGQ3_db[,!(names(BGQ3_db) %in% c("neighbourhood","neighbourhood_group_cleansed"))]

# we keep only bnbs in the city of Bergamo - i.e. 1 neighbourhood
# manually, we can code n=relevant neighbourhood (i.e. city center, airport)

# code a function to extrac them all
extract_unique <- function(df, column_name) {
  # Remove brackets and quotes, split into a list, and convert to lowercase
  all_words <- tolower(unlist(strsplit(gsub("\\[|\\]|\"", "", df[[column_name]]), ", ")))
  
  # Get unique values
  unique_words <- unique(all_words)
  
  return(unique_words)
}

# make col lower capital
BGQ3_db$location <- tolower(BGQ3_db$name)
# Extract location words
unique_location_db <- extract_unique(BGQ3_db, "name")
print(unique_location_db) # more than 3.6k different amenities

# city centre: città alta (old/down/upper town), centro , central*
# airport, aeroporto, (names of the airport:) caravaggio, orio al Serio
# recode neighbourhood strings to make them more easy to handle
BGQ3_db$location <- gsub("[^A-Za-z0-9]", '',  BGQ3_db$location)

# Define neigh keywords - already checked if meaningful in the list
location_list <- list(
  neighbourhood_cleansed_centre = c('%city%', '%central%','%centre%', '%center%','%centro%', '%cittàalta%', '%downtown%', '%oldtown%', '%uppertown%'),
  neighbourhood_cleansed_airport = c('%aeroporto%', '%airport%', '%orio%', '%caravaggio%')
  )

# Loop through each amenity, create dummies
for (location in names(location_list)) {
  BGQ3_db[[location]] <- 0
  BGQ3_db[[location]] <- as.integer(
    Reduce('|', lapply(location_list[[location]], function(pattern) BGQ3_db$location %like% pattern))
  )
  BGQ3_db[[location]] <- as.numeric(BGQ3_db[[location]])
}

BGQ3_db$neighbourhood_f <- 'basic_location'
BGQ3_db$neighbourhood_f <- ifelse((BGQ3_db$neighbourhood_cleansed_centre == 1) == T, 'centre', BGQ3_db$neighbourhood_f)
BGQ3_db$neighbourhood_f <- ifelse((BGQ3_db$neighbourhood_cleansed_airport == 1) == T, 'airport', BGQ3_db$neighbourhood_f)
BGQ3_db$neighbourhood_f = as.factor(BGQ3_db$neighbourhood_f)

# drop location var
BGQ3_db <- BGQ3_db[!names(BGQ3_db) %in% c("location") ]

### Type of Room ---- 
# Room type - Main variable 
length(which(is.na(BGQ3_db$room_type))) # no NAs
unique(BGQ3_db$room_type)

# Property type - more specific
length(which(is.na(BGQ3_db$property_type))) # no NAs
unique(BGQ3_db$property_type)

# keep if property type is Apartment, House or Townhouse
BGQ3_db <- BGQ3_db[!(BGQ3_db$room_type %like% 'Hotel%'),]

# let's check in property_type - more detailed
BGQ3_db$property_type <- tolower(BGQ3_db$property_type)
BGQ3_db <- BGQ3_db[!(BGQ3_db$property_type %like% c('%hostel','%guesthouse','%hotel','%bed and breakfast', '%camper/rv%','boat')),]

# create factor and dummies on room_type
BGQ3_db$room_type_f <- as.factor(BGQ3_db$room_type)


BGQ3_db <- BGQ3_db %>% mutate(variable = fct_recode(room_type_f,"Entire_prop" = "Entire home/apt",
                                                    "Private_room"   = "Private room", "Shared_room" = "Shared room"))
# No share room category
BGQ3_db <- dummy_cols(BGQ3_db, select_columns = "room_type_f", remove_first_dummy = TRUE) #remove option to avoid multicollinearity


### Guests capacity ---- 

# Nr accepted people 
class(BGQ3_db$accommodates)
length(which(is.na(BGQ3_db$accommodates)))

summary(BGQ3_db$accommodates)

### Size of the property ----

# Now let's classify the apt size by the nr of bedrooms
summary(BGQ3_db$bedrooms) # no Nas

BGQ3_db$bedrooms_f <- NA  
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms == 0 ] <- '0Broom'
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms == 1 ] <- '1Broom'
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms == 2 ] <- '2Broom'
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms == 3 ] <- '3Broom'
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms == 4 ] <- '4Broom'
BGQ3_db$bedrooms_f[BGQ3_db$bedrooms >= 5 ] <- '5+Broom'
BGQ3_db$bedrooms_f <- as.factor(BGQ3_db$bedrooms_f)

### Bathrooms (nr and shared dummy) ----

# Nr. Bathrooms
summary(BGQ3_db$bathrooms)
# no missing

# turn into numeric variable
BGQ3_db$bathrooms <- as.numeric(BGQ3_db$bathrooms)

BGQ3_db <- BGQ3_db[!is.na(BGQ3_db$bathrooms),]
# Categorical: no bathroom, one bathroom, >1 bathroom (as integers)
BGQ3_db$bathrooms_f <- NA
BGQ3_db$bathrooms_f[BGQ3_db$bathrooms == 0 ] <- '0'
BGQ3_db$bathrooms_f[BGQ3_db$bathrooms > 0 & BGQ3_db$bathrooms < 2 ] <- '1'
BGQ3_db$bathrooms_f[BGQ3_db$bathrooms >= 2] <- '2_over' 
BGQ3_db$bathrooms_f <- as.factor(BGQ3_db$bathrooms_f)
table(BGQ3_db$bathrooms_f)

# Dummy Shared Bath
BGQ3_db$d_sharebath <- 0
BGQ3_db$d_sharebath[BGQ3_db$bathrooms_text %like% c('%share%', '%Share%')] <- 1
BGQ3_db$d_sharebath[is.na(BGQ3_db$bathrooms)] <- NA
table(BGQ3_db$d_sharebath)


### Amenities ----

# code a function to extrac them all
extract_unique_amenities <- function(df, column_name) {
  # Remove brackets and quotes, split into a list, and convert to lowercase
  all_amenities <- tolower(unlist(strsplit(gsub("\\[|\\]|\"", "", df[[column_name]]), ", ")))
  
  # Get unique values
  unique_amenities <- unique(all_amenities)
  
  return(unique_amenities)
}

# make col lower capital
BGQ3_db$amenities <- tolower(BGQ3_db$amenities)

# Example usage
unique_amenities_db <- extract_unique_amenities(BGQ3_db, "amenities")
# print(unique_amenities_db) # more than 3.6k different amenities

# Define amenities and corresponding keywords - already checked if meaningful in the list
amenities_list <- list(
  d_am_selfcheckin = c('%check%', '%check-in%', '%self%', '%check in%'),
  d_am_locker = c('%safe%', '%lock%'),
  d_am_housekeepingserv = c('%cleaning available%', '%housekeeping%'),
  d_am_fastwifi = c('%fast wifi%', '%internet%'),
  d_am_outdoor = c('%balcon%', '%view%', '%garden%', '%skyline%', '%patio%', '%loggia%', '%outdoor%'),
  d_am_petsallowed = c('%pets allowed%'),
  d_am_kitchen = c('%kitchen%'),
  d_am_essentials = c('%linen%', '%shampoo%', '%kit %')
)

# Loop through each amenity, create dummies
for (amenity in names(amenities_list)) {
  BGQ3_db[[amenity]] <- 0
  BGQ3_db[[amenity]] <- as.integer(
    Reduce('|', lapply(amenities_list[[amenity]], function(pattern) BGQ3_db$amenities %like% pattern))
  )
  BGQ3_db[[amenity]] <- as.numeric(BGQ3_db[[amenity]])
}

# Additional rules for exceptions
BGQ3_db$d_am_locker[BGQ3_db$amenities %like% "%baby safety gates%"] <- 0
BGQ3_db$d_am_outdoor[BGQ3_db$amenities %like% "%outdoor furniture%"] <- 0


# table(BGQ3_db$d_am_kitchen) # most bnbs have kitchens

### Reviews ----

cols <- names(BGQ3_db)[c(20:26,28:35)]

print("NAs by variable")
for (col in cols) {
  
  na_nr = sum(is.na(BGQ3_db[[col]]) )
  
  if (sum(is.na(BGQ3_db[[col]])) > 0) {
    
    print(col)
    print(na_nr)
    
  }
}

print(cols)
# keep max/min nights , nr reviews, availabilities (no NAs)
# drop score rating and review per month (too many NAs, ~20%)
summary(BGQ3_db$number_of_reviews)


# DATA VISUALISATION ----

## Price - Dependent variable ----

summary(BGQ3_db$price_n)
quantile(BGQ3_db$price_n, probs = c(0.95, 0.96,0.99), na.rm = T) 
# 99% distribution at 1000
summary(BGQ3_db$price_ln)

p1 <- ggplot(BGQ3_db, aes(x = price_n)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "cadetblue", color = "black", alpha = 0.6) +
  xlim(0,1000)+
  geom_density(color = "aquamarine4", size = .67) +
  labs(title = "Level scale", x = "", y = "Density")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond"))

p2 <- ggplot(BGQ3_db, aes(x = price_ln)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "hotpink4", color = "black", alpha = 0.6) +
  xlim(0,11.5)+
  geom_density(color = "magenta4", size = .67) +
  labs(title = "Log scale", x = "", y = "")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond"))

plot_p <- ggarrange(p1,p2)

pp<- annotate_figure(plot_p, top = text_grob("USD Price distribution AirBnB, BGQ3", 
                                             color = "black", family = "Garamond", size = 14))

pp

## Room type ----

ggplot(BGQ3_db, aes(x= room_type, y= price_ln, fill = room_type)) + 
  geom_boxplot(alpha=0.5) +
  xlab('')+ ylab('USD log-price')+
  ggtitle('Room Type')+
  scale_fill_brewer(palette="BuPu")+ 
  theme_bw() +
  theme(
    legend.position="none",
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond")) 


#### Interaction between Room type and dummy Pets allowed


ggplot(BGQ3_db, aes(x= d_am_petsallowed, y= price_ln, fill = as.factor(d_am_petsallowed))) + 
  geom_boxplot(alpha=0.5) +
  xlab('')+ ylab('USD log-price')+
  labs(fill = "Pets allowed")+
  scale_fill_brewer(palette="BuPu")+ 
  theme_bw() +
  theme(
    legend.position="bottom", legend.title = element_text(family = "Garamond"),
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    axis.title.x = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond")) 

# no variation 


## Apt size (Nr. bedrooms) ----

b1<- ggplot(BGQ3_db, aes(x= as.factor(bedrooms), y= price_ln, fill = as.factor(bedrooms))) + 
  geom_boxplot(alpha=0.5) +
  xlab('')+ ylab('USD log-price')+
  ggtitle('Nr. Bedrooms')+
  scale_fill_brewer(palette="BuPu")+ 
  theme_bw() +
  theme(
    legend.position="none",
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond")) 

b2<- ggplot(BGQ3_db, aes(x= as.factor(bedrooms_f), y= price_ln, fill = as.factor(bedrooms_f))) + 
  geom_boxplot(alpha=0.5) +
  xlab('')+ ylab('')+
  ggtitle('Categorical (Apt size)')+
  scale_fill_brewer(palette="BuPu")+ 
  theme_bw() +
  theme(
    legend.position="none",
    axis.text.x = element_text(family = "Garamond", face = "bold", angle = 90),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond")) 

plot_aptsize <- ggarrange(b1,b2)

annotate_figure(plot_aptsize , top = text_grob("Flat size numeric vs categorical, BGQ3", color = "black", family = "Garamond", size = 14))


BGQ3_db <- dummy_cols(BGQ3_db, select_columns = "bedrooms_f", remove_first_dummy = TRUE) #remove option to avoid multicollinearity


### Correlation tabs wrt price (factors)

dummy_vars <- BGQ3_db %>%
  select(starts_with("d_am_"), starts_with("bedrooms_f"), starts_with('room_type_f'),price_ln) %>%
  mutate_all(~ as.numeric(.))

# Compute correlation matrix
cor_matrix <- cor(dummy_vars, use = "pairwise.complete.obs")

# and plot correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Matrix of Dummy Amenities") +
  theme(
    legend.title = element_text(family = "Garamond"),
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    plot.title = element_text(family = "Garamond")) 


dummy_vars <- BGQ3_db %>%
  select(starts_with("d_am_"), starts_with("bedrooms_f"), starts_with('room_type_f'),price_ln) %>%
  mutate_all(~ as.numeric(.))

# Compute correlation matrix
cor_matrix <- cor(dummy_vars, use = "pairwise.complete.obs")

# and plot correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Matrix of Dummy Amenities") +
  theme(
    legend.title = element_text(family = "Garamond"),
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    plot.title = element_text(family = "Garamond")) 

## Accommodates nr  - functional form ----

ggplot(BGQ3_db,aes(x= accommodates , y = price_ln)) + 
  geom_point(color='dodgerblue3', alpha = 0.8)+  # Scatter plot for points
  stat_smooth(method='lm', formula='y ~ poly(x, 1)', color='firebrick4', size = .75) + 
  labs(title="Accomodates functional form", x='', y='USD Log Price')+
  theme_bw() +
  theme(
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    axis.title.y = element_text(family = "Garamond"),
    axis.title.x = element_text(family = "Garamond"),
    plot.title = element_text(family = "Garamond")) 

## Cross tab among numericals ----

numericals <- c("host_resptime_n", "host_response_rate", 'bedrooms', "accommodates","review_scores_rating","number_of_reviews","minimum_nights",'maximum_nights',"beds", 'availability_30','availability_60','availability_90','availability_365')

num_vars <- BGQ3_db %>%
  select(sort(numericals), price_ln) %>%
  mutate_all(~ as.numeric(.))

# Compute correlation matrix
cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")

# and plot correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Matrix of Numerical variables")+
  theme(
    legend.title = element_text(family = "Garamond"),
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    plot.title = element_text(family = "Garamond")) 

write_rds(BGQ3_db, 'BGQ3_db_clean.rds')



