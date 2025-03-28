# This files prepares the data for Milan Q3 - as simple R file

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

# Define folders and source functions - change directories

setwd("C:/Users/Castelli/Desktop/WU_exams/CEU_WiSe25/Machine_Learning/Assignment2/PART2")

source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/theme_bg.R")
source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/da_helper_functions.R")

# read db
#MIQ3_db <- read_csv("MIQ3_raw.csv",na = c("N/A", "NA") )
MIQ3_db <- read_csv("https://osf.io/u92nh/download",na = c("N/A", "NA") )

names(MIQ3_db)
length(unique(MIQ3_db$id)) # unique ID for each observation

# drop some un-meaningful cols: e.g. scraping ids, url, descriptions, some host information etc.
MIQ3_db <- MIQ3_db[!(names(MIQ3_db) %in% c('listing_url', 'scrape_id', 'source', 'description','neighborhood_overview',"host_name","host_location", 'picture_url','host_url', 'host_about','host_thumbnail_url', 'has_availability', 'host_picture_url','host_listings_count','host_total_listings_count','host_has_profile_pic', 'latitude','longitude', 'calendar_updated','calendar_last_scraped') ) ]
MIQ3_db <- MIQ3_db[!(names(MIQ3_db) %like% c('calculated_host_listings%') ) ]


# delete additional cols - this time quickly done by column index
MIQ3_db <- MIQ3_db[,-c(2,11, 12, 27:32, 38:40, 49:50)]
names(MIQ3_db)

# DATA CLEANING ----
## Dep Var: price ---- 

MIQ3_db$price_n <- substr(MIQ3_db$price, 2,nchar(MIQ3_db$price))
MIQ3_db$price_n <- gsub(",", "", MIQ3_db$price_n)
MIQ3_db$price_n <- as.numeric(MIQ3_db$price_n)
unique(MIQ3_db$price[is.na(MIQ3_db$price_n)])

print('Nr missing prices')
NROW(which(is.na(MIQ3_db$price_n))) 

# Drop missing prices
MIQ3_db <- MIQ3_db[!(is.na(MIQ3_db$price_n)),]
# and compute the log scale for comparison (later)
MIQ3_db$price_ln <- log(MIQ3_db$price_n)

## Host information ----

print("Nr. properties per host")
dim(MIQ3_db)[1]/length(unique(MIQ3_db$host_id)) # accom. /host
# some have multiple properties
head(MIQ3_db)
# change host rates into numeric
MIQ3_db$host_acceptance_rate <- gsub('%','', MIQ3_db$host_acceptance_rate )
MIQ3_db$host_acceptance_rate <- as.numeric(MIQ3_db$host_acceptance_rate)

MIQ3_db$host_response_rate <- gsub('%','', MIQ3_db$host_response_rate )
MIQ3_db$host_response_rate <- as.numeric(MIQ3_db$host_response_rate)

cols <- which(names(MIQ3_db) == "host_response_time"):which(names(MIQ3_db) == "host_is_superhost")

print("NAs by variable")
for (col in cols) {
  
  na_nr = sum(is.na(MIQ3_db[[col]]) )
  
  if (sum(is.na(MIQ3_db[[col]])) > 0) {
    
    print(names(MIQ3_db)[col])
    print(na_nr)
    
  }
}

# Deal with Missings 

# replace response rate with median
MIQ3_db$d_miss_resp_rate <- 0
MIQ3_db$d_miss_resp_rate[is.na(MIQ3_db$host_response_rate)] <- 1
value_resp_rate = median(MIQ3_db$host_response_rate, na.rm = T)
MIQ3_db$host_response_rate[is.na(MIQ3_db$host_response_rate)] <- value_resp_rate

# replace categorical host_response_time & host_is_superhost with mode
table(MIQ3_db$host_response_time) # mode: within an hour 

MIQ3_db$d_miss_resp_time <- 0
MIQ3_db$d_miss_resp_time[is.na(MIQ3_db$host_response_time)] <- 1
MIQ3_db$host_response_time[is.na(MIQ3_db$host_response_time)] <- 'within an hour'

# create numerical variable host_resptime_n
MIQ3_db$host_resptime_n <- NA
MIQ3_db$host_resptime_n[MIQ3_db$host_response_time == "within an hour"]<- 1
MIQ3_db$host_resptime_n[MIQ3_db$host_response_time == "within a few hours"]<- 2
MIQ3_db$host_resptime_n[MIQ3_db$host_response_time == "within a day" ]<- 3
MIQ3_db$host_resptime_n[MIQ3_db$host_response_time == "a few days or more"]<- 4
summary(MIQ3_db$host_resptime_n)

# superhost - recode
which(is.na(MIQ3_db$host_is_superhost)) # FALSE
sum(length(which(is.na(MIQ3_db$host_is_superhost))))  

MIQ3_db$d_miss_superhost <- 0
MIQ3_db$d_miss_superhost[which(is.na(MIQ3_db$host_is_superhost))] <- 1
MIQ3_db$host_is_superhost[which(is.na(MIQ3_db$host_is_superhost))] <- 'FALSE'


## Property ----
### Location (Neighborhood) ----

# first, keep only BnBs within the city boundaries
# cols: "neighbourhood" "neighbourhood_cleansed" "neighbourhood_group_cleansed"   
which(is.na(MIQ3_db$neighbourhood_cleansed)) # start from this column 
unique(MIQ3_db$neighbourhood_cleansed[is.na(MIQ3_db$neighbourhood) | 
                                        !(MIQ3_db$neighbourhood %like% 'Milan%')])

# drop places lying outside the metro-line system (by vars: neighbourhood and neighbourhood_group_cleansed)
unique(MIQ3_db$neighbourhood)
# drop neighbourhoods outside the metro system 
MIQ3_db = MIQ3_db[!(MIQ3_db$neighbourhood %like% 
                      c('Cantalupa%','Noverasco%','Settimo Mila%','Bresso%',
                        "Chiaravalle%","Rozzano%",
                        "Vimodrone%","Cormano%") ),]

# in the analysis, we'll keep only neighbourhood_cleansed, so let's drop the other vars
MIQ3_db <- MIQ3_db[,!(names(MIQ3_db) %in% c("neighbourhood","neighbourhood_group_cleansed"))]

# recode neighbourhood strings to make them more easy to handle
MIQ3_db$neighbourhood_cleansed <- gsub("[^A-Za-z0-9]", "", MIQ3_db$neighbourhood_cleansed)

# and factor neighbourhood
MIQ3_db$neighbourhood_f = as.factor(MIQ3_db$neighbourhood_cleansed)
# and dummies
MIQ3_db <- dummy_cols(MIQ3_db, select_columns = "neighbourhood_cleansed", remove_first_dummy = TRUE) #remove option to avoid multicollinearity

### Type of Room ---- 
# Room type - Main variable 
length(which(is.na(MIQ3_db$room_type))) # no NAs
unique(MIQ3_db$room_type)

# Property type - more specific
length(which(is.na(MIQ3_db$property_type))) # no NAs
unique(MIQ3_db$property_type)

# keep if property type is Apartment, House or Townhouse
MIQ3_db <- MIQ3_db[!(MIQ3_db$room_type %like% 'Hotel%'),]

# let's check in property_type - more detailed
MIQ3_db$property_type <- tolower(MIQ3_db$property_type)
MIQ3_db <- MIQ3_db[!(MIQ3_db$property_type %like% c('%hostel','%guesthouse','%hotel','%bed and breakfast', '%camper/rv%','boat')),]

# create factor and dummies on room_type
MIQ3_db$room_type_f <- as.factor(MIQ3_db$room_type)

MIQ3_db <- MIQ3_db %>% mutate(variable = fct_recode(room_type_f,"Entire_prop" = "Entire home/apt",
                                                    "Private_room"   = "Private room", "Shared_room" = "Shared room"))

MIQ3_db <- dummy_cols(MIQ3_db, select_columns = "room_type_f", remove_first_dummy = TRUE) #remove option to avoid multicollinearity


### Guests capacity ---- 

# Nr accepted people 
class(MIQ3_db$accommodates)
length(which(is.na(MIQ3_db$accommodates)))

summary(MIQ3_db$accommodates)

### Size of the property ----

# Now let's classify the apt size by the nr of bedrooms
summary(MIQ3_db$bedrooms) # few Nas, drop them
MIQ3_db<- MIQ3_db[!is.na(MIQ3_db$bedrooms),]

MIQ3_db$bedrooms_f <- NA  
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms == 0 ] <- '0Broom'
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms == 1 ] <- '1Broom'
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms == 2 ] <- '2Broom'
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms == 3 ] <- '3Broom'
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms == 4 ] <- '4Broom'
MIQ3_db$bedrooms_f[MIQ3_db$bedrooms >= 5 ] <- '5+Broom'
MIQ3_db$bedrooms_f <- as.factor(MIQ3_db$bedrooms_f)

### Bathrooms (nr and shared dummy) ----

# Nr. Bathrooms
summary(MIQ3_db$bathrooms)
# 3 missing

# turn into numeric variable
MIQ3_db$bathrooms <- as.numeric(MIQ3_db$bathrooms)

MIQ3_db <- MIQ3_db[!is.na(MIQ3_db$bathrooms),]
# Categorical: no bathroom, one bathroom, >1 bathroom (as integers)
MIQ3_db$bathrooms_f <- NA
MIQ3_db$bathrooms_f[MIQ3_db$bathrooms == 0 ] <- '0'
MIQ3_db$bathrooms_f[MIQ3_db$bathrooms > 0 & MIQ3_db$bathrooms < 2 ] <- '1'
MIQ3_db$bathrooms_f[MIQ3_db$bathrooms >= 2] <- '2_over' 
MIQ3_db$bathrooms_f <- as.factor(MIQ3_db$bathrooms_f)
table(MIQ3_db$bathrooms_f)

# Dummy Shared Bath
MIQ3_db$d_sharebath <- 0
MIQ3_db$d_sharebath[MIQ3_db$bathrooms_text %like% c('%share%', '%Share%')] <- 1
MIQ3_db$d_sharebath[is.na(MIQ3_db$bathrooms)] <- NA
table(MIQ3_db$d_sharebath)


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
MIQ3_db$amenities <- tolower(MIQ3_db$amenities)

# Example usage
unique_amenities_db <- extract_unique_amenities(MIQ3_db, "amenities")
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
  MIQ3_db[[amenity]] <- 0
  MIQ3_db[[amenity]] <- as.integer(
  Reduce('|', lapply(amenities_list[[amenity]], function(pattern) MIQ3_db$amenities %like% pattern))
)
  MIQ3_db[[amenity]] <- as.numeric(MIQ3_db[[amenity]])
}

# Additional rules for exceptions
MIQ3_db$d_am_locker[MIQ3_db$amenities %like% "%baby safety gates%"] <- 0
MIQ3_db$d_am_outdoor[MIQ3_db$amenities %like% "%outdoor furniture%"] <- 0


# table(MIQ3_db$d_am_kitchen) # most bnbs have kitchens

### Reviews ----

cols <- names(MIQ3_db)[c(20:26,28:35)]

print("NAs by variable")
for (col in cols) {
  
  na_nr = sum(is.na(MIQ3_db[[col]]) )
  
  if (sum(is.na(MIQ3_db[[col]])) > 0) {
    
    print(col)
    print(na_nr)
    
  }
}

print(cols)
# keep max/min nights , nr reviews, availabilities (no NAs)
# drop score rating and review per month (too many NAs, ~20%)
summary(MIQ3_db$number_of_reviews)


# DATA VISUALISATION ----

## Price - Dependent variable ----

summary(MIQ3_db$price_n)
quantile(MIQ3_db$price_n, probs = c(0.95, 0.96,0.99), na.rm = T) 
# 99% distribution at 1000
summary(MIQ3_db$price_ln)

p1 <- ggplot(MIQ3_db, aes(x = price_n)) +
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

p2 <- ggplot(MIQ3_db, aes(x = price_ln)) +
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

pp<- annotate_figure(plot_p, top = text_grob("USD Price distribution AirBnB, MIQ3", 
                                             color = "black", family = "Garamond", size = 14))

pp

## Room type ----

ggplot(MIQ3_db, aes(x= room_type, y= price_ln, fill = room_type)) + 
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


ggplot(MIQ3_db, aes(x= d_am_petsallowed, y= price_ln, fill = as.factor(d_am_petsallowed))) + 
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

b1<- ggplot(MIQ3_db, aes(x= as.factor(bedrooms), y= price_ln, fill = as.factor(bedrooms))) + 
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

b2<- ggplot(MIQ3_db, aes(x= as.factor(bedrooms_f), y= price_ln, fill = as.factor(bedrooms_f))) + 
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

annotate_figure(plot_aptsize , top = text_grob("Flat size numeric vs categorical, MIQ3", color = "black", family = "Garamond", size = 14))


MIQ3_db <- dummy_cols(MIQ3_db, select_columns = "bedrooms_f", remove_first_dummy = TRUE) #remove option to avoid multicollinearity


### Correlation tabs wrt price (factors)

dummy_vars <- MIQ3_db %>%
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


dummy_vars <- MIQ3_db %>%
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

ggplot(MIQ3_db,aes(x= accommodates , y = price_ln)) + 
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

num_vars <- MIQ3_db %>%
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

write_rds(MIQ3_db, 'MIQ3_db_clean.rds')



