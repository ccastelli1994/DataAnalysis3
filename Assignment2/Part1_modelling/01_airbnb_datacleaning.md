Building a prediction model on house prices - 01 data cleaning
================
Chiara Castelli

This file reads, cleans and visualise data on the main AirBnb database (i.e. City: Milan, Q: 2024-Q1) 

# Reading the Database


``` r
# drop some columns 
# not meaningful ones: e.g. scraping ids, url, descriptions, some host information etc.
MIQ1_db <- MIQ1_db[!(names(MIQ1_db) %in% c('listing_url', 'scrape_id', 'source', 'description','neighborhood_overview',"host_name","host_location", 'picture_url','host_url', 'host_about','host_thumbnail_url', 'has_availability', 'host_picture_url','host_listings_count','host_total_listings_count','host_has_profile_pic', 'latitude','longitude', 'calendar_updated','calendar_last_scraped') ) ]
MIQ1_db <- MIQ1_db[!(names(MIQ1_db) %like% c('calculated_host_listings%') ) ]

#sort(unique(MIQ1_db$last_scraped)) # date of scrapes is 24/03,25/03 and 05/04

# delete additional cols - this time quickly done by column index
MIQ1_db <- MIQ1_db[,-c(2,11, 12, 27:32, 38:40, 49:50)]
names(MIQ1_db)
```

    ##  [1] "id"                           "name"                        
    ##  [3] "host_id"                      "host_since"                  
    ##  [5] "host_response_time"           "host_response_rate"          
    ##  [7] "host_acceptance_rate"         "host_is_superhost"           
    ##  [9] "host_neighbourhood"           "neighbourhood"               
    ## [11] "neighbourhood_cleansed"       "neighbourhood_group_cleansed"
    ## [13] "property_type"                "room_type"                   
    ## [15] "accommodates"                 "bathrooms"                   
    ## [17] "bathrooms_text"               "bedrooms"                    
    ## [19] "beds"                         "amenities"                   
    ## [21] "price"                        "minimum_nights"              
    ## [23] "maximum_nights"               "availability_30"             
    ## [25] "availability_60"              "availability_90"             
    ## [27] "availability_365"             "number_of_reviews"           
    ## [29] "last_review"                  "review_scores_rating"        
    ## [31] "review_scores_accuracy"       "review_scores_cleanliness"   
    ## [33] "review_scores_checkin"        "review_scores_communication" 
    ## [35] "review_scores_location"       "review_scores_value"         
    ## [37] "reviews_per_month"

According to this list, we can define 3 main variable groups: - Host
information - Property characteristics : location and specific
features - Reservation: availability and review

# 1 Data Cleaning

In this section we work on the cleaning and editing of some variables/
group of variables.

## 1.1 Price (dependent variable)

``` r
# which(is.na(MIQ1_db$price))
# unique(MIQ1_db$price) # some empty strings - NA

MIQ1_db$price_n <- substr(MIQ1_db$price, 2,nchar(MIQ1_db$price))
MIQ1_db$price_n <- gsub(",", "", MIQ1_db$price_n)
MIQ1_db$price_n <- as.numeric(MIQ1_db$price_n)
unique(MIQ1_db$price[is.na(MIQ1_db$price_n)])
```

    ## [1] ""

``` r
print('Nr missing prices')
```

    ## [1] "Nr missing prices"

``` r
NROW(which(is.na(MIQ1_db$price_n))) 
```

    ## [1] 515

Drop missing prices

``` r
MIQ1_db <- MIQ1_db[!(is.na(MIQ1_db$price_n)),]
```

and compute the log scale for comparison (later)

``` r
# compute log for comparison
MIQ1_db$price_ln <- log(MIQ1_db$price_n)
```

## 1.2 Host information

``` r
print("Nr. properties per host")
```

    ## [1] "Nr. properties per host"

``` r
dim(MIQ1_db)[1]/length(unique(MIQ1_db$host_id)) # accom. /host
```

    ## [1] 1.78

``` r
# some have multiple properties

head(MIQ1_db)
```

    ## # A tibble: 6 × 39
    ##        id name          host_id host_since host_response_time host_response_rate
    ##     <dbl> <chr>           <dbl> <date>     <chr>              <chr>             
    ## 1 3.92e 7 The Best Ren…  4.42e6 2012-12-16 within an hour     97%               
    ## 2 1.78e 7 Studio nel c…  2.00e7 2014-08-14 a few days or more 0%                
    ## 3 1.08e18 PrimoPiano -…  2.50e6 2012-05-30 within an hour     90%               
    ## 4 5.83e17 Italianway -…  2.77e7 2015-02-13 within an hour     99%               
    ## 5 6.26e17 One Suite Me…  4.59e8 2022-05-14 within an hour     90%               
    ## 6 6.09e17 Casa Loca      1.63e8 2017-12-15 within an hour     100%              
    ## # ℹ 33 more variables: host_acceptance_rate <chr>, host_is_superhost <lgl>,
    ## #   host_neighbourhood <chr>, neighbourhood <chr>,
    ## #   neighbourhood_cleansed <chr>, neighbourhood_group_cleansed <lgl>,
    ## #   property_type <chr>, room_type <chr>, accommodates <dbl>, bathrooms <dbl>,
    ## #   bathrooms_text <chr>, bedrooms <dbl>, beds <dbl>, amenities <chr>,
    ## #   price <chr>, minimum_nights <dbl>, maximum_nights <dbl>,
    ## #   availability_30 <dbl>, availability_60 <dbl>, availability_90 <dbl>, …

``` r
# change host rates into numeric

MIQ1_db$host_acceptance_rate <- gsub('%','', MIQ1_db$host_acceptance_rate )
MIQ1_db$host_acceptance_rate <- as.numeric(MIQ1_db$host_acceptance_rate)

MIQ1_db$host_response_rate <- gsub('%','', MIQ1_db$host_response_rate )
MIQ1_db$host_response_rate <- as.numeric(MIQ1_db$host_response_rate)

cols <- which(names(MIQ1_db) == "host_response_time"):which(names(MIQ1_db) == "host_is_superhost")

print("NAs by variable")
```

    ## [1] "NAs by variable"

``` r
for (col in cols) {
  
  na_nr = sum(is.na(MIQ1_db[[col]]) )
  
  if (sum(is.na(MIQ1_db[[col]])) > 0) {
  
    print(names(MIQ1_db)[col])
    print(na_nr)
    
  }
}
```

    ## [1] "host_response_time"
    ## [1] 2665
    ## [1] "host_response_rate"
    ## [1] 2665
    ## [1] "host_acceptance_rate"
    ## [1] 1712
    ## [1] "host_is_superhost"
    ## [1] 21

``` r
# Deal with Missings 

# first check corr with dep variable
cor(MIQ1_db$price_n, MIQ1_db$host_acceptance_rate, use = 'complete.obs')
```

    ## [1] 0.0153

``` r
cor(MIQ1_db$price_n, MIQ1_db$host_response_rate, use = 'complete.obs')
```

    ## [1] -0.0498

``` r
# keep response rate ( higher corr). replace with median
MIQ1_db$d_miss_resp_rate <- 0
MIQ1_db$d_miss_resp_rate[is.na(MIQ1_db$host_response_rate)] <- 1
value_resp_rate = median(MIQ1_db$host_response_rate, na.rm = T)
MIQ1_db$host_response_rate[is.na(MIQ1_db$host_response_rate)] <- value_resp_rate

# replace categorical host_response_time & host_is_superhost with mode
table(MIQ1_db$host_response_time) # mode: within an hour 
```

    ## 
    ## a few days or more       within a day within a few hours     within an hour 
    ##                675               1570               2234              14447

``` r
MIQ1_db$d_miss_resp_time <- 0
MIQ1_db$d_miss_resp_time[is.na(MIQ1_db$host_response_time)] <- 1
MIQ1_db$host_response_time[is.na(MIQ1_db$host_response_time)] <- 'within an hour'

# create numerical variable host_resptime_n
MIQ1_db$host_resptime_n <- NA
MIQ1_db$host_resptime_n[MIQ1_db$host_response_time == "within an hour"]<- 1
MIQ1_db$host_resptime_n[MIQ1_db$host_response_time == "within a few hours"]<- 2
MIQ1_db$host_resptime_n[MIQ1_db$host_response_time == "within a day" ]<- 3
MIQ1_db$host_resptime_n[MIQ1_db$host_response_time == "a few days or more"]<- 4
summary(MIQ1_db$host_resptime_n)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00    1.00    1.00    1.34    1.00    4.00

``` r
# superhost - recode
table(MIQ1_db$host_is_superhost) # FALSE
```

    ## 
    ## FALSE  TRUE 
    ## 15425  6145

``` r
MIQ1_db$d_miss_superhost <- 0
MIQ1_db$d_miss_superhost[is.na(MIQ1_db$d_miss_superhost)] <- 1
MIQ1_db$host_is_superhost[is.na(MIQ1_db$host_is_superhost)] <- 'FALSE'
```

## 1.3 Property

### Location (Neighborhood)

### Property characteristics

#### Type of Room

We would like to drop any type of accommodation that is not a private
accomodation - i.e. drop Hotel room

``` r
# keep if property type is Apartment, House or Townhouse
MIQ1_db <- MIQ1_db[!(MIQ1_db$room_type %like% 'Hotel%'),]

# let's check in property_type - more detailed
MIQ1_db$property_type <- tolower(MIQ1_db$property_type)
MIQ1_db <- MIQ1_db[!(MIQ1_db$property_type %like% c('%hostel','%guesthouse','%hotel','%bed and breakfast', '%camper/rv%','boat')),]

# creat factor and dummies on room_type
MIQ1_db$room_type_f <- as.factor(MIQ1_db$room_type)

MIQ1_db <- MIQ1_db %>% mutate(variable = fct_recode(room_type_f,"Entire_prop" = "Entire home/apt",
     "Private_room"   = "Private room", "Shared_room" = "Shared room"))

MIQ1_db <- dummy_cols(MIQ1_db, select_columns = "room_type_f", remove_first_dummy = TRUE) #remove option to avoid multicollinearity
```

#### Guests capacity

#### Size of the property

Lets create a categorical variable starting from the nr of bedrooms

``` r
# Now let's classify the apt size by the nr of bedrooms
summary(MIQ1_db$bedrooms) # few Nas, drop them
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    1.00    1.00    1.19    1.00    8.00       7

``` r
MIQ1_db<- MIQ1_db[!is.na(MIQ1_db$bedrooms),]

MIQ1_db$bedrooms_f <- NA  
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms == 0 ] <- '0Broom'
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms == 1 ] <- '1Broom'
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms == 2 ] <- '2Broom'
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms == 3 ] <- '3Broom'
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms == 4 ] <- '4Broom'
MIQ1_db$bedrooms_f[MIQ1_db$bedrooms >= 5 ] <- '5+Broom'
MIQ1_db$bedrooms_f <- as.factor(MIQ1_db$bedrooms_f)
```

#### Bathrooms (nr and shared dummy)

``` r
# Nr. Bathrooms
summary(MIQ1_db$bathrooms)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    1.00    1.00    1.15    1.00    7.00       2

``` r
# 2 missings

# # replace with text when there is no info
# MIQ1_db$bathrooms[is.na(MIQ1_db$bathrooms)] <- substr(MIQ1_db$bathrooms_text[is.na(MIQ1_db$bathrooms)], 1, 2)
# 
# MIQ1_db$bathrooms[MIQ1_db$bathrooms == "Ha"] <- "0.5"
# 
# conditions <- substr(MIQ1_db$bathrooms,2,2) == "." & (MIQ1_db$bathrooms != "0.5") &
#                     !(is.na(MIQ1_db$bathrooms)) 
# 
# MIQ1_db$bathrooms[conditions == T] <- substr(MIQ1_db$bathrooms_text[conditions == T], 1, 3)
# 
# print('Final nr of NAs')
# length(which(is.na(MIQ1_db$bathrooms))) 

# finally, turn into numeric variable
MIQ1_db$bathrooms <- as.numeric(MIQ1_db$bathrooms)

MIQ1_db <- MIQ1_db[!is.na(MIQ1_db$bathrooms),]
# Categorical: no bathroom, one bathroom, >1 bathroom (as integers)
MIQ1_db$bathrooms_f <- NA
MIQ1_db$bathrooms_f[MIQ1_db$bathrooms == 0 ] <- '0'
MIQ1_db$bathrooms_f[MIQ1_db$bathrooms > 0 & MIQ1_db$bathrooms < 2 ] <- '1'
MIQ1_db$bathrooms_f[MIQ1_db$bathrooms >= 2] <- '2_over' 
MIQ1_db$bathrooms_f <- as.factor(MIQ1_db$bathrooms_f)
table(MIQ1_db$bathrooms_f)
```

    ## 
    ##      0      1 2_over 
    ##     45  18641   2565

``` r
# Dummy Shared Bath
MIQ1_db$d_sharebath <- 0
MIQ1_db$d_sharebath[MIQ1_db$bathrooms_text %like% c('%share%', '%Share%')] <- 1
MIQ1_db$d_sharebath[is.na(MIQ1_db$bathrooms)] <- NA
table(MIQ1_db$d_sharebath)
```

    ## 
    ##     0     1 
    ## 19952  1299

#### Amenities

Let’s select some meaningful ones by key words, and create specific
dummies.

``` r
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
  MIQ1_db[[amenity]] <- 0
  MIQ1_db[[amenity]] <- as.integer(
  Reduce('|', lapply(amenities_list[[amenity]], function(pattern) MIQ1_db$amenities %like% pattern))
)
  MIQ1_db[[amenity]] <- as.factor(MIQ1_db[[amenity]])
}

# Additional rules for exceptions
MIQ1_db$d_am_locker[MIQ1_db$amenities %like% "%baby safety gates%"] <- 0
MIQ1_db$d_am_outdoor[MIQ1_db$amenities %like% "%outdoor furniture%"] <- 0


# table(MIQ1_db$d_am_kitchen) # most bnbs have kitchens
```

## 1.4 Reviews

# 2 Data Visualisation

## 2.1 Price - Dependent variable

``` r
summary(MIQ1_db$price_n)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      14      79     105     171     159   90000

``` r
quantile(MIQ1_db$price_n, probs = c(0.95, 0.96,0.99), na.rm = T) 
```

    ##  95%  96%  99% 
    ##  399  440 1000

``` r
# 99% distribution at 1000
summary(MIQ1_db$price_ln)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.64    4.37    4.65    4.77    5.07   11.41

``` r
p1 <- ggplot(MIQ1_db, aes(x = price_n)) +
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

p2 <- ggplot(MIQ1_db, aes(x = price_ln)) +
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

annotate_figure(plot_p, top = text_grob("USD Price distribution AirBnB, MIQ1", 
               color = "black", family = "Garamond", size = 14))
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-17-1.png)

Log distribution looks well-behaved, go for log transformation.

## 2.2 Factoral variables

### Room type

``` r
ggplot(MIQ1_db, aes(x= room_type, y= price_ln, fill = room_type)) + 
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
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-18-1.png)

#### Pets allowed dummy

(tried also interaction with room type, but no meaningful results in this case)

``` r
MIQ1_db %>% tabyl(bathrooms, d_sharebath) # there's variation across categories
```

    ##  bathrooms     0    1
    ##        0.0    39    6
    ##        0.5    54    7
    ##        1.0 16918 1098
    ##        1.5   484   80
    ##        2.0  2126   97
    ##        2.5   102    8
    ##        3.0   172    1
    ##        3.5    17    0
    ##        4.0    26    2
    ##        4.5     7    0
    ##        5.0     5    0
    ##        6.0     1    0
    ##        7.0     1    0

``` r
ggplot(MIQ1_db, aes(x= d_am_petsallowed, y= price_ln, fill = as.factor(d_am_petsallowed))) + 
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
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-19-1.png)

``` r
# no difference    
```

### Apt size (Nr. bedrooms)

``` r
b1<- ggplot(MIQ1_db, aes(x= as.factor(bedrooms), y= price_ln, fill = as.factor(bedrooms))) + 
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

b2<- ggplot(MIQ1_db, aes(x= as.factor(bedrooms_f), y= price_ln, fill = as.factor(bedrooms_f))) + 
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

annotate_figure(plot_aptsize , top = text_grob("Flat size numeric vs categorical, MIQ1", color = "black", family = "Garamond", size = 14))
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-20-1.png)

Keep categorical - create dummies

Also, let’s check the correlation with the **accommodates** variable:

``` r
cor(MIQ1_db$bedrooms, MIQ1_db$accommodates , use = 'complete.obs')
```

    ## [1] 0.614

High, but no multicollinearity. Consider to include them first
separately then together.

### Correlation tabs wrt price (factors)

Among dummies: Self check-in and locker are highly correlated *Price
correlation: no amenity seems to be extremely meaningful. Having a
kitchen shows highest coeff, essentials and locker the lowest*

``` r
dummy_vars <- MIQ1_db %>%
  select(starts_with("d_am_"), starts_with("bedrooms_f"), starts_with('room_type_f'),price_ln) %>%
  mutate_all(~ as.numeric(.))

# Compute correlation matrix
cor_matrix <- cor(dummy_vars, use = "pairwise.complete.obs")

# and plot correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Matrix of Dummy Amenities")+
  theme(
    legend.title = element_text(family = "Garamond"),
    axis.text.x = element_text(family = "Garamond", face = "bold"),
    axis.text.y = element_text(family = "Garamond", face = "bold"),
    plot.title = element_text(family = "Garamond")) 
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-23-1.png)

Selection criteria: at least ~ .05 coefficient. Amenities: keep them
all, might be relevant for other samples

## 2.3 Numerical variables - functional form

### Accommodates nr

We include it as numerical, with linear fit

``` r
ggplot(MIQ1_db,aes(x= accommodates , y = price_ln)) + 
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
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-24-1.png)


### Correlation tab numerical variables with price


Select variables we want to include in the correlation tab

``` r
numericals <- c("host_resptime_n", "host_response_rate", 'bedrooms', "accommodates","review_scores_rating","number_of_reviews","minimum_nights",'maximum_nights',"beds", 'availability_30','availability_60','availability_90','availability_365')

num_vars <- MIQ1_db %>%
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
```

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-25-1.png)

Selection criteria: at least ~ .05 coefficient. Drop host_response rate.
We use accommodates (higher correlation than beds, drop it), plus
bedrooms (as categorical), availability_30 (highest corr)

# 3. save cleaned database

``` r
write_rds(MIQ1_db, 'MIQ1_db_clean.rds')
```
