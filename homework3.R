#SET YOUR WORKING DIRECTORY
setwd('/home/hmont/Documents/cours/TARTU-UNIVERSITY/S2/Business-Analytics/Homeworks/BA-Homework-03')

#download necessary libraries (this needs to be run only once)
#install.packages('rvest')
#install.packages('qdap')
#install.packages('ggplot2')

#activate libraries
library(qdap)
library(rvest)
library(ggplot2)
library(dplyr)
library(stringr)

# URL of the Inside Airbnb website
url <- "https://insideairbnb.com/get-the-data/"

# Read the webpage
webpage <- read_html(url)

# Extract the list of cities from the webpage
cities <- webpage %>%
  html_nodes("h3") %>%
  html_text() 

# Randomly choose a city from the list
#random_city <- sample(cities, 1)
# We got the city Prague (and the put it in variable so it wouldn't change when we run everything)
random_city <- "Prague, Prague, Czech Republic"

# Print the randomly chosen city
print(random_city)
#This will be your city. 

#Now go to the webpage to download the listings.csv.gz UNDER your city. 
all_links_on_the_page=webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.data.frame()

#let's remove country or state name from location
city_name_only = random_city %>% str_split(',',simplify = TRUE) %>%  as.data.frame() %>% select(1) %>% tolower()

#if the city contains more than. two words, e.g. The Hague, then add a dash in between (that's how data the link is constructed on the webpage)
if( lengths(strsplit(city_name_only, "\\W+")) > 1){
  city_name_only=str_replace(city_name_only,' ','-')
}


#build the url for YOUR CITY
url_for_download = all_links_on_the_page[grep(city_name_only,all_links_on_the_page[,1])[2],1]
#check the URL, you can also validate it in the browser just for your curiousity
print(url_for_download)


#Download the file and choose name
download.file(url_for_download,'homework_dataset.csv.gz')

#downloaded file with be in csv.gz format. To change this we first need to read the file in R in csv then save it in our path.
listings_df  = read.csv("homework_dataset.csv.gz")

#From now on you will use listings_df variable as your variable that will contain your city's data
#Familiarize yourself with the data. Note that this dataset includes more columns that what we had in the lectures.

#=====Exercises and graded parts start here. 

#Part 1 ==============================================================

# Write all the codes below the exercise
# Comment your answer when a question is asked.

#WRITE YOUR CITY NAME HERE-------------> Prague, Prague, Czech Republic

#EXERCISE 1 (6 points) ========================================

# a) (1 point)
# check the data format, what kind of variable do you have?
str(listings_df)
# There are num, chr, logical and int variables in the dataset.

# b) (1 point)
# subset your data to keep only 1000 rows
listings_short=head(listings_df,1000)

# c) (1 point) 
# are there any missing values in your dataset's important columns, such as id, host_id or price? If yes, remove the rows where there are any missing values. 
# if there are no missing values, explain how you checked for them and how you would remove them. 
# We can check which cells might be empty with the is.na function we saw in class. 
# We can obviously not check them all manually so we'll make a loop.
which(is.na(listings_short$id))
which(is.na(listings_short$host_id))
which(is.na(listings_short$price))

# We can see that right now there are no missing values in those important columns. If there was, we would remove the rows with the following code
# We first store the missing row numbers in a variable, and then delete them from the dataframe with the minus sign
# missing_rows <- which(is.na(listings_short$id))
# listings_short <- listings_short[-missing_rows, ]
# We will use this method in d) where after data type change, due to coercion NA appear.

# d) (2 points) 
# check the data type of all your columns. Are there some that needs to be changes, e.g. from integer to date or from date to text etc. 
# are there any other modifications needed to be done to column types or values to prepare your data for analysis. 
# hint: pay attention to price column specifically and make any necessary changes.
str(listings_short)

#change the id to integer
listings_short$id <- as.integer(listings_short$id)

# Clean columns with text and numbers
listings_short$price <- as.numeric(gsub("[^0-9.]", "", listings_short$price))
listings_short$bathrooms_text <- as.numeric(gsub("[^0-9.]", "", listings_short$bathrooms_text))

#cleaning of price needed
missing_rows <- which(is.na(listings_short$price))
listings_short <- listings_short[-missing_rows, ]


# Convert 'minimum_nights' and 'maximum_nights' columns to integer
listings_short$minimum_nights <- as.integer(listings_short$minimum_nights)
listings_short$maximum_nights <- as.integer(listings_short$maximum_nights)

# Convert 'host_response_rate' and 'host_acceptance_rate' columns to numeric comment: it introduced null values because some characters are empty
listings_short$host_response_rate <- as.numeric(gsub("[%]", "", listings_short$host_response_rate))
listings_short$host_acceptance_rate <- as.numeric(gsub("[%]", "", listings_short$host_acceptance_rate))

# Convert date columns to Date type
listings_short$last_scraped <- as.Date(listings_short$last_scraped)
listings_short$host_since <- as.Date(listings_short$host_since)
listings_short$first_review <- as.Date(listings_short$first_review)
listings_short$last_review <- as.Date(listings_short$last_review)
listings_short$calendar_last_scraped <- as.Date(listings_short$calendar_last_scraped)

# Convert boolean columns to logical (as.logical didn't work because it didn't understand "t" and "f")
listings_short$host_is_superhost <- ifelse(listings_short$host_is_superhost == "t", TRUE,
                                           ifelse(listings_short$host_is_superhost == "f", FALSE, NA))
listings_short$host_has_profile_pic <- ifelse(listings_short$host_has_profile_pic == "t", TRUE,
                                              ifelse(listings_short$host_has_profile_pic == "f", FALSE, NA))
listings_short$host_identity_verified <- ifelse(listings_short$host_identity_verified == "t", TRUE,
                                                ifelse(listings_short$host_identity_verified == "f", FALSE, NA))
listings_short$has_availability <- ifelse(listings_short$has_availability == "t", TRUE,
                                          ifelse(listings_short$has_availability == "f", FALSE, NA))
listings_short$instant_bookable <- ifelse(listings_short$instant_bookable == "t", TRUE,
                                          ifelse(listings_short$instant_bookable == "f", FALSE, NA))

# Convert 'number_of_reviews' columns to integer
listings_short$number_of_reviews <- as.integer(listings_short$number_of_reviews)

#remove the collumns with more than 999 missing values
str(listings_short)
for (col in names(listings_short)){
  na_count <- sum(is.na(listings_short[[col]]))
  if (na_count > 999) {
    listings_short[[col]] <- NULL
  }
}

# Drop columns with inconsistent or unnecessary data
listings_short$host_picture_url <- NULL
listings_short$host_thumbnail_url <- NULL
listings_short$host_url <- NULL
listings_short$picture_url <- NULL
listings_short$listing_url <- NULL
listings_short$amenities <- NULL

# Check the modified data types

str(listings_short)



# e) (1 points) 
# save the resulting change in a new dataframe and save the dataframe locally on your computer. 
updated_listings <- listings_short
write.csv(updated_listings,'listings_updated.csv')
  


#EXERCISE 2 (10 points) ==============


# a) (7 points)
#Using GGPLOT package, replicate the exact same figure as 1a) in HOMEWORK GUIDE file, part 1. 
#Please note, examples are for Amsterdam.
#Hint: Revenue is defined by prices paid. So if a guest books 1 night, the host gets revenue equal to nightly price.

#we need to calculate the annual revenue assuming that every night is booked
updated_listings$annual_revenue = updated_listings$price * 365

# we calculate the revenue per neighborhood
revenue_per_neighborhood <- updated_listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    number_of_listings = n(),
    avg_revenue = mean(annual_revenue, na.rm = TRUE)
  ) %>%
  arrange(desc(number_of_listings)) %>%
  mutate(neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = unique(neighbourhood_cleansed)))



#store max listings and max revenue for later use in the scaling process
max_listings <- max(revenue_per_neighborhood$number_of_listings)
max_revenue <- max(revenue_per_neighborhood$avg_revenue)

ggplot(revenue_per_neighborhood, aes(x = neighbourhood_cleansed)) +
  geom_bar(aes(y = number_of_listings), stat = "identity", fill = "#69b3a2") +
  geom_line(aes(y = avg_revenue / max_revenue * max_listings, group = 1), color = "black") +
  scale_y_continuous(
    name = "Number of Listings",
    sec.axis = sec_axis(~ . / max_listings * max_revenue, 
                        name = "Average Revenue (in millions CZK)",
                        labels = scales::comma_format(suffix = "M", accuracy = 0.01, scale = 1e-6))
  ) +
  ggtitle("Listings vs. Prices Per Neighborhood") +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
  



# b) (3 points)
#Explain the results in the above figure in detail by commenting below. 
#In the above figure it is clear that the amount of listings is the highest in the Praha 1 district because that district is in the center of the city
#and most tourists that book on such platforms choose to stay near the center.
#The popularity of renting a place outside the center rapidly declines.
#The highest Average Revenue is in Praha 2 district because it is considered the area of the historic establishment of the Czech ruler
#and therefore the value of the property is the highest. 
#Comment: on the course moodle page, there is a ggplot guide that has instructions on how to do double axis plots.

#EXERCISE 3 (10 points) ==============


# a) (7 points)
#Using GGPLOT package, replicate the exact same figure as 1b) in HOMEWORK GUIDE file, part 1. 
#Please note, examples are for Amsterdam.
#Hint: Revenue is defined by prices paid. So if a guest books 1 night, the host gets revenue equal to nightly price.

ggplot(revenue_per_neighborhood, aes(x = number_of_listings, y = avg_revenue, color = neighbourhood_cleansed)) +
  geom_point(aes(size = number_of_listings)) + 
  geom_text(aes(label = neighbourhood_cleansed), vjust = -1) + 
  labs(
    x = "Total Number of Listings",
    y = "Total estimated revenue (in millions CZK)",
    size = "Number of Listings",
    title = "Listings vs. Average Revenue Per Neighborhood"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )



#pie-chart part
room_type_counts <- table(updated_listings$room_type)
room_type_counts_df <- data.frame(room_type = names(room_type_counts),
                                  count = as.numeric(room_type_counts))

ggplot(room_type_counts_df, aes(x = "", y = count, fill = room_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer("Room Types", palette = "Dark2") +
  ggtitle("Type of Rooms") +
  ylab("") +
  xlab("") +
  theme_minimal()


# b) (3 points)
#Explain the results in the above figure in detail by commenting below. 
# The figure above shows a relationship between the number of listings and the total estimated revenue.
# Each point represents a neighborhood. Praha 1 and Praha 2 are neighborhoods with more listings and the also generate higher revenue.
# Praha 2 indicates the highest revenue per listing. In comparison Praha 3 with its relatively high number of listings ans low revenue shows its a less desirable than Praha 1, or Praha 2. 
# Other districts have negligible amount of listings apart from Praha 5, 10, 6, 7, 8. indicating that they are residential neighborhoods without 
#the need for turism.






#Part 2 ==================================================================

#You need to use the file that you saved in Exercise 1d) in your working directory and upload it Snowflake, do all the analysis there and copy the results here

# a)  (2 points)

#QUESION:
#Copy your codes of creating the listings table here. Remember to make sure all columns types are correct.

# CREATE TABLE LISTINGS (
#   void INT,
#   id INT,
#   scrape_id BIGINT,
#   last_scraped DATE,
#   source VARCHAR,
#   name VARCHAR,
#   neighborhood_overview VARCHAR,
#   host_id INT,
#   host_name VARCHAR,
#   host_since DATE,
#   host_location VARCHAR,
#   host_about VARCHAR,
#   host_response_time VARCHAR,
#   host_response_rate INT,
#   host_acceptance_rate INT,
#   host_is_superhost BOOLEAN,
#   host_neighbourhood VARCHAR,
#   host_listings_count INT,
#   host_total_listings_count INT,
#   host_verifications VARCHAR,
#   host_has_profile_pic BOOLEAN,
#   host_identity_verified BOOLEAN,
#   neighbourhood VARCHAR,
#   neighbourhood_cleansed VARCHAR,
#   latitude FLOAT,
#   longitude FLOAT,
#   property_type VARCHAR,
#   room_type VARCHAR,
#   accommodates INT,
#   bathrooms_text VARCHAR,
#   beds INT,
#   price FLOAT,
#   minimum_nights INT,
#   maximum_nights INT,
#   minimum_minimum_nights INT,
#   maximum_minimum_nights INT,
#   minimum_maximum_nights INT,
#   maximum_maximum_nights INT,
#   minimum_nights_avg_ntm FLOAT,
#   maximum_nights_avg_ntm FLOAT,
#   has_availability BOOLEAN,
#   availability_30 INT,
#   availability_60 INT,
#   availability_90 INT,
#   availability_365 INT,
#   calendar_last_scraped DATE,
#   number_of_reviews INT,
#   number_of_reviews_ltm INT,
#   number_of_reviews_l30d INT,
#   first_review DATE,
#   last_review DATE,
#   review_scores_rating FLOAT,
#   review_scores_accuracy FLOAT,
#   review_scores_cleanliness FLOAT,
#   review_scores_checkin FLOAT,
#   review_scores_communication FLOAT,
#   review_scores_location FLOAT,
#   review_scores_value FLOAT,
#   instant_bookable BOOLEAN,
#   calculated_host_listings_count INT,
#   calculated_host_listings_count_entire_homes INT,
#   calculated_host_listings_count_private_rooms INT,
#   calculated_host_listings_count_shared_rooms INT,
#   reviews_per_month FLOAT
# );


# b) (3 points)

#QUESTION:
#Find the number of listings in each neighborhood that have a minimum nights greater than 5 and the price less than 150 dollars.
#Sort the results by minimum nights increasing order.

#ANSWER:

# SELECT neighbourhood_cleansed, COUNT(*) AS num_listings
# FROM LISTINGS
# WHERE minimum_nights > 5 AND price < 150
# GROUP BY neighbourhood_cleansed
# ORDER BY minimum_nights ASC;

# This request does not give me any result because there are no listing with minmum_nights > 5 and price < 150.
# So it then tries to order an empty result which gives out an error.


# c) (3 points)

#QUESTION:
# Identify the host with the most listings in your city. use sorting and limiting commands to do so. 

#ANSWER:

# SELECT host_id, COUNT(*) AS num_listings
# FROM LISTINGS
# GROUP BY host_id
# ORDER BY num_listings DESC
# LIMIT 1;

# HOST_ID	NUM_LISTINGS
# 227945	32

# d) (3 points)

#QUESTION:
#Calculate the annual revenue generated from all listings in your city that have a rating between 8 and 10, and where the listing's neighborhoods description shows that the the listing is a walking distance from center. 
# use keywords like 'walk' and 'center' to filter out the listings that are not easily walkable to the center.

#ANSWER:

# SELECT SUM(price * 365) AS annual_revenue
# FROM LISTINGS
# WHERE review_scores_rating >= 8 AND review_scores_rating <= 10
# AND LOWER(neighborhood_overview) LIKE '%walk%'
# AND LOWER(neighborhood_overview) LIKE '%center%';

# This request does not give me any results as there are no listing with a review score between 8 and 10.

# e) (3 points)


#QUESTION: 
#Then find the top 5 neighborhoods in your city that have the highest average rating, but limit the highest average rating to be at least 3.

#ANSWER:

# SELECT neighbourhood_cleansed AS neighborhood, 
# AVG(review_scores_rating) AS avg_rating
# FROM LISTINGS
# GROUP BY neighbourhood_cleansed
# HAVING AVG(review_scores_rating) >= 3
# ORDER BY avg_rating DESC
# LIMIT 5;

# NEIGHBORHOOD	AVG_RATING
# Praha 21	5
# Praha 16	5
# Kunratice	4.89
# Praha 9	4.873076923
# Praha 11	4.85






