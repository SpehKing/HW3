#SET YOUR WORKING DIRECTORY
setwd("/Users/gasper/Documents/faks/Multimedija/2. letnik/2.semester/DSBA/HW3")


#download necessary libraries (this needs to be run only once)
install.packages('rvest')
install.packages('qdap')
install.packages('ggplot2')

#activate libraries
library(qdap)
library(rvest)
library(ggplot2)
library(dplyr)

# URL of the Inside Airbnb website
url <- "https://insideairbnb.com/get-the-data/"

# Read the webpage
webpage <- read_html(url)

# Extract the list of cities from the webpage
cities <- webpage %>%
  html_nodes("h3") %>%
  html_text() 

# Randomly choose a city from the list
random_city <- sample(cities, 1)

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

#WRITE YOUR CITY NAME HERE------------->Prague

#EXERCISE 1 (6 points) ========================================

# a) (1 point)
# check the data format, what kind of variable do you have?
#In the data format the variables that are there are int, chr, num, logi.

# b) (1 point)
# subset your data to keep only 1000 rows
listings_short=head(listings_df,1000)



# c) (1 point) 
# are there any missing values in your dataset's important columns, such as id, host_id or price? If yes, remove the rows where there are any missing values. 
# if there are no missing values, explain how you checked for them and how you would remove them. 
which(is.na(listings_short$price))
which(is.na(listings_short$id))
which(is.na(listings_short$host_id))

missing_rows <- which(is.na(listings_short$id))
listings_short <- listings_short[-missing_rows, ]




# d) (2 points) 
# check the data type of all your columns. Are there some that needs to be changes, e.g. from integer to date or from date to text etc. 
# are there any other modifications needed to be done to column types or values to prepare your data for analysis. 
# hint: pay attention to price column specifically and make any necessary changes.

for (col in names(listings_short)){
  na_count <- sum(is.na(listings_short[[col]]))
  if (na_count > 999) {
    listings_short[[col]] <- NULL
  }
}
listings_short$amenities <- NULL
print(listings_short$price)

listings_short$id <- as.integer(listings_short$id)

listings_short$last_scraped <- as.Date(listings_short$last_scraped)
listings_short$host_since <- as.Date(listings_short$host_since)
listings_short$first_review <- as.Date(listings_short$first_review)
listings_short$last_review <- as.Date(listings_short$last_review)
listings_short$calendar_last_scraped <- as.Date(listings_short$calendar_last_scraped)

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



# e) (1 points) 
# save the resulting change in a new dataframe and save the dataframe locally on your computer. 
updated_listings <- listings_short
write.csv(updated_listings,'listings_updated.csv')





#EXERCISE 2 (10 points) ==============


# a) (7 points)
#Using GGPLOT package, replicate the exact same figure as 1a) in HOMEWORK GUIDE file, part 1. 
#Please note, examples are for Amsterdam.
#Hint: Revenue is defined by prices paid. So if a guest books 1 night, the host gets revenue equal to nightly price.



# b) (3 points)
#Explain the results in the above figure in detail by commenting below. 



#EXERCISE 3 (10 points) ==============


# a) (7 points)
#Using GGPLOT package, replicate the exact same figure as 1b) in HOMEWORK GUIDE file, part 1. 
#Please note, examples are for Amsterdam.
#Hint: Revenue is defined by prices paid. So if a guest books 1 night, the host gets revenue equal to nightly price.



# b) (3 points)
#Explain the results in the above figure in detail by commenting below. 







#Part 2 ==================================================================

#You need to use the file that you saved in Exercise 1d) in your working directory and upload it Snowflake, do all the analysis there and copy the results here

# a)  (2 points)

#QUESION:
#Copy your codes of creating the listings table here. Remember to make sure all columns types are correct.



# b) (3 points)

#QUESTION:
#Find the number of listings in each neighborhood that have a minimum nights greater than 5 and the price less than 150 dollars.
#Sort the results by minimum nights increasing order.

#ANSWER:




# c) (3 points)

#QUESTION:
# Identify the host with the most listings in your city. use sorting and limiting commands to do so. 

#ANSWER:



# d) (3 points)

#QUESTION:
#Calculate the annual revenue generated from all listings in your city that have a rating between 8 and 10, and where the listing's neighborhoods description shows that the the listing is a walking distance from center. 
# use keywords like 'walk' and 'center' to filter out the listings that are not easily walkable to the center.

#ANSWER:



# e) (3 points)


#QUESTION: 
#Then find the top 5 neighborhoods in your city that have the highest average rating, but limit the highest average rating to be at least 3.


#ANSWER:







