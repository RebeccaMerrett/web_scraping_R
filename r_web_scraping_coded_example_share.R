# Automated Web Scraping in R

# Let's start with a quick demonstration of scraping 
# the main head and body text of a single web page 
#install.packages("rvest") #Uncomment this to install this package
library(rvest)

marketwatch_wbpg <- read_html(
  "https://www.marketwatch.com/story/bitcoin-jumps-after-credit-scare-2018-10-15"
)

marketwatch_wbpg %>%
  html_node("title") %>% #See HTML source code for data within this tag
  html_text()

marketwatch_wbpg %>%
  html_nodes("p") %>% #See HTML source code for data within this tag
  html_text()

# Let's read in all news on Bitcoin using the
# Marketwatch source
marketwatch_bitcoin_articles <- read_html(
  "https://www.marketwatch.com/search?q=bitcoin&m=Keyword&rpp=15&mp=0&bd=false&rs=false"
)

# Grab all URLs on the page
urls <- marketwatch_bitcoin_articles %>%
  html_nodes("div.searchresult a") %>% #See HTML source code for data within this tag
  html_attr("href")

urls

# Grab all datetimes on the page
datetime <- marketwatch_bitcoin_articles %>%
  html_nodes("div.deemphasized span") %>% #See HTML source code for data within this tag
  html_text()

datetime

# Filter datetimes that do not follow a consistent format
datetime2 <- c()
for(i in datetime){
  correct_datetime <- grep("Today", i, invert=T, value=T)
  datetime2 <- append(datetime2, correct_datetime)
}

datetime <- datetime2

datetime

# Convert datetime text to a standard time format
#install.packages("lubridate") #Uncomment this to install this package
library(lubridate)

# First remove periods from datetime, as lubridate 
# cannot interpret a.m. and p.m. with periods
datetime_clean <- gsub("\\.","",datetime)

datetime_parse <- parse_date_time(
  datetime_clean, "%I:%M %p %m/%d/%Y"
)
datetime_parse

# Convert all ET (Eastern Time) datetime values to 
# your local time - e.g. PT (Pacific Time)
datetime_convert <- ymd_hms(
  datetime_parse, tz = "US/Eastern"
)
datetime_convert <- with_tz(
  datetime_convert, "US/Pacific"
)
datetime_convert

# Create a dataframe containing the urls of the web 
# pages and their converted datetimes
marketwatch_webpgs_datetimes <- data.frame(
  WebPg=urls, DateTime=datetime_convert
)
dim(marketwatch_webpgs_datetimes)

# Take the difference between the your current time
# and the published datetime of the web pg and add 
# as a column to the dataframe
diff_in_hours <- difftime(
  Sys.time(), marketwatch_webpgs_datetimes$DateTime, units = "hours"
)
diff_in_hours
diff_in_hours <- as.double(diff_in_hours)
diff_in_hours
marketwatch_webpgs_datetimes$DiffHours <- diff_in_hours
head(marketwatch_webpgs_datetimes)

# Filter rows of the dataframe that contain 
# DiffHours of less than an hour
marketwatch_latest_data <- subset(
  marketwatch_webpgs_datetimes, DiffHours < 1
)
marketwatch_latest_data

# Loop through web pg URLs, read and grab the title 
# and body text, and store in a dataframe to get 
# the data ready for analysis
titles <- c()
bodies <- c()
for(i in marketwatch_latest_data$WebPg){
  
  marketwatch_latest_wbpg <- read_html(i)
  title <- marketwatch_latest_wbpg %>%
    html_node("title") %>%
    html_text()
  titles <- append(titles, title)
  
  marketwatch_latest_wbpg <- read_html(i)
  body <- marketwatch_latest_wbpg %>%
    html_nodes("p") %>%
    html_text()
  one_body <- paste(body, collapse=" ")
  bodies <- append(bodies, one_body)
  
}

marketwatch_latest_data$Title <- titles
marketwatch_latest_data$Body <- bodies

names(marketwatch_latest_data)
marketwatch_latest_data$Title
marketwatch_latest_data$Body[1]


# Summarize the body of the text to extract the most 
# relevant, key info

# Note: There are other ways to analyze the text:
# Learn text analytics/natural language processing 
# and important machine learning concepts: 
# https://datasciencedojo.com/bootcamp/curriculum/ 

# Before summarizing the text, we need to clean it 
# of uneccessary whitespace, new lines, etc 
#install.packages("stringr") #Uncomment this to install this package
library(stringr)
clean_text_bodies <- str_squish(
  marketwatch_latest_data$Body
  )
clean_text_bodies[1]

# Loop through each body text and grab the top 3 
# sentences with the most relevant information
#install.packages("LSAfun") #Uncomment this to install this package
library(LSAfun)
summary <- c()
for(i in clean_text_bodies){
  top_info <- genericSummary(i,k=3);
  one_summary <- paste(top_info, collapse=" ")
  summary <- append(summary, one_summary)
}

summary

marketwatch_latest_data$Summary <- summary

# Email the results of the summaries, along with 
# the titles
#install.packages("sendmailR") #Uncomment this to install this package
library(sendmailR)

marketwatch_title_summary <- c()
for(i in 1:length(marketwatch_latest_data$Summary)){
  marketwatch_title_summary <- append(marketwatch_title_summary, marketwatch_latest_data$Title[i])
  marketwatch_title_summary <- append(marketwatch_title_summary, marketwatch_latest_data$Summary[i])
}

marketwatch_title_summary

from <- "<rebecca.merrett@gmail.com>"
to <- "<rebecca.merrett@gmail.com>"
subject <- "Hourly Summary of Bitcoin Events"
body <- marketwatch_title_summary             
mailControl <- list(smtpServer="ASPMX.L.GOOGLE.COM") #Use Google for Gmail accounts

sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)

#Schedule this script to run every hour
