# Data Science Dojo Meetup: Automated Web Scraping in R

# Let's start scraping the main head and paragraph text/comments of a single Reddit page
#install.packages("rvest")
library(rvest)

reddit_wbpg <- read_html("https://www.reddit.com/r/politics/comments/a1j9xs/partisan_election_officials_are_inherently_unfair/")

reddit_wbpg %>%
  html_node("title") %>%
  html_text()  

reddit_wbpg %>%
  html_nodes("p.s90z9tc-10") %>%
  html_text()

# Let's scrape the time and URL of all latest pages published on Reddit's r/politics
reddit_political_news <- read_html("https://www.reddit.com/r/politics/new/")

time <- reddit_political_news %>%
  html_nodes("a._3jOxDPIQ0KaOWpzvSQo-1s") %>% 
  html_text()

time

urls <- reddit_political_news %>%
  html_nodes("a._3jOxDPIQ0KaOWpzvSQo-1s") %>%
  html_attr("href")

urls

# Create a dataframe containing the URLs of the Reddit news pages and their published times
reddit_newspgs_times <- data.frame(NewsPage=urls, PublishedTime=time)
#Check the dimensions 
dim(reddit_newspgs_times)

# Filter dataframe by rows that contain a time published in minutes (i.e. within the hour)
reddit_recent_data <- reddit_newspgs_times[grep("minute|now", reddit_newspgs_times$PublishedTime),]
#Check the dimensions (# items will be less if not all pages were published within mins)
dim(reddit_recent_data)

# Loop through urls, grab the main head and paragraph text of comments, 
# store in their own vectors, and create a dataframe to get it ready for analysis/modeling

titles <- c()
comments <- c()
for(i in reddit_recent_data$NewsPage){ 
  
  reddit_recent_data <- read_html(i)
  body <- reddit_recent_data %>%
    html_nodes("p.s90z9tc-10") %>%
    html_text()
  comments = append(comments, body)
  
  reddit_recent_data <- read_html(i)
  title <- reddit_recent_data %>%
    html_node("title") %>%
    html_text()
  titles = append(titles, rep(title,each=length(body)))
  
}

reddit_hourly_data <- data.frame(Headline=titles, Comments=comments)
dim(reddit_hourly_data)
head(reddit_hourly_data$Comments)

# Remove disclaimer comments included in all pages so this doesn't flood the comments and skew results
disclaimers <- c(
  "As a reminder, this subreddit is for civil discussion.",
  "In general, be courteous to others. Attack ideas, not users. Personal insults, shill or troll accusations, hate speech, any advocating or wishing death/physical harm, and other rule violations can result in a permanent ban.",
  "If you see comments in violation of our rules, please report them.",
  "I am a bot, and this action was performed automatically. Please contact the moderators of this subreddit if you have any questions or concerns."
)

reddit_hourly_data_no_disclaimers <- subset(
  reddit_hourly_data, !(Comments %in% c(disclaimers))
)

dim(reddit_hourly_data_no_disclaimers)
head(reddit_hourly_data_no_disclaimers$Comments)

# Score the overall sentiment of each comment
# This library scores sentiment by taking into account the whole sentence
# It takes into account surrounding words of a target word such as 'not happy'
# which cancels out positive sentiment
# A negative value means sentiment is more negative than positive
# A positive values means the sentiment is more positive than negative
#install.packages('sentimentr')
library(sentimentr)

# Comment out this line so it does not cause errors when scheduling to run the script
#sentiment(reddit_hourly_data_no_disclaimers$Comments)

# Treat comments as characters, not factors
# Convert to a format sentiment() function accepts
reddit_hourly_data_no_disclaimers$Comments <- as.character(reddit_hourly_data_no_disclaimers$Comments)

sentiment_scores <- sentiment(reddit_hourly_data_no_disclaimers$Comments)
head(sentiment_scores)

# Average the scores across all comments
average_sentiment_score <- sum(sentiment_scores$sentiment)/length(sentiment_scores$sentiment)
average_sentiment_score

# Email the results of the analysis
#install.packages("sendmailR")
library(sendmailR)
from <- "<rebecca.merrett@gmail.com>"
to <- "<rebecca.merrett@gmail.com>"
subject <- "Hourly Sentiment Score on Current US Political Situation"
body <- c("On a scale of 1 to -1 people feel: ", average_sentiment_score)            
mailControl <- list(smtpServer="ASPMX.L.GOOGLE.COM") #Use Google for Gmail accounts

sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)

# Schedule this script to run every hour to keep track of the overall sentiment 
# Idea to take this further: Instead of emailing the hourly results, 
# store the average sentiment score in a table every hour to plot it 
# over time or see how changes over time