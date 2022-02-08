# Load Packages and Twitter Library
library(rtweet)
## plotting and pipes - tidyverse!
library(tidyverse)
## text mining library
library(tidytext)

## PII Library
# library(dotenv)

# API Setup: Set up Twitter API and Ingest API Keys (*omitted*)
## App Name
# appname <- "RSentimentEDA"

## Create Token named "twitter_token"
# twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret,
#   access_token = access_token,
#   access_secret = access_secret)

# Preliminary Research
## Obtain friends of a twitter account
fds <- get_friends("drewbyts")
fds

## Search for morningbrew twitter posts containing multiple keywords
rt <- search_tweets(q = "morningbrew AND web3", n = 15)

## Obtain liked tweets of a twitter account
mbrew <- get_favorites("MorningBrew", n = 30)

# Primary Research: Exploratory Data Analysis (EDA)
## Hypothesis - MBs account generated most of its reaction when paired Web3, Crypto, in a video form.
## Methodology - MorningBrew Tweets from 2021-09-26 to 2022-01-20 

### Obtain max number of tweets from @morningbrew (09-26-2021 to 01-20-2022)
morningbrew <- get_timeline("@MorningBrew", n= 3200)
glimpse(morningbrew)

### Transform dataset 
mb_no_quotes_rt <- morningbrew %>% filter(is_quote == FALSE & is_retweet==FALSE)

### Create An "Organic" Data set
mb_organic <- mb_no_quotes_rt

### Create a "Pure Organic" Data set (removes quote tweets, retweets, and replies to other users)
mb_organic_organic <- mb_organic[is.na(mb_organic$reply_to_user_id),]

## EDA
### Analyze "Blow up" Tweets (tweets greater than avg of 399 favorites) - 327 values
mb_blow_up <- mb_organic_organic %>% filter(favorite_count > 399)

### Identify "Blow Up" Tweets with a URL - 2 blow up tweets have a URL
url <- mb_blow_up[!is.na(mb_blow_up$urls_url), ]

### Identify "Blow Up" Tweets with a Photo - 222 tweets have a photo
photo <- mb_blow_up[!is.na(mb_blow_up$media_type), ]

### Create a data set of "Blow Up" tweets with no media types - 105 tweets have neither
none <- mb_blow_up[is.na(mb_blow_up$media_type), ]

## Exploratory Visualization
#* Create Time Series Plot
ggplot(data = none, aes(x = created_at, y = favorite_count))+
  geom_bar(stat="identity", color = "#00AFBB")

#* Finding 1: 224 "Blow Up" tweets have both a photo and a URL

#* Finding 2: 105 "Blow Up" tweets had no photo and or URL


# Secondary Research: Machine Learning
## Null Hypothesis: A linear relationship does NOT exist between favorites and retweets 
## Alternative Hypothesis: A linear relationship exists between favorites and retweets

## Linear Regression - Dependent Variable (Y) is retweets and Independent Variable (x) is favorites
scatter.smooth(x=mb_organic_organic$favorite_count, y=mb_organic_organic$retweet_count, main = "Retweet ~ Favorite")

## Correlation - Suggests Level of Linear Dependence Between Two Variables
cor(mb_organic_organic$favorite_count, mb_organic_organic$retweet_count)

## LinearMod - Retweets as a Function of Favorites - Equation of y = 0.1603x - 18.69
linearMod <- lm(retweet_count ~ favorite_count, data = mb_organic_organic)
print(linearMod)

## Check for Statistical Significance - Stat. Significance if p values are less than 0.05
summary(linearMod)

##* Takeaway - We reject the null hypothesis (i.e a linear relationship exists)


# Extra:
## Post a tweet from R Studio (/SUCCESS/, see @drewbyts tweets from Jan. 16)
### post_tweet("If this works, it means I succesfully accessed Twitter's API through R studio. *thanos gif*.")