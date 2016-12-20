# Trump Tweet Data
# Matches the example used by David Robinson's Variance
# Explained article on Trump and our own case study on
# Trump's tweets.
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
trumpTweets <- trump_tweets_df
cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}
trumpTweets$statusSource <- cleanFun(trumpTweets$statusSource)
Encoding(trumpTweets$text) <- "unknown"
trumpTweets
