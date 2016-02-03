library(dplyr)

#This code simply runs the necessary functions from my R package (parseTweetFiles repository)

stoplist = scan("~/Documents/honors_project/stop_list", what = "character")
process_files("~/Documents/honors_project/raw_data/april", "~/Documents/honors_project/edited_data/april", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/may", "~/Documents/honors_project/edited_data/may", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/june", "~/Documents/honors_project/edited_data/june", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/jan", "~/Documents/honors_project/edited_data/jan", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/feb", "~/Documents/honors_project/edited_data/feb", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/feb1", "~/Documents/honors_project/edited_data/feb1", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

process_files("~/Documents/honors_project/raw_data/feb2", "~/Documents/honors_project/edited_data/feb2", 
              vars = c("text", "lang", "time_zone", "created_at"), stoplist = stoplist)

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/april")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "aprilTweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/may1")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "may1Tweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/may2")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "may2Tweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/may3")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "may3Tweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/jan")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "janTweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/feb1")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "feb1Tweets.RData")

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/feb2")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "feb2Tweets.RData")


process_files("~/Documents/honors_project/raw_data/", "~/Documents/honors_project/edited_data/stat421", 
              vars = c("text", "lang", "time_zone", "created_at", "friends_count", "followers_count", 
                       "favourites_count", "retweet_count", "truncated", "geo_enabled", "retweeted",
                       "url"), stoplist = stoplist)

tweets.df = make_tweet_df("~/Documents/honors_project/edited_data/stat421")
english.tweets.df = filter(tweets.df, lang == "en")
save(english.tweets.df, file = "stat421Tweets.RData")
