library(dplyr)
library(maptpx)
library(tm)
library(topicmodels)
library(lda)
library(stringi)
library(LDAvis)

#This code fits all the lda models and visualizes them, the necessary results are also saved. The model_time_point will eventually become part of the R package

load("./rdatafiles/aprilTweetsTest.RData") #Kinda slow
#english.tweets.df$created_at = as.POSIXct(english.tweets.df$created_at) #SLOW :<
#save(english.tweets.df, file = "aprilTweetsTest.RData")

english.tweets.df$created_at = as.POSIXct(english.tweets.df$created_at)

step = 1.5
#time = as.POSIXct('2015-04-25 6:11', tz = "GMT")
time = as.POSIXct('2015-04-24 18:11', tz = "GMT")
steps = step*60*60*0:7

times = time + steps

K = topic_k = sapply(times, model_time_point, difference = step)



#Function to fit everything starting from a time point with given step length

model_time_point = function(start_time, difference) {
  
  text = filter(english.tweets.df, created_at >= start_time & created_at <= start_time+(difference*60*60))$text
  
  corp = Corpus(VectorSource(text))
  dtm = DocumentTermMatrix(corp)
  
  dtm = removeSparseTerms(dtm, 0.999)
  dtm.dense = as.matrix(dtm)
  doc.length = rowSums(dtm.dense)
  dtm = dtm[doc.length > 0,]
  testmap = maptpx::topics(dtm, K = c(5:55), kill = 3)
  
  BF = testmap$BF
  save(BF, file = paste("./maptpx_BF/", start_time, "BF"))
  bestmap = maptpx::topics(dtm, K = testmap$K)
  
  vocab = colnames(dtm)
  doc.length = doc.length[which(doc.length != 0)]
  term.frequency = colSums(dtm.dense)
  term.frequency = term.frequency[which(term.frequency != 0)]
  theta = bestmap$omega
  phi = t(bestmap$theta)
  save(theta, file = paste("./maptpx_theta/", start_time, "maptpx", testmap$K, "topic model theta", sep = " "))
  
  json <- createJSON(phi = phi,
                     theta = theta, 
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
  serVis(json, out.dir = paste("./maptpx_vis/", start_time, "maptpx", testmap$K, "topic model", sep = " "),
         open.browser = FALSE)
  
  return(testmap$K)
}
