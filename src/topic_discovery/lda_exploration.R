# read in some stopwords:
library(tm)
library(sentimentr)
library(readr)
library(readxl)
library(lda)
library(LDAvis)

df_posts <- read_csv("../../data/poststhreemainsubreddits_uncompleteshortlist_of_users.csv")

stop_words <- stopwords("SMART")

# pre-processing:
df_posts$title <- gsub("'", "", df_posts$title)  # remove apostrophes
df_posts$title <- gsub("[[:punct:]]", " ", df_posts$title)  # replace punctuation with space
df_posts$title <- gsub("[[:cntrl:]]", " ", df_posts$title)  # replace control characters with space
df_posts$title <- gsub("^[[:space:]]+", "", df_posts$title) # remove whitespace at beginning of documents
df_posts$title <- gsub("[[:space:]]+$", "", df_posts$title) # remove whitespace at end of documents
df_posts$title <- tolower(df_posts$title)  # force to lowercase


df_sentences <- get_sentences(df_posts$title)
df_posts_sentiment <- sentiment_by(df_sentences, averaging.function = sentimentr::average_weighted_mixed_sentiment)

df_posts$avg_sentiment <- df_posts_sentiment$ave_sentiment
df_posts$sd_sentiment <- df_posts_sentiment$sd_sentiment


# tokenize on space and output as a list:
doc.list <- strsplit(df_posts$title, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  

# MCMC and model tuning parameters:
K <- 25
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:


set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

## NB SAVE FOR LATER USE
#saveRDS(object = fit, file = "FittedLDAGibbsSampler_SUBSET.RDS")
#fit <- readRDS("FittedLDAGibbsSampler_FINAL.RDS")
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

LDAResults <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = LDAResults$phi, 
                   theta = LDAResults$theta, 
                   doc.length = LDAResults$doc.length, 
                   vocab = LDAResults$vocab, 
                   term.frequency = LDAResults$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)



# Further testing
set.seed(357)
t1 <- Sys.time()


# use lengths function for list poes cunt 

df_posts$docs <- documents
df_posts_non_zero <- df_posts[lapply(df_posts$docs,length)>0, ]

#documents <- documents[lapply(documents,length)>0]

# params <- sample(c(-1, 1), K, replace=TRUE)
# 
# fit <- slda.em(documents = df_posts_non_zero$docs, K = K, vocab = vocab, 
#                                    num.e.iterations = G, alpha = alpha, params=params,
#                                    eta = eta, variance = 0.25,lambda = 1.0,logistic=FALSE,
#                                     annotations = df_posts_non_zero$upvote_ratio, method="sLDA")
# 
# theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
# phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
# 
# LDAResults <- list(phi = phi,
#                    theta = theta,
#                    doc.length = doc.length,
#                    vocab = vocab,
#                    term.frequency = term.frequency)
# 
# 
# # create the JSON object to feed the visualization:
# json <- createJSON(phi = LDAResults$phi, 
#                    theta = LDAResults$theta, 
#                    doc.length = LDAResults$doc.length, 
#                    vocab = LDAResults$vocab, 
#                    term.frequency = LDAResults$term.frequency)


#serVis(json, out.dir = 'vis', open.browser = TRUE)


topics = c("Princess Diamond Cruise", "Random Topic 1", "Medical Research and Vaccine", "Statistics Reporting 1",
           "US and World News", "Statistics Reporting 5", "Nursing Homes and Hospitals", "American Politics","Olympics 2020","Travel Restrictions",
           "StayHome and Social Distancing","Medical Equipment and Supply","Statistics Reporting 2","Consumer Supply Chain","Statistics Reporting 3",
           "Statistics Reporting 4", "Schools and City Closure", "Economic Impact", "Chinese Geo-Politics", "Asia Reporting")


topics_new = c("Chinese Geo-Politics", "Statistics Reporting 1", "American Politics and Response 1", "Princess Diamond Cruise", 
           "Nursing Homes and Hospitals", "Statistics Reporting 2", "American Politics and Response 2",  "American Politics and Response 3", 
           "Statistics Reporting 3", "Olympics 2020 and Events", "Consumer Supply Chain", "Medical Equipment and Supply", "Economic Impact 1","Medical Research and Vaccine",
           "World News 1", "Random Topic 1", "Economic Impact 2", "World News 2", "StayHome and Social Distancing 2", "Travel Restrictions")

topics_new = c("Nursing Homes and Hospitals", "American Politics and News 1", "Medical Equipment and Supply",
               "Statistics Reporting 1", "World News 1", "American Politics and News 2", "Medical Research and Vaccine 1",
               "Medical Research and Vaccine 2", "China Geopolitics 1", "World News 2", "Olympics 2020 and Events",
               "Travel Restrictions", "Statistics Reporting 2", "Random Topic 1", "American Politics and News 3",
               "World News 3", "Statistics Reporting 3", "StayHome and Social Distancing 1", "Princess Diamond Cruise",
               "Economic Impact 1", "World News 4", "Economic Impact 2", "China Geopolitics 2", "StayHome and Social Distancing 2", "Consumer Supply Chain")

topics_new_main_subreddits = c("China Geopolitics 1", "American Politics and News 1", "Statistics Reporting 1", "Medical Research and Vaccine 1", "Princess Diamond Cruise",
                               "Statistics Reporting 2", "Statistics Reporting 3", "Medical Research and Vaccine 2", "Economic Impact", "StayHome and Social Distancing 1",
                               "Olympics 2020 and Events", "StayHome and Social Distancing 2", "American Politics and News 2", "Travel Restrictions", "American Politics and News 3",
                               "Statistics Reporting 4", "Consumer Supply Chain", "World News 1", "Nursing Homes and Hospitals", "Statistics Reporting 5", "Medical Equipment and Supply",
                               "World News 2", "StayHome and Social Distancing 3", "China Geopolitics 2", "Medical Research and Vaccine 3")


predicted.docsums <- slda.predict.docsums(documents,
                                          fit$topics, 
                                        alpha = 0.02,
                                          eta=0.02)
predicted.docsums <- data.frame(t(predicted.docsums))

predicted.proportions <- round(predicted.docsums / rowSums(predicted.docsums), digits = 2) * 100

top.words <- top.topic.words(fit$topics, 30, by.score=TRUE)

top.words <- structure(data.frame(top.words),
                       names = topics_new_main_subreddits)
topics_data = structure(data.frame(predicted.proportions), 
                 names = topics_new_main_subreddits)



topics_data <- topics_data %>%
  mutate(`Statistics Reporting` = pmax(`Statistics Reporting 1`, `Statistics Reporting 2`,`Statistics Reporting 3`, `Statistics Reporting 4`, `Statistics Reporting 5`),
         `World News` = pmax(`World News 1`, `World News 2`),
         `American Politics and News` = pmax(`American Politics and News 1`, `American Politics and News 2`, `American Politics and News 3`),
         `StayHome and Social Distancing` = pmax(`StayHome and Social Distancing 1`, `StayHome and Social Distancing 2`, `StayHome and Social Distancing 3`),
         `China Geopolitics` = pmax(`China Geopolitics 1`, `China Geopolitics 2`),
         `Medical Research and Vaccine` = pmax(`Medical Research and Vaccine 1`, `Medical Research and Vaccine 2`, `Medical Research and Vaccine 3`))


topics_data <- topics_data[, !(colnames(topics_data) %in% c("Statistics Reporting 1", "Statistics Reporting 2",
                                                            "Statistics Reporting 3", "World News 1", 
                                                            "World News 2", "Statistics Reporting 4", "Statistics Reporting 5",
                                                            "China Geopolitics 1",
                                                            "China Geopolitics 2", "American Politics and News 1",
                                                            "American Politics and News 2", "American Politics and News 3",
                                                            "Medical Research and Vaccine 3", "StayHome and Social Distancing 3",
                                                            "StayHome and Social Distancing 1", "StayHome and Social Distancing 2",
                                                            "Medical Research and Vaccine 1", "Medical Research and Vaccine 2"))]



df_posts_with_topics <- cbind(df_posts, topics_data)
df_posts_with_topics$X1 <- rownames(df_posts_with_topics)
df_posts_with_topics <- df_posts_with_topics[, !(colnames(df_posts_with_topics) %in% c("docs"))]

num_cols_df_posts = length(colnames(df_posts))
num_reduced_topics = length(colnames(topics_data))
df_posts_with_topics = reshape2::melt(df_posts_with_topics, 
                                      measure.vars = 11:23,
                                      variable.name = "topic", 
                                      value.name = "topic_score")

#d <- data.table(df_posts_with_topics, key="X1")

df_posts_final = df_posts_with_topics %>% 
  arrange(desc(topic_score)) %>% 
  group_by(X1) %>% slice(1)
write_csv(df_posts_final, "three_main_subreddit_posts_with_topic_and_sentiment.csv")

#df_posts_final = reshape2::dcast(df_posts_final, X1~topic, value.var="topic_score")


## FOR MODELLING

topics_data[topics_data<45]=0
topics_data[topics_data>=45]=1

topics_data <- topics_data %>%
  mutate(`Statistics Reporting` = ifelse(`Statistics Reporting 1` == 1 | `Statistics Reporting 2` == 1 |
                                            `Statistics Reporting 3` == 1 | `Statistics Reporting 4` == 1 | `Statistics Reporting 5` == 1, 1, 0))


topics_data <- topics_data %>%
  mutate(`GeoPolitics` = ifelse(`Chinese Geo-Politics` == 1| `American Politics` == 1, 1, 0))
topics_data <- topics_data[, !(colnames(topics_data) %in% c("Statistics Reporting 1", "Statistics Reporting 2",
                                               "Statistics Reporting 4", "Statistics Reporting 3", 
                                               "Statistics Reporting 5", "American Politics",
                                               "Chinese Geo-Politics", "Random Topic 1"))]

df_posts_with_topics <- cbind(df_posts, topics_data)
df_posts_with_topics <- df_posts_with_topics[, !(colnames(df_posts_with_topics) %in% c("docs"))]
write.csv2(df_posts_with_topics, file="reddit_posts_with_topic_and_sentiment.csv")
