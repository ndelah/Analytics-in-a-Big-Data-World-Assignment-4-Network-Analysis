library(plotly)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(streamgraph)
library(umap)

par(ask=F)

df_posts_with_topics <- read.csv('../../data/three_main_subreddit_posts_with_topic_and_sentiment.csv')
df_posts_with_topics$date_of_publish <- as.Date(df_posts_with_topics$date, format="%Y-%m-%d")

FILTER_SUBREDDIT <- 'coronavirus'
DO_FILTER = F
FILTER_SENTIMENT = T

if (DO_FILTER) {
  df_posts_filter <- df_posts_with_topics %>%
    filter(subreddit == FILTER_SUBREDDIT)
} else {
  df_posts_filter <- df_posts_with_topics
}

df_posts_filter_neg <- df_posts_filter %>%
  filter(avg_sentiment < -0.3)

df_posts_filter_pos <- df_posts_filter %>%
  filter(avg_sentiment > 0.3)

# Plot
df_posts_filter %>%
  filter(score < 200) %>%
  filter(score > 50) %>%
  
  #mutate(df_posts_with_topics = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=topic, y=score, fill=topic, color=topic)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

df_posts_filter %>%
  ggplot( aes(x=topic, y=upvote_ratio, fill=topic)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  xlab("")

# what are very popular posts saying in terms of sentiment across topic
df_posts_filter %>%
  filter(score > 10) %>%
  filter(upvote_ratio < 0.6) %>%
  ggplot( aes(x=topic, y=avg_sentiment, fill=topic)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  xlab("")


df_trending = df_posts_filter_neg # choose df_posts_filter, or df_posts_filter_neg or df_posts_filter_pos


df_trend_by_day  <- df_trending %>%
  group_by(topic, date_of_publish) %>%
  summarize(mean_sentiment = mean(avg_sentiment, na.rm = T),
            mean_score = mean(score, na.rm = T),
            num_topics = dplyr::n()) 
  
tmp <- df_trend_by_day %>%
  mutate(topic2=topic)
tmp %>%
  ggplot( aes(x=date_of_publish, y=num_topics)) +
  geom_line( data=tmp %>% dplyr::select(-topic), aes(group=topic2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=topic), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A topic sentiment") +
  facet_wrap(~topic)


tmp %>%
  ggplot( aes(x=date_of_publish, y=mean_sentiment)) +
  geom_line( data=tmp %>% dplyr::select(-topic), aes(group=topic2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=topic), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A topic sentiment") +
  facet_wrap(~topic)


