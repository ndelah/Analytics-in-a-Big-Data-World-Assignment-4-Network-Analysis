library(ggplot2)
topics = list(
"Medical Research, Vaccine and Drugs" = c("journal", "findings","drug", "vaccine", "trial", "clinical", "antibodies", "fda","research","kit", "protein", "treatment", "epidemiological","study", "experimental"),
"Supply Chain"= c("amazon", "store", "grocery", "mask", "toilet", "sanitizer","hand","panic","sales","hoarding","price","supply", "supermarket"),
"Geo-Politics"= c("trump", "administration","cdc","briefing","pompeo","secretary","response"),
"Communist Dogs"= c("china", "chinese", "conspiracy","wuhan","xi", "theory", "wet", "ccp", "communist","propaganda","bat","markets","disinformation"),
"Medical Supplies" = c("ventilators", "respirators","protective","equipment","medical","beds","gear","surgical","n95","3m","gowns","shortages","doctors","nhs"),
"Economics" = c("dow", "stimulus","package","economic","financial","jobs","recession","bonds","oil","industry","bank","unemployment","plunge","futures","losses","income","impact","relief","wall street","stock market"),
"Olympics" = c("games", "olympics","marathon","tokyo","japan"),
"Events and Gatherings"= c("cancelled", "postponed", "canceled","events","gatherings","postpone","concerns","cancel", "flights", "travel", "borders"),
"Travel and Lockdown"= c("school", "borders", "foreigners", "quarantine", "lockdown", "nationals", "entry","returning","border","suspend","visa","tourist", "stranded", "restriction"),
"Social Distancing"= c("police", "riots", "protest","stay home", "home","gatherings","beaches","violating", "rules","fines","parks","distancing","social","indoors","order"),
"News and Updates"= c("death toll", "deaths", "hospitalization","new case","recovery", "recovered", "update", "reports","confirmed","rising","population"),
"Drugs"= c("remdesivir", "hydroxychloroquine", "malaria", "gilead", "vitamins")
)

top3_topics = list(
  "Medical Research, Vaccine and Drugs" = c("journal", "findings","drug", "vaccine", "trial", "clinical", "antibodies", "fda","research","kit", "protein", "treatment", "epidemiological","study", "experimental", "remdesivir", "hydroxychloroquine", "malaria", "gilead", "vitamins"),
  "Geo-Politics"= c("trump", "administration","cdc","briefing","pompeo","secretary","response","china", "chinese", "conspiracy","wuhan","xi", "wet", "ccp", "communist","propaganda","bat","markets","disinformation"),
  "Economics" = c("dow", "stimulus","package","economic","financial","jobs","recession","bonds","oil","industry","bank","unemployment","plunge","futures","losses","income","impact","relief","wall street","stock market"),
  "Medical Supplies" = c("ventilators", "respirators","protective","equipment","medical","beds","gear","surgical","n95","3m","gowns","shortages","doctors","nhs"),
  "Supply Chain"= c("amazon", "store", "grocery", "mask", "toilet", "sanitizer","hand","panic","sales","hoarding","price","supply", "supermarket")
  
)

query_half = 'MATCH (u:User)-[e2]-(p:Post)-[e1]-(s:Subreddit{display_name:oneoftheother19subreddits})
WHERE u.username in //Add Massive list of usernames here and
 toLower(p.title) =~ '

query_other_half = ' \nRETURN distinct u.id, u.username, u.link_karma, u.comment_karma , s.id, s.display_name,p.id, p.created_utc_str, p.score, p.upvote_ratio ,p.title
ORDER BY p.score DESC
LIMIT 10000'


subquery = '.*coronavirus.*'

for (topic in top3_topics) {
  for (word in topic) {
        subquery = paste0(subquery,'|.*',word,'.*')
      }
}

full_query = paste0(query_half, '\"', subquery, '\"', query_other_half)

