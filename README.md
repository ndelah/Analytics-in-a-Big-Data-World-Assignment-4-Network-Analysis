# Assignment 4: Network Analysis
The fourth assignment for Advanced Analytics in Big Data. Here we analyze a large social graph from Reddit using Neo4j and Gephi as a graph visualization tool.


## Data
Our task for this assignment was to use Neo4j and Gephi to perform an analysis on a dataset from a social graph extracted from the social aggregator platform reddit (www.reddit.com), relating to the COVID-19 outbreak. Around mid-January, redditors started to discuss the virus, in particular on the subreddits “/r/china_flu”, “/r/coronavirus” and (later) “/r/COVID19”. All these subreddits were set up and moderated by a common group of users.
The data was extracted as follows:
- For the three subreddits mentioned above, all submissions starting from the 1st of January 2020 onwards were gathered (the actual first submission appears later in January)
- For all submissions, all comments made by users were extracted
- For all users who commented or submitted, we also extract comments and submissions they have made to other subreddits
- The extraction has been running up until about the 20th of April

For this assignment, we were basically free to explore anything we deemed interesting, and present our findings in a report. The main goal was to get familiar with Neo4j and Gephi, but also to hone our "storytelling" skills.

## Report Summary
People need tools that process news on two levels. On one hand, the volume of news makes it impossible to follow everything. It is difficult to compare how important two pieces of news are to the people's perspective. So we need to be able to track how a certain subject evolves. Does it stay relevant, does it grow faster or does it die off? On the other hand, we need tools that summarize groups of news under topics.

To this end, in this assignment:
- We perform a general exploratory analysis of Reddit to understand how it works.
- We analyse the origins of the reposts on the platform.
- We We perform LDA to extract the recurring themes from the corona pandemic.
- We Finally we use the insights gathered in those preliminary analyses to query the graph containing that data.
