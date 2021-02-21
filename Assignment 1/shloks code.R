library(dplyr)
rm(list = ls())
set.seed(29677068) 
webforum <- read.csv("webforum.csv")
webforum <- webforum [sample(nrow(webforum), 20000), ]

webforum <- webforum %>% mutate(Date = as.Date(Date,format = "%Y-%m-%d"))
webforum <- webforum %>% mutate(Month = as.numeric(format(Date,"%m")))
webforum <- webforum %>% mutate(Year = as.numeric(format(Date,"%Y")))

webforum <- webforum %>% mutate(Month = as.numeric(format(Date,"%m")))
webforum <- webforum %>% mutate(Year = as.numeric(format(Date,"%Y")))
webforum <- webforum %>% filter(WC>0)
webforum <- webforum %>% filter(AuthorID>0)

dec_2005 <- webforum %>% filter(Year==2005) %>% filter(Month==12) %>% select(1,2) %>% arrange(ThreadID)

popular_authors <- dec_2005 %>% group_by(AuthorID) %>% summarise(posts = n()) %>% arrange(desc(posts))
top_10_authors <- popular_authors[1:10,]

top1_author = top_10_authors$AuthorID[1]
top2_author = top_10_authors$AuthorID[2]
top3_author = top_10_authors$AuthorID[3]
top4_author = top_10_authors$AuthorID[4]
top5_author = top_10_authors$AuthorID[5]
top6_author = top_10_authors$AuthorID[6]
top7_author = top_10_authors$AuthorID[7]
top8_author = top_10_authors$AuthorID[8]
top9_author = top_10_authors$AuthorID[9]
top10_author = top_10_authors$AuthorID[10]

dec_2005 <- dec_2005 %>% filter(AuthorID == top1_author|AuthorID == top2_author|AuthorID == top3_author|AuthorID == top4_author|AuthorID == top5_author|AuthorID == top6_author|AuthorID == top7_author|AuthorID == top8_author|AuthorID == top9_author|AuthorID == top10_author)

dec_2005 <- dec_2005 %>% group_by(ThreadID) %>% mutate(x = list(AuthorID)) %>% arrange(AuthorID)
