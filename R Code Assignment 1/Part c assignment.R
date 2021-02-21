##Question c) Can you define, graph and describe the social network that exists at a particular point in
#time, for example over one month? How does this change in the following mo nths?
library(zoo)
attach(webforum)
library(plyr)
install.packages("zoo")
install.packages("plyr")
NetThr = webforum[!(webforum$AuthorID==-1),]
NetThr = NetThr[!(NetThr$WC==0),]
NetThr$Date = as.Date(NetThr$Date, "%d/%m/%Y")
NetThr$Date = as.yearmon(NetThr$Date, "%Y-%m")

#Most Aactive Month (Dec 2005)

NetThread2 =aggregate(NetThr[5:29], NetThr[3], mean)
fqcNetwork = as.data.frame(count(NetThr, "Date"))
View(fqcNetwork)
fqcNetwork[which.max(fqcNetwork$freq),] #Month Dec 2005 is the most active 
maxNetThr = subset(NetThr, NetThr$Date=="Dec 2005")
AuthNetworkFreq= data.frame(count(maxNetThr, c("AuthorID")))
TopEightAuthNetwork = as.data.frame(head(AuthNetworkFreq[order(-AuthNetworkFreq[2]),],8))
TopEightAuthNetworkinfo = subset(maxNetThr,AuthorID%in%TopEightAuthNetwork$AuthorID)
AdjacencyTable = TopEightAuthNetworkinfo[, -c(4:29)]
AdjacencyTable = AdjacencyTable[, -c(3)]
View(AdjacencyTable)                   
library(igraph)
library(igraphdata)
data(karate)
View(karate)
graph	<- graph.data.frame(AdjacencyTable,	directed=FALSE)
get.adjacency(graph)
plot(graph, vertex.colour = "red")
gg = graph_from_literal(76174 : 34292	: 64019 : 81721 : 81525 : 41237 : 54582 : 79878 -- 76174: 34292: 64019	: 81721 : 41237 : 54582 : 79878)
plot(gg)

#Next Month (Jan 2006)
NextSecNetThr = subset(NetThr, NetThr$Date=="Jan 2006")
Auth2Freq= data.frame(count(NextSecNetThr, c("AuthorID")))
TopEight2AthNet = as.data.frame(head(Auth2Freq[order(-Auth2Freq[2]),],8))
TopEight2Info = subset(NextSecNetThr,AuthorID%in%TopEight2AthNet$AuthorID)
AdjacencyTable2 = TopEight2Info[, -c(4:29)]
AdjacencyTable2 = AdjacencyTable2[, -c(3)]
View(AdjacencyTable2)   
cc = graph.formula(34292-39170, 34292-53655, 34292-83270, 34292-83344, 34292-54960, 34292-47875, 34292-83488, 39170-34292, 39170-53655, 39170-83270, 39170-83344, 47875-53655, 47875-34292, 47875-83344, 47875-83488, 53655-34292, 53655-39170, 53655-83270, 53655-83344, 53655-47875, 53655-83488, 54960-34292, 54960-83344, 54960-83488, 83270-34292, 83270-39170, 83270-53655, 83270-83344, 83344-34292, 83344-39170, 83344-53655, 83344-83270, 83344-47875, 83344-83488, 83344-54960, 83488-34292, 83488-47875, 83488-53655, 83488-83344, 83488-54960)
plot(cc)
