install.packages("readr")
library(readr)
rm(list = ls())
set.seed(29677068)
webforum = read_csv("webforum.csv")
webforum = webforum [sample(nrow(webforum), 20000), ]


# Question a
#Describe your data: How active are participants, and are there periods where this increases
#or decreases? Is there a trend over time?

install.packages("ggplot2")
library(ggplot2)

WF = webforum
WF = subset(webforum, WC != 0)
WF$tempdate = as.Date(WF$Date, "%d/%m/%Y") # Create a temporary table for year
WF$year = as.numeric(format(WF$tempdate, "%Y")) # Extract the year 
WF$tempdate = NULL
WF$tempdate2 = as.Date(WF$Date, "%d/%m/%Y" )
WF$month = as.numeric(format(WF$tempdate2, "%m"))# extract the month
WF$tempdate2 = NULL
attach(WF)
#Second Trial
AuthMonthYear = as.table(by(WC,	list(year, month), length))
AuthMonthYear = as.data.frame(AuthMonthYear)
View(AuthMonthYear)
colnames(AuthMonthYear) = c("Year", "Month", "WordCount")

r = ggplot(data = AuthMonthYear)
r = r + geom_col(mapping	=	aes(x = Month, y = WordCount))
r	=	r	+	scale_x_discrete(breaks	=	c(1,	2,	3,	4,	5,	6,	7, 8,	9, 10, 11, 12))
r  = r + facet_wrap(~Year, nrow = 2)
r  = r + ylab("Number of Posts")
r = r + ggtitle("Activity of Participants from 2002-2011")
r

#First Trial
AuthWordCount = as.table(by(WC, year, length)) # Using length get the no of observations of word count in each yea
AuthWordCount = as.data.frame(AuthWordCount)
colnames(AuthWordCount) = c("Year", "WordCount")
AuthWordCount$Year = as.numeric(AuthWordCount$Year)
View(AuthWordCount)
View(WF)

m = ggplot(AuthWordCount, aes(x = Year, y = WordCount, group = 1))
m = m + geom_line(color ="blue")
m =  m + ylab("No. of WordCount Observations")
m = m + ggtitle("Number of Word Counts Observed for all Authors over 10 years")
m


#s= n = ggplot(AuthTime, aes(x = Time, y =WordCount, group = 1))

#s = ggplot(data = AuthMonthYear, aes(x = Month, y = WordCount, line = Year))
#s = ggplot(mapping = aes(x = Month, y = WordCount, color = Year))
#s = s + geom_point() + facet_wrap(~Year, ncol = 3)
#s
#HEATMAP
g	=	ggplot(data	=	AuthMonthYear,	aes(x	=	Month,	y	=	Year))
g	=	g	+	geom_tile(aes(fill = WordCount))
g	=	g	+	ggtitle("Number of Posts made over 2002-2011")
g	=	g	+	scale_x_discrete(breaks	=	c(1,	2,	3,	4,	5,	6,	7, 8,	9,	10,	11,	12))
g

install.packages("scales")
library(scales)
#Activity of post over a day
WF$temptime = as.POSIXct(WF$Time, format = "%H:%M")
attach(WF)
ggplot(data = WF, aes(x= temptime, y=WC)) + 
  labs(title="Amount of Word Count v Time of Day (Forum)", x="Time", y="Word Count") + 
  geom_line(colour="darkblue") +
  scale_x_datetime(date_breaks = "4 hour", labels = date_format("%H:%M") )

#AuthDay = as.table(by(AuthorID, temptime, length))
#AuthDay = as.data.frame(AuthDay)
#View(AuthDay)
#ggplot(data=AuthDay, aes(x=temptime, y=Freq)) + 
 # labs(title="Amount of Word Count v Time of Day (Forum)", x="Time", y="Word Count") + 
 # geom_line(colour="darkblue") +
 #scale_x_datetime(date_breaks = "4 hour", labels = date_format("%H:%M") )


#Question a (Second Dot Point) : Looking at the linguistic variables, do these change over time? Is there a relationship
#between them?
install.packages("zoo")
install.packages("dplyr")
install.packages("reshape2")
library(zoo)
library(reshape2)
library(dplyr)
library(ggplot2)
WFliwcsumm = WF[c(3,6,7,8,9,10,17,18,19)]
View(WFliwcsumm)
WFliwcsumm$Date = as.Date(WF$Date, "%d/%m/%Y")
WFliwcsumm$Date = as.yearmon(WFliwcsumm$Date, "%Y-%m")
ling_month_year = aggregate(WFliwcsumm, by = list(WFliwcsumm$Date), mean)
View(ling_month_year)
ling_month_year$Group.1 = NULL
lingmelt = melt(ling_month_year, id.vars = "Date")
View(lingmelt)
View(ling_month_year)
colnames(lingmelt) = c("Date", "variable", "Avgvalue")
View(lingmelt)

library(ggplot2)
lg = ggplot(data = lingmelt, aes(x = as.yearmon(Date), y =Avgvalue, colour = variable, group =variable))
lg = lg + geom_line()
lg = lg + theme(axis.line = element_line(colour = "maroon", size =1, linetype = "solid"))
lg = lg + xlab("Time")
lg = lg + ylab("Mean Values")
lg = lg + ggtitle("LIWC Variables Over Time")
lg

WFCorrelation = WFliwcsumm
View(WFCorrelation)
WFCorrelation$Date = as.Date(WF$Date, "%d/%m/%Y") # Create a temporary table for year
WFCorrelation$Date = as.numeric(format(WFCorrelation$Date, "%Y"))
CorrAvg = aggregate(WFCorrelation, by = list(WFliwcsumm$Date), mean)
View(CorrAvg)
CorrAvg$Group.1 = NULL
by(CorrAvg[2:9], factor(CorrAvg$Date), cor)

install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
Z = cor(CorrAvg[2:9])
View(Z)
corrplot(Z, type="upper", order="hclust", tl.col= "black", tl.srt= 45, col=brewer.pal(n=8, name="PuOr"))

#Extra Work
attach(WF)
install.packages("dplyr")
library(dplyr)
liwc_analytic = as.table(by(WF[6:19],list(year, month), sum))
liwc_analytic_mean=as.data.frame(apply(liwc_analytic[6:17],2,mean))
View(liwc_analytic_mean)
colnames(liwc_analytic) = c("Year", "AnalyticSum")
liwc_clout = as.data.frame(as.table(by(Clout, list(year), sum)))
colnames(liwc_clout)= c("Year", "CloutSum")

liwc = merge(liwc_analytic, liwc_clout, by = "Year")
View(liwc)
aggregate(WF[5], WF[4], min)

WF$temptime = sort(Time, increasing = TRUE)
attach(WF)
AuthTime = as.table(by(WC, temptime, length))
AuthTime = as.data.frame(AuthTime)
colnames(AuthTime) = c("Time", "WordCount")

n = ggplot(AuthTime, aes(x = Time, y =WordCount, group = 1))
n = n + geom_line(color ="blue")
n =  n + ylab("No. of WordCount Observations")
n = n + ggtitle("Number of Word Counts Observed for all Authors over a 24hr Day")
n
View(temptime)
View(AuthTime)

WF2 = webforum
View(WF2)

WF2 = webforum
WF2$tempmonth = as.Date(WF2$Date, "%d/%m/Y") # Create a temporary table for year
WF2$month = as.numeric(format(WF2$month, "%m")) # Extract the month
WF$tempdate = NULL
attach(WF)
AuthWordCount = as.table(by(WC, year, length)) # Using length get the no of observations of word count in each yea
AuthWordCount = as.data.frame(AuthWordCount)
colnames(AuthWordCount) = c("Year", "WordCount")
AuthWordCount$Year = as.numeric(AuthWordCount$Year)
View(AuthWordCount)

WF2[which.min(as.Date(WF2$Date, "%d/%m/%Y")),]
WF2[which.max(as.Date(WF2$Date,"%d/%m/%Y")),]


WFX	=	WF2[as.Date(WF2$Date,"%d/%m/%y")	>	as.Date("31/12/2007","%d/%m/%y"),]
View(WFX)
WFX	=	WF2[as.Date(WF2$Date,"%d/%m/%y") <	as.Date("1/01/2011","%d/%m/%y"),]
View(WFX)





#Part B (First Dot point) : Analyse the language used by groups.
# 1. Threads indicate groups of participants communicating on the same topic. Describe the
#    threads present in your data.
attach(webforum)
cleanWF = webforum[!(webforum$WC==0),]
attach(cleanWF)
install.packages("plyr")
install.packages("scales")
library(plyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
groupThread = aggregate(cleanWF[5:29], cleanWF[1], mean)
freqCount= data.frame(count(cleanWF,c("ThreadID")))
freqCount[which.max(freqCount$freq),] #this line of code tells us which ThreadID has thr highest frequency 
maxthread = subset(cleanWF, cleanWF$ThreadID == "283958")
maxthread$Time <- as.POSIXct(maxthread$Time, format="%H:%M")

ggplot(data=maxthread, aes(x=maxthread$Time,y=maxthread$WC)) + 
  labs(title="Amount of Word Count v Time of Day (Most Active Thread)", x="Time", y="Word Count") +
  geom_line(colour="darkgreen") +
  scale_x_datetime(date_breaks = "4 hour", labels = date_format("%H:%M"))

####(Still First Dot Point)
#2.Threads indicate groups of participants communicating on the same topic. Describe the
# threads present in your data. 
v = webforum[!(webforum$WC==0),]
View(v)
View(u)
u = aggregate(v[5:29], v[1], mean)
count1 = data.frame(count(v, c("ThreadID")))
t10 = as.data.frame(head(count1[order(-count1[2]),],10))
t10I=subset(v,ThreadID%in%t10$ThreadID)
``
attach(t10I)
q = as.data.frame(as.table(by(Analytic, ThreadID, mean)))
colnames(q) = c("ThreadID", "Analytic")
View(q)
j = as.data.frame(as.table(by(Clout, ThreadID, mean)))
colnames(j) = c("ThreadID", "Clout")
View(j)
y = as.data.frame(as.table(by(Authentic, ThreadID, mean)))
colnames(y) = c("ThreadID", "Authentic")
View(y)
d = as.data.frame(as.table(by(Tone, ThreadID, mean)))
colnames(d) = c("ThreadID", "Tone")
View(d)
SummBgraph = merge(q, j, by = "ThreadID")
SummBgraph = merge(SummBgraph, y, by = "ThreadID")
SummBgraph = merge(SummBgraph, d, by = "ThreadID")
library(dplyr)
mydata = t10I
View(mydata)
mydata = aggregate((ppron + i + we + you + shehe + they + number + affect + posemo + negemo+ anx + anger + social + family + friend + leisure + money + relig + swear + QMark)~ThreadID, t10I, mean)
colnames(mydata) = c("ThreadID", "OtherLIWC")
SummBgraph = merge(SummBgraph, mydata, by ="ThreadID")
library(plyr)
library(dplyr)
library(reshape2)
MeltSumm = melt(data = SummBgraph, id.vars = "ThreadID")
library(ggplot2)
r = ggplot(data = MeltSumm)
r = r + geom_col(mapping	=	aes(x = variable, y = value, fill = variable))
r	=	r	+	scale_fill_manual(breaks =	c("Analytic",	 "Clout",  "Authentic",	  "Tone",	"OtherLIWC"), values = c("red",  "blue",   "orange", "green", "yellow"))
r = r + facet_wrap(~ThreadID, nrow = 2)
r = r + ylab("Percentage")
r = r + ggtitle("Percentage of LIWC Variables in Top 10 Threads")
r <- r + theme(axis.text = element_text(size = 6.35))
r


#Second dot point (Part B) By analysing the linguistic variables for all or some of the threads, is it possible to see a
#difference in the language used by these different groups?

######Single Thread v Multiple Threads – Standard deviation
cleanWF = webforum[!(webforum$WC==0),]
#Group Data by Thread
groupT = aggregate(cleanWF[5:29], cleanWF[1], mean)
attach(groupT)
#Find the most active thread and subset
freqC= data.frame(count(cleanWF, c("ThreadID")))
View(maxthread)
freqC[which.max(freqC$freq),]#Most active thread is 283958ID
#Find the top 15 active threads and subset
topFifteen=as.data.frame(head(freqC[order(-freqC[2]),],15))
topFifteenInfo=subset(cleanWF,ThreadID%in%topFifteen$ThreadID)#Each threads info
#Finding SD of most active thread
SDmaxthread=(apply(maxthread[6:17],2,sd))
SDmaxthread=as.data.frame(SDmaxthread)
#Finding SD of top 15 threads
SDtopFifteen=apply(topFifteenInfo[6:17],2,sd)
SDmaxthread$SDtopFifteen=as.data.frame(SDtopFifteen)
View(SDmaxthread)
#Binding and transposing attributes in order to melt data 
SDmaxthread2=t(rbind(t(SDmaxthread[,2]),t(SDmaxthread[,1])))
#melting data and renaming columns
SDThreadMelt=melt(SDmaxthread2)
colnames(SDThreadMelt)=c("LIWCVariables","Thread","Values")


ggplot(SDThreadMelt, aes(x=LIWCVariables,y=Values,fill=Thread))+
  geom_bar(stat="identity", width=.5,position="dodge")+
  ggtitle("Most Active Thread v Top Fifteen Active Threads Standard Deviation Graph")+
  scale_fill_brewer(palette = "Accent", labels=c("Top 15 Threads", "Most Active Thread (ID:283958)"))+theme_minimal()



#####Single Thread vs Multiple Thread (Authors)
#Get rid of anonymous authors
NAAnon = cleanWF[!(cleanWF$AuthorID==-1),]
#Group data by Author
groupingAuth=aggregate(NAAnon[6:29],NAAnon[2], mean)
#Find most frequency of authors post
attach(groupingAuth)
freqC2= data.frame(count(NAAnon,"AuthorID"))
#Find most frequently posting author
freqC2[which.max(freqC2$freq),] #AuthorID 39170 is the most frequently psoting author
maxauthor=subset(NAAnon,NAAnon$AuthorID=="39170")
#Finding which forums, the author is most active in
MostActThread = data.frame(count(maxauthor, "ThreadID"))
MostActThread[which.max(MostActThread$freq),]# ThreadID 145223 is the thread which the author is most active in
MAAT=subset(NAAnon,NAAnon$ThreadID=="145223")# Most Active Author Thread
#Finding the SD of the thread the author is most active in
SDThrAuth=(apply(maxauthor[6:17],2,sd))
SDThrAuth=as.data.frame(SDThrAuth)
#Finding the SD of the author in their most active thread
SDMAToA=apply(MAAT[6:17],2,sd)
SDThrAuth$SDMAToA=as.data.frame(SDMAToA)
#Binding so that the data can be melted
SDThrAuth2=t(rbind(t(SDThrAuth[,2]),t(SDThrAuth[,1])))
#Melting the data and change the column names
SDThreadMelt2=melt(SDThrAuth2)
colnames(SDThreadMelt2)=c("LIWCVariables","Author","Values")
#creating bar graph of SDMActive v SDtopFifteen
library(ggplot2)

ggplot(SDThreadMelt2, aes(x=LIWCVariables,y=Values,fill=Author))+
  geom_bar(stat="identity",width=.5,position="dodge")+
  ggtitle("Author across forums vs Author in a single thread (SD): For AuthorID:39170")+
  scale_fill_brewer(palette = "Accent", (labels=c("Top 15 Threads", "Most Active Thread (ID: 145223)"))) + theme_minimal()

#### Part 3 - Does the language used within threads change over time?
View(WF)
cleanWF = WF[!(webforum$WC==0),]
ThreadGroup = aggregate(cleanWF[6:31], cleanWF[1], mean)
FrequencyCount= data.frame(count(cleanWF, c("ThreadID")))
Top30Threads = as.data.frame(head(FrequencyCount[order(-FrequencyCount[2]),],30)) 
top30Info=subset(cleanWF,ThreadID%in%Top30Threads$ThreadID)

A1 = aggregate(cbind(Analytic, Clout, Authentic, Tone)~ ThreadID + year, top30Info, mean)
A2 = aggregate((ppron + i + we + you + shehe + they + number + affect + posemo + negemo+ anx + anger + social + family + friend + leisure + money + relig + swear + QMark)~ThreadID + year, top30Info, mean)
colnames(A2) = c("ThreadID", "year", "LIWCOther")
A3 = merge(A1, A2, by = c("ThreadID", "year"))
A3$ThreadID = NULL
A4 = aggregate(A3[2:6], A3[1], mean)
A5 = melt(data= A4, id.vars = "year")






## c) Can you define, graph and describe the social network that exists at a particular point in
#time, for example over one month? How does this change in the following months?

View(karate)







