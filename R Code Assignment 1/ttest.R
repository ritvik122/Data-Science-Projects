#Most popular thread in every year
attach(webforum)
#2004
ttest = webforum[!(webforum$WC==0),]
View(ttest)
ttest$Date = as.Date(ttest$Date, "%d/%m/%Y") # Create a temporary table for year
ttest$Date = as.numeric(format(ttest$Date, "%Y")) # Extract the year
#Pick year 2004
ttest2004 = subset(ttest, ttest$Date=="2004")
View(ttest2004)
attach(ttest2004)
t.test(Analytic, Authentic)
#frequency 
freqttest= data.frame(count(ttest2004, c("ThreadID")))
View(freqttest)
freqttest[which.max(freqttest$freq),]
#Thread ID 145223 is the most popular one in 2004
TopThread2004 = subset(ttest2004, ttest2004$ThreadID == "145223")
View(TopThread2004)
TopThread2004$AuthorID = NULL
TopThread2004$Time = NULL
TopThread2004$WC = NULL
tt04 = aggregate((Analytic + Clout + Authentic + Tone + ppron + i + we + you + shehe + they + number + affect + posemo + negemo+ anx + anger + social + family + friend + leisure + money + relig + swear + QMark)~ThreadID + Date, TopThread2004 , mean)
View(tt04)
tt04 = aggregate(TopThread2004[3:26], TopThread2004[2], mean)
tt04Final = melt(data = tt04, id.vars = "Date")
View(tt04Final)
colnames(tt04) = c("ThreadID", "Date", "Mean of all LIWC")



#2006
ttest2 = webforum[!(webforum$WC==0),]
View(ttest2)
ttest2$Date = as.Date(ttest2$Date, "%d/%m/%Y") # Create a temporary table for year
ttest2$Date = as.numeric(format(ttest2$Date, "%Y")) # Extract the year 
#Pick year 2006
ttest2006 = subset(ttest2, ttest2$Date=="2006")
View(ttest2006)
#frequency 
freqttest2= data.frame(count(ttest2006, c("ThreadID")))
View(freqttest2)
freqttest2[which.max(freqttest2$freq),]
#Thread ID 145223 is the most popular on in 2006
TopThread2006 = subset(ttest2006, ttest2006$ThreadID == "145223")
View(TopThread2006)
TopThread2006$AuthorID = NULL
TopThread2006$Time = NULL
TopThread2006$WC = NULL
tt06 = aggregate((Analytic + Clout + Authentic + Tone + ppron + i + we + you + shehe + they + number + affect + posemo + negemo+ anx + anger + social + family + friend + leisure + money + relig + swear + QMark)~ThreadID + Date, TopThread2004 , mean)
View(tt04)
tt06 = aggregate(TopThread2006[3:26], TopThread2006[2], mean)
tt06Final = melt(data = tt06, id.vars = "Date")
View(tt06Final)
colnames(tt06) = c("ThreadID", "Date", "Mean of all LIWC")
ttFinalFinal = merge(tt04Final, tt06Final, by = "variable")
attach(ttFinalFinal)
t.test(value.x, value.y)


###2009
#Thread ID 563904 is the most popular in 2009
ttest3 = webforum[!(webforum$WC==0),]
View(ttest3)
ttest3$Date = as.Date(ttest3$Date, "%d/%m/%Y") # Create a temporary table for year
ttest3$Date = as.numeric(format(ttest3$Date, "%Y")) # Extract the year 
#Pick year 2009
ttest2009 = subset(ttest3, ttest3$Date=="2009")
View(ttest2009)
#frequency 
freqttest3= data.frame(count(ttest2009, c("ThreadID")))
View(freqttest3)
freqttest3[which.max(freqttest3$freq),]
#Thread ID 563904 is the most popular on in 2009
TopThread2009 = subset(ttest2009, ttest2009$ThreadID == "145223")
View(TopThread2009)
TopThread2009$AuthorID = NULL
TopThread2009$Time = NULL
TopThread2009$WC = NULL
tt09 = aggregate(TopThread2009[3:26], TopThread2009[2], mean)
tt09Final = melt(data = tt09, id.vars = "Date")
View(tt09Final)
colnames(tt09) = c("ThreadID", "Date", "Mean of all LIWC")
ttFinalFinal = merge(tt04Final, tt09Final, by = "variable")
attach(ttFinalFinal)
t.test(value.x, value.y)


