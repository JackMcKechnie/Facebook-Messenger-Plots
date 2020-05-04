#-------- Setup --------
library(plyr)
library(ggplot2)
library(ggpmisc)
library("stringr")

setwd("C:\\Users\\jack-\\Desktop\\JSONS")
getwd()

tgt <- read.csv("out.csv")
colnames(tgt) <- c("Sender","Time","Message")

#Convert ms to YYYY-MM-DD HH:MM:SS
tgt$Time <- tgt$Time / 1000
tgt$Time <- as.POSIXct(tgt$Time, origin="1970-01-01", tz="GMT")

#-------- Who sends the most messages --------
most.messages <- count(tgt,'Sender')
most.messages <- most.messages[order(most.messages$freq,decreasing=T),]

plot <- ggplot(data = most.messages, aes(x=reorder(Sender, -freq),y=freq))
messages.per.person <- plot + geom_bar(stat="identity",aes(fill=freq)) +
  scale_fill_gradient2(low='Red', mid='Blue', high='Red') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = c(0.9,0.8)) +
  xlab("Name") +
  ylab("No. of messages sent") +
  geom_text(aes(label=freq,vjust=-0.25)) +
  ggtitle("Number of Messages Sent Per Person")

messages.per.person

#-------- Most popular time of day --------
time.of.day <- tgt

#Convert time to a string 
as.character(time.of.day$Time)
#Create substring of just the hours and convert to factor
time.of.day$Time <- as.factor(substring(time.of.day$Time,12,13))

time.of.day <- count(time.of.day,'Time')
time.of.day <- times[order(time.of.day$Time,decreasing=F),]

messages.over.time <- ggplot(data = time.of.day, aes(x=Time,y=freq,group=1))
messages.over.time <- messages.over.time + geom_point() + geom_smooth(span=0.3,fill="Pink",colour="darkred")
messages.over.time <- messages.over.time + ylab("No. of messages sent")+ theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label=freq,vjust=-0.25,hjust=0.5,angle=0)) +
  ggtitle("Message Frequency Over The Day")

messages.over.time

#-------- Length of messages sent --------

len.of.messages <- tgt
#Exclude photos, videos and gifs
len.of.messages <- len.of.messages[!(len.of.messages$Message == "[PHOTO/GIF/VIDEO]"),]
len.of.messages$Message <- str_length(len.of.messages$Message)

u <- ggplot(data = len.of.messages, aes(x = Sender,y = Message, colour = Sender))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2)  + geom_point()
message.length <- u + geom_boxplot(size=1.2)  + geom_jitter() +
  ylab("Length of message (Characters)") +
  theme(legend.position = "none",axis.text.x = element_text(angle = 90))

message.length

#-------- Most popular day of the week --------
day.of.week <- tgt
day.of.week$Time <- weekdays(day.of.week$Time)

days <- count(time.pop$Date)
days <- days[order(days$freq,decreasing=T),]
days
