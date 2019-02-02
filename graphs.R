library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

fradat<- read.csv("silvopoolsl.csv")
fradat
pa <- fradat[(fradat$treat=="pa"),]
pa
meanc<- aggregate(.~treat+pool+moq,pa,mean)
meanc
#paHUM <- pa[(pa$pool=="HUM"),]


ggplot()+
  geom_bar(aes(x = moq,y=carbon, fill = pool ), data = meanc, stat="identity")

sumc<- aggregate(.~treat+moq,pa,sum)
sumc
