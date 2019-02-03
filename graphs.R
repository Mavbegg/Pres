library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

fradat<- read.csv("silvopoolsl.csv")
fradat
#fradat$frav <- as.factor(fradat$frav)
notot <- fradat[!(fradat$frac=="tot"),]

meanc<- aggregate(.~treat+hor+frac,notot,mean)
meanc

se <- function(x) sd(x)/sqrt(length(x))

sec<- aggregate(.~treat+hor+frac,notot,se)
sec



ggplot(meanc,aes(x= treat, y=c))+
  geom_bar(aes(fill=frac),stat="identity")+
  geom_errorbar(aes(ymin= sec, 
                    ymax= sec),
                width=.2,                    # Width of the error bars
                position="identity")


frapro<- read.csv("fraper.csv")
frapro
frapol <- gather(frapro, frac, c, s.c:doc, factor_key= TRUE)
frapol
Afr <- frapol[(frapol$hor=="A"),]
Afr

Afrs <- Afr[!(Afr$frac=="S.A"),]

h <- Afrs[(Afrs$treat=="H4") | (Afrs$treat=="H2")| (Afrs$treat=="P"), ]
h

hd <- Afr[(Afr$treat=="H4") | (Afr$treat=="H2")| (Afr$treat=="P"), ]
hd
meanhd<- aggregate(.~treat+frac,Afr,mean)

meanhd
hlplot <- ggplot(hd, aes(x=frac, y=c, fill= factor(treat, levels=c("H2","H4","P")))) + 
  geom_boxplot()+
  labs(x = "Fraction", y = "Fraction of total SOC /%") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=14))+ 
  annotate("text", x=5, y=90, label= "Hybrid Larch A", size=4)+
  scale_x_discrete(limits= c("s.c","S.A","pom","doc","rsoc"),labels=c("s.c" = "s+c", "S.A" = "S+A","pom" = "POM","rsoc" = "rSOC", "doc"="DOC" )) +
  scale_fill_brewer(name = "Plot", labels=c("Wood plot", "Silvopasture", "Pasture" ), palette = "Set1") + 
  theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
 
hlplot
###

sd <- Afr[(Afr$treat=="S4") | (Afr$treat=="S2")| (Afr$treat=="P"), ]
sd

spplot <- ggplot(sd, aes(x=frac, y=c, fill= factor(treat, levels=c("S2","S4","P")))) + 
  geom_boxplot()+
  labs(x = "Fraction", y = "Fraction of total SOC /%") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=14))+ 
  annotate("text", x=4, y=90, label= "Scots Pine A", size=4)+
  scale_x_discrete(limits= c("s.c","S.A","pom","doc","rsoc"),labels=c("s.c" = "s+c", "S.A" = "S+A","pom" = "POM","rsoc" = "rSOC", "doc"="DOC" )) +
  scale_fill_brewer(name = "Plot", labels=c("Wood plot", "Silvopasture", "Pasture" ), palette = "Set1") + 
  theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
spplot

###

Afr <- frapol[(frapol$hor=="B"),]
Afr

Afrs <- Afr[!(Afr$frac=="S.A"),]

h <- Afrs[(Afrs$treat=="H4") | (Afrs$treat=="H2")| (Afrs$treat=="P"), ]
h

hd <- Afr[(Afr$treat=="H4") | (Afr$treat=="H2")| (Afr$treat=="P"), ]
hd
meanhd<- aggregate(.~treat+frac,Afr,mean)

meanhd
hlplot <- ggplot(hd, aes(x=frac, y=c, fill= factor(treat, levels=c("H2","H4","P")))) + 
  geom_boxplot()+
  labs(x = "Fraction", y = "Fraction of total SOC /%") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=14))+ 
  annotate("text", x=4, y=90, label= "Hybrid Larch B", size=4)+
  scale_x_discrete(limits= c("s.c","S.A","pom","doc","rsoc"),labels=c("s.c" = "s+c", "S.A" = "S+A","pom" = "POM","rsoc" = "rSOC", "doc"="DOC" )) +
  scale_fill_brewer(name = "Plot", labels=c("Wood plot", "Silvopasture", "Pasture" ), palette = "Set1") + 
  theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

hlplot
###

sd <- Afr[(Afr$treat=="S4") | (Afr$treat=="S2")| (Afr$treat=="P"), ]
sd

spplot <- ggplot(sd, aes(x=frac, y=c, fill= factor(treat, levels=c("S2","S4","P")))) + 
  geom_boxplot()+
  labs(x = "Fraction", y = "Fraction of total SOC /%") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=14))+ 
  annotate("text", x=4, y=90, label= "Scots Pine B", size=4)+
  scale_x_discrete(limits= c("s.c","S.A","pom","doc","rsoc"),labels=c("s.c" = "s+c", "S.A" = "S+A","pom" = "POM","rsoc" = "rSOC", "doc"="DOC" )) +
  scale_fill_brewer(name = "Plot", labels=c("Wood plot", "Silvopasture", "Pasture" ), palette = "Set1") + 
  theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
spplot
