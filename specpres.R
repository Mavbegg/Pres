
rm(list=ls()) #empties the workspace
setwd("Z:/data/PhD/PhDdata/FTIRdata/allrename")
library("R.utils")
library("ChemoSpec")
library("IDPmisc")
library("baseline")
library(robustbase)
library(pcaPP)

#load spectra: 

files2SpectraObject(gr.crit = c( "ink","kohle","pre","gs","tas", "plant", "hy","pet","p2", "niox", "naox", "hcl"), gr.cols = c("blue", "black", "green", "red", "orange", "purple", "yellow", "coral", "dimgrey", "gold", "darkred", "darkorchid"), freq.unit = "wavenumber", int.unit = "Absorbance", descrip = "Peat spectra",header=F , out.file = "SC", debug=T, sep=",",nrows= 1882 )
SC <- loadObject("SC.RData")


SC$data <- -log(SC$data)# absorbance



#exploratary analysis:
par(bg="transparent")
myt <- expression(~bold(~IR~Spectra))
plotSpectra(SC, main = myt,
            which = c( 105:110), xlim = c(4000,400),
            yrange = c(0, 10), offset = 1, lab.pos = 2200)
plotSpectra(SC, main = myt,
            which = c(1, 2,3), xlim = c(3500, 2000),
            yrange = c(0, 3.5), offset = 1, lab.pos = 3000)






SC2 <- baselineSpectra(SC, int = FALSE, method = "rfbaseline", retC = TRUE) #baseline correction

myt <- expression(~bold(~IR~Spectra))
plotSpectra(SC2, main = myt,
            which = c(1,2,3,101,100), xlim = c(4000,400),
            yrange = c(0, 6), offset = 1, lab.pos = 2200)

grep("plant", SC$names) #find samples 
surveySpectra(SC2, method = "iqr", main = myt, by.gr = FALSE)


surveySpectra(SC2, method = "iqr", main = "Detail of Carbonyl Region",
              by.gr = FALSE, xlim = c(1650, 1800))

surveySpectra(SC2, method = "iqr", main = "Detail of Empty Region",
              by.gr = FALSE, xlim = c(1800, 2500), ylim = c(0.0, 0.05))


SC3 <- removeFreq(SC2, rem.freq = SC2$freq > 1800 & SC2$freq < 2500)
sumSpectra(SC3)

check4Gaps(SC$freq, SC$data[1,], plot = TRUE)

selec <- removeGroup(SC3, rem.group= c("hcl","hy","naox","tas", "plant", "kohle","p2","pet","niox","gs")) # choose specific groups to work on

HCA <- hcaSpectra(selec, main = myt)





class <- c_pcaSpectra(selec, choice = "noscale")
plotScores(selec, main = myt, class,
           pcs = c(1,2), ellipse = "rob", tol = 0.01)

robust <- r_pcaSpectra(selec, choice = "noscale")
plotScores(selec, main = myt, robust,
           pcs = c(1,2), ellipse = "rob", tol = 0.01)

diagnostics <- pcaDiag(selec, class, pcs = 2, plot = "OD")

plotScores3D(selec, class, main = myt, ellipse = FALSE)



#global pls with pls package:
library(dplyr)




scdata <- SC3$data #convert absorbance subset to object, plas can't handle the subset otherwise, don't know why. 
scfreq <- SC3$freq
colnames(scdata) <-scfreq



grep("ATRc", SC$names) #find samples with carbon measured
grep("ATR00c", SC$names)

scdatac <- scdata[c(1:40,42:47,96:97,109:117,130:137), ] #samples with known %c



ele.values <-read.csv("Z:/data/PhD/PhDdata/FTIRdata/Taselements.csv", sep= ",") #%C and other element values
c.values <- ele.values[,3] 


cpeat.specs <- data.frame(c.values, I(scdatac))
library(pls)
library(caret)
library(MASS)
library(AppliedPredictiveModeling)
library(lars)
library(elasticnet)

cpls.ir <- plsr(c.values ~ scdatac, ncomp = 10, data = cpeat.specs[1:60,], validation="LOO", header=T)

summary(cpls.ir)

plot(RMSEP(cpls.ir), legendpos= "topright")
plot(cpls.ir, ncomp = 6, asp = 1, line = TRUE)
plot(cpls.ir, plottype = "scores", comps = 1:2)
explvar(cpls.ir)

plot(cpls.ir, "loadings", comps = 1:2, legendpos = "topleft", xlab = 'nm')
abline(h = 0)

cpls.pred<-predict(cpls.ir, ncomp = 6, newdata = cpeat.specs[61:65,])
plot(c.values[61:65], cpls.pred)
cpls.eval <- data.frame(obs=c.values[61:65], pred=cpls.pred[,1,1])
cpls.eval
defaultSummary (cpls.eval)

# local prediction using mbl

library(resemble)

library(prospectr)

d1 <- t(diff(t(scdatac), differences = 1)) 


ctrl1 <- mblControl(sm = 'pc', pcSelection = list('opc', 40),
                    valMethod = 'NNv', center = TRUE)

sbl.u <- mbl(Yr = c.values[1:60], Xr = scdatac[1:60, ], Yu = NULL, Xu = scdatac[61:65, ],
             mblCtrl = ctrl1,
             dissUsage = 'none',
             k = seq(10, 50, by = 5),
             method = 'wapls1', pls.c =c(3,8))

getPredictions(sbl.u)
plot(sbl.u)

predicted.sblu <- getPredictions(sbl.u)$Nearest_neighbours_25


sblu.eval <- data.frame(obs=c.values[61:65], pred=predicted.sblu)
sblu.eval
defaultSummary (sblu.eval)