# Final Analysis Code
# Last Update 8/30/2018

# This file contains all data processing and analysis steps performed for the paper "Enclothed Cognition's Effect on Selective Attention Fails to Replicate"

# Since there are many different steps included below, it is recommended that you run sections of the code one at a time instead of trying to run the entire script at once

# This code makes use of the following packages: psych, ez, lme4, ggplot2

#Any questions should be directed to Dr. Devin Burns: BurnsDe@mst.edu

##################################################
#  Data Import
##################################################

#Assumes working directory is already set to a primary folder for analysis that has data files in 4 different folders correpsonding to the different collection locations.
#Outputs a copy of the data for each location within that folder, and also a combined data file in the root folder.
setwd("C:/Users/20176239/Dropbox/jobb/PhD/Projects/2018_OSC_Replication_Value/examples/stroop/Burns_etal_data/Data/")

for(location in 1:4){
  setwd(paste("Location",location,sep=""))
  fileList=list.files()
for(file in fileList){
  if(substr(file,nchar(file)-2,nchar(file))=="csv" & substring(file,nchar(file)-7,nchar(file)-4)!="Data"){  #making sure I only capture original data files
    data <- read.csv(file)
    data=data[,c("trials.thisN","word","ink","type","Response.corr","Response.rt","participant","condition")] #the subset of variables I want
    colnames(data)=c("trial","word","ink","cond","corr","rt","ID","coat") #renaming
    data=subset(data,trial!="") #remove extra lines from beginning and end
    if(location==4 & data[1,"ID"]!=106){data$ID=as.numeric(substr(data[1,"ID"],8,10))} #fix data input error
    data$rt=data$rt*1000 #convert to milliseconds
    data$trial=data$trial+1 #count from 1 instead of 0
    if(location=="2"){data$ID=data$ID+100} #ensure no ID duplicates
    if(location=="4"){data$ID=data$ID+200}
    if(location=="3" & data$ID[1]==1){data$ID=146} #fix input error
    
    data$coat=as.character(data$coat)
    data[data$coat=="L "|data$coat=="L","coat"]="y" #fix input error
    data[data$coat==" N" | data$coat=="N\\"|data$coat=="N","coat"]="n" #fix input error
    #the following block of code records if the ink or word dimensions repeated their values from the last trial 
    data$wordRepeat="n"
    data$inkRepeat="n"
    for(i in 2:length(data$word)){
      if(data$word[i]==data$word[i-1])
        data[i,"wordRepeat"]="y"
      if(data$ink[i]==data$ink[i-1])
        data[i,"inkRepeat"]="y"
    }
    #combine neutral and congruent for comparison with original
    data$AGCond="Inc" 
    levels(data$AGCond)=c("Inc","Neu")
    data[data$cond=="congruent" | data$cond=="neutral","AGCond"]="Neu"
    #record the previous trial type
    data$LastCond=data$AGCond
    data[2:nrow(data),"LastCond"]=data[1:nrow(data)-1,"AGCond"]
    data$LastCond[1]=NA
    if(!exists("locationData")){locationData=data} #initialize data frame
    else{locationData=rbind(locationData,data)} #incorporate new participant data
  }}
locationData$location=location
write.table(locationData,file=paste("Location",location,"Data.csv",sep=""),sep=",",row.names=F) #export single location data set
#build combined data set
if(location==1){allData=locationData} #initialize
else{allData=rbind(allData,locationData)} #combine
remove(locationData)
setwd("..") #return to root
}
write.table(allData,file="allData.csv",sep=",",row.names=F)

##################################################
#  Data Cleaning
##################################################

#This block of code excludes RTs below 250ms or greater than 3 standard deviations above each participant's mean for a particular stimulus type, and outputs relevant details from the cleaning process and some descriptives

library(psych)

allData=read.csv("allData.csv") #if above code not run
cleanData=allData
write("Data Cleaning Info",file="cleaning.xls") #Export relevant data from cleaning
trials=nrow(cleanData)
write(paste("\n Total trials",trials,sep="\t"),file="cleaning.xls",append=TRUE)
write(paste("\n","# of Trials","% of Trials", "% Correct",sep="\t"),file="cleaning.xls",append=TRUE)
#lower cutoff fixed at 250
fast=cleanData[cleanData$rt<250,]
cleanData=cleanData[cleanData$rt>=250,]
write(paste("RTs < 250",nrow(fast),round(nrow(fast)/trials,3),mean(fast$corr),sep="\t"),file="cleaning.xls",append=TRUE)

#upper cutoff of each participant's mean + 3*sd
IDList=unique(cleanData$ID)
condList=unique(cleanData$cond)
for(ID in IDList){
    m=mean(cleanData[cleanData$corr==1 & cleanData$ID==ID,"rt"])
    sd=sd(cleanData[cleanData$corr==1 & cleanData$ID==ID,"rt"])
    cleanData=cleanData[cleanData$ID!=ID | cleanData$rt<m+3*sd,]
}
slow=trials-nrow(cleanData)
write(paste("RTs > mean+3*sd",slow,round(slow/trials,3),sep="\t"),file="cleaning.xls",append=TRUE)

#finding outlier participants whose ave RT is 2 sd above mean
agg=aggregate(rt~ID,cleanData,FUN=mean)
m=mean(agg$rt)
sd=sd(agg$rt)
slowPs=agg[agg$rt>m+2*sd,"ID"]
cleanData=cleanData[!(cleanData$ID %in% slowPs),]
write(paste("Slow participants:",length(slowPs),sep="\t"),file="cleaning.xls",append=TRUE)

#finding low accuracy participants
agg=aggregate(corr~ID,cleanData,FUN=mean)
lowAccPs=agg[agg$corr<.9,"ID"]
cleanData=cleanData[!(cleanData$ID %in% lowAccPs),]
write(paste("Low Acc participants:",length(lowAccPs),sep="\t"),file="cleaning.xls",append=TRUE)
  
write(paste("Remaining trials",nrow(cleanData),round(nrow(cleanData)/trials,3),mean(cleanData$corr),sep="\t"),file="cleaning.xls",append=TRUE)

write(paste("\n RT (correct)","Mean","SD","Skew","Kutosis",sep="\t"),file="cleaning.xls",append=TRUE)
desc=describe(allData[allData$corr==1,"rt"])
write(paste("All Data",round(desc$mean,3),round(desc$sd,3),round(desc$skew,3),round(desc$kurtosis,3),sep="\t"),file="cleaning.xls",append=TRUE)
desc=describe(allData[allData$corr==1 & allData$ID %in% slowPs,"rt"])
write(paste("Slow Ps",round(desc$mean,3),round(desc$sd,3),round(desc$skew,3),round(desc$kurtosis,3),sep="\t"),file="cleaning.xls",append=TRUE)
desc=describe(cleanData[cleanData$corr==1,"rt"])
write(paste("Clean Data",round(desc$mean,3),round(desc$sd,3),round(desc$skew,3),round(desc$kurtosis,3),sep="\t"),file="cleaning.xls",append=TRUE)
#output clean data
write.table(cleanData,file="cleanData.csv",sep=",",row.names=F)

# ##################################################
# #  Results
# ##################################################
# 
# #This code provides all of the calculations for the analyses reported in the paper
# #To run the results separately from the data import and cleaning, just start with importing the data:
# 
# cleanData <- read.csv("cleanData.csv")
# cleanData$location=as.factor(cleanData$location)
# cleanData$ID=as.factor(cleanData$ID)
# cleanData$AGCond=relevel(cleanData$AGCond,"Neu")
# cleanData$LastCond=relevel(cleanData$LastCond,"Neu")
# 
# #full data set to evaluate impact of exclusion criteria
# allData=read.csv("allData.csv")
# allData$AGCond=relevel(allData$AGCond,"Neu")
# 
# ########## Stroop Analyses #####################
# 
# ### Mixed-Design ANOVA
# library(ez)
# options(contrasts = c("contr.treatment", "contr.poly"))
# 
# aggData=aggregate(cbind(corr,rt)~ID*AGCond+coat,data=cleanData,FUN=mean)
# accANOVA=ezANOVA(data=aggData,dv=corr,wid=ID,within=AGCond,between=coat,type=2)
# print(accANOVA)
# 
# aggDataAll=aggregate(cbind(corr,rt)~ID*AGCond+coat,data=allData,FUN=mean)
# accANOVAall=ezANOVA(data=aggDataAll,dv=corr,wid=ID,within=AGCond,between=coat,type=2)
# print(accANOVAall)
# 
# # The below makes use of Adam and Galinsky's data, and is commented out since we do not have permission to share their data
# # ### Mixed-Design ANOVA of Adam & Galinsky's Data
# # AG=read.csv("AGClean.csv")
# # AGrtANOVA=ezANOVA(data=AG,dv=RT,wid=SONAID,within=Cond,between=LabCoat,type=2)
# # print(AGrtANOVA)
# 
# ### Mixed-Design ANOVA of new data
# rtANOVA=ezANOVA(data=aggData,dv=rt,wid=ID,within=AGCond,between=coat,type=2)
# print(rtANOVA)
# 
# rtANOVAall=ezANOVA(data=aggDataAll,dv=rt,wid=ID,within=AGCond,between=coat,type=2)
# print(rtANOVAall)
# 
# 
# ### Generalized Linear Mixed-Effects Models
# library(lme4)
# 
# #The below is an extrapolation of trial by trial data from Adam and Galinsky based on mean accuracy and trial numbers. They have requested that anyone interested in reanalyzing their results contact them directly to request their data.
# #AGData=read.csv("A&G Extrapolated Data.csv")
# #AGData$cond=relevel(AGData$cond,"Neu")
# #AGData$ID=as.factor(AGData$ID)
# #AGGLM=glmer(corr~cond*coat+(cond|ID),AGData,binomial)
# #print(summary(AGGLM))
# #print(paste("Odds for Condition (no coat):",round(1/exp(fixef(AGGLM)[2]),2),sep=" "))
# #print(paste("Odds for Condition (yes coat):",round(1/exp(sum(fixef(AGGLM)[c(2,4)])),2),sep=" "))
# 
# #new data
# cleanData$corr=as.factor(cleanData$corr)
# AccGLM=glmer(corr~AGCond*coat+(AGCond|ID),cleanData,binomial) 
# print(summary(AccGLM))
# print(paste("Odds ratio for Condition:",round(1/exp(fixef(AccGLM)[2]),2),sep=" "))
# 
# # The below makes use of Adam and Galinsky's data, and is commented out since we do not have permission to share their data
# # #Combined Data set
# # combDataNew=cleanData[,c("ID","coat","AGCond","corr")]
# # combDataNew$ID=as.numeric(as.character(combDataNew$ID))
# # names(combDataNew)[3]="cond"
# # combDataOld=AGData
# # #ensure no duplicate IDs
# # combDataOld$ID=as.numeric(as.character(combDataOld$ID))+1000
# # combData=rbind(combDataNew,combDataOld)
# # combData$ID=as.factor(combData$ID)
# # combGLM=glmer(corr~cond*coat+(cond|ID),combData,binomial)
# # print(summary(combGLM))
# 
# #The below 4 lines of code take ~5 min to simulate
# AccCI95=confint(AccGLM,parm="AGCondInc:coaty") 
# print(AccCI95)
# AccCI90=confint(AccGLM,parm="AGCondInc:coaty",level=.9) 
# print(AccCI90)
# 
# # The below makes use of Adam and Galinsky's data, and is commented out since we do not have permission to share their data
# # AGCI95=confint(AGGLM,parm="condInc:coaty")
# # print(AGCI95)
# # AGCI90=confint(AGGLM,parm="condInc:coaty",level=.9)
# # print(AGCI90)
# 
# #Approximating d33% (takes ~6 hrs to simulate!)
# # AGGLMtest=AGGLM
# # fixef(AGGLMtest)["condInc:coaty"]=.66
# # set.seed(65401) #for reproducibility
# # powerSim(AGGLMtest,fixed("condInc:coaty","z"),nsim=10000)
# 
# #The below checks to see if the first 50 trials differ
# cleanData$block=ceiling(cleanData$trial/50)
# Acc2=glmer(corr~AGCond*LastCond+wordRepeat*inkRepeat+block+(AGCond|ID),cleanData,binomial)
# print(anova(Acc,Acc2))
# #test for pooling conditions ("cond" has 3 levels)
# Acc3=glmer(corr~cond*LastCond+wordRepeat*inkRepeat+(cond|ID),cleanData,binomial)
# print(anova(Acc,Acc3))
# 
# ########## Sequential Analyses ################
# 
# #best fitting model:
# RT=glmer(rt~AGCond*LastCond+wordRepeat*inkRepeat+(AGCond|ID),subset(cleanData,corr==1),family=inverse.gaussian(link="identity"))
# print(summary(RT))
# 
# RT2=glmer(rt~AGCond*LastCond+wordRepeat*inkRepeat+block+(AGCond|ID),subset(cleanData,corr==1),family=inverse.gaussian(link="identity"))
# print(anova(RT,RT2))
# #test for collapsing across neutral/congruent
# RT3=glmer(rt~cond*LastCond+wordRepeat*inkRepeat+(cond|ID),subset(cleanData,corr==1),family=inverse.gaussian(link="identity"))
# print(summary(RT3))
# 
# #######################################################
# #                      Figures                        #
# #######################################################
# 
# #initial function for summarizing within participant data for plotting using method from Morey (2008).
# # used from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# 
# summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
#   # Ensure that the betweenvars and withinvars are factors
#   factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],FUN=is.factor, FUN.VALUE=logical(1))
#   if (!all(factorvars)) {
#     nonfactorvars <- names(factorvars)[!factorvars]
#     message("Automatically converting the following non-factors to factors: ",paste(nonfactorvars, collapse = ", "))
#     data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
#   }
#   # Get the means from the un-normed data
#   datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
#   # Drop all the unused columns (these will be calculated with normed data)
#   datac$sd <- NULL
#   datac$se <- NULL
#   datac$ci <- NULL
#   # Norm each subject's data
#   ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
#   # This is the name of the new column
#   measurevar_n <- paste(measurevar, "_norm", sep="")
#   # Collapse the normed data - now we can treat between and within vars the same
#   ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
#   # Apply correction from Morey (2008) to the standard error and confidence interval
#   #  Get the product of the number of conditions of within-S variables
#   nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,FUN.VALUE=numeric(1)))
#   correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
#   # Apply the correction factor
#   ndatac$sd <- ndatac$sd * correctionFactor
#   ndatac$se <- ndatac$se * correctionFactor
#   ndatac$ci <- ndatac$ci * correctionFactor
#   # Combine the un-normed means with the normed results
#   merge(datac, ndatac)
# }
# 
# 
# ###### Figure 1: Stroop Errors
# 
# #########Source "summary funcs.R" before running#################
# library(ggplot2)
# data <- read.csv("cleanData.csv", head= TRUE)
# data$err=1-data$corr
# data$cond=relevel(data$cond,"neutral")
# data$cond=relevel(data$cond,"congruent")
# 
# dataSum <- summarySEwithin(data, measurevar="err", withinvars="cond", betweenvars="coat",idvar="ID")
# pd <- position_dodge(1)
# p<-ggplot(dataSum, aes(x=cond, y=err, fill=coat)) + 
#   geom_bar(stat="identity", position=pd) +  
#   geom_errorbar(aes(ymin=err-se, ymax=err+se), colour="black", width=.1, position=pd) +
#   xlab("Stimulus type") +
#   ylab("% Errors") +
#   scale_colour_hue(l=40) +  # Use darker colors, lightness=40
#   ggtitle("Stroop Errors")
# 
# ggsave("Acc.pdf",plot=p, width=4, height=4)
# 
# # Figure 2: Stroop RTs
# 
# data=subset(data,corr==1)
# 
# dataSum <- summarySEwithin(data, measurevar="rt", withinvars="cond", betweenvars="coat", idvar="ID")
# RtMax=floor(max(dataSum$rt)/25)+1
# RtMin=floor(min(dataSum$rt)/25)
# p<-ggplot(dataSum, aes(x=cond, y=rt, fill=coat)) + 
#   geom_bar(stat="identity", position=pd) +  
#   geom_errorbar(aes(ymin=rt-se, ymax=rt+se), colour="black", width=.1, position=pd) +
#   xlab("Stimulus type") +
#   ylab("Response Time (ms)") +
#   coord_cartesian(ylim=c(RtMin*25,RtMax*25)) +
#   scale_y_continuous(breaks=RtMin*25+25*0:(RtMax-RtMin)) +
#   scale_colour_hue(l=40) +  # Use darker colors, lightness=40
#   ggtitle("Stroop RT")
# ggsave("RT.pdf",plot=p, width=4, height=4)
# 
# #Figure 3: Small telescopes
# # Adapted from code by Uri Simonsohn (uws@wharton.upenn.edu)
# 
# h95=c(1.92,.49) #upper limits of 95% CIs
# h90=c(1.77,.42) #upper limits of 90% CIs
# d=c(.99,.02) #point estimates
# l95=c(.08,-.45) #lower limits of 95% CIs
# l90=c(.23,-.38) #lower limits of 90% CIs
# d33=.66 #lowest detectible effect under original
# 
# StudyLabels=c("Adam & Galinsky (2012)\nN=58 p * 50 t","This Study\nN=192 p * 150 t")
# 
# #save plot to file
# png("Telescopes.png",width=6000,height=5000, res=1000)
# x=c(1,3)
# #Margins for graph to allow y-label with two lines of text
# par(mar=c(4.1,6.1,.5,2.1))
# #Draw the point estimates
# plot(x,d, xlim=c(0,4.5),ylim=c(min(l95)-.2,max(h95)+.2),col='black',pch=c(15,16,16),cex=c(1.5,.75,.75),xlab="",xaxt="n",ylab="")
# #Label y-axis
# mtext("Parameter Estimate",side=2,line=4.2,cex=1.2)
# mtext("from logistic mixed-effects model",side=2,line=3,cex=1)
# #Label x-axis
# axis(side=1,at=x,labels=StudyLabels,line=1.25,tck=0,lwd=0)
# #Draw the CI lines
# for (i in 1:2)
# {
#   #90% CI
#   lines(c(x[i],x[i]),c(h90[i],l90[i]),lwd=2)
#   #95-90%
#   lines(c(x[i],x[i]),c(h95[i],h90[i]),lty=3,col="black",lwd=2)
#   lines(c(x[i],x[i]),c(l95[i],l90[i]),lty=3,col="black",lwd=2)
# }
# #Add line at 0
# abline(h=0,col=153)  
# #Add line at d33%
# lines(c(0,5),c(d33,d33),lty=2)
# #Text near the 33% line
# text(x=4,y=d33+.05,"Small Effect")
# text(x=4,y=d33-.05,"(d33%)")
# #Add CI legend
# legend(x=2,max(h95)+.2,legend=c("90% Confidence Interval","95% Confidence Interval"),cex=.85,lty=c(1,3),col=c("black","black"),lwd=2,bty="n")
# dev.off()