#Version 22 Oct 2019
# DATA ANALYSIS OF EXPERIMENT 2
library(ggplot2)
library(gdata)
library(knitr)
library(kableExtra)
library(devtools)

summarySE <- dget("summarySE.R")
multiplot <- dget("multiplot.R")

# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

#1. Importing data
# Data for graphs (for now)
# Import prepared comparison data between control conditions in Experiment 1 and Experiment 2
controlcomparisondata<- read.csv("ComparisonControlsR.csv")
# Import compared comparison data for full-partial angle plotting in Experiment 2
fullpartialcomparisondata <- read.csv("fullpartialcomparison.csv")
# Import data separated for 65 degree condition and 107 degree condition, for final visualisation
exp107data <- read.csv("Exp2_107forgraph.csv")
exp65data <- read.csv("Exp2_65forgraph.csv")


# Import full dataset (for statistical analysis)
exp2 <- read.csv("./experiment2.csv")

#first process the experimental data into the right format
exp2_ma <- unlist(exp2)
exp2_ma <- data.frame(exp2_ma[11:510])

#creating a vector for participant number, P01 to P05 with 100 repetitions each
p <- rep("P01",100); q <- rep("P02",100); r <- rep("P03",100); s <- rep("P04",100); t <- rep("P05",100)
participantvec <- c(p,q,r,s,t)

#creating a vector for high or low contrast - 10 repetitions for each condition
hc <- rep("HC",10); lc <- rep("LC",10)
indcontrastvec <- c(hc,hc,hc,lc,hc,hc,lc,lc,lc,hc) #per participant
contrastvec <- rep(indcontrastvec,5) #for 5 participants

#creating a vector for full or partial angle - 10 repetitions for each condition
fa <- rep("FA",10); pa <- rep("PA",10)
indfpvec <- c(fa,fa,pa,pa,pa,pa,pa,pa,pa,pa) #per participant
fpvec <- rep(indfpvec,5) #for 5 participants

#creating a vector for continuous or dashed line - 10 repetitions for each condition
cl <- rep("CONT",10); dl <- rep("DASH",10)
indclvec <- c(cl,cl,cl,dl,dl,cl,dl,cl,cl,dl) #per participant
clvec <- rep(indclvec,5) #for 5 participants

#creating a vector for anglesize - 10 repetitions for each condition
s <- rep("65",10); l <- rep("107",10)
indasvec <- c(s,l,s,l,s,l,s,l,s,l) #per participant
asvec <- rep(indasvec,5) #for 5 participants

#creating a vector for test or control - 10 repetitions for each condition
cc <- rep("CONTROL",10); tc <- rep("TEST",10)
intcvec <- c(cc,cc,tc,tc,tc,tc,tc,tc,tc,tc) #per participant
tcvec <- rep(intcvec,5) #for 5 participants

#adding all vectors to the dataset
exp2_full <- cbind(exp2_ma,participantvec,contrastvec,fpvec,clvec,asvec,tcvec)
colnames(exp2_full) <- c("matchedangle","participant","contrast","fullpartial","linetype","anglesize","controltest")


#2. Convert to long format for JASP originally (plots were based on this format)

### Make data in JASP format for Experiment 2 ## first matched angles, then calculate them into weighted angles
experiment2 <- read.csv("experiment2.csv")
experiment2data <- drop.levels(experiment2)
p1ex2longvecMA <-c( mean(as.numeric(as.character(experiment2data[,1]))), mean(as.numeric(as.character(experiment2data[,2]))), mean(as.numeric(as.character(experiment2data[,3]))), mean(as.numeric(as.character(experiment2data[,4]))), mean(as.numeric(as.character(experiment2data[,5]))), mean(as.numeric(as.character(experiment2data[,6]))), mean(as.numeric(as.character(experiment2data[,7]))), mean(as.numeric(as.character(experiment2data[,8]))), mean(as.numeric(as.character(experiment2data[,9]))), mean(as.numeric(as.character(experiment2data[,10]))))
p2ex2longvecMA <-c( mean(as.numeric(as.character(experiment2data[,11]))), mean(as.numeric(as.character(experiment2data[,12]))), mean(as.numeric(as.character(experiment2data[,13]))), mean(as.numeric(as.character(experiment2data[,14]))), mean(as.numeric(as.character(experiment2data[,15]))), mean(as.numeric(as.character(experiment2data[,16]))), mean(as.numeric(as.character(experiment2data[,17]))), mean(as.numeric(as.character(experiment2data[,18]))), mean(as.numeric(as.character(experiment2data[,19]))), mean(as.numeric(as.character(experiment2data[,20]))))
p3ex2longvecMA <-c( mean(as.numeric(as.character(experiment2data[,21]))), mean(as.numeric(as.character(experiment2data[,22]))), mean(as.numeric(as.character(experiment2data[,23]))), mean(as.numeric(as.character(experiment2data[,24]))), mean(as.numeric(as.character(experiment2data[,25]))), mean(as.numeric(as.character(experiment2data[,26]))), mean(as.numeric(as.character(experiment2data[,27]))), mean(as.numeric(as.character(experiment2data[,28]))), mean(as.numeric(as.character(experiment2data[,29]))), mean(as.numeric(as.character(experiment2data[,30]))))
p4ex2longvecMA <-c( mean(as.numeric(as.character(experiment2data[,31]))), mean(as.numeric(as.character(experiment2data[,32]))), mean(as.numeric(as.character(experiment2data[,33]))), mean(as.numeric(as.character(experiment2data[,34]))), mean(as.numeric(as.character(experiment2data[,35]))), mean(as.numeric(as.character(experiment2data[,36]))), mean(as.numeric(as.character(experiment2data[,37]))), mean(as.numeric(as.character(experiment2data[,38]))), mean(as.numeric(as.character(experiment2data[,39]))), mean(as.numeric(as.character(experiment2data[,40]))))
p5ex2longvecMA <-c( mean(as.numeric(as.character(experiment2data[,41]))), mean(as.numeric(as.character(experiment2data[,42]))), mean(as.numeric(as.character(experiment2data[,43]))), mean(as.numeric(as.character(experiment2data[,44]))), mean(as.numeric(as.character(experiment2data[,45]))), mean(as.numeric(as.character(experiment2data[,46]))), mean(as.numeric(as.character(experiment2data[,47]))), mean(as.numeric(as.character(experiment2data[,48]))), mean(as.numeric(as.character(experiment2data[,49]))), mean(as.numeric(as.character(experiment2data[,50]))))
widedataex2MA <- rbind(p1ex2longvecMA,p2ex2longvecMA,p3ex2longvecMA,p4ex2longvecMA,p5ex2longvecMA)
widedataframeex2MA <- data.frame(widedataex2MA)

names(widedataframeex2MA)[1]<-paste("Control_65")
names(widedataframeex2MA)[2]<-paste("Control_107")
names(widedataframeex2MA)[3]<-paste("HC_CTN_65")
names(widedataframeex2MA)[4]<-paste("LC_DS_107")
names(widedataframeex2MA)[5]<-paste("HC_DS_65")
names(widedataframeex2MA)[6]<-paste("HC_CTN_107")
names(widedataframeex2MA)[7]<-paste("LC_DS_65")
names(widedataframeex2MA)[8]<-paste("LC_CTN_107")
names(widedataframeex2MA)[9]<-paste("LC_CTN_65")
names(widedataframeex2MA)[10]<-paste("HC_DS_107")

write.csv(widedataframeex2MA, file = "Exp2_")

#3. Generate Plots
# A plot to compare experiment 1 and experiment 2
controlcomparisondata$Experiment = as.character(controlcomparisondata$Experiment)
newvector <- c(1,1,1,1,1)
exp2plotdata <- cbind(widedataframeex2MA,newvector)

p <- ggplot(controlcomparisondata, aes(x=Experiment, group=Experiment, fill=Experiment, y=X65)) + 
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(alpha=0.4, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Experiment")+
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (65?)")+
  theme(#axis.title.x="Experiment",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p

p <- ggplot(controlcomparisondata, aes(x=Experiment, group=Experiment, fill=Experiment, y=X107)) + 
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(alpha=0.4, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Experiment")+
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (107?)")+
  theme(#axis.title.x="Experiment",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p

# A plot to compare Full angles to Partial angles within Experiment 2
controlcomparisondata$Experiment = as.character(controlcomparisondata$Experiment)
newvector <- c(1,1,1,1,1)
exp2plotdata <- cbind(widedataframeex2MA,newvector)

p <- ggplot(fullpartialcomparisondata, aes(x=Full.Partial, group=Full.Partial, fill=Full.Partial, y=X65)) + 
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(alpha=0.4, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Angle Type")+
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (65?)")+
  theme(#axis.title.x="Angle Type",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p

p <- ggplot(fullpartialcomparisondata, aes(x=Full.Partial, group=Full.Partial, fill=Full.Partial, y=X107)) + 
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  #geom_boxplot(width=0.05)+
  geom_point(alpha=0.4, size =2.5)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Angle Type") +
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (107?)")+
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = "none",
    axis.line = element_line(colour = 'black', size = 0.6)) 
# annotate("text", x = 4, y = 25, label = "Some text")
p

# A plot of all conditions
###
p1 <- ggplot(exp65data, aes(x=Group, group=Group, fill=Group, y=Measurement)) + 
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(alpha=0.4, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Condition")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (65?)")+
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p1

p2 <- ggplot(exp107data, aes(x=Group, group=Group, fill=Group, y=Measurement)) + 
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(alpha=0.4, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Condition")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (107?)")+
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6))
p2

multiplot(p1,p2,cols=1)

#4. ANOVA
## Separate analysis to compare between full and partial
## The reason for this is that the experiment was designed as 2*2*2 for the partial angles, with only 2 full angles (not 2*2*2*2)
## We are only comparing the control stimuli (high contrast, continuous line) with partial angles of the same contrast and line type!
res.aov2 <- aov(matchedangle ~ participant * fullpartial * anglesize, data = exp2_full[which(exp2_full$contrast=="HC" & exp2_full$linetype=="CONT"),])
summary(res.aov2)
etaSquared( res.aov2, type = 2, anova = TRUE )

## Repeated measures ANOVA, 4-way (only partial angles)
str(exp2_full)
res.aov2 <- aov(matchedangle ~ participant * contrast * linetype * anglesize, data = exp2_full[which(exp2_full$fullpartial=="PA"),])
summary(res.aov2)
etaSquared( res.aov2, type = 2, anova = TRUE )


#5. Generate plots using exp2_full dataset, to check if it is the same
# skip comparison between experiment 1 and experiment 2
# below: comparison between full and partial angle
plot1 <- ggplot(exp2_full[exp2_full$anglesize == "65" & exp2_full$contrast=="HC" & exp2_full$linetype=="CONT", ], aes(x=fullpartial, y=matchedangle)) + 
  geom_jitter(alpha = 1/3)+
  #geom_violin(lwd=0.6)+
  #geom_point(alpha=0.4, size =2.5)+
  geom_boxplot(width=0.05)+
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  labs(y = "Matched Angle (degrees)")+
  labs(x = "Angle Type")+
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Matched angles (65 degrees), full angle vs. partial angle")+
  theme(#axis.title.x="Angle Type",
    #axis.title.y="Matched Angle (degrees)",
    #legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
plot1

plot2 <- ggplot(exp2_full[exp2_full$anglesize == "107" & exp2_full$contrast=="HC" & exp2_full$linetype=="CONT", ], aes(x=fullpartial, y=matchedangle)) + 
  geom_jitter(alpha = 1/3)+
  #geom_violin(lwd=0.6)+
  #geom_point(alpha=0.4, size =2.5)+
  geom_boxplot(width=0.05)+
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  labs(y = "Matched Angle (degrees)")+
  labs(x = "Angle Type")+
  scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Matched angles (107 degrees), full angle vs. partial angle")+
  theme(#axis.title.x="Angle Type",
    #axis.title.y="Matched Angle (degrees)",
    #legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
plot2

#### experiment 2 multiplot for ALL REMAINING CONDITIONS
p1 <- ggplot(exp2_full[exp2_full$anglesize == "65" & exp2_full$fullpartial=="PA", ], aes(x=linetype, colour=contrast, shape=contrast, y=matchedangle)) + 
  geom_jitter(alpha = 1/2)+
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  #geom_point(alpha=0.4, size =2.5)+
  geom_boxplot(width=0.2)+
  labs(y = "Matched Angle (degrees)")+
  labs(x = "Line Type")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Matched angles (65 degrees)")+
  theme(#axis.title.x="Line Type",
    #axis.title.y="Matched Angle (Degrees)",
    #legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p1

p2 <- ggplot(exp2_full[exp2_full$anglesize == "107" & exp2_full$fullpartial=="PA", ], aes(x=linetype, colour=contrast, shape=contrast, y=matchedangle)) + 
  geom_jitter(alpha = 1/2)+
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  #geom_point(alpha=0.4, size =2.5)+
  geom_boxplot(width=0.2)+
  labs(y = "Matched Angle (degrees)")+
  labs(x = "Line Type")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Matched angles (65 degrees)")+
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (Degrees)",
    #legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p2


#plot 5 different conditions (test conditions plus control condition) for angle size 65
exp107data2 <- read.csv("Exp2_107forgraph2.csv")
exp65data2 <- read.csv("Exp2_65forgraph2.csv")

p1 <- ggplot(Exp2_65forgraph2, aes(x=Group, group=Group, fill=Group, y=Measurement)) + 
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(shape=1, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Condition")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (65 degrees)")+
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p1

p2 <- ggplot(Exp2_107forgraph2, aes(x=Group, group=Group, fill=Group, y=Measurement)) + 
  geom_hline(yintercept=107,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(shape=1, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Condition")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(90,130)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (107 degrees)")+
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (?)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) 
p2

#running ANOVA
sumexp2<-summarySE(exp2_full, measurevar="matchedangle",groupvars=c("participant","contrast","fullpartial","linetype","anglesize","controltest"))
sumexp2subset<-sumexp2[which(sumexp2$controltest=="TEST"),]
res.aov2 <- aov(matchedangle ~ participant + contrast + linetype + anglesize, data = sumexp2subset)
summary(res.aov2)
etaSquared( res.aov2, type = 2, anova = TRUE )

write.csv(sumexp2subset,file="sumexp2subset.csv")
write.csv(sumexp2,file="sumexp2.csv")

### new part after discussion with Chris
with(sumexp2subset,summary(aov(matchedangle~anglesize*as.factor(linetype)*as.factor(contrast)+Error(as.factor(participant)/(anglesize*as.factor(linetype)*as.factor(contrast))))))


####### making agTb2 from exp2full to give similar plot to exp 1
agTb2 <- with(exp2_full,aggregate(matchedangle,list(participant=participant,contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),mean))

#agTb2$SD <- with(exp2_full,aggregate(matchedangle,list(participant=participant,contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),sd))$x

agTb2$nrObs <- 0.1*with(exp2_full,aggregate(matchedangle,list(participant=participant,contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),length))$x


#agTb2$SE <- agTb2$SD / sqrt(agTb2$nrObs)  ## Standardized error of the mean
#agTb2$CI <- agTb2$SE * 1.96  ### 95 % Confidence Interval
#agTb2$condition<-paste(agTb2$contrast,agTb2$linetype,agTb2$anglesize,agTb2$contrast,agTb2$fullpartial)


## using agTb3 to calculate mean, SD, SE and CI
agTb3 <- with(exp2_full,aggregate(matchedangle,list(contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),mean))
agTb3$SD <- with(exp2_full,aggregate(matchedangle,list(contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),sd))$x
agTb3$nrObs <- 0.1*with(exp2_full,aggregate(matchedangle,list(contrast=contrast, fullpartial=fullpartial,linetype=linetype,anglesize=anglesize),length))$x
agTb3$SE <- agTb3$SD / sqrt(agTb3$nrObs)  ## Standardized error of the mean
agTb3$CI <- agTb3$SE * 1.96  ### 95 % Confidence Interval
agTb3$condition<-paste(agTb3$contrast,agTb3$linetype,agTb3$anglesize,agTb3$contrast,agTb3$fullpartial)
labels<-c("Full Angle, Control","High Contrast, Continuous","Low Contrast, Continuous","High Contrast, Dashed","Low Contrast, Dashed","Full Angle, Control","High Contrast, Continuous","Low Contrast, Continuous", "High Contrast, Dashed", "Low Contrast, Dashed")

meanvec<-c(rep(agTb3$x[1],5),rep(agTb3$x[2],5),rep(agTb3$x[3],5),rep(agTb3$x[4],5),rep(agTb3$x[5],5),rep(agTb3$x[6],5),rep(agTb3$x[7],5),rep(agTb3$x[8],5),rep(agTb3$x[9],5),rep(agTb3$x[10],5))
CIvec<-c(rep(agTb3$CI[1],5),rep(agTb3$CI[2],5),rep(agTb3$CI[3],5),rep(agTb3$CI[4],5),rep(agTb3$CI[5],5),rep(agTb3$CI[6],5),rep(agTb3$CI[7],5),rep(agTb3$CI[8],5),rep(agTb3$CI[9],5),rep(agTb3$CI[10],5))

agTb2$mean<-meanvec
agTb2$CI<-CIvec

#### visualising AgTb2
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

p1 <- ggplot(agTb2[which(agTb2$anglesize==65),], aes(x=condition, group=condition, fill=condition, y=x)) + 
  geom_hline(yintercept=65,lwd=0.6,linetype="dotted")+
  #geom_violin(lwd=0.6)+
  geom_point(shape=1, size =2.5)+
  #geom_boxplot(width=0.05)+
  labs(y = "Matched Angle (?)")+
  labs(x = "Condition")+
  #scale_x_discrete("HCC","HCD","LCC","LCD")+
  #scale_fill_manual(values=c("#00BFC4", "snow2"))+
  ylim(40,80)+
  #scale_x_discrete()+
  theme_classic()+
  ggtitle("Mean matched angle (65 degrees)")+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), width=.2,
                position=position_nudge(-0.2)) +
  theme(#axis.title.x="Condition",
    #axis.title.y="Matched Angle (degrees)",
    legend.position = "none",
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.6)) +
  geom_point(aes(y=mean),shape=16, size =3,position=position_nudge(-0.2))+
  scale_x_discrete(breaks=unique(agTb2$condition), 
                   labels=addline_format(c("Full Angle, Control","High Contrast, Continuous","Low Contrast, Continuous","High Contrast, Dashed","Low Contrast, Dashed","Full Angle, Control","High Contrast, Continuous","Low Contrast, Continuous", "High Contrast, Dashed", "Low Contrast, Dashed")))
p1
