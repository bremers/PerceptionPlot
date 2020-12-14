setwd("/Users/bremers/Documents/Thesis/ThesisAnalyses_July2019")  ## adjust for your computer
library(Hmisc)

tb <- read.csv("sumall3.csv",sep=",",header=TRUE)

##Calculate grand averages per condition (means over all particpants)
agTb <- with(tb,aggregate(actual_matchedangle,list(condition=condition, proxangs= proxangs),mean))

agTb$SD <- with(tb,aggregate(actual_matchedangle,list(condition=condition, proxangs= proxangs),sd))$x

agTb$nrObs <- with(tb,aggregate(actual_matchedangle,list(condition=condition, proxangs= proxangs),length))$x


agTb$SE <- agTb$SD / sqrt(agTb$nrObs)  ## Standardized error of the mean
agTb$CI <- agTb$SE * 1.96  ### 95 % Confidence Interval

par(pty="s")
plot(c(0,0),c(0,0),type="n",main="Matched Angle across participants",xlim=c(40,115),ylim=c(25,130),xlab="Proximal Angle (degrees)",ylab="Matched Angle (degrees)")


cnd <- c("P","S","H")
col <- c("grey","blue","red")
type <- c(22,21,24)

# for error bar plotting
agTb2<-agTb[agTb$nrObs > 3, ]

for (counter in c(1,2,3))
{
	## give each condition specific layout
	cond <- cnd[counter]
	cl <- col[counter]
	tp <- type[counter]
	
	with(tb[tb$condition == cond,],points(proxangs, actual_matchedangle,col=cl,pch=tp))
	
	
	## now add means on top in a larger symbol. As error bars I now use95% CI
	for (vw in unique(agTb2[agTb2 $condition == cond,]$proxangs))
	{
	
		with(agTb[agTb $condition == cond & agTb$proxangs == vw,],points(proxangs, x,col="black",pch=tp,size=1.5,bg="black"))  ## slightly larger symbol for mean per condition
		with(agTb2[agTb2 $condition == cond & agTb2$proxangs == vw,],lines(c(proxangs, proxangs), c(x-CI,x+ CI)))
		with(agTb2[agTb2 $condition == cond & agTb2$proxangs == vw,],lines(c(proxangs-2, proxangs+2), c(x-CI,x-CI)))
		with(agTb2[agTb2 $condition == cond & agTb2$proxangs == vw,],lines(c(proxangs-2, proxangs+2), c(x+ CI,x+ CI)))
	}	
	
}


### nu ook een trend line fitten
#linetype <- c(3,2,1)
linet <- c(3,6,2)

for (counter in c(1,2,3))
{
	cond <- cnd[counter]
	cl <- col[counter]
	linetype <- linet[counter]
	
	model <- with(agTb2[agTb2 $condition == cond,],lm(x~proxangs))
	interc <- coef(model)[1]
	slope <- coef(model)[2]
	
	print(c(counter,interc,slope))
	
	### use intercept and slope to calculate specific values
	#lines(c(60,260),c((interc+60*slope),(interc+260*slope)),col=cl,lty=linetype)
}

minor.tick(nx=2, ny=2, tick.ratio=0.7)
minor.tick(nx=4, ny=4, tick.ratio=0.5)
minor.tick(nx=20, ny=20, tick.ratio=0.3)
#legend("topright", inset=.02, legend=c("Screen", "AR HUD", "Physical"), title="Display Type", pch=c(21,22,23),
      # col=c("blue","red","grey"), lty=c(6,2,3), cex=0.95, seg.len=5)

abline(0,1,col="black",lty="dotted")

legend("topleft", inset=.02, legend=c("Screen", "AR HUD", "Physical","Proximal Angle Size"), title="Legend", pch=c(21,24,22,NA),
 col=c("blue","red","grey","black"), lty=c(NA,NA,NA,3), cex=0.95, seg.len=2)
#use this lty if using trendlines:  lty=c(6,2,3,3),

	#with(agTb[agTb $condition == cond,],abline(lm(x~proxangs)))
	


### Heuristic for hypothesis tests with alpha at 0.05: If 95% CIs do not overlap, then there is a significant difference. If they do overlap, then it is harder to say. The more they overlap, the more likely that there is NOT a difference.
## So: in this case the 95% CI helps: it shows that "blue" is always different from grey.
## it also shows that some blue are different from others (the two in the middle are quite similar, the extremes are different)
## Grey overlaps a lot: the 95% CIs of each bar overlaps with the mean of the other conditions. No difference.


### Now let's test with anova:


with(tb,summary(aov(actual_matchedangle~condition*as.factor(proxangs)+Error(as.factor(participant)/(condition*as.factor(proxangs))))))

### let op: ik heb viewing height nu ook als factor genomen. Dus 150 cm is niet 5 keer zoveel als 30 cm.


### also calculate means and SDs per condition (I have not done that now)
with(tb,tapply(actual_matchedangle,condition,mean,stdev))

Prows<-agTb[(which(agTb$condition=="P")),]
Pmean<-mean(Prows$x)
Psd<-sd(Prows$x)
Srows<-agTb[(which(agTb$condition=="S")),]
Smean<-mean(Srows$x)
Ssd<-sd(Srows$x)
Hrows<-agTb[(which(agTb$condition=="H")),]
Hmean<-mean(Hrows$x)
Hsd<-sd(Hrows$x)

postHocHeight <- with(tb,aggregate(actual_matchedangle,list(proxangs = proxangs,participant=participant),mean))
with(postHocHeight,pairwise.t.test(x, proxangs,p.adjust.method="holm"))

radiansindegree<-0.0174532925
degreesinradian<-57.2957795
h<-c(0:280)/100
#lines(atan(1/h)*degreesinradian*2,col="black",lty="dotted")

### Text die in artikel kan staan (ik weet even niet alle namen zo snel, dus af en toe een letter waar een woord moet staan). In plaats van "condition" is het beter als je een inhoudelijke term hebt. "Condition" is ambigu. Is het "View type" (standing or sitting?)
## A 2 (condition: P or S) x 4 (viewingheight) repeated measures ANOVA found a main effect of condition, F(1, 9) = 88.03, p < .0001. Viewing angle was heigher in S (M = 81.3 degrees, SD = ...) compared to P (M = 44 degrees, SD = ...). There was also a main effect of viewing height, F(3,27) = 143, p < .0001. In Figure XX it can be seen that both conditions have a downward slope: matched angle decreases with an increase of viewing height.

### deze zin is wel heel belangrijk. Let op: ik gebruik hier de 95% CIs om te redeneren over waar wel en niet verschillen zijn. Het patroon is erg "in your face" duidelijk door de CIs (en losse punten)
### These main effect were also affected by an interaction effect between condition and viewing height, F(3, 27) = 25.32, p < .0001. As Figure XX illustrates, the interaction pattern is that in the S condition (blue) the matched angle increases with a decrease in viewing angle. By contrast, in the P condition (grey), there seems to be less or no effect of viewing height on the matched angle, as indicated by the strong overlap between the 95% confidence intervals.



### alternatief voor zin over main effect van vieuwing height:
### deze zin wellicht weglaten, maar voor zekerheid toegevoegd: (standaard is om bij "interactie effect" niet de hoofdeffecten te veel te interpreteren)
#Holm-corrected post-hoc test showed that all angles differed significantly from each other, with the exception of the 117 and 132 conditions.



### Zodra je ook de data hebt van de rode conditie kun je ook daarvan de slope en intercept uitrekenen. Ideaal gezien lijken die slope en intercept dan heel erg op elkaar. Zie boven voor heo ik slope en intercept uitreken.


### Plotting the expected angle
radiansindegree<-0.0174532925
degreesinradian<-57.2957795
#h<-c(60:270)/100
#angle<-atan(1/h)*degreesinradian*2
#baseline<-cbind(h,angle)
#plot((baseline),xlab="Viewing Height (cm)",main="Actual Angle based on viewing height",xlim=c(0.60,2.70),pch=20,cex=0.5,xaxt="n",ylim=c(0,150),ylab="Actual Angle (degrees)")
#axis(1, at=seq(1, 2.5, by=0.5), labels=c("100","150","200","250"))
#minor.tick(nx=5, ny=5, tick.ratio=0.5)


proxangs<-c(107,107,102,93,82,82,76,76,76,69,65,65,65,59,57,48)
agTb$proxangs<-proxangs
#plot(agTb$proxangs,agTb$x,col=agTb$condition,xlim=c(30,110),ylim=c(30,110),xlab="Proximal Angle Size",ylab="Matched Angle Size",main="Matched Angle Size versus Expected (Proximal/Stimulus Angle)")


#legend("topleft", inset=.02, legend=c("Screen", "AR HUD", "Physical"), title="Legend",pch=1,
#       col=c("green","black","red"), cex=0.95, seg.len=5)

#plot(agTb$proxangs,agTb$x,col=agTb$condition,xlim=c(110,30),ylim=c(30,110),xlab="Proximal Angle Size",ylab="Matched Angle Size",main="Matched Angle Size versus Expected (Proximal/Stimulus Angle)")
#plot(agTb$proxangs,agTb$x,col=agTb$condition,xlim=c(30,110),ylim=c(30,110),xlab="Proximal Angle Size",ylab="Matched Angle Size",main="Matched Angle Size versus Expected (Proximal/Stimulus Angle)")
