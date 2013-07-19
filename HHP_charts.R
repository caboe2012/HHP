# HHP_charts.R
# makes pretty charts
# TVN, started 21 may 2011
source("HHP_header.R")
source("HHP_functions.R")

total.time <- 0

tic()
right.a <- read.csv("~/Dropbox/raw/HHP/right_a.csv")
right.b <- read.csv("~/Dropbox/raw/HHP/right_b.csv")
right.c <- read.csv("~/Dropbox/raw/HHP/right_c.csv")
right.a <- right.a[,-c(1,6,7,8)]
right.b <- right.b[,-c(1,6,7,8)]
right.c <- right.c[,-c(1,6,7,8)]
toc()

library(lattice) 
bwplot(logdays ~ logdays, data=right.a, ylab='DaysInHospital',xlab='Predictor')

tic()
right.a.log <- right.a
right.b.log <- right.b
right.c.log <- right.c
transform <- c(5,6,7,8, 18:234)
right.a.log[,transform] <- log1p(right.a.log[,transform])
right.b.log[,transform] <- log1p(right.b.log[,transform])
right.c.log[,transform] <- log1p(right.c.log[,transform])
right.a.log[,"LabCount"] <- log1p(log1p(right.a.log[,"LabCount"]))
right.b.log[,"LabCount"] <- log1p(log1p(right.b.log[,"LabCount"]))
right.c.log[,"LabCount"] <- log1p(log1p(right.c.log[,"LabCount"]))
logfix <- c("SO","GYNECA","HEART2","HIPFX","METAB3","SEPSIS","IndependentLab", "InpatientHospital","UrgentCare")
right.a.log[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.a.log[,logfix])))))))
right.b.log[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.b.log[,logfix])))))))
right.c.log[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.c.log[,logfix])))))))
toc()

names <- colnames(right.a)[c(19:233)]

# qqHist_full
tic()
for(temp in names){
print(paste('full_',temp,sep=""))
png(paste("~/Dropbox/plots/HHP/qqHist/qqHist_",temp,"_all.png",sep=""),width=1280,height=800,bg="transparent")
par(mfrow=c(2,2),family="Ideal Sans Medium",ps=18) 
hist(right.b[,temp], prob=T, xlab='', main=paste('Histogram of ', temp, ' (all)')) 
lines(density(right.b[,temp],na.rm=T))
rug(jitter(right.b[,temp]))
qqPlot(right.b[,temp],main=paste('Normal QQ plot of ',temp,' (all)'),ylab=temp)

hist(right.b.log[,temp], prob=T, xlab='', main=paste('Histogram of ', temp, ' (all/logfix)')) 
lines(density(right.b.log[,temp],na.rm=T))
rug(jitter(right.b.log[,temp]))
qqPlot(right.b.log[,temp],main=paste('Normal QQ plot of ',temp, ' (all/logfix)'),ylab=temp)
dev.off()
}
toc()

# qqHist_nonzero
tic()
for(temp in names){
print(paste('nonzero_',temp,sep=""))
png(paste("~/Dropbox/plots/HHP/qqHist/qqHist_",temp,"_nonzero.png",sep=""),width=1280,height=800,bg="transparent")
par(mfrow=c(2,2),family="Ideal Sans Medium",ps=18) 
hist(right.b[,temp][right.b[,temp] >0], prob=T, xlab='', main=paste('Histogram of ', temp, ' (nonzero)')) 
lines(density(right.b[,temp][right.b[,temp] >0],na.rm=T))
rug(jitter(right.b[,temp][right.b[,temp] >0]))
qqPlot(right.b[,temp][right.b[,temp] >0],main=paste('Normal QQ plot of ',temp,' (nonzero)'),ylab=temp)

hist(right.b.log[,temp][right.b.log[,temp] >0], prob=T, xlab='', main=paste('Histogram of ', temp, ' (nonzero/logfix)')) 
lines(density(right.b.log[,temp][right.b.log[,temp] >0],na.rm=T))
rug(jitter(right.b.log[,temp][right.b.log[,temp] >0]))
qqPlot(right.b.log[,temp][right.b.log[,temp] >0],main=paste('Normal QQ plot of ',temp, ' (nonzero/logfix)'),ylab=temp)
dev.off()
}
toc()

# number of days in hospital, y2
png("~/Dropbox/plots/HHP/logHospital.png",width=1280,height=768,pointsize=14)
par(mfrow=c(2,1))
hist(log(sort.hospital.y2$DaysInHospital), main="log(Days in Hospital), year 2",family="Ideal Sans Book",col="lightgreen",xlim=c(0,log(18)),breaks=20)
hist(log(sort.hospital.y3$DaysInHospital), main="log(Days in Hospital), year 3",family="Ideal Sans Book",col="lightgreen",xlim=c(0,log(18)),breaks=20)
dev.off()



# charlson index and dsfs

png("~/Dropbox/plots/HHP/charlson_DSFS.png",width=1280,height=768,pointsize=18)
par(mfrow=c(2,2))
max.charlson <- max(table(sort.claims.y1$CharlsonIndex))
max.charlson.sick <- max(table(sort.claims.y1.sick$CharlsonIndex))
max.DSFS <- max(table(sort.claims.y1$DSFS))
max.DSFS.sick <- max(table(sort.claims.y1.sick$DSFS))
barplot(table(sort.claims.y1$CharlsonIndex), main="Charlson Index, All Y1 Claims",col="lightgreen",ylim=c(0,max.charlson + max.charlson * .1),family="Ideal Sans Book")
barplot(table(sort.claims.y1$DSFS), main="DSFS of Y1 Claims",col="lightgreen",ylim=c(0,max.DSFS + max.DSFS * .1),family="Ideal Sans Book")

barplot(table(sort.claims.y1.sick$CharlsonIndex), main=paste("Charlson Index, Y1 Sick Claims, sickthreshold = ", sickthreshold),col="lightgreen",,ylim=c(0,max.charlson.sick + max.charlson.sick * .1),family="Ideal Sans Book")
barplot(table(sort.claims.y1.sick$DSFS), main=paste("DSFS of Y1 Sick Claims = ", sickthreshold),col="lightgreen",ylim=c(0, max.DSFS.sick + max.DSFS.sick * .1),family="Ideal Sans Book")
dev.off()

# comparison of procedures

png("~/Dropbox/plots/HHP/procedures.png",width=1280,height=768,pointsize=14)
procedure.barplot <- barplot(table(sort.claims.y1$ProcedureGroup),main="Procedures performed in all Y1 claims",col="lightgreen",ylim=c(0,max(table(sort.claims.y1$ProcedureGroup))+10000),family="Ideal Sans Book")
legend("topright",paste(levels(sort.claims.y1$ProcedureGroup)[2:18],lookup.procedures$Description))
#text(1:18, 1:18, labels = 1:18,pos = 3)
dev.off()

png("~/Dropbox/plots/HHP/proceduressick.png",width=1280,height=768,pointsize=14)
procedure.sick.barplot <- barplot(table(sort.claims.y1.sick$ProcedureGroup),main=paste("Procedures performed in all Sick Y1 claims, sickthreshold = ",sickthreshold),col="lightgreen",ylim=c(0,max(table(sort.claims.y1.sick$ProcedureGroup))+1000),family="Ideal Sans Book")
legend("topright",paste(levels(sort.claims.y1.sick$ProcedureGroup)[2:18],lookup.procedures$Description))
dev.off()

png("~/Dropbox/plots/HHP/conditions.y1.png",width=1280,height=768,pointsize=14)
ggplot(sort.claims.y1)+geom_histogram(fill="sienna2",aes(x=PrimaryConditionGroup))+coord_flip()
dev.off()

ggplot(claims)+geom_histogram(fill="sienna2",aes(x=PCP))+coord_flip()
ggplot(claims[claims$any.hospital > 0,])+geom_histogram(fill="sienna2",aes(x=PCP))+coord_flip()

inpatient <- subset(sort.claims.y1,LengthOfStay!="")
ggplot(inpatient)+geom_histogram(aes(x=PrimaryConditionGroup,fill=LengthOfStay))+coord_flip()

# find distributiion of hospital utilization by claim
ggplot(sort.claims.y1)+geom_histogram(fill='firebrick',aes(x=LengthOfStay))+coord_flip()+scale_y_log10()
 
# find distribution of number of hospitalizations (not length) by patient 
# is there a cleaner way to do this?
Claims_Y1$hospitalized <- Claims_Y1$LengthOfStay!=""
num.hosp <- as.data.frame(table(Claims_Y1$MemberID,Claims_Y1$hospitalized))
num.hosp <- subset(num.hosp, Var2==TRUE)
ggplot(num.hosp) + geom_histogram(aes(x=Freq), fill='midnightblue')
# a little easier to see hot spotters with a rescaled y-axis (can't use log-scale for zero values)
ggplot(num.hosp) + geom_histogram(aes(x=Freq), fill='midnightblue', binwidth=0.5) + scale_y_sqrt()
