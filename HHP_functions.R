# Timekeepers
tic <- function(){a <<- Sys.time()}
toc <- function(){b <<- Sys.time()
                  print(b-a); total.time <<- total.time + b-a
                  }

HHPScore <- function (p, a) {sqrt(mean((log1p(p) - log1p(a))^2))}
HHPScoreLog1p <- function (log1p.p, log1p.a) {
    sqrt(mean((log1p.p - log1p.a)^2))
}
hhp.summary.l1p <- function (data, lev = NULL, model = NULL) {
    z <- c(HHP = HHPScoreLog1p(data[, "pred"], data[, "obs"]))
    return(z)
}

err  <- function(obs, pred){
  sqrt( 1/length(obs) * sum((log(pred+1) - log(obs+1))^2))
}

cutOff <- function(x, y=0, z=15){
    x <- ifelse(x < y, y, x)
    x <- ifelse(x > z, z, x)
    return(x)
  }

make.numeric <- function (cv, FUN = mean) {
### make a character vector numeric by splitting on '-'
    laply(strsplit(gsub("[^[:digit:]]+",
                         " ",
                         cv,
                         perl = TRUE),
                    " ",
                    fixed = TRUE),
           function (x) FUN(as.numeric(x)))
}

fixFactorsClaims.fn <- function(claims){
  print("Well-forming LoS and DSFS")
	# Length of Stay
	z <- make.numeric(claims$LengthOfStay)
	z.week <- grepl("week", claims$LengthOfStay, fixed = TRUE)
	z[z.week] <- z[z.week] * 7          # Weeks are 7 days
	z[is.nan(z)] <- 0
	claims$LengthOfStay <- z
	los.levels <- c("", "1 day", sprintf("%d days", 2:6),
	                "1- 2 weeks", "2- 4 weeks", "4- 8 weeks", "8-12 weeks",
	                "12-26 weeks", "26 weeks")
	claims$DSFS.claims.months <- make.numeric(claims$DSFS)
	## dsfs is an ordered factor and gives the ordering of the claims
	DSFS.levels <- c("0- 1 month", sprintf("%d-%2d months", 1:11, 2:12))
	claims$DSFS <- factor(claims$DSFS, levels = DSFS.levels, ordered = TRUE)
	## Index as numeric
	claims$CharlsonIndex.numeric <- make.numeric(claims$CharlsonIndex)
	claims$CharlsonIndex <- factor(claims$CharlsonIndex, ordered = TRUE)
	                              
	## lolfix for PayDelay
	claims$PayDelay[claims$PayDelay=="162+"] <- "163"
	claims$PayDelay <- as.integer(claims$PayDelay)
	return(claims)
}

add.charls.fn <- function(members){
  years <- c("1","2","3")
	print("Adding maxCharls to members")
	for(i in years){
	print(i)
		temp.df <- aggregate(CharlsonIndex.numeric ~ 
							 MemberID, eval(as.name(paste("clean",i,sep="."))), 
							 max)
		members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
		colnames(members)[length(colnames(members))] <- paste("maxCharls",i,sep=".")
	print("Adding meanCharls")
	print(i)
		temp.df <- aggregate(CharlsonIndex.numeric ~ 
							 MemberID, eval(as.name(paste("sort.claims",i,sep="."))), 
							 mean)
		members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
		colnames(members)[length(colnames(members))] <- paste("meanCharls",i,sep=".")	
	}
	return(members)
}

getCleanClaims <- function(x="Y1", y=hospital.y2){
	sand <- claims[claims$Year==x,]
	all.in <- sand$MemberID %in% y$MemberID
	sand <- sand[all.in,]
}

getCleanDrugs <- function(x="Y1", y=hospital.y2){
 	sand <- drug.count[drug.count$Year==x,]
 	all.in <- sand$MemberID %in% y$MemberID
 	sand <- sand[all.in,]
 }
 
 
getCleanLabs <- function(x="Y1", y=hospital.y2){
 	sand <- lab.count[lab.count$Year==x,]
 	all.in <- sand$MemberID %in% y$MemberID
 	sand <- sand[all.in,]
}

fixDrugs.fn <- function(drug.count){
  DSFS.levels <- c("0- 1 month", sprintf("%d-%2d months", 1:11, 2:12))
  drug.count$DrugCount <- as.integer(drug.count$DrugCount)
  drug.count$num.rx <- 1
	drug.count$DSFS.drug.months <- make.numeric(drug.count$DSFS)
	drug.count$DSFS <- factor(drug.count$DSFS, levels = DSFS.levels, ordered = TRUE)
	return(drug.count)
}

fixLabs.fn <- function(lab.count){
	DSFS.levels <- c("0- 1 month", sprintf("%d-%2d months", 1:11, 2:12))
	lab.count$LabCount <- as.integer(lab.count$LabCount)
	
  lab.count$num.labs <- 1
	lab.count$DSFS.lab.months <- make.numeric(lab.count$DSFS)
	lab.count$DSFS <- factor(lab.count$DSFS, levels = DSFS.levels, ordered = TRUE)
	return(lab.count)
}

compare.factors <- function(compare,sickthreshold,year,scientific=TRUE){
if(scientific){options(scipen = 6)}
temp <- merge(members,eval(as.name(paste("hospital.y",year,sep=""))),by.x="MemberID",by.y="MemberID",all.y=TRUE,sort=FALSE)
thesick <- temp$MemberID[temp$DaysInHospital > 0]

a <- sort(table(claims[,compare])/2668990)
a <- as.data.frame(cbind(attr(a,"name"),a[]))
colnames(a) <- c(compare,"all")
a$all <- as.character(a$all)
a$all <- as.numeric(a$all)

b <- sort(table(claims[,compare][claims$MemberID %in% thesick])/535558)
b <- as.data.frame(cbind(attr(b,"name"),b[]))
colnames(b) <- c(compare,"sick")
b$sick <- as.character(b$sick)
b$sick <- as.numeric(b$sick)

c <- merge(a,b,by.x=compare)
c$diff <- abs(c$all-c$sick)
c <- c[order(c$diff,decreasing=TRUE),]
return(c)
}

makeTab <- function(x,y) {
  temp <- table(x,y)
  class(temp) <- "matrix"
  temp <- as.data.frame(temp, stringsAsFactors = FALSE)
  temp <- cbind(row.names(temp),temp)
  temp[,1] <- as.numeric(as.character(temp[,1]))
  colnames(temp) <- c("MemberID",colnames(temp)[-1])
  temp
  }
  
mergeIt <- function(x,y=temp) { merge(x, y, by.x="MemberID",by.y="MemberID", all.x=TRUE, sort=FALSE)}


add.charls.fn <- function(members){
  years <- c("1","2","3")
	print("Adding maxCharls to members")
	for(i in years){
	print(i)
		temp.df <- aggregate(CharlsonIndex.numeric ~ 
							 MemberID, eval(as.name(paste("clean",i,sep="."))), 
							 max)
		members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
		colnames(members)[length(colnames(members))] <- paste("maxCharls",i,sep=".")
	print("Adding meanCharls")
	print(i)
		temp.df <- aggregate(CharlsonIndex.numeric ~ 
							 MemberID, eval(as.name(paste("clean",i,sep="."))), 
							 mean)
		members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
		colnames(members)[length(colnames(members))] <- paste("meanCharls",i,sep=".")	
	}
	return(members)
}

add.claimsTime.fn <- function(members){
	years <- c("1","2","3")
	print("Adding claims span to members")
 # initializaing counter
clean.1$numclaims <- 1
clean.2$numclaims <- 1
clean.3$numclaims <- 1
	for(i in years){
	print(i)
	temp.df <- aggregate(DSFS.claims.months ~ 
						MemberID, eval(as.name(paste("clean",i,sep="."))), 
						function(i){length(unique(i))})
	members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
	colnames(members)[length(colnames(members))] <- paste("claims.span",i,sep=".")			
	print("Adding number of claims to members")	
	print(i)
	temp.df <- aggregate(numclaims ~ MemberID,eval(as.name(paste("clean",i,sep="."))),sum)
	members <- merge(members,temp.df,by.x="MemberID",by.y="MemberID",all.x=TRUE,sort=FALSE)
	colnames(members)[length(colnames(members))] <- paste("numclaims",i,sep=".")
	}
	return(members)
}

compare.factors <- function(compare,sickthreshold,year,scientific=TRUE){
  if(scientific){options(scipen = 6)}
  temp <- merge(members,eval(as.name(paste("hospital.y",year,sep=""))),by.x="MemberID",by.y="MemberID",all.y=TRUE,sort=FALSE)
  thesick <- temp$MemberID[temp$DaysInHospital > 0]

  a <- sort(table(claims[,compare])/2668990)
  a <- as.data.frame(cbind(attr(a,"name"),a[]))
  colnames(a) <- c(compare,"all")
  a$all <- as.character(a$all)
  a$all <- as.numeric(a$all)

  b <- sort(table(claims[,compare][claims$MemberID %in% thesick])/535558)
  b <- as.data.frame(cbind(attr(b,"name"),b[]))
  colnames(b) <- c(compare,"sick")
  b$sick <- as.character(b$sick)
  b$sick <- as.numeric(b$sick)

  c <- merge(a,b,by.x=compare)
  c$diff <- abs(c$all-c$sick)
  c <- c[order(c$diff),]
  return(c)
}
