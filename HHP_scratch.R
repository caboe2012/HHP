source("HHP_header.R")
source("HHP_functions.R")

total.time <- 0 # calculating total CPU hours

  tic()
  claims <- read.csv(file = "~/Dropbox/raw/HHP/Claims.csv",
                          colClasses = c(
  	                          rep("factor", 7),
  	                          "character",    # paydelay
  	                          "character",  # LengthOfStay
  	                          "character",  # dsfs
  	                          "factor",     # PrimaryConditionGroup
  	                          "character",  # CharlsonIndex
  	                          "factor",		# ProcedureGroup
  	                          "integer"		# SupLOS
  	                          ),
  	                      comment.char = "",na.strings="")	
  toc()

 tic()
 members <- read.csv("~/Dropbox/raw/HHP/Members.csv",na.strings="")
  hospital.y2 <- read.csv("~/Dropbox/raw/HHP/DaysInHospital_Y2.csv")
  hospital.y3 <- read.csv("~/Dropbox/raw/HHP/DaysInHospital_Y3.csv")
  lookup.pcg <- read.csv("~/Dropbox/raw/HHP/Lookup PrimaryConditionGroup.csv")
  lookup.procedures <- read.csv("~/Dropbox/raw/HHP/Lookup ProcedureGroup.csv")
  drug.count <- read.csv("~/Dropbox/raw/HHP/DrugCount.csv")
  lab.count <- read.csv("~/Dropbox/raw/HHP/LabCount.csv")
  hospital.y4 <- read.csv(file="~/Dropbox/raw/HHP/Target.csv")
  toc()

  tic()
  drug.count <- fixDrugs.fn(drug.count)
  toc()
  
  tic()
  lab.count <- fixLabs.fn(lab.count)
  toc()

  tic()
  claims <- fixFactorsClaims.fn(claims)
  toc()

  tic()
  hospital.y2$logdays <- log1p(hospital.y2$DaysInHospital)
  hospital.y3$logdays <- log1p(hospital.y3$DaysInHospital)
  hospital.y4$logdays <- NA
  hospital.y2$bindays <- ifelse(hospital.y2$DaysInHospital > 0, 1, 0)
  hospital.y3$bindays <- ifelse(hospital.y3$DaysInHospital > 0, 1, 0)
  hospital.y4$bindays <- NA
  toc()
  
  tic()
  clean.1 <- getCleanClaims("Y1", hospital.y2)
  clean.2 <- getCleanClaims("Y2", hospital.y3)
  clean.3 <- getCleanClaims("Y3", hospital.y4)
  toc()
  
  tic()
  cleandrugs.1 <- getCleanDrugs("Y1", hospital.y2)
  cleandrugs.2 <- getCleanDrugs("Y2", hospital.y3)
  cleandrugs.3 <- getCleanDrugs("Y3", hospital.y4)
  toc()

  tic()
  cleanlabs.1 <- getCleanLabs("Y1", hospital.y2)
  cleanlabs.2 <- getCleanLabs("Y2", hospital.y3)
  cleanlabs.3 <- getCleanLabs("Y3", hospital.y4)
  toc()
 
  tic()
  PCPlookup.1 <- makeTab(clean.1$MemberID, clean.1$PCP)
  PCPlookup.2 <- makeTab(clean.2$MemberID, clean.2$PCP)
  PCPlookup.3 <- makeTab(clean.3$MemberID, clean.3$PCP)
  vendorlookup.1 <- makeTab(clean.1$MemberID, clean.1$Vendor)
  vendorlookup.2 <- makeTab(clean.2$MemberID, clean.2$Vendor)
  vendorlookup.3 <- makeTab(clean.3$MemberID, clean.3$Vendor)
  toc()

  # appending things to members
  tic()
  members <- add.charls.fn(members)
  members <- add.claimsTime.fn(members)
toc()

  ############################
  ############################
  ############################
  # creating the right files
tic()  
right.a <- merge(hospital.y2, members[,-c(6:9,12:15)], by.x="MemberID",by.y="MemberID", all.x=TRUE, sort=FALSE)
  colnames(right.a) <- gsub(".1","",colnames(right.a))
  right.b <- merge(hospital.y3, members[,-c(4,5,8:11,14:15)], by.x="MemberID",by.y="MemberID", all.x=TRUE, sort=FALSE)
  colnames(right.b) <- gsub(".2","",colnames(right.b))
  right.c <- merge(hospital.y4, members[,-c(4:7,10:13)], by.x="MemberID",by.y="MemberID", all.x=TRUE, sort=FALSE)
  colnames(right.c) <- gsub(".3","",colnames(right.c))
  toc()

  # Age/Sex as a boolean
tic()  
right.a$age1 <- 0
	right.a$age2 <- 0
	right.a$age3 <- 0
	right.a$age4 <- 0
	right.a$age5 <- 0
	right.a$age6 <- 0
	right.a$age7 <- 0
	right.a$age8 <- 0
	right.a$age9 <- 0
	right.a$agemissing <- 0
  right.a$age1[right.a$AgeAtFirstClaim == "0-9"] <- 1
	right.a$age2[right.a$AgeAtFirstClaim == "10-19"] <- 1
	right.a$age3[right.a$AgeAtFirstClaim == "20-29"] <- 1
	right.a$age4[right.a$AgeAtFirstClaim == "30-39"] <- 1
	right.a$age5[right.a$AgeAtFirstClaim == "40-49"] <- 1
	right.a$age6[right.a$AgeAtFirstClaim == "50-59"] <- 1
	right.a$age7[right.a$AgeAtFirstClaim == "60-69"] <- 1
	right.a$age8[right.a$AgeAtFirstClaim == "70-79"] <- 1
	right.a$age9[right.a$AgeAtFirstClaim == "80+"] <- 1
  right.a$agemissing[is.na(right.a$AgeAtFirstClaim)] <- 1
  right.b$age1 <- 0
  right.b$age2 <- 0
	right.b$age3 <- 0
	right.b$age4 <- 0
	right.b$age5 <- 0
	right.b$age6 <- 0
	right.b$age7 <- 0
	right.b$age8 <- 0
	right.b$age9 <- 0
  right.b$agemissing <- 0
	right.b$age1[right.b$AgeAtFirstClaim == "0-9"] <- 1
	right.b$age2[right.b$AgeAtFirstClaim == "10-19"] <- 1
	right.b$age3[right.b$AgeAtFirstClaim == "20-29"] <- 1
	right.b$age4[right.b$AgeAtFirstClaim == "30-39"] <- 1
	right.b$age5[right.b$AgeAtFirstClaim == "40-49"] <- 1
	right.b$age6[right.b$AgeAtFirstClaim == "50-59"] <- 1
	right.b$age7[right.b$AgeAtFirstClaim == "60-69"] <- 1
	right.b$age8[right.b$AgeAtFirstClaim == "70-79"] <- 1
	right.b$age9[right.b$AgeAtFirstClaim == "80+"] <- 1
  right.b$agemissing[is.na(right.b$AgeAtFirstClaim)] <- 1
  right.c$age1 <- 0
  right.c$age2 <- 0
	right.c$age3 <- 0
	right.c$age4 <- 0
	right.c$age5 <- 0
	right.c$age6 <- 0
	right.c$age7 <- 0
	right.c$age8 <- 0
	right.c$age9 <- 0
  right.c$agemissing <- 0
	right.c$age1[right.c$AgeAtFirstClaim == "0-9"] <- 1
	right.c$age2[right.c$AgeAtFirstClaim == "10-19"] <- 1
	right.c$age3[right.c$AgeAtFirstClaim == "20-29"] <- 1
	right.c$age4[right.c$AgeAtFirstClaim == "30-39"] <- 1
	right.c$age5[right.c$AgeAtFirstClaim == "40-49"] <- 1
	right.c$age6[right.c$AgeAtFirstClaim == "50-59"] <- 1
	right.c$age7[right.c$AgeAtFirstClaim == "60-69"] <- 1
	right.c$age8[right.c$AgeAtFirstClaim == "70-79"] <- 1
	right.c$age9[right.c$AgeAtFirstClaim == "80+"] <- 1
  right.c$agemissing[is.na(right.c$AgeAtFirstClaim)] <- 1
  right.a$F <- 0  
  right.b$F <- 0
	right.c$F <- 0
  right.a$F[right.a$Sex == "F"] <- 1
  right.b$F[right.b$Sex == "F"] <- 1
  right.c$F[right.c$Sex == "F"] <- 1
  right.a$M <- 0
  right.b$M <- 0
  right.c$M <- 0
  right.a$M[right.a$Sex == "M"] <- 1
  right.b$M[right.b$Sex == "M"] <- 1
  right.c$M[right.c$Sex == "M"] <- 1
  right.a$sexmissing <- 0
  right.b$sexmissing <- 0 
  right.c$sexmissing <- 0
  right.a$sexmissing[is.na(right.a$Sex)] <- 1
  right.b$sexmissing[is.na(right.b$Sex)] <- 1
  right.c$sexmissing[is.na(right.c$Sex)] <- 1
# interaction terms
right.a$age1.M <- 0 # AGE AND M
right.a$age2.M <- 0
right.a$age3.M <- 0
right.a$age4.M <- 0
right.a$age5.M <- 0
right.a$age6.M <- 0
right.a$age7.M <- 0
right.a$age8.M <- 0
right.a$age9.M <- 0
right.a$agemissing.M <- 0
right.b$age1.M <- 0
right.b$age2.M <- 0
right.b$age3.M <- 0
right.b$age4.M <- 0
right.b$age5.M <- 0
right.b$age6.M <- 0
right.b$age7.M <- 0
right.b$age8.M <- 0
right.b$age9.M <- 0
right.b$agemissing.M <- 0
right.c$age1.M <- 0
right.c$age2.M <- 0
right.c$age3.M <- 0
right.c$age4.M <- 0
right.c$age5.M <- 0
right.c$age6.M <- 0
right.c$age7.M <- 0
right.c$age8.M <- 0
right.c$age9.M <- 0
right.c$agemissing.M <- 0
right.a$age1.M[right.a$age1 == 1 & right.a$M == 1] <- 1
right.a$age2.M[right.a$age2 == 1 & right.a$M == 1] <- 1
right.a$age3.M[right.a$age3 == 1 & right.a$M == 1] <- 1
right.a$age4.M[right.a$age4 == 1 & right.a$M == 1] <- 1
right.a$age5.M[right.a$age5 == 1 & right.a$M == 1] <- 1
right.a$age6.M[right.a$age6 == 1 & right.a$M == 1] <- 1
right.a$age7.M[right.a$age7 == 1 & right.a$M == 1] <- 1
right.a$age8.M[right.a$age8 == 1 & right.a$M == 1] <- 1
right.a$age9.M[right.a$age9 == 1 & right.a$M == 1] <- 1
right.a$agemissing.M[right.a$agemissing == 1 & right.a$M == 1] <- 1
right.b$age1.M[right.b$age1 == 1 & right.b$M == 1] <- 1
right.b$age2.M[right.b$age2 == 1 & right.b$M == 1] <- 1
right.b$age3.M[right.b$age3 == 1 & right.b$M == 1] <- 1
right.b$age4.M[right.b$age4 == 1 & right.b$M == 1] <- 1
right.b$age5.M[right.b$age5 == 1 & right.b$M == 1] <- 1
right.b$age6.M[right.b$age6 == 1 & right.b$M == 1] <- 1
right.b$age7.M[right.b$age7 == 1 & right.b$M == 1] <- 1
right.b$age8.M[right.b$age8 == 1 & right.b$M == 1] <- 1
right.b$age9.M[right.b$age9 == 1 & right.b$M == 1] <- 1
right.b$agemissing.M[right.b$agemissing == 1 & right.b$M == 1] <- 1
right.c$age1.M[right.c$age1 == 1 & right.c$M == 1] <- 1
right.c$age2.M[right.c$age2 == 1 & right.c$M == 1] <- 1
right.c$age3.M[right.c$age3 == 1 & right.c$M == 1] <- 1
right.c$age4.M[right.c$age4 == 1 & right.c$M == 1] <- 1
right.c$age5.M[right.c$age5 == 1 & right.c$M == 1] <- 1
right.c$age6.M[right.c$age6 == 1 & right.c$M == 1] <- 1
right.c$age7.M[right.c$age7 == 1 & right.c$M == 1] <- 1
right.c$age8.M[right.c$age8 == 1 & right.c$M == 1] <- 1
right.c$age9.M[right.c$age9 == 1 & right.c$M == 1] <- 1
right.c$agemissing.M[right.c$agemissing == 1 & right.c$M == 1] <- 1
right.a$age1.F <- 0 # AGE AND F
right.a$age2.F <- 0
right.a$age3.F <- 0
right.a$age4.F <- 0
right.a$age5.F <- 0
right.a$age6.F <- 0
right.a$age7.F <- 0
right.a$age8.F <- 0
right.a$age9.F <- 0
right.a$agemissing.F <- 0
right.b$age1.F <- 0
right.b$age2.F <- 0
right.b$age3.F <- 0
right.b$age4.F <- 0
right.b$age5.F <- 0
right.b$age6.F <- 0
right.b$age7.F <- 0
right.b$age8.F <- 0
right.b$age9.F <- 0
right.b$agemissing.F <- 0
right.c$age1.F <- 0
right.c$age2.F <- 0
right.c$age3.F <- 0
right.c$age4.F <- 0
right.c$age5.F <- 0
right.c$age6.F <- 0
right.c$age7.F <- 0
right.c$age8.F <- 0
right.c$age9.F <- 0
right.c$agemissing.F <- 0
right.a$age1.F[right.a$age1 == 1 & right.a$F == 1] <- 1
right.a$age2.F[right.a$age2 == 1 & right.a$F == 1] <- 1
right.a$age3.F[right.a$age3 == 1 & right.a$F == 1] <- 1
right.a$age4.F[right.a$age4 == 1 & right.a$F == 1] <- 1
right.a$age5.F[right.a$age5 == 1 & right.a$F == 1] <- 1
right.a$age6.F[right.a$age6 == 1 & right.a$F == 1] <- 1
right.a$age7.F[right.a$age7 == 1 & right.a$F == 1] <- 1
right.a$age8.F[right.a$age8 == 1 & right.a$F == 1] <- 1
right.a$age9.F[right.a$age9 == 1 & right.a$F == 1] <- 1
right.a$agemissing.F[right.a$agemissing == 1 & right.a$F == 1] <- 1
right.b$age1.F[right.b$age1 == 1 & right.b$F == 1] <- 1
right.b$age2.F[right.b$age2 == 1 & right.b$F == 1] <- 1
right.b$age3.F[right.b$age3 == 1 & right.b$F == 1] <- 1
right.b$age4.F[right.b$age4 == 1 & right.b$F == 1] <- 1
right.b$age5.F[right.b$age5 == 1 & right.b$F == 1] <- 1
right.b$age6.F[right.b$age6 == 1 & right.b$F == 1] <- 1
right.b$age7.F[right.b$age7 == 1 & right.b$F == 1] <- 1
right.b$age8.F[right.b$age8 == 1 & right.b$F == 1] <- 1
right.b$age9.F[right.b$age9 == 1 & right.b$F == 1] <- 1
right.b$agemissing.F[right.b$agemissing == 1 & right.b$F == 1] <- 1
right.c$age1.F[right.c$age1 == 1 & right.c$F == 1] <- 1
right.c$age2.F[right.c$age2 == 1 & right.c$F == 1] <- 1
right.c$age3.F[right.c$age3 == 1 & right.c$F == 1] <- 1
right.c$age4.F[right.c$age4 == 1 & right.c$F == 1] <- 1
right.c$age5.F[right.c$age5 == 1 & right.c$F == 1] <- 1
right.c$age6.F[right.c$age6 == 1 & right.c$F == 1] <- 1
right.c$age7.F[right.c$age7 == 1 & right.c$F == 1] <- 1
right.c$age8.F[right.c$age8 == 1 & right.c$F == 1] <- 1
right.c$age9.F[right.c$age9 == 1 & right.c$F == 1] <- 1
right.c$agemissing.F[right.c$agemissing == 1 & right.c$F == 1] <- 1
right.a$age1.sexmissing <- 0 # AGE AND MISSING SEX
right.a$age2.sexmissing <- 0
right.a$age3.sexmissing <- 0
right.a$age4.sexmissing <- 0
right.a$age5.sexmissing <- 0
right.a$age6.sexmissing <- 0
right.a$age7.sexmissing <- 0
right.a$age8.sexmissing <- 0
right.a$age9.sexmissing <- 0
right.a$agemissing.sexmissing <- 0
right.b$age1.sexmissing <- 0
right.b$age2.sexmissing <- 0
right.b$age3.sexmissing <- 0
right.b$age4.sexmissing <- 0
right.b$age5.sexmissing <- 0
right.b$age6.sexmissing <- 0
right.b$age7.sexmissing <- 0
right.b$age8.sexmissing <- 0
right.b$age9.sexmissing <- 0
right.b$agemissing.sexmissing <- 0
right.c$age1.sexmissing <- 0
right.c$age2.sexmissing <- 0
right.c$age3.sexmissing <- 0
right.c$age4.sexmissing <- 0
right.c$age5.sexmissing <- 0
right.c$age6.sexmissing <- 0
right.c$age7.sexmissing <- 0
right.c$age8.sexmissing <- 0
right.c$age9.sexmissing <- 0
right.c$agemissing.sexmissing <- 0
right.a$age1.sexmissing[right.a$age1 == 1 & right.a$sexmissing == 1] <- 1
right.a$age2.sexmissing[right.a$age2 == 1 & right.a$sexmissing == 1] <- 1
right.a$age3.sexmissing[right.a$age3 == 1 & right.a$sexmissing == 1] <- 1
right.a$age4.sexmissing[right.a$age4 == 1 & right.a$sexmissing == 1] <- 1
right.a$age5.sexmissing[right.a$age5 == 1 & right.a$sexmissing == 1] <- 1
right.a$age6.sexmissing[right.a$age6 == 1 & right.a$sexmissing == 1] <- 1
right.a$age7.sexmissing[right.a$age7 == 1 & right.a$sexmissing == 1] <- 1
right.a$age8.sexmissing[right.a$age8 == 1 & right.a$sexmissing == 1] <- 1
right.a$age9.sexmissing[right.a$age9 == 1 & right.a$sexmissing == 1] <- 1
right.a$agemissing.sexmissing[right.a$agemissing == 1 & right.a$sexmissing == 1] <- 1
right.b$age1.sexmissing[right.b$age1 == 1 & right.b$sexmissing == 1] <- 1
right.b$age2.sexmissing[right.b$age2 == 1 & right.b$sexmissing == 1] <- 1
right.b$age3.sexmissing[right.b$age3 == 1 & right.b$sexmissing == 1] <- 1
right.b$age4.sexmissing[right.b$age4 == 1 & right.b$sexmissing == 1] <- 1
right.b$age5.sexmissing[right.b$age5 == 1 & right.b$sexmissing == 1] <- 1
right.b$age6.sexmissing[right.b$age6 == 1 & right.b$sexmissing == 1] <- 1
right.b$age7.sexmissing[right.b$age7 == 1 & right.b$sexmissing == 1] <- 1
right.b$age8.sexmissing[right.b$age8 == 1 & right.b$sexmissing == 1] <- 1
right.b$age9.sexmissing[right.b$age9 == 1 & right.b$sexmissing == 1] <- 1
right.b$agemissing.sexmissing[right.b$agemissing == 1 & right.b$sexmissing == 1] <- 1
right.c$age1.sexmissing[right.c$age1 == 1 & right.c$sexmissing == 1] <- 1
right.c$age2.sexmissing[right.c$age2 == 1 & right.c$sexmissing == 1] <- 1
right.c$age3.sexmissing[right.c$age3 == 1 & right.c$sexmissing == 1] <- 1
right.c$age4.sexmissing[right.c$age4 == 1 & right.c$sexmissing == 1] <- 1
right.c$age5.sexmissing[right.c$age5 == 1 & right.c$sexmissing == 1] <- 1
right.c$age6.sexmissing[right.c$age6 == 1 & right.c$sexmissing == 1] <- 1
right.c$age7.sexmissing[right.c$age7 == 1 & right.c$sexmissing == 1] <- 1
right.c$age8.sexmissing[right.c$age8 == 1 & right.c$sexmissing == 1] <- 1
right.c$age9.sexmissing[right.c$age9 == 1 & right.c$sexmissing == 1] <- 1
right.c$agemissing.sexmissing[right.c$agemissing == 1 & right.c$sexmissing == 1] <- 1
toc()

# charlson counts
tic()
temp <- makeTab(clean.1$MemberID,clean.1$CharlsonIndex.numeric)
colnames(temp)[-1] <-gsub("^","ChCo.",colnames(temp[,-1]))
right.a <- mergeIt(right.a)
temp <- makeTab(clean.2$MemberID,clean.2$CharlsonIndex.numeric)
colnames(temp)[-1] <-gsub("^","ChCo.",colnames(temp[,-1]))
right.b <- mergeIt(right.b)
temp <- makeTab(clean.3$MemberID,clean.3$CharlsonIndex.numeric)
colnames(temp)[-1] <-gsub("^","ChCo.",colnames(temp[,-1]))
right.c <- mergeIt(right.c)
right.a$ChIs.0 <- 0
right.b$ChIs.0 <- 0
right.c$ChIs.0 <- 0
right.a$ChIs.0[which(right.a$ChCo.0 > 0)] <- 1
right.b$ChIs.0[which(right.b$ChCo.0 > 0)] <- 1
right.c$ChIs.0[which(right.c$ChCo.0 > 0)] <- 1
right.a$ChIs.1.5 <- 0
right.b$ChIs.1.5 <- 0
right.c$ChIs.1.5 <- 0
right.a$ChIs.1.5[which(right.a$ChCo.1.5 > 0)] <- 1
right.b$ChIs.1.5[which(right.b$ChCo.1.5 > 0)] <- 1
right.c$ChIs.1.5[which(right.c$ChCo.1.5 > 0)] <- 1
right.a$ChIs.3.5 <- 0
right.b$ChIs.3.5 <- 0
right.c$ChIs.3.5 <- 0
right.a$ChIs.3.5[which(right.a$ChCo.3.5 > 0)] <- 1
right.b$ChIs.3.5[which(right.b$ChCo.3.5 > 0)] <- 1
right.c$ChIs.3.5[which(right.c$ChCo.3.5 > 0)] <- 1
right.a$ChIs.5 <- 0
right.b$ChIs.5 <- 0
right.c$ChIs.5 <- 0
right.a$ChIs.5[which(right.a$ChCo.5 > 0)] <- 1
right.b$ChIs.5[which(right.b$ChCo.5 > 0)] <- 1
right.c$ChIs.5[which(right.c$ChCo.5 > 0)] <- 1
toc()

# Hospital stuff
tic()
right.a$NoHospital <- 0
right.b$NoHospital <- 0
right.c$NoHospital <- 0
right.a$NoHospital[which(right.a$bindays == 0)] <- 1
right.b$NoHospital[which(right.b$bindays == 0)] <- 1
right.c$NoHospital[which(right.c$bindays == 0)] <- 1
right.a$HospitalTen <- 0
right.b$HospitalTen <- 0
right.c$HospitalTen <- 0
right.a$HospitalTen[which(right.a$DaysInHospital > 10)] <- 1
right.b$HospitalTen[which(right.b$DaysInHospital > 10)] <- 1
right.c$HospitalTen[which(right.c$DaysInHospital > 10)] <- 1
toc()

  # appending PCG
tic()
  temp <- makeTab(clean.1$MemberID, clean.1$PrimaryConditionGroup)
  right.a <- mergeIt(right.a)
  temp <- makeTab(clean.2$MemberID, clean.2$PrimaryConditionGroup)
  right.b <- mergeIt(right.b)
  temp <- makeTab(clean.3$MemberID, clean.3$PrimaryConditionGroup)
  right.c <- mergeIt(right.c)
toc()

  # appending PlaceSvc
tic()
  temp <- makeTab(clean.1$MemberID, clean.1$PlaceSvc)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.a <- mergeIt(right.a)
  temp <- makeTab(clean.2$MemberID, clean.2$PlaceSvc)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.b <- mergeIt(right.b)
  temp <- makeTab(clean.3$MemberID, clean.3$PlaceSvc)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.c <- mergeIt(right.c)
toc()

# appending specialty
tic()
  temp <- makeTab(clean.1$MemberID, clean.1$Specialty)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.a <- mergeIt(right.a)
  temp <- makeTab(clean.2$MemberID, clean.2$Specialty)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.b <- mergeIt(right.b)
  temp <- makeTab(clean.3$MemberID, clean.3$Specialty)
  colnames(temp) <- gsub(" ","",colnames(temp))
  right.c <- mergeIt(right.c)
toc()

  # appending procedure
tic()
  temp <- makeTab(clean.1$MemberID, clean.1$ProcedureGroup)
  right.a <- mergeIt(right.a)
  temp <- makeTab(clean.2$MemberID, clean.2$ProcedureGroup)
  right.b <- mergeIt(right.b)
  temp <- makeTab(clean.3$MemberID, clean.3$ProcedureGroup)
  right.c <- mergeIt(right.c)
toc()

  ######################################################################
  #adding LengthOfStay
tic()  
temp <- aggregate(LengthOfStay ~ MemberID,clean.1,sum)
  right.a <- mergeIt(right.a)
  temp <- aggregate(LengthOfStay ~ MemberID,clean.2,sum)
  right.b <- mergeIt(right.b)
  temp <- aggregate(LengthOfStay ~ MemberID,clean.3,sum)
  right.c <- mergeIt(right.c)
right.a$LoS50 <- 0
right.b$LoS50 <- 0
right.c$LoS50 <- 0
right.a$LoS50[right.a$LengthOfStay > 50] <- 1
right.b$LoS50[right.b$LengthOfStay > 50] <- 1
right.c$LoS50[right.c$LengthOfStay > 50] <- 1
  toc()



  # add DSFS
tic()
   temp <- aggregate(DSFS.claims.months ~ MemberID,clean.1,max)
   right.a <- mergeIt(right.a)
   temp <- aggregate(DSFS.claims.months ~ MemberID,clean.2,max)
   right.b <- mergeIt(right.b)
   temp <- aggregate(DSFS.claims.months ~ MemberID,clean.3,max)
   right.c <- mergeIt(right.c)
toc()

tic()
  temp <- aggregate(DrugCount ~ MemberID,cleandrugs.1,sum)
  right.a <- mergeIt(right.a)
  temp <- aggregate(DrugCount ~ MemberID,cleandrugs.2,sum)
  right.b <- mergeIt(right.b)
  temp <- aggregate(DrugCount ~ MemberID,cleandrugs.3,sum)
  right.c <- mergeIt(right.c)
toc()

tic()  
  temp <- aggregate(num.rx ~ MemberID,cleandrugs.1,sum)
  right.a <- mergeIt(right.a)
  temp <- aggregate(num.rx ~ MemberID,cleandrugs.2,sum)
  right.b <- mergeIt(right.b)
  temp <- aggregate(num.rx ~ MemberID,cleandrugs.3,sum)
  right.c <- mergeIt(right.c)
toc()

tic()
   temp <- aggregate(DSFS.drug.months ~ MemberID,cleandrugs.1,max)
   right.a <- mergeIt(right.a)
   temp <- aggregate(DSFS.drug.months ~ MemberID,cleandrugs.2,max)
   right.b <- mergeIt(right.b)
   temp <- aggregate(DSFS.drug.months ~ MemberID,cleandrugs.3,max)
   right.c <- mergeIt(right.c)
toc()
  # PCP. big.
  # PCP.compare <- compare.factors("PCP", 0,"2")
  
  
  # play with this parameter
tic()  
window.PCP <-
    c("1303","2136","2448","2469","3394","4025","4313","4523","5300","9524","10164","11148","13281","16757","18175","18880","20090","20893","21146","21579","22193","23056","26051","27467","30569","30870","32724","33193","33303","33843","35565","35832","36452","36955","36990","37301","37759","37796","38110","38583","38762","39372","39946","40607","41370","42381","43790","44164","44537","46162","46795","47414","48905","51763","56126","59950","62284","62871","63771","64709","70119","70171","70222","70553","70686","71040","71847","72000","72351","73550","73982","74354","75037","75876","76634","77134","78718","80381","80533","81146","82373","86472","86510","86658","86723","87960","88511","88661","89127","90868","91972","92411","93075","94201","94891","96614","98627","98900","99068","99196") 
  temp <- PCPlookup.1[,c("MemberID",window.PCP)]
  colnames(temp)[-1] <-gsub("^","PCP.",colnames(temp[,-1]))
  right.a <- mergeIt(right.a)
  temp <- PCPlookup.2[,c("MemberID",window.PCP)]
  colnames(temp)[-1] <-gsub("^","PCP.",colnames(temp[,-1]))
  right.b <- mergeIt(right.b)
  temp <- PCPlookup.3[,c("MemberID",window.PCP)]
  colnames(temp)[-1] <-gsub("^","PCP.",colnames(temp[,-1]))
  right.c <- mergeIt(right.c)
toc()

  # Vendor. Even bigger.
tic()
vendor.compare <- compare.factors("Vendor", 0,"2")
  # play with this parameter
  window.vendor <-
    c("9717","2610","3194","3556","6476","1110","1224","1403","1526","1648","2400","2518","2536","2862","3066","3274","3698","4254","4725","4913","4962","5054","5597","5606","6178","7063","7850","7912","9722") 
  window.vendor <- as.character(tail(vendor.compare$Vendor,20))
  temp <- vendorlookup.1[,c("MemberID",window.vendor)]
  colnames(temp)[-1] <-gsub("^","vendor.",colnames(temp[,-1]))
  right.a <- mergeIt(right.a)
  temp <- vendorlookup.2[,c("MemberID",window.vendor)]
  colnames(temp)[-1] <-gsub("^","vendor.",colnames(temp[,-1]))
  right.b <- mergeIt(right.b)
  temp <- vendorlookup.3[,c("MemberID",window.vendor)]
  colnames(temp)[-1] <-gsub("^","vendor.",colnames(temp[,-1]))
  right.c <- mergeIt(right.c)
toc()  

#  temp <- makeTab(clean.1$MemberID, clean.1$LengthOfStay)
#  colnames(temp)[-1] <-gsub("^","LoS.",colnames(temp[,-1]))
#  right.a <- mergeIt(right.a)
#  temp <- makeTab(clean.2$MemberID, clean.2$LengthOfStay)
#  colnames(temp)[-1] <-gsub("^","LoS.",colnames(temp[,-1]))
#  right.b <- mergeIt(right.b)
#  temp <- makeTab(clean.3$MemberID, clean.3$LengthOfStay)
#  colnames(temp)[-1] <-gsub("^","LoS.",colnames(temp[,-1]))
#  right.c <- mergeIt(right.c)
  
 #adding SupLOS
   temp <- makeTab(clean.1$MemberID, clean.1$SupLOS)
  colnames(temp)[-1] <- gsub("^","SupLos.",colnames(temp[,-1])) 
  right.a <- mergeIt(right.a)
   temp <- makeTab(clean.2$MemberID, clean.2$SupLOS)
  colnames(temp)[-1] <- gsub("^","SupLos.",colnames(temp[,-1])) 
   right.b <- mergeIt(right.b)
   temp <- makeTab(clean.3$MemberID, clean.3$SupLOS)
  colnames(temp)[-1] <- gsub("^","SupLos.",colnames(temp[,-1])) 
   right.c <- mergeIt(right.c)

#     # # add paydelay
#   temp <- aggregate(PayDelay ~ MemberID,clean.1,max)
#   right.a <- mergeIt(right.a)
#   temp <- aggregate(PayDelay ~ MemberID,clean.2,max)
#   right.b <- mergeIt(right.b)
#   temp <- aggregate(PayDelay ~ MemberID,clean.3,max)
#   right.c <- mergeIt(right.c)
#   
  #now for labs
  temp <- aggregate(LabCount ~ MemberID,cleanlabs.1,sum)
  right.a <- mergeIt(right.a)
  temp <- aggregate(LabCount ~ MemberID,cleanlabs.2,sum)
  right.b <- mergeIt(right.b)
  temp <- aggregate(LabCount ~ MemberID,cleanlabs.3,sum)
  right.c <- mergeIt(right.c)

 #   temp <- aggregate(num.labs ~ MemberID,cleanlabs.1,sum)
#   right.a <- mergeIt(right.a)
#   temp <- aggregate(num.labs ~ MemberID,cleanlabs.2,sum)
#   right.b <- mergeIt(right.b)
#   temp <- aggregate(num.labs ~ MemberID,cleanlabs.3,sum)
#   right.c <- mergeIt(right.c)
#   
#    temp <- aggregate(DSFS.lab.months ~ MemberID,cleanlabs.1,max)
#     right.a <- mergeIt(right.a)
#     temp <- aggregate(DSFS.lab.months ~ MemberID,cleanlabs.2,max)
#     right.b <- mergeIt(right.b)
#     temp <- aggregate(DSFS.lab.months ~ MemberID,cleanlabs.3,max)
#     right.c <- mergeIt(right.c)
 
  
# truncated claims fix (not helpful)
#right.a[right.a$numclaims == 43 & right.a$ClaimsTruncated == 1,"numclaims"] <- 44  
#right.b[right.b$numclaims == 43 & right.b$ClaimsTruncated == 1,"numclaims"] <- 44
#right.c[right.c$numclaims == 44 & right.c$ClaimsTruncated == 1,"numclaims"] <- 45

# pregnancy stuff
right.a$is.preggo <- 0
right.b$is.preggo <- 0
right.c$is.preggo <- 0
right.a$is.preggo[right.a$PRGNCY > 0] <- 1
right.b$is.preggo[right.b$PRGNCY > 0] <- 1
right.c$is.preggo[right.c$PRGNCY > 0] <- 1
right.a$preggo.labtests <- right.a$is.preggo * right.a$LabCount
right.b$preggo.labtests <- right.b$is.preggo * right.b$LabCount
right.c$preggo.labtests <- right.c$is.preggo * right.c$LabCount
clean.1$preggo.inpat <- 0
clean.1$preggo.inpat[which(clean.1$PrimaryConditionGroup == "PRGNCY" & clean.1$PlaceSvc == "Inpatient Hospital")] <- 1
clean.2$preggo.inpat <- 0
clean.2$preggo.inpat[which(clean.2$PrimaryConditionGroup == "PRGNCY" & clean.2$PlaceSvc == "Inpatient Hospital")] <- 1
clean.3$preggo.inpat <- 0
clean.3$preggo.inpat[which(clean.3$PrimaryConditionGroup == "PRGNCY" & clean.3$PlaceSvc == "Inpatient Hospital")] <- 1
temp <- aggregate(preggo.inpat ~ MemberID, clean.1, sum)
right.a <- mergeIt(right.a)
temp <- aggregate(preggo.inpat ~ MemberID, clean.2, sum)
right.b <- mergeIt(right.b)
temp <- aggregate(preggo.inpat ~ MemberID, clean.3, sum)
right.c <- mergeIt(right.c)
right.a$preggo.male <- 0
right.b$preggo.male <- 0
right.c$preggo.male <- 0
right.a$preggo.male[right.a$PRGNCY > 0 & right.a$M > 0] <- 1
right.b$preggo.male[right.b$PRGNCY > 0 & right.b$M > 0] <- 1
right.c$preggo.male[right.c$PRGNCY > 0 & right.c$M > 0] <- 1
right.a$age3.preggo <- 0
right.b$age3.preggo <- 0
right.c$age3.preggo <- 0
right.a$age3.preggo[right.a$age3 == 1 & right.a$is.preggo == 1] <- 1
right.b$age3.preggo[right.b$age3 == 1 & right.b$is.preggo == 1] <- 1
right.c$age3.preggo[right.c$age3 == 1 & right.c$is.preggo == 1] <- 1
right.a$age4.preggo <- 0
right.b$age4.preggo <- 0
right.c$age4.preggo <- 0
right.a$age4.preggo[right.a$age4 == 1 & right.a$is.preggo == 1] <- 1
right.b$age4.preggo[right.b$age4 == 1 & right.b$is.preggo == 1] <- 1
right.c$age4.preggo[right.c$age4 == 1 & right.c$is.preggo == 1] <- 1
right.a$age5.preggo <- 0
right.b$age5.preggo <- 0
right.c$age5.preggo <- 0
right.a$age5.preggo[right.a$age5 == 1 & right.a$is.preggo == 1] <- 1
right.b$age5.preggo[right.b$age5 == 1 & right.b$is.preggo == 1] <- 1
right.c$age5.preggo[right.c$age5 == 1 & right.c$is.preggo == 1] <- 1
right.a$age6.preggo <- 0
right.b$age6.preggo <- 0
right.c$age6.preggo <- 0
right.a$age6.preggo[right.a$age6 == 1 & right.a$is.preggo == 1] <- 1
right.b$age6.preggo[right.b$age6 == 1 & right.b$is.preggo == 1] <- 1
right.c$age6.preggo[right.c$age6 == 1 & right.c$is.preggo == 1] <- 1
right.a$age7.preggo <- 0
right.b$age7.preggo <- 0
right.c$age7.preggo <- 0
right.a$age7.preggo[right.a$age7 == 1 & right.a$is.preggo == 1] <- 1
right.b$age7.preggo[right.b$age7 == 1 & right.b$is.preggo == 1] <- 1
right.c$age7.preggo[right.c$age7 == 1 & right.c$is.preggo == 1] <- 1
right.a$age8.preggo <- 0
right.b$age8.preggo <- 0
right.c$age8.preggo <- 0
right.a$age8.preggo[right.a$age8 == 1 & right.a$is.preggo == 1] <- 1
right.b$age8.preggo[right.b$age8 == 1 & right.b$is.preggo == 1] <- 1
right.c$age8.preggo[right.c$age8 == 1 & right.c$is.preggo == 1] <- 1
right.a$age9.preggo <- 0
right.b$age9.preggo <- 0
right.c$age9.preggo <- 0
right.a$age9.preggo[right.a$age9 == 1 & right.a$is.preggo == 1] <- 1
right.b$age9.preggo[right.b$age9 == 1 & right.b$is.preggo == 1] <- 1
right.c$age9.preggo[right.c$age9 == 1 & right.c$is.preggo == 1] <- 1
right.a$agemissing.preggo <- 0
right.b$agemissing.preggo <- 0
right.c$agemissing.preggo <- 0
right.a$agemissing.preggo[right.a$agemissing == 1 & right.a$is.preggo == 1] <- 1
right.b$agemissing.preggo[right.b$agemissing == 1 & right.b$is.preggo == 1] <- 1
right.c$agemissing.preggo[right.c$agemissing == 1 & right.c$is.preggo == 1] <- 1

write.csv(right.a, file="~/Dropbox/raw/HHP/right_a.csv",row.names=FALSE)
write.csv(right.b, file="~/Dropbox/raw/HHP/right_b.csv",row.names=FALSE)
write.csv(right.c, file="~/Dropbox/raw/HHP/right_c.csv",row.names=FALSE)
