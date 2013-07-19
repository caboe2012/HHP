source("HHP_header.R")
source("HHP_functions.R")

total.time <- 0

tic()
right.a <- read.csv("~/Dropbox/raw/HHP/right_a.csv")
right.b <- read.csv("~/Dropbox/raw/HHP/right_b.csv")
right.c <- read.csv("~/Dropbox/raw/HHP/right_c.csv")
right.a <- right.a[,-c(5,6,7)]
right.b <- right.b[,-c(5,6,7)]
right.c <- right.c[,-c(5,6,7)]
toc()

tic()
transform <- c(5,6,7,8,62:269) 
right.a[,transform] <- log1p(right.a[,transform])
right.b[,transform] <- log1p(right.b[,transform])
right.c[,transform] <- log1p(right.c[,transform])
right.a[,"LabCount"] <- log1p(log1p(right.a[,"LabCount"]))
right.b[,"LabCount"] <- log1p(log1p(right.b[,"LabCount"]))
right.c[,"LabCount"] <- log1p(log1p(right.c[,"LabCount"]))
logfix <- c("SO","GYNECA","HEART2","HIPFX","METAB3","SEPSIS","IndependentLab", "InpatientHospital","UrgentCare")
right.a[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.a[,logfix])))))))
right.b[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.b[,logfix])))))))
right.c[,logfix] <- log1p(log1p(log1p(log1p(log1p(log1p(log1p(right.c[,logfix])))))))
toc()

tic()
before.add <- c("MemberID","ClaimsTruncated","logdays","maxCharls",
                "claims.span",
                "numclaims","EM","Emergency","DrugCount","num.rx","Internal","UrgentCare","LabCount","PRGNCY","CHF","COPD","HEART2","LoS50","HospitalTen","NoHospital")
toc()

# MAKE TRAINING/TESTING FILES
tic()
training <- right.b[,-c(3,60,61)]  
temp <- right.a[,before.add]
colnames(temp) <- gsub("$",".before",colnames(temp))
training <- merge(training, temp,
                  by.x="MemberID",by.y="MemberID.before",all.x=TRUE,sort=FALSE)
testing <-  right.c[,-c(3,60,61)]
temp <- right.b[,before.add]
colnames(temp) <- gsub("$",".before",colnames(temp))
testing <- merge(testing, temp,
                  by.x="MemberID",by.y="MemberID.before",all.x=TRUE,sort=FALSE)
toc()

# IMPUTE NAs AS ZERO
tic()
training[is.na(training)] <- 0
testing[is.na(testing)] <- 0
toc()

tic()
hhp.functions <- lmFuncs
hhp.functions$summary <- hhp.summary.l1p
control.rfe.lm <- rfeControl(functions = hhp.functions, 
                                rerank = TRUE,
                                workers = 16,
                                method = "repeatedcv",
                                number = 10,
                                returnResamp = "final",
                                computeFunction = mclapply,
                                computeArgs=list(mc.preschedule = FALSE, 
                                                 mc.set.seed = FALSE)
                            )
lol.control <- trainControl(workers = 32,
                                method = 'repeatedcv',
                                number = 25, #Folds
                                repeats = 25, #Repeats
                                returnResamp = "final",
                                computeFunction = mclapply,
                                computeArgs=list(mc.preschedule = FALSE, 
                                                 mc.set.seed = FALSE)
                            )
toc()

# The sample 
tic()
set.seed(110685)
thechosen <- sample(1:dim(training)[1],71435)
model.training <- training[thechosen,-c(1,3)]
model.training.response <- training[thechosen, 3] 
#lol.grid <- createGrid("rf", 10, model.training)
lol.grid <- data.frame(.mtry = c(2))
toc()

######### train
tic()
model.1 <- train(model.training, model.training.response,
               method="rf",ntree=50,
               metric="RMSE",
               trControl=lol.control,
               tuneGrid=lol.grid
               )
toc()
#########


######## rfe
tic()
sizes <- seq(100, 275,by=1)
model.1 <- rfe(model.training[,c(1:14,16,17,19:27,29:37,53:265,267:298)], model.training.response, sizes,
                metric = "HHP", maximize = FALSE,
                rfeControl = control.rfe.lm)
toc()
#############

######## single lm
tic()
model.1 <- lm(logdays ~ . , data=training[,-c(1)])
toc()

##### single rf
tic()
registerDoMC(32)
mcoptions <- list(preschedule = FALSE, set.seed = FALSE)
model.1 <- foreach(ntree = rep(50,32), .combine = combine, .packages = "randomForest") %dopar% 
          randomForest(x=model.training[,optvariables], y=model.training.response,
                  ntree = ntree, nodesize=50, mtry=3,
        			  .options.multicore=mcoptions,keep.forest=TRUE)
png(file=paste("~/Dropbox/plots/HHP/RFVarImp-",format(Sys.time(),"%m-%d-%Y-%H%M"),
               ".png",sep=""),bg="transparent",width=1280,height=800)
varImpPlot(model.1)
dev.off()
toc()
###############

tic()
prediction <- predict(model, testing)
prediction <- expm1(prediction)
prediction <- cutOff(prediction)
prediction <- data.frame(testing$MemberID,prediction)
colnames(prediction) <- c("MemberID","DaysInHospital")
toc()

tic()
submission <- read.csv("~/Dropbox/raw/HHP/Target.csv");submission <- submission[,-3]
submission <- merge(submission,prediction,
                    by.x="MemberID",by.y="MemberID",all.x="TRUE",sort=FALSE)
write.csv(submission,file=paste("~/Dropbox/submissions/HHP/HHP_submission-",format(Sys.time(),
                                            "%m-%d-%Y-%H%M"),".csv",sep=""),
          quote=FALSE,row.names=FALSE)
toc()
