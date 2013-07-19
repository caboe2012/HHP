##########################################
# Some R function to plot your position
# on a Kaggle leaderboard
##########################################
library(XML)

plotKaggleLeaderboard <- function(theURL,myTeam,topX=100,title){
	    
	#this library does all the hard work
	#for windows users to install this packages see
	#http://cran.r-project.org/bin/windows/contrib/r-release/ReadMe
	#and http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.13/
	
	#go and read the tables from the web page
	tables <- readHTMLTable(theURL)
	
	#get the table of interest
	#names(tables)
	lb <- tables[['leaderboard-table']]
	
	#see what the columns are
	#colnames(lb)
	#nrow(lb)
	
	#convert to numeric - see ?readHTMLTable
	#numeric_cols <- c('AUC','Entries')
	#numeric_cols <- c('RMSLE','Entries')
	numeric_cols <- c(4,5)
	
	lb[numeric_cols] = lapply(lb[numeric_cols], function(x) as.numeric(gsub(".* ", "", as.character(x))))
	
	
	########################################
	#tidy up the team name
	#this can be simplified a lot
	#(but this seems to work ok)
	########################################
	team_col <- c('Team Name')
	
	#the field is messy
	#lb[team_col]
	
	#split the field by "\r\n" than denotes the break between fields within a field 
	the_team <- lapply(lb[team_col], function(x) strsplit(as.character(x), "\r\n"))
	
	#summary(the_team)
	#length(the_team)
	
	z <- the_team[[1]]
	totalTeams <- length(z)
	
	#create a field 
	theTeam = vector(mode='character',length=totalTeams)
	
	for(i in 1:totalTeams){
	tn <- unlist(z[i])
	theTeam[i] <- tn[1]
	}
	
	#append to rest of data
	lb$theTeam <- theTeam
	
	
	#####################
	#      plot
	#####################
	myRank <- which(lb$theTeam == myTeam)
	
	#plot the topX
	error_data <- lb[,4]
	mytext = paste("Team",myTeam,"is in position",myRank,"out of",totalTeams)
	
	plot(error_data[1:topX]
	    ,col = 'blue'
	    ,main = paste(title,"- leaderboard at",Sys.Date())
	    , sub = mytext
	    , family="Ideal Sans Book"
	    , xlab = 'Rank'
	    , ylab = 'Error'
	    )
	
	#mark my position
	points(myRank,error_data[myRank],col="red",pch=19)
	
	}

leaderboard.fn <- function(printScreen=TRUE, writeFile=TRUE){
	compURL = theurl <- "http://www.heritagehealthprize.com/c/hhp/Leaderboard"
	compTeam <- 'Black Swan'
	compTopX <- 120
	compTitle <- 'HHP'
	
	if(printScreen){
	plotKaggleLeaderboard(theURL = compURL,myTeam = compTeam,topX = compTopX,title =compTitle)
	}
	
	if(writeFile){
	png(paste("~/Dropbox/leaderboards/HHP/leaderboard-",format(Sys.Date(), "%m-%d-%Y")
            ,".png",sep=""),width=1200,height=800,pointsize=18,bg=
  "transparent")
	plotKaggleLeaderboard(theURL = compURL,myTeam = compTeam,topX = compTopX,title =compTitle)
	dev.off()
	}
}

leaderboard.fn(FALSE,TRUE)
