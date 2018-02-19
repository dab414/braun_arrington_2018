## note: I learned how to use data.table() maybe 75% through this analysis; so at some places you'll see me use the archaic aggregate() function, and in other places I utilize data.table() for summarizing data across conditions

#import libraries ----
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)


#import data ----
wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/data for uploading/"
wd <- "REPLACE THIS TEXT WITH WORKING DIRECTORY PATH"
setwd(wd)

## pulling the "raw" dataset, the one that basically gets spit out of eprime (after we aggregate all subject data together with eprime's eDataAid)
current_data <-read.table('data/easy_feed_raw.txt', sep="\t", header=TRUE)



#keep important vars ----
## what these important vars mean is explained in './data/var_code.txt'
keep <- t(matrix(c("Subject", "subject",
					"colorL.SubTrial.", "colorl",
					"colorR.SubTrial.", "colorr",
					"Procedure.SubTrial.", "procedure",
					"responselocation.SubTrial.","responselocation",
					"blocks.Sample", "block",
					"stim1.SubTrial.", "stim",
					"SubTrial", "trial",
					"RSI.SubTrial.","rsi",
					"blocktime", "blocktime",
					"taskcode.SubTrial.","taskcode",
					"Taskattempt.SubTrial.","taskattempt",
					"transition.SubTrial.","transition",
					"stimulus.RT.SubTrial.","rt",
					"pointvalueL.SubTrial.","pvl",
					"pointvalueR.SubTrial.","pvr"
					 ),nrow=2))
					
					
current_data <- current_data[,keep[,1]]
colnames(current_data) <- keep[,2]

## this data frame is meant to keep a record of what subjects are trimmed due to what trimming criteria
subject_log <- data.frame(description = 'initial subjects', amount = nrow(aggregate(current_data$rt, by = list(current_data$subject), FUN = mean)))


#trim by procedure and compute vars  ----
  #**trim procedure ---- remove all practice trials
    current_data <- current_data[current_data$procedure=="trialproc",]
    current_data <- subset(current_data,select=-c(procedure))


  #**compute error ---- make a numeric variable to represent error trials
    current_data$error <- ifelse(current_data$taskcode=="error",1,0)

  #**compute errortrim ---- this marks both error trials and the trials following error trials as 1, else = 0
    current_data$error.trim <- ifelse(current_data$error==1 | shift(current_data$error==1),1,0)

  #**stim rep ---- was the exact stimulus that was presented on the previous trial presented again on the current trial? if so = 1, else = 0
    current_data$stim.rep <- ifelse(current_data$stim == shift(current_data$stim), 1, 0)

  #**compute current + other ---- markers for whether the value for the previously and non-previously performed tasks either changed or remained constant from the previous trial
    current_data$current <- "" 
    current_data$other <- ""
    
	## the 'colorl' and 'colorr' vars let us know whether the value on the left or right decreased (red), increased (green), or remained constant (black) from the previous trial
	## but we need to translate this information with respect to what task the P performed on the previous trial to code for current and other
	## the 'responselocation' var lets us know whether the P performed the task associated with the left value or the right value
	
	## check whether the P performed the task associated with the left or right value on the previous trial, and then check how that specific value transitioned on the current trial
    current_data$current <- ifelse(shift(current_data$responselocation=="left") & current_data$colorl=="red",1,ifelse(shift(current_data$responselocation=="left") & current_data$colorl=="black",0,
                                      ifelse(shift(current_data$responselocation=="right") & current_data$colorr=="red",1,ifelse(shift(current_data$responselocation=="right") & current_data$colorr=="black",0,""))))
    
    current_data$other <- ifelse(shift(current_data$responselocation=="right") & current_data$colorl=="green",1,ifelse(shift(current_data$responselocation=="right") & current_data$colorl=="black",0,
                                                                      ifelse(shift(current_data$responselocation=="left") & current_data$colorr=="green",1,ifelse(shift(current_data$responselocation=="left") & current_data$colorr=="black",0,""))))

  #**compute difference ---- other task value minus current task value
    current_data$pvc <- ifelse(shift(current_data$responselocation=="left"),current_data$pvl,ifelse(shift(current_data$responselocation=="right"),current_data$pvr,""))
    current_data$pvo <- ifelse(shift(current_data$responselocation=="right"),current_data$pvl,ifelse(shift(current_data$responselocation=="left"),current_data$pvr,""))
    current_data$difference <- as.numeric(current_data$pvo) - as.numeric(current_data$pvc)																 

  #**transcode ---- did the P switch tasks from the previous trial? if so = 1, else = 0
    current_data$transcode <- ifelse(current_data$transition=="Rep",0,ifelse(current_data$transition=="Switch",1,""))
    current_data$transcode <- as.numeric(current_data$transcode)

  #**point sum ----
    current_data$pvsum <- current_data$pvl + current_data$pvr
    
  #**center block ---- center the block variable so that block 6 is now 0 in the centered distribution, with new range -6:6
    current_data$block_c <- current_data$block - 6





#subject and trial trimming ---- I made sure to do all trimming AFTER var computing, because bad things happen otherwise
	## for example, filtering out error trials before computing 'current' and 'other' would lead to a situation where we might no longer be referencing the task performed at trial N-1 to determine task codings at trial N

  #**filter out startblock ----
    current_data <- current_data[current_data$transition != "StartBlock",]																  

  #**error subjects and trimming ----
	#make a dataframe detailing error rates for all subjects
    error.subjects.all <- aggregate(current_data$error,by=list(current_data$subject),FUN=mean)
	#make a dataframe for only subjects who pass the .15 error threshold
    error.subjects.bad <- error.subjects.all[error.subjects.all[,2]>.15,]
    #if at least one subject has an error rate above threshold, filter all of these subjects out of the overall data
    if (nrow(error.subjects.bad)>0){
    	current_data <- current_data[-c(which(current_data$subject %in% error.subjects.bad[,1])),]
		#update subject log
    	subject_log <- rbind(subject_log, data.frame(description='error exclusion', amount = nrow(error.subjects.bad)))
    }

	#**save original observations ---- same idea as subject log but for trials
	#this dataframe will eventually let me know how many trials were lost due to what trimming criteria
    trial_summary <- data.frame(description = 'original trials (after subject trimming)', amount = nrow(current_data))
    
  #**compute and save out run length by blocks and subjects
  # run length is a special measure that we used to try to examine individual differences, it's not relevant to the main analyses
    current_data <- as.data.table(current_data)
    run_length <- current_data[, rle(transcode), by = .(subject, block)][values == 0][, values := NULL] 
	  current_data <- as.data.frame(current_data)
	  write.csv(run_length, paste(wd, 'data/run_length.csv',sep=''), row.names = F)
    
  #**error trim ---- remove error trials and trials following error trials
    rows_before <- nrow(current_data)
    current_data <- current_data[current_data$error.trim==0,]
    current_data <- current_data[2:nrow(current_data),]
    trial_summary <- rbind(trial_summary, data.frame(description = 'error_rows', amount = rows_before - nrow(current_data)))
    rm(rows_before)
    current_data <- subset(current_data, select=-c(error,error.trim))

#trim 2 sd on RT ---- save this out as a special RT dataset that I can use for analyzing performance data only
#this means that, for analyzing choice data, no trials are trimmed based on RT
  rows_before <- nrow(current_data)
  current_data_rt <- current_data[current_data$rt < mean(current_data$rt) + 2 * sd(current_data$rt) & current_data$rt > mean(current_data$rt) - 2 * sd(current_data$rt),]
  trial_summary <- rbind(trial_summary, data.frame(description = 'rt_rows', amount = rows_before - nrow(current_data_rt)))

  #save an instance of the data as it is (current_data_bulk) and trim down the main dataset of interest according to the difference variable
  #trim observations more extreme than +/- 3 on the difference variable
  current_data_bulk <- current_data
  current_data <- current_data[current_data$difference <= 3 & current_data$difference >= -3,]
  
  #update the trial log
  trial_summary <- rbind(trial_summary, data.frame(description=c('difference trim rows lost','final observations (not including rt trim)'), 
                                                   amount = c(nrow(current_data_bulk) - nrow(current_data), nrow(current_data))))
  trial_summary <- rbind(trial_summary, data.frame(description = 'percent lost', amount = (1 - (trial_summary[5,2] / trial_summary[1,2]))*100))
  trial_summary <- cbind(trial_summary, data.frame(percentage = c('NA',round((trial_summary$amount[2] / trial_summary$amount[1])*100, digits = 2),'NA',
                                                                  round((trial_summary$amount[4] / (trial_summary$amount[1] - trial_summary$amount[2]))*100, digits = 2), 'NA','NA')))
  
 
 ## check how many trials each subject lost
 current_data_bulk <- as.data.table(current_data_bulk)
 current_data <- as.data.table(current_data)
 
 data_loss <- current_data_bulk[, .(original = .N), by = subject]
 temp <- current_data[, .(post_trim = .N), by = subject]
 data_loss <- join(data_loss, temp, by = 'subject')
 rm(temp)
 data_loss[,prop_lost := (original - post_trim) / original]
 data_loss <- data_loss[order(prop_lost, decreasing = T)]
 

 subject_log <- rbind(subject_log, data.frame(description='final subjects', amount = nrow(aggregate(current_data$rt, by = list(current_data$subject), FUN = mean))))
 
 
#keep only the vars of interest and write out preprocessed datasets ----
  current_data <- current_data[,c("subject","rsi","block","block_c","blocktime","trial","current","other","difference","stim.rep","pvsum","rt","transcode")]
  current_data_bulk <- current_data_bulk[,c("subject","rsi","block","block_c","blocktime","trial","current","other","difference","stim.rep","pvsum","rt","transcode")]
  write.csv(current_data_bulk, paste(wd,'data/easy_clean_full.csv',sep=""),row.names=F)
  write.csv(current_data, paste(wd,'data/easy_clean.csv',sep=""),row.names=F)
  write.csv(current_data, paste(wd, 'data/easy_clean_rt.csv',sep=''), row.names = F)
  rm(keep)
  
   
   
   
