### this script is identical to 'preprocessing.r' except that error outlier subjects are determined by +/- 2 SDs off of subject means
### the overall goal is to make sure the pattern of effects doesn't change depending on error trimming criterion
### see: './scripts/lrt_checks/error_data/' for scripts that run analyses on the dataset that is output from this file


#import libraries ----
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)


#import data ----
wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/"
setwd(wd)
current_data <-read.table(paste(wd,'data/easy_feed_raw.txt',sep=""), sep="\t", header=TRUE)



#keep important vars ----
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


subject_log <- data.frame(description = 'initial subjects', amount = nrow(aggregate(current_data$rt, by = list(current_data$subject), FUN = mean)))


#trim by procedure and compute vars  ----
  #**trim procedure ----
    current_data <- current_data[current_data$procedure=="trialproc",]
    current_data <- subset(current_data,select=-c(procedure))


  #**compute error ----
    current_data$error <- ifelse(current_data$taskcode=="error",1,0)

  #**compute errortrim ----
    current_data$error.trim <- ifelse(current_data$error==1 | shift(current_data$error==1),1,0)

  #**stim rep ----
    current_data$stim.rep <- ifelse(current_data$stim == shift(current_data$stim), 1, 0)

  #**compute current + other ----
    current_data$current <- "" 
    current_data$other <- ""
    
    current_data$current <- ifelse(shift(current_data$responselocation=="left") & current_data$colorl=="red",1,ifelse(shift(current_data$responselocation=="left") & current_data$colorl=="black",0,
                                      ifelse(shift(current_data$responselocation=="right") & current_data$colorr=="red",1,ifelse(shift(current_data$responselocation=="right") & current_data$colorr=="black",0,""))))
    
    current_data$other <- ifelse(shift(current_data$responselocation=="right") & current_data$colorl=="green",1,ifelse(shift(current_data$responselocation=="right") & current_data$colorl=="black",0,
                                                                      ifelse(shift(current_data$responselocation=="left") & current_data$colorr=="green",1,ifelse(shift(current_data$responselocation=="left") & current_data$colorr=="black",0,""))))

  #**compute difference ----
    current_data$pvc <- ifelse(shift(current_data$responselocation=="left"),current_data$pvl,ifelse(shift(current_data$responselocation=="right"),current_data$pvr,""))
    current_data$pvo <- ifelse(shift(current_data$responselocation=="right"),current_data$pvl,ifelse(shift(current_data$responselocation=="left"),current_data$pvr,""))
    current_data$difference <- as.numeric(current_data$pvo) - as.numeric(current_data$pvc)																 

  #**transcode ----
    current_data$transcode <- ifelse(current_data$transition=="Rep",0,ifelse(current_data$transition=="Switch",1,""))
    current_data$transcode <- as.numeric(current_data$transcode)

  #**point sum ----
    current_data$pvsum <- current_data$pvl + current_data$pvr
    
  #**difference bucket ---- NOT DOING THIS ANYMORE
    #current_data$difference.bucket <- ifelse(current_data$difference < -3, -4, ifelse(current_data$difference > 3, 4, current_data$difference))
	
  #**center block ----
    current_data$block_c <- current_data$block - 6


#subject and trial trimming ----

  #**filter out startblock ----
    current_data <- current_data[current_data$transition != "StartBlock",]																  

  #**error subjects and trimming ----
    error.subjects.all <- aggregate(current_data$error,by=list(current_data$subject),FUN=mean)
	
	### HERE is the crucial difference from 'preprocessing.r'
		#error.subjects.bad <- error.subjects.all[error.subjects.all[,2]>.15,]
		error.subjects.bad <- error.subjects.all[error.subjects.all[,2] > sd(error.subjects.all[,2]) + mean(error.subjects.all[,2]),]
	
    if (nrow(error.subjects.bad)>0){
    	current_data <- current_data[-c(which(current_data$subject %in% error.subjects.bad[,1])),]
    	subject_log <- rbind(subject_log, data.frame(description='error exclusion', amount = nrow(error.subjects.bad)))
    }

	#**save original observations ----
    trial_summary <- data.frame(description = 'original trials (after subject trimming)', amount = nrow(current_data))
    
  #**compute and save out run length by blocks and subjects
    current_data <- as.data.table(current_data)
    run_length <- current_data[, rle(transcode), by = .(subject, block)][values == 0][, values := NULL] 
	  current_data <- as.data.frame(current_data)
	  write.csv(run_length, paste(wd, 'data/run_length.csv',sep=''), row.names = F)
    
  #**error trim ----
    rows_before <- nrow(current_data)
    current_data <- current_data[current_data$error.trim==0,]
    current_data <- current_data[2:nrow(current_data),]
    trial_summary <- rbind(trial_summary, data.frame(description = 'error_rows', amount = rows_before - nrow(current_data)))
    rm(rows_before)
    current_data <- subset(current_data, select=-c(error,error.trim))

#trim 2 sd on RT ---- 
  rows_before <- nrow(current_data)
  current_data_rt <- current_data[current_data$rt < mean(current_data$rt) + 2 * sd(current_data$rt) & current_data$rt > mean(current_data$rt) - 2 * sd(current_data$rt),]
  trial_summary <- rbind(trial_summary, data.frame(description = 'rt_rows', amount = rows_before - nrow(current_data_rt)))


#new zero val trimming approach
#trim outliers on blocktime
 
#trim by block first -- by subject! ----
	# 
	# subject.blocktime <- aggregate(current_data$blocktime, by = list(current_data$subject), FUN = function(x) c(mean(x),sd(x)))
	# subject.blocktime <- do.call(data.frame, subject.blocktime)
	# colnames(subject.blocktime) <- c("subject","mean.blocktime","sd.blocktime")
	# current_data.blocktrim <- join(current_data,subject.blocktime, by = 'subject')
	# before.trim <- nrow(current_data)
	# current_data.blocktrim <- current_data.blocktrim[current_data.blocktrim$blocktime <= 2*current_data.blocktrim$sd.blocktime + current_data.blocktrim$mean.blocktime,]
	# data.lost <- 1 - (nrow(current_data.blocktrim)/before.trim)
	# current_data.blocktrim <- subset(current_data.blocktrim, select = -c(mean.blocktime, sd.blocktime))
	# 
	#  
 
 trial_summary <- rbind(trial_summary, data.frame(description='final observations (not including rt trim)', amount = nrow(current_data)))
 trial_summary <- rbind(trial_summary, data.frame(description = 'percent lost', amount = (1 - (trial_summary[4,2] / trial_summary[1,2]))*100))
 subject_log <- rbind(subject_log, data.frame(description='final subjects', amount = nrow(aggregate(current_data$rt, by = list(current_data$subject), FUN = mean))))
 
 current_data_bulk <- current_data
 current_data <- current_data[current_data$difference <= 3 & current_data$difference >= -3,]
 
 ## check how many trials each subject lost
 current_data_bulk <- as.data.table(current_data_bulk)
 current_data <- as.data.table(current_data)
 
 data_loss <- current_data_bulk[, .(original = .N), by = subject]
 temp <- current_data[, .(post_trim = .N), by = subject]
 data_loss <- join(data_loss, temp, by = 'subject')
 rm(temp)
 data_loss[,prop_lost := (original - post_trim) / original]
 data_loss <- data_loss[order(prop_lost, decreasing = T)]
 ## remove subject 39 as an outlier
 current_data <- current_data[subject != 39]

#slim down and write ----
  current_data <- current_data[,c("subject","rsi","block","block_c","blocktime","trial","current","other","difference","stim.rep","pvsum","rt","transcode")]
  current_data_bulk <- current_data_bulk[,c("subject","rsi","block","block_c","blocktime","trial","current","other","difference","stim.rep","pvsum","rt","transcode")]
  #write.csv(current_data_bulk, paste(wd,'data/easy_clean_full.csv',sep=""),row.names=F)
  write.csv(current_data, paste(wd,'data/easy_clean_errors.csv',sep=""),row.names=F)
  #write.csv(current_data, paste(wd, 'data/easy_clean_rt.csv',sep=''), row.names = F)
  rm(keep)
  
   
   
   
