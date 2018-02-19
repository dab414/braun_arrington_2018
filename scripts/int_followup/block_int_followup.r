library(lme4)

  #wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
wd <- 'C:/Experiments/Dave/Dropbox/Exps/easy_feed_spring2017/'
setwd(wd)

current_data <- read.csv(paste(wd, 'data/easy_clean.csv',sep=''),header = T)


#block analyses  interaction follow-up ----

  #base -- -3:3 difference data, run these first cuz i doubt it'll converge. then use this as a guide for what to start the bulk analysis at		
  		
	#compute contrast terms
    current_data$difference_lo <- current_data$difference + 3
    current_data$difference_hi <- current_data$difference - 3

	#difference -- high
		block_hidiff <- glmer(transcode ~ block_c * difference_hi + (1 + block_c * difference_hi | subject), data = current_data, family = 						binomial(link = 'logit'), nAGQ = 1)
		save(block_hidiff, file = paste(wd, 'results/int_followup/block/block_hidiff.rdata', sep=''))
	
	#difference -- low
		block_lodiff <- glmer(transcode ~ block_c * difference_lo + (1 + block_c * difference_lo | subject), data = current_data, family = 						binomial(link = 'logit'), nAGQ = 1)
		save(block_lodiff, file = paste(wd, 'results/int_followup/block/block_lodiff.rdata', sep=''))
  		

## write out results
			
  sink(file = paste(wd, 'results/int_followup/block/block_followup_summary.txt',sep=''))
  summary(block_hidiff)
  paste('odds ratios of fixed effects', exp(fixef(block_hidiff)))
  summary(block_lodiff)
  paste('odds ratios of fixed effects', exp(fixef(block_lodiff)))
  sink(NULL)