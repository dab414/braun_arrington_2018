library(lme4)

  #wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
wd <- 'C:/Experiments/Dave/Dropbox/Exps/easy_feed_spring2017/'
setwd(wd)

current_data <- read.csv(paste(wd, 'data/easy_clean_errors.csv',sep=''),header = T)



#block analyses ----

  #base -- -3:3 difference data

  	#most complex model -- base
  		base1_block <- glmer(transcode ~ block_c * difference + (1 + block_c * difference | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  		save(base1_block, file = paste(wd, 'results/lrt_check/error_check/base1_block.rdata', sep=''))
  	
  	#next step down -- base 
  		base2_block <- glmer(transcode ~ block_c * difference + (1 + block_c + difference | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  		save(base2_block, file = paste(wd, 'results/lrt_check/error_check/base2_block.rdata', sep=''))
  		
		
#trial analyses ----

  #base -- -3:3 difference data, run these first cuz i doubt it'll converge. then use this as a guide for what to start the bulk analysis at		
  		
  	#most complex model -- base
  		base1_trial <- glmer(transcode ~ current * other * difference + (1 + current + other + difference | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  		save(base1_trial, file = paste(wd, 'results/lrt_check/error_check/base1_trial.rdata', sep=''))
  	
  	#next step down -- base
  		base2_trial <- glmer(transcode ~ current * other * difference + (1 + current + other | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  			save(base2_trial, file = paste(wd, 'results/lrt_check/error_check/base2_trial.rdata', sep=''))
  		
 