#if line 3 returns an error, you may need to first install the lme4 package
#run the code: install.packages('lme4')
library(lme4)

#wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
wd <- 'C:/Experiments/Dave/Dropbox/Exps/easy_feed_spring2017/'
wd <- "REPLACE THIS TEXT WITH WORKING DIRECTORY PATH"
setwd(wd)

current_data <- read.csv('data/easy_clean.csv',header = T)
current_data_bulk <- read.csv('data/easy_clean_full.csv',header = T)


### The overall goal of this script is to find the best-fitting models for both the difference-trimmed data and the non-difference-trimmed data. 
### We want to make sure that the pattern of effects doesn't change depending upon the difference-trimming criterion
### Because we're interested in controlling for between-subject variability, random effects will be estimated within subjects
### The general approach to finding the best fitting model is to begin with the maximally complex model allowed by the design of the experiment (all random effects) 
### We then evaluate the model by two criteria:
	### Does the model converge?
		### If no, remove a random-effects parameter and run again
		### If yes:
			### Run a likelihood ratio test between the converged model and the next-most simple model (removing one parameter)
				## this is done with the "./scripts/lrt_checks/lrt_check.r" script
			### If the difference between the models is significant, this suggests that the dropped parameter explains a significant amount of the between-subject variability
				### thus, the model with more parameters is deemed the best-fitting model
			### If the difference between the models is not significant, repeat this process by taking the simpler model from this comparison, dropping another parameter, comparing those two, etc.

# some of these analyses may take some time to converge -- the longest ones ran about ~ 20-30 mins on reasonably (but not lightning) fast computers
			
#block analyses ----

  #base -- -3:3 difference data

  	#most complex model -- base
  		base1_block <- glmer(transcode ~ block_c * difference + (1 + block_c * difference | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  		save(base1_block, file = paste(wd, 'results/lrt_check/base1_block.rdata', sep=''))
  	
  	#next step down -- base 
  		base2_block <- glmer(transcode ~ block_c * difference + (1 + block_c + difference | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
  		save(base2_block, file = paste(wd, 'results/lrt_check/base2_block.rdata', sep=''))

  #bulk -- -10:10 difference data

  	#most complex model
  		bulk1_block <- glmer(transcode ~ block_c * difference + (1 + block_c * difference | subject), data = current_data_bulk, family = binomial(link = 'logit'), nAGQ = 1)
  		save(bulk1_block, file = paste(wd, 'results/lrt_check/bulk1_block.rdata', sep=''))

  	#next step down
  		bulk2_block <- glmer(transcode ~ block_c * difference + (1 + block_c + difference | subject), data = current_data_bulk, family = binomial(link = 'logit'), nAGQ = 1)
  		save(bulk2_block, file = paste(wd, 'results/lrt_check/bulk2_block.rdata', sep=''))


		
### output results of all analyses to txt file
	sink(file = 'results/lrt_check/model_fit_summary.txt')
	summary(base1_block)
	paste('Odds ratio of fixed effects', exp(fixef(base1_block)))
	summary(base2_block)
	paste('Odds ratio of fixed effects', exp(fixef(bulk2_block)))
	summary(bulk1_block)
	paste('Odds ratio of fixed effects', exp(fixef(bulk1_block)))
	summary(bulk2_block)
	paste('Odds ratio of fixed effects', exp(fixef(bulk2_block)))
	sink(NULL)
	
	## visually inspect model_fit_summary.txt to assess convergence
	## adjust models to get convergence before running 'lrt_check.r' script
	


