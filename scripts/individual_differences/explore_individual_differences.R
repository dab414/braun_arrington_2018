## explore individual differences
	## a script dedicated to exploring and summarizing what individual differences might be present in the data
	## the reader is encouraged to conduct his / her own exploratory analyses

#import libraries ----
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)
## run ggpairs_mod.r (in same directory as this script) to modify the GGally namespace before importing this library
library(GGally)

#import data ----
wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/"
setwd(wd)
current_data <- as.data.table(read.csv(paste(wd,'data/easy_clean_full.csv',sep=""), header=TRUE))
run_length <- as.data.table(read.csv(paste(wd,'data/run_length.csv',sep=""), header=TRUE))

#this returns a 4-column data table: subject, block, lengths (how many runs), values (of which type of transition); note length != nrow(current_data)
#current_data[, rle(transcode), by = .(subject, block)]


## block level

  block_level <- run_length[, .(avg_rl = mean(lengths), var_rl = var(lengths)), by = .(subject, block)]
  block_times <- current_data[, .(block_time = mean(blocktime)), by = .(subject, block)]
  block_level <- join(block_level, block_times, by = c('subject','block'))
  block_level <- join(block_level, current_data[, .(switch = mean(transcode)), by = .(subject, block)], by = c('subject','block'))
  
  rm(block_times)
  
  ## does run length vary across blocks?
    block_runs <- block_level[, .(run_length = mean(avg_rl), run_length_se = (sd(avg_rl)) / sqrt(.N)), by = block]
    plot(block_runs$block, block_runs$run_length)
    abline(lm(block_runs$run_length ~ block_runs$block), col = 'red')
    write.table(block_runs, paste(wd, 'results/individual_differences/block_runs.txt',sep=''), sep='\t', row.names = F)
    rm(block_runs)
    
## subject level
    
  ## is run length related to block times
    subject_level <- block_level[, .(run_length = mean(avg_rl), run_length_sd = sd(avg_rl), 
                                     block_time = mean(block_time), block_time_sd = sd(block_time)), by = subject]
    
  ## compute reward sensitivity
    reward_sensitivity <- current_data[(current == 0 & other == 0) | (current == 1 & other == 1), mean(transcode), 
                                       by = .(subject, block, (current == 0 & other == 0))]
    
    reward_sensitivity <- reshape(reward_sensitivity, timevar = 'current', idvar = c('subject','block'), direction = 'wide')
    colnames(reward_sensitivity)[3:4] <- c('low','high')
    reward_sensitivity[, diff := high - low][,c('low','high') := .(NULL, NULL)]
    subject_level <- join(subject_level, 
                          reward_sensitivity[, .(reward_sensitivity = mean(diff), reward_sensitivity_sd = sd(diff)), by = subject], by = 'subject')
    block_level <- join(block_level, reward_sensitivity, by = c('subject','block'))
    
    
  ## compute switch rates and difference distribution  
    switch_rates <- current_data[, .(mean_switch = mean(transcode)), by = .(subject, block)][, .(mean_switch = mean(mean_switch),sd_switch = sd(mean_switch)), by = subject]
    diff_distribution <- current_data[, .(mean_difference = mean(difference)), by = .(subject, block)][,.(mean_difference = mean(mean_difference), sd_difference = sd(mean_difference)), by = subject]
    subject_level <- join(subject_level, switch_rates, by = 'subject')
    subject_level <- join(subject_level, diff_distribution, by = 'subject')
    
  ## compute rt and switch cost
    
    ## trim rt outliers first
      temp <- current_data[, .(mean_rt = mean(rt), sd_rt = sd(rt)), by = .(subject, block)][, .(mean_rt = mean(mean_rt), sd_rt = mean(sd_rt)), by = subject]
      current_data <- join(current_data, temp, by = 'subject')
      current_data <- current_data[rt <= 2*sd_rt + mean_rt]
      
    ## compute switch cost
      temp <- current_data[, .(mean_rt = mean(na.omit(rt))), by = .(subject, block, transcode)]
      temp <- reshape(temp, timevar = 'transcode', idvar = c('subject', 'block'), direction = 'wide')
      temp <- temp[, switch_cost := mean_rt.1 - mean_rt.0][, .(mean_switch_cost = mean(na.omit(switch_cost)), sd_switch_cost = sd(na.omit(switch_cost))), by = subject]
      subject_level <- join(subject_level, temp, by = 'subject')
      
    ## compute rt
      temp <- current_data[, .(mean_rt = mean(rt)), by = .(subject, block)][, .(mean_rt = mean(mean_rt), sd_rt = sd(mean_rt)), by = subject]
      subject_level <- join(subject_level, temp, by = 'subject')

        
    write.table(subject_level, paste(wd, 'results/individual_differences/subject_level.txt',sep = ''), sep = '\t', row.names = F)
    
	## code to reproduce Figure 2
    ggpairs(subject_level, 
            columns = c('mean_switch','mean_switch_cost','mean_rt','block_time','reward_sensitivity'),
            columnLabels = c('Switch Rate','Switch Cost','Response Time','Block Time', 'Reward Sensitivity'),
            diag = list(continuous = 'bar'),
            lower = list(continuous = 'smooth'),
            axisLabels = 'none') +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = 'white')
                  )
    
  
## investigate sensitivity predicting run length
    
    #person center switch rates
      block_level <- join(block_level, current_data[, .(person_mean_switch = mean(transcode)), by = subject], by = 'subject')
      block_level[, switch_c := switch - person_mean_switch]
    
    model1 <- lm(avg_rl ~ diff + switch_c, data = block_level)
    
    write.table(block_level[, .(run_length = mean(avg_rl), switch = mean(switch), sensitivity = mean(diff)), by = subject], 
                paste(wd, 'results/individual_differences/rl_sensitivity.txt', sep=''), sep='\t', row.names = F)
    
    