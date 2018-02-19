## a script for computing and writing out many of the marginal & cell means of interest

#import libraries ----
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)
library(GGally)

#import data ----
wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/"
setwd(wd)
current_data <- as.data.table(read.csv(paste(wd,'data/easy_clean.csv',sep=""), header=TRUE))

### block level

  #block main effect
    block_main <- current_data[, .(switch = mean(transcode)), by = .(subject, block)][,.(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = block]
    
  #difference main effect
    difference_main <- current_data[, .(switch = mean(transcode)), by = .(subject, difference)][,.(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = difference][order(difference)]

  #two-way cell means
    block_difference <- current_data[, .(switch = mean(na.omit(transcode))), by = .(subject, block, difference)][, .(switch = mean(na.omit(switch)), se = sd(na.omit(switch)) / sqrt(.N)), by = .(block, difference)][order(block,difference)]
    
    
### trial level
    
  #current and other main effects
    current_main <- current_data[, .(switch = mean(transcode)), by = .(subject, current)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = current]
    other_main <- current_data[, .(switch = mean(transcode)), by = .(subject, other)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = other]
    
    #two-way
      current_other <- current_data[, .(switch = mean(transcode)), by = .(subject, current, other)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = .(current, other)][order(current,other)]
    
  #difference two-ways
    current_difference <- current_data[, .(switch = mean(transcode)), by = .(subject, difference, current)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = .(difference, current)][order(difference, current)]
    
    other_difference <- current_data[, .(switch = mean(transcode)), by = .(subject, difference, other)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = .(difference, other)][order(difference, other)]
    

#write out means to txt files
sink(file = paste(wd, 'results/glmm_analyses/block_analyses/block_cell_means.txt',sep=''))
block_main
difference_main
block_difference
sink(NULL)

sink(file = paste(wd, 'results/glmm_analyses/trial_analyses/trial_cell_means.txt',sep=''))
current_main
other_main
current_difference
other_difference
sink(NULL)



