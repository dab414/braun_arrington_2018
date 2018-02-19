library(lmtest)
wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
setwd(wd)

load(paste(wd, 'results/lrt_check/error_check/base1_block.rdata',sep=''))
load(paste(wd, 'results/lrt_check/error_check/base2_block.rdata',sep=''))
load(paste(wd, 'results/lrt_check/error_check/base1_trial.rdata',sep=''))
load(paste(wd, 'results/lrt_check/error_check/base2_trial.rdata',sep=''))

base_block_test <- lrtest(base1_block, base2_block)
trim_block_test <- lrtest(trim1_block, trim2_block)
base_trial_test <- lrtest(base1_trial, base2_trial)
trim_trial_test <- lrtest(trim1_trial, trim2_trial)

sink(file = paste(wd, 'results/lrt_check/error_check/error_models_summary.txt',sep=''))
summary(base1_block)
summary(base2_block)
summary(base1_trial)
summary(base2_trial)
sink(NULL)