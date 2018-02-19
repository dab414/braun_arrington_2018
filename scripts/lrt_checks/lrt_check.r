library(lmtest)
wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
setwd(wd)

load(paste(wd, 'results/lrt_check/base1_block.rdata',sep=''))
load(paste(wd, 'results/lrt_check/base2_block.rdata',sep=''))
load(paste(wd, 'results/lrt_check/bulk1_block.rdata',sep=''))
load(paste(wd, 'results/lrt_check/bulk2_block.rdata',sep=''))

base_block_test <- lrtest(base1_block, base2_block)
bulk_block_test <- lrtest(bulk1_block, bulk2_block)

sink(file = 'results/lrt_check/lrt_results.txt')
base_block_test
bulk_block_test
sink(NULL)

## visually inspect 'lrt_results.txt' to determine which model fits best based off of LRT results