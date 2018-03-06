library(lme4)
library(data.table)
library(dplyr)
library(lmtest)
library(blmeco)
library(tidyr)

current_data <- fread('../data/easy_clean.csv')

current_data <- current_data %>%
  mutate(difference_lo = difference + 3,
         difference_hi = difference - 3)



start_time <- Sys.time()
block_hidiff <- glmer(transcode ~ block_c * difference_hi + (1 + block_c * difference_hi | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
stop_time <- Sys.time()
hidiff_runtime <- stop_time - start_time
saveRDS(block_hidiff, '../other_resources/block_hidiff.rds')




block_hidiff <- readRDS('../other_resources/block_hidiff.rds')




start_time <- Sys.time()
block_lodiff <- glmer(transcode ~ block_c * difference_lo + (1 + block_c * difference_lo | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
stop_time <- Sys.time()
lodiff_runtime <- stop_time - start_time
saveRDS(block_lodiff, '../other_resources/block_lodiff')


block_lodiff <- readRDS('../other_resources/block_lodiff.rds')






start_time <- Sys.time()
block_hidiff_stepdown <- glmer(transcode ~ block_c * difference_hi + (1 + block_c + difference_hi | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
stop_time <- Sys.time()
hidiff_runtime_stepdown <- stop_time - start_time
saveRDS(block_hidiff_stepdown, '../other_resources/block_hidiff_stepdown.rds')



block_hidiff_stepdown <- readRDS('../other_resources/block_hidiff_stepdown.rds')




start_time <- Sys.time()
block_hidiff_stepdown <- glmer(transcode ~ block_c * difference_lo + (1 + block_c + difference_lo | subject), data = current_data, family = binomial(link = 'logit'), nAGQ = 1)
stop_time <- Sys.time()
lodiff_runtime_stepdown <- stop_time - start_time
saveRDS(block_lodiff_stepdown, '../other_resources/block_lodiff_stepdown')



block_lodiff <- readRDS('../other_resources/block_lodiff_stepdown.rds')




sink(file = '../other_resources/runtime_report.txt')
paste('The block at high difference analysis took', hidiff_runtime/60, 'minutes to run')
paste('The block at low difference analysis took', lodiff_runtime/60, 'minutes to run')
paste('The block at high difference step-down analysis took', hidiff_runtime_stepdown/60, 'minutes to run')
paste('The block at high difference analysis took', hidiff_runtime_stepdown/60, 'minutes to run')
sink(NULL)

