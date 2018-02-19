### analyzing performance data

wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/"
setwd(wd)

library(ez)
library(data.table)

current_data <- as.data.table(read.csv(paste(wd, 'data/easy_clean_rt.csv',sep=''), header=T))

current_data$current <- as.factor(current_data$current)
current_data$other <- as.factor(current_data$other)
current_data$rsi <- as.factor(current_data$rsi)
current_data$transcode <- as.factor(current_data$transcode)

#it's weirdly telling me some subjects don't have observations in all of the conditions
  #subject 23 never switched when RSI == 200
  #subject 28 never repeated when RSI == 200
  ## cutting both of them

current_data <- current_data[subject != 23 & subject != 28]

model1 <- ezANOVA(data = current_data, wid = .(subject), within = .(rsi, transcode), dv = 'rt', detailed = T, type = 2)
model1$ANOVA <- cbind(model1$ANOVA, n2p = model1$ANOVA[,'SSn'] / (model1$ANOVA[, 'SSn'] + model1$ANOVA[,'SSd']))

omnibus_ssd <- model1$ANOVA[4, 'SSd']
omnibus_dfd <- model1$ANOVA[4,'DFd']
omnibus_msd <- omnibus_ssd / omnibus_dfd

#rsi == 200
short_rsi <- current_data[rsi == 200]
short_rsi_model <- ezANOVA(data = short_rsi, wid = .(subject), within = .(transcode), dv = 'rt', detailed = T, type = 2)
short_rsi_ssn <- short_rsi_model$ANOVA[2,'SSn']
short_rsi_dfn <- short_rsi_model$ANOVA[2,'DFn']

short_rsi_msn <- short_rsi_ssn / short_rsi_dfn
f_short_rsi <- short_rsi_msn / omnibus_msd
p_short_rsi <- pf(f_short_rsi, short_rsi_dfn, omnibus_dfd, lower.tail = F)
short_rsi_effect <- short_rsi_ssn / (short_rsi_ssn + omnibus_ssd)
paste("F(",short_rsi_dfn,", ",omnibus_dfd, ") = ", round(f_short_rsi, digits =2), ", p = ", round(p_short_rsi, digits = 5), ', np2 = ', round(short_rsi_effect, digits = 2), sep='')


#rsi == 1100
long_rsi <- current_data[rsi == 1100]
long_rsi_model <- ezANOVA(data = long_rsi, wid = .(subject), within = .(transcode), dv = 'rt', detailed = T, type = 2)
long_rsi_ssn <- long_rsi_model$ANOVA[2,'SSn']
long_rsi_dfn <- long_rsi_model$ANOVA[2,'DFn']

long_rsi_msn <- long_rsi_ssn / long_rsi_dfn
f_long_rsi <- long_rsi_msn / omnibus_msd
p_long_rsi <- pf(f_long_rsi, long_rsi_dfn, omnibus_dfd, lower.tail = F)
long_rsi_effect <- long_rsi_ssn / (long_rsi_ssn + omnibus_ssd)
paste("F(",long_rsi_dfn,", ",omnibus_dfd, ") = ", round(f_long_rsi, digits =2), ", p = ", round(p_long_rsi, digits = 5), ', np2 = ', round(long_rsi_effect, digits = 2), sep='')


#marginal means
rsi_main <- current_data[, .(rt = mean(rt)), by = .(subject, rsi)][, .(mean = mean(rt),se = sd(rt) / sqrt(.N)), by = rsi]
switch_main <- current_data[, .(rt = mean(rt)), by = .(subject, transcode)][, .(mean = mean(rt),se = sd(rt) / sqrt(.N)), by = transcode]

mmeans <- current_data[, .(rt = mean(rt)), by = .(subject, rsi, transcode)][, .(mean = mean(rt),se = sd(rt) / sqrt(.N)), by = .(rsi, transcode)]




## taking a look at current and other

#find subjects with missing cells
missing_data <- current_data[, .(rt = mean(rt)), by = .(subject,current, other, rsi, transcode)][, .(obs = .N), by =subject][obs != 16]
current_data <- current_data[! subject %in% missing_data$subject]

complex_model <- ezANOVA(data = current_data, wid = .(subject), within = .(current,other,rsi,transcode), dv = 'rt', type = 2, detailed = T)

sink(file = paste(wd, 'results/anova/complex_model.txt',sep=''))
complex_model
sink(NULL)






