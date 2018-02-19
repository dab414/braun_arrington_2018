## running an ANOVA on the 2x2 design for task transitions (i.e., task selections)

library(ez)
library(data.table)

wd <- "D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/"
setwd(wd)

current_data <- read.csv(paste(wd, 'data/easy_clean_full.csv',sep=''), header=T)

current_data$current <- as.factor(current_data$current)
current_data$other <- as.factor(current_data$other)

# running a 2(current task value: decrease vs. constant) X 2(other task value: increase vs. constant) within-subjects ANOVA on proportion of task switches
model1 <- ezANOVA(data = current_data, wid = .(subject), within = .(current, other), dv = 'transcode', type = 2, detailed = T)

omnibus_mse <- model1$ANOVA[4,'SSd'] / model1$ANOVA[4,'DFd']
ssd <- model1$ANOVA[4,'SSd']

attach(current_data)
sub_data <- current_data[(current == 0 & other == 1) | (current == 1 & other == 0),]
detach(current_data)
attach(sub_data)
sub_data$int_code <- ifelse(current == 0 & other == 1, 'other change','current change')
detach(sub_data)
sub_data$int_code <- as.factor(sub_data$int_code)


#following up on the currentXother interaction by analyzing a subset of the data and adjusting the mean-squared error term
int_model <- ezANOVA(data = sub_data, wid = .(subject), within = .(int_code), dv = .(transcode), type = 2, detailed = T)

int_msn <- int_model$ANOVA[2,'SSn']

effect_size <- int_msn / (int_msn + ssd)
f_value <- int_msn / omnibus_mse
df1 <- 1
df2 <- model1$ANOVA[4,'DFd']
p_value <- pf(f_value, df1, df2, lower.tail = F)

current_data <- as.data.table(current_data)
omnibus_model <- cbind(model1$ANOVA, data.frame(partial_eta = model1$ANOVA[,'SSn'] / (model1$ANOVA[,'SSn'] + model1$ANOVA[,'SSd'])))

sink(file = paste(wd, 'results/anova/anova_results.txt',sep=''))
print('Omnibus Model')
omnibus_model
print('Special Contrast')
paste("F(",df1,", ",df2, ") = ", round(f_value, digits =2), ", p = ", round(p_value, digits = 5), ', np2 = ', round(effect_size, digits = 2), sep='')
current_data[, .(switch = mean(transcode)), by = .(subject, current, other)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = .(current,other)][order(current,other)]
sink(NULL)

sink(file = paste(wd, 'results/anova/temp.txt',sep=''))
current_data[, .(switch = mean(transcode)), by = .(subject, current)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = current]
current_data[, .(switch = mean(transcode)), by = .(subject, other)][, .(switch = mean(switch), se = sd(switch) / sqrt(.N)), by = other]
sink(NULL)