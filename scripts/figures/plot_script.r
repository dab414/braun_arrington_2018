### this script contains some exploratory plotting, as well as the code used to produce Figure 4, see comment below

library(ggplot2)
library(data.table)
library(effects)
library(lme4)

wd <- 'D:/Users/dbrau/Google Drive/GRAD/Research/By Project/Special Issue Manuscript/analysis/easy_feed_spring2017/'
current_data <- as.data.table(read.csv(paste(wd, 'data/easy_clean.csv',sep=''), header = T))
load(paste(wd, 'results/lrt_check/base1_block.rdata',sep=''))

## plotting the predicted impact of block on probability of switching; estimates generated from the GLMM

store <- Effect('block_c', base1_block, xlevels = 12)

proba <- data.frame(block = 1:12, fit = plogis(store$fit), upper = plogis(store$upper), lower = plogis(store$lower))
probb <- current_data[, .(trans = mean(transcode)), by = .(subject, block)]
proba$block <- as.factor(proba$block)

plot1 <- ggplot(data = proba, aes(x = block, y = fit, group = 1)) +
		geom_point(stat = 'summary', fun.y = sum, size = 1.5) + 
		stat_summary(fun.y = sum, geom = 'line', size = 1.5) + 
		geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) + 
		geom_point(data = probb, aes(x = block, y = trans), alpha = .2) + 
		geom_violin(data = probb, aes(y = trans, group = block), alpha = .4, fill = 'purple', scale = 'count')

plot1

### difference -- full data; same as above, except for difference variable

current_data_bulk <- as.data.table(read.csv(paste(wd, 'data/easy_clean_full.csv',sep=''), header = T))
load(paste(wd, 'results/lrt_check/bulk_check/bulk2_block.rdata',sep=''))

store <- Effect('difference', bulk2_block, xlevels = 21)

proba <- data.frame(difference = -10:10, fit = plogis(store$fit), upper = plogis(store$upper), lower = plogis(store$lower))
probb <- current_data[, .(trans = mean(transcode)), by = .(subject, difference)]
proba$difference <- as.factor(proba$difference)

plot1 <- ggplot(data = proba, aes(x = difference, y = fit, group = 1)) +
  geom_point(stat = 'summary', fun.y = sum, size = 1.5) + 
  stat_summary(fun.y = sum, geom = 'line', size = 1.5) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) + 
  geom_point(data = probb, aes(x = difference, y = trans), alpha = .2) + 
  geom_violin(data = probb, aes(y = trans, group = difference), alpha = .4, fill = 'purple', scale = 'count')

plot1


### difference --- trim data

current_data <- as.data.table(read.csv(paste(wd, 'data/easy_clean.csv',sep=''), header = T))
load(paste(wd, 'results/lrt_check/base1_block.rdata',sep=''))

store <- Effect('difference', base1_block, xlevels = 7)

proba <- data.frame(difference = -3:3, fit = plogis(store$fit), upper = plogis(store$upper), lower = plogis(store$lower))
probb <- current_data[, .(trans = mean(transcode)), by = .(subject, difference)]
proba$difference <- as.factor(proba$difference)
probb$difference <- as.factor(probb$difference)


##plot with full distribution
plot1 <- ggplot(data = proba, aes(x = difference, y = fit, group = 1)) +
  geom_point(stat = 'summary', fun.y = sum, size = 1.5) + 
  stat_summary(fun.y = sum, geom = 'line', size = 1.5) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) + 
  geom_point(data = probb, aes(x = difference, y = trans), alpha = .2) + 
  geom_violin(data = probb, aes(y = trans, group = difference), alpha = .4, fill = 'purple', scale = 'count')

plot1

windowsFonts(Arial = windowsFont('TT Arial'))

## apa plot -- THE CODE TO REPRODUCE FIGURE 4
  plot2 <- ggplot(data = proba, aes(x = difference, y = fit, group = 1)) +
            geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) +
            geom_line() + 
            geom_point(size = 3) + 
            scale_x_discrete("Difference (Other - Current)", breaks = -3:3) +
            scale_y_continuous("Probability of Switching Tasks", limits = c(0, 1), breaks = seq(0,1,by = .1)) + 
            theme_bw() + 
            theme(axis.title.x = element_text(face = 'bold', size = 12, family = 'Arial'),
                 axis.title.y = element_text(face = 'bold', size = 12, angle = 90, family = 'Arial'),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()
                 #plot.margin = unit(c(1,1,1,1), "cm")
                 )
  
  plot2
  
  ggsave(file = paste(wd, 'results/figures/Figure 2.png',sep=''), plot = plot2, width = 5.5, height = 3.5, units = 'in')
  
  
  
### INDIFFERENCE POINT BY BLOCK 
  ## indifference = (-int - (xblock) * (Bblock)) / (Bdiff + Xblock * Binteraction)
  ## remember the model was run on centered values of block
  
  intercept <- summary(base1_block)$coefficients[1,1]
  bblock <- summary(base1_block)$coefficients[2,1]
  bdiff <- summary(base1_block)$coefficients[3,1]
  bint <- summary(base1_block)$coefficients[4,1]
  
  plot_data <- data.frame(block_model = -5:6, block_x = 1:12)
  plot_data$fit <- (-intercept - plot_data$block_model * bblock) / (bdiff + plot_data$block_model * bint)
  
  ## generate some type of variability around these estimates
  
    #subject-wise estimates -- I'm not sure that pulling these subject estimates was the best approach to estimating overall variability in indifference point
      subject_estimates <- coef(base1_block)$subject
    
    subject_predictions <- array(,length(-5:6 * nrow(subject_estimates)))  
      
    #iterate over subjects
      for (i in 1:(nrow(subject_estimates))){
      #iterate over blocks
        for (j in -5:6){
        #if it's the first iteration, make a dataframe
          if (i == 1 & j == 1){
            subject_predictions <- data.frame(subject = i, block = j, indifference = (-subject_estimates[i,1] - j * subject_estimates[i,2]) / (subject_estimates[i,3] + j * subject_estimates[i,4]))
          #otherwise append on to the existing one
          } else subject_predictions <- rbind(subject_predictions, data.frame(subject = i, block = j, indifference = (-subject_estimates[i,1] - j * subject_estimates[i,2]) / (subject_estimates[i,3] + j * subject_estimates[i,4])))
        }
        
      }
  
  subject_predictions <- as.data.table(subject_predictions)
  #trim outliers
  subject_predictions <- subject_predictions[indifference < 2 * sd(indifference) + mean(indifference) & indifference > 2*sd(indifference) - mean(indifference)]
    
  plot_data$se <- subject_predictions[, .(se = sd(indifference) / sqrt(.N)), by = block][,se]
  
  plot_data$block_x <- as.factor(plot_data$block_x)
  
  #plot it out
  indifference_plot <- ggplot(data = plot_data, aes(x = block_x, y = fit, group = 1)) +
    geom_errorbar(aes(ymin = fit - se, ymax = fit + se), width = 0.3) +
    geom_line() + 
    geom_point(size = 3) + 
    scale_x_discrete("Block", breaks = 1:12) +
    scale_y_continuous("Indifference Point") +#, limits = c(0,2), breaks = seq(0,2,by = .2)) + 
    theme_bw() + 
    theme(axis.title.x = element_text(face = 'bold', size = 12, family = 'Arial'),
          axis.title.y = element_text(face = 'bold', size = 12, angle = 90, family = 'Arial'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #plot.margin = unit(c(1,1,1,1), "cm")
    )
  
  indifference_plot
  
  ggsave(file = paste(wd, 'results/figures/Figure 3.png',sep=''), plot = indifference_plot, width = 5.5, height = 3.5, units = 'in')
  
  
  
  
  
  
  
