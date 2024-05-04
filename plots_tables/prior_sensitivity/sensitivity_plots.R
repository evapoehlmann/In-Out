#Set7 
df = read.csv("preferred_model_table.csv")
se_high = read.csv("prior_sensitivity_analysis/high_setting/preferred_model_high.csv")
se_low = read.csv("prior_sensitivity_analysis/low_setting/preferred_model_low.csv")

set7 <- df[, c(1,8)]

set7 <- cbind(set7, se_high[, 8], se_low[, 8])
colnames(set7) <- c("Model","original", "high", "low")

set7[1,1] <- "Unconstrained"
set7[2,1] <- "Positive"
set7[3,1] <- "Common"

set7[3,2] <- 1
set7[3,3] <- 1
set7[2,4] <- 1


set7[, 2:4] <- as.data.frame(sapply(set7[,2:4], as.numeric))

df_long <- reshape2::melt(set7, id.vars = "Model", variable.name = "prior_setting", value.name = "bayes_factor")


print(df_long)

library(ggplot2)
library(tidyr)
library(dplyr)

plot7 <- ggplot(df_long, aes(x=prior_setting, y = bayes_factor, group = Model)) + 
  geom_line() +
  geom_point(aes(shape = Model)) + 
  theme_classic() + 
  ylab("Bayes Factor") + 
  xlab("Prior Setting") +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + 
  scale_shape_manual(values = c("Common" = 0, "Unconstrained" = 8, "Positive" = 19)) + 
  guides(shape = guide_legend(title = "")) + 
  ggtitle("Set 7")

# Set 9 
  

set9 <- df[, c(1,10)]

set9 <- cbind(set9, se_high[, 10], se_low[, 10])
colnames(set9) <- c("Model","original", "high", "low")

set9[1,1] <- "Unconstrained"
set9[2,1] <- "Positive"
set9[3,1] <- "Common"

set9[3,2] <- 1
set9[2,3] <- 1
set9[2,4] <- 1


set9[, 2:4] <- as.data.frame(sapply(set9[,2:4], as.numeric))

df_long <- reshape2::melt(set9, id.vars = "Model", variable.name = "prior_setting", value.name = "bayes_factor")



plot9 <- ggplot(df_long, aes(x=prior_setting, y = bayes_factor, group = Model)) + 
  geom_line() +
  geom_point(aes(shape = Model)) + 
  theme_classic() + 
  ylab("Bayes Factor") + 
  xlab("Prior Setting") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + 
  scale_shape_manual(values = c("Common" = 0, "Unconstrained" = 8, "Positive" = 19)) + 
  guides(shape = guide_legend(title = "")) + 
  ggtitle("Set 9")

# Set 10

set10 <- df[, c(1,11)]

set10 <- cbind(set10, se_high[, 11], se_low[, 11])
colnames(set10) <- c("Model","original", "high", "low")

set10[1,1] <- "Unconstrained"
set10[2,1] <- "Positive"
set10[3,1] <- "Common"

set10[1,2] <- 1
set10[3,3] <- 1
set10[1,4] <- 1


set10[, 2:4] <- as.data.frame(sapply(set10[,2:4], as.numeric))

df_long <- reshape2::melt(set10, id.vars = "Model", variable.name = "prior_setting", value.name = "bayes_factor")


plot10 <- ggplot(df_long, aes(x=prior_setting, y = bayes_factor, group = Model)) + 
  geom_line() +
  geom_point(aes(shape = Model)) + 
  theme_classic() + 
  ylab("Bayes Factor") + 
  xlab("Prior Setting") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + 
  scale_shape_manual(values = c("Common" = 0, "Unconstrained" = 8, "Positive" = 19)) + 
  guides(shape = guide_legend(title = "")) + 
  ggtitle("Set 10")


library(gridExtra)
legend_grob <- cowplot::get_legend(plot7)
combined_plots <- arrangeGrob(plot7 + theme(legend.position = "none"),
                              plot9 + theme(legend.position = "none"),
                              plot10 + theme(legend.position = "none"),
                              ncol = 3)


grid.arrange(combined_plots, legend_grob, ncol = 1, heights = c(4, 1))



  