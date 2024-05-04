
library(ggplot2)

#truncated normal density function
trunc_normal_density <- function(x, mean = 0, sd, lower_bound = 0) {
  adjustment <- pnorm(lower_bound, mean, sd, lower = TRUE)
  scale <- 1 / (1 - adjustment)
  density <- dnorm(x, mean, sd) * (x > lower_bound) * scale
  return(density)
}


x_values <- seq(0, 1, length.out = 1000)

density1 <- trunc_normal_density(x_values, sd = 0.3)
density2 <- trunc_normal_density(x_values, sd = 0.1)
density3 <- trunc_normal_density(x_values, sd = 0.03)
df <- data.frame(
  x = c(x_values, x_values, x_values),
  Density = c(density1, density2, density3),
  Distribution = factor(rep(c("0.3", "0.1", "0.03"), each = 1000))
)


ggplot(df, aes(x = x, y = Density, linetype = Distribution)) +
  geom_line(size = 1.2) +
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +  
  labs(x = expression(delta),
       linetype = bquote(SD[delta])) + 
  theme_minimal() +
  theme(
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14)) 

ggsave("plots_tables/prior_sensitivity/distribution_plot.pdf", device = cairo_pdf(), width = 12, height = 10)
