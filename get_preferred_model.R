dump = read.csv("bayes_factors2.csv")
retransformed_values <- exp(dump[, 2:16])
Mcp <- c()
i <- 1
for (i in 1:15) {
  retransformed_values[3,i] <- retransformed_values[1, i]/retransformed_values[2,i]
}
rownames(retransformed_values) <- c("BFuc", "BFup", "BFpc")


df <- data.frame(matrix(NA, nrow = 3, ncol = 15))

rownames(df) <- c("Mu", "Mp", "Mc")
colnames(df) <- c(1:15)


for (i in 1:15) {
  if (retransformed_values[1,i] > 1 && retransformed_values[2,i] > 1) { #unconstrained wins
    df[1,i] <- "*"
    df[2,i] <- retransformed_values[2,i]
    df[3,i] <- retransformed_values[1,i]
  } else if (retransformed_values[3,i] > 1) { #positive wins
    df[2,i] <- "*"
    df[3,i] <- retransformed_values[3,i]
    df[1,i] <- 1/retransformed_values[2,i]
  } else {
    df[3,i] <- "*"
    df[1,i] <- 1/retransformed_values[1,i]
    df[2,i] <- 1/retransformed_values[3,i]
  }
}

install.packages("rempsyc")
library(rempsyc)
nice_table(df, 
           title = c("Bayes Factor Table"))
write.csv(df, "preferred_model_table.csv", row.names = TRUE)


#sensitivity 1
dump = read.csv("prior_sensitivity_analysis/low_setting/bf_sensitivity1.csv")
retransformed_values <- exp(dump[, 2:16])
Mcp <- c()
i <- 1
for (i in 1:15) {
  retransformed_values[3,i] <- retransformed_values[1, i]/retransformed_values[2,i]
}
rownames(retransformed_values) <- c("BFuc", "BFup", "BFpc")


df <- data.frame(matrix(NA, nrow = 3, ncol = 15))

rownames(df) <- c("Mu", "Mp", "Mc")
colnames(df) <- c(1:15)


for (i in 1:15) {
  if (retransformed_values[1,i] > 1 && retransformed_values[2,i] > 1) { #unconstrained wins
    df[1,i] <- "*"
    df[2,i] <- retransformed_values[2,i]
    df[3,i] <- retransformed_values[1,i]
  } else if (retransformed_values[3,i] > 1) { #positive wins
    df[2,i] <- "*"
    df[3,i] <- retransformed_values[3,i]
    df[1,i] <- 1/retransformed_values[2,i]
  } else {
    df[3,i] <- "*"
    df[1,i] <- 1/retransformed_values[1,i]
    df[2,i] <- 1/retransformed_values[3,i]
  }
}

nice_table(df, 
           title = c("Bayes Factor Table"))
write.csv(df, "prior_sensitivity_analysis/low_setting/preferred_model_low.csv", row.names =TRUE)


#sensitivity 2
dump = read.csv("prior_sensitivity_analysis/high_setting/bf_sensitivity2.csv")
retransformed_values <- exp(dump[, 2:16])
Mcp <- c()
i <- 1
for (i in 1:15) {
  retransformed_values[3,i] <- retransformed_values[1, i]/retransformed_values[2,i]
}
rownames(retransformed_values) <- c("BFuc", "BFup", "BFpc")


df <- data.frame(matrix(NA, nrow = 3, ncol = 15))

rownames(df) <- c("Mu", "Mp", "Mc")
colnames(df) <- c(1:15)


for (i in 1:15) {
  if (retransformed_values[1,i] > 1 && retransformed_values[2,i] > 1) { #unconstrained wins
    df[1,i] <- "*"
    df[2,i] <- retransformed_values[2,i]
    df[3,i] <- retransformed_values[1,i]
  } else if (retransformed_values[3,i] > 1) { #positive wins
    df[2,i] <- "*"
    df[3,i] <- retransformed_values[3,i]
    df[1,i] <- 1/retransformed_values[2,i]
  } else { #common wins
    df[3,i] <- "*"
    df[1,i] <- 1/retransformed_values[1,i]
    df[2,i] <- 1/retransformed_values[3,i]
  }
}

nice_table(df, 
           title = c("Bayes Factor Table"))
write.csv(df, "prior_sensitivity_analysis/high_setting/preferred_model_high.csv", row.names = TRUE)

