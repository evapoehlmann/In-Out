library(xtable)
df = read.csv("preferred_model_table.csv")
rownames(df) <- c("Unconstrained", "Positive", "Common")

df <- df[, 2:16]
colnames(df) <- c(1:15)
table1 <- df[, 1:5]
table2 <- df[, 6:10]
table3 <- df[, 11:15]


#table 1
table1[1,] <- 0
table1 <- as.data.frame(sapply(table1, as.numeric))
table1[, sapply(table1, is.numeric)] <- round(table1[, sapply(table1, is.numeric)], 2)

table1[1,] <- "*"

latex_table <- xtable(table1)
print(latex_table, include.rownames = TRUE)



#table 2
table2[1,1] <-0
table2[1,5] <- 0
table2[3, 2:4] <- 0

table2 <- as.data.frame(sapply(table2, as.numeric))
table2[, sapply(table2, is.numeric)] <- round(table2[, sapply(table2, is.numeric)], 2)

table2[1,1] <- "*"
table2[1,5] <- "*"
table2[3, 2:4] <- "*"


latex_table <- xtable(table2)
print(latex_table, include.rownames = TRUE)


#table 3
table3[1, 1:2] <- 0
table3[1, 4] <- 0
table3[2, 3] <- 0 
table3[2, 5] <- 0

table3 <- as.data.frame(sapply(table3, as.numeric))
table3[, sapply(table3, is.numeric)] <- round(table3[, sapply(table3, is.numeric)], 2)


table3[1, 1:2] <- "*"
table3[1, 4] <- "*"
table3[2, 3] <- "*"
table3[2, 5] <- "*"


latex_table <- xtable(table3)
print(latex_table, include.rownames = TRUE)


#sensitivity low

df = read.csv("prior_sensitivity_analysis/low_setting/preferred_model_low.csv")
rownames(df) <- c("Unconstrained", "Positive", "Common")

df <- df[, 2:16]
colnames(df) <- c(1:15)
table1 <- df[, 1:5]
table2 <- df[, 6:10]
table3 <- df[, 11:15]


#table 1
table1[1,] <- 0
table1 <- as.data.frame(sapply(table1, as.numeric))
table1[, sapply(table1, is.numeric)] <- round(table1[, sapply(table1, is.numeric)], 2)

table1[1,] <- "*"
latex_table <- xtable(table1)
print(latex_table, include.rownames = TRUE)




#table 2
table2[1,1] <-0
table2[2,2] <- 0
table2[3,3] <- 0
table2[2,4] <- 0
table2[1,5] <- 0


table2 <- as.data.frame(sapply(table2, as.numeric))
table2[, sapply(table2, is.numeric)] <- round(table2[, sapply(table2, is.numeric)], 2)

table2[1,1] <-"*"
table2[2,2] <- "*"
table2[3,3] <- "*"
table2[2,4] <- "*"
table2[1,5] <- "*"



latex_table <- xtable(table2)
print(latex_table, include.rownames = TRUE)


#table 3
table3[1, 1:2] <- 0
table3[1, 4] <- 0
table3[2, 3] <- 0 
table3[2, 5] <- 0

table3 <- as.data.frame(sapply(table3, as.numeric))
table3[, sapply(table3, is.numeric)] <- round(table3[, sapply(table3, is.numeric)], 2)


table3[1, 1:2] <- "*"
table3[1, 4] <- "*"
table3[2, 3] <- "*"
table3[2, 5] <- "*"


latex_table <- xtable(table3)
print(latex_table, include.rownames = TRUE)


#sensitivity setting high


df = read.csv("prior_sensitivity_analysis/high_setting/preferred_model_high.csv")
rownames(df) <- c("Unconstrained", "Positive", "Common")

df <- df[, 2:16]
colnames(df) <- c(1:15)
table1 <- df[, 1:5]
table2 <- df[, 6:10]
table3 <- df[, 11:15]


#table 1
table1[1,] <- 0
table1 <- as.data.frame(sapply(table1, as.numeric))
table1[, sapply(table1, is.numeric)] <- round(table1[, sapply(table1, is.numeric)], 2)

table1[1,] <- "*"
latex_table <- xtable(table1)
print(latex_table, include.rownames = TRUE)




#table 2
table2[1,1] <-0
table2[3,2] <- 0
table2[3,3] <- 0
table2[2,4] <- 0
table2[3,5] <- 0


table2 <- as.data.frame(sapply(table2, as.numeric))
table2[, sapply(table2, is.numeric)] <- round(table2[, sapply(table2, is.numeric)], 2)

table2[1,1] <- "*"
table2[3,2] <- "*"
table2[3,3] <- "*"
table2[2,4] <- "*"
table2[3,5] <- "*"



latex_table <- xtable(table2)
print(latex_table, include.rownames = TRUE)


#table 3
table3[1, 1:2] <- 0
table3[1, 4] <- 0
table3[2, 3] <- 0 
table3[2, 5] <- 0

table3 <- as.data.frame(sapply(table3, as.numeric))
table3[, sapply(table3, is.numeric)] <- round(table3[, sapply(table3, is.numeric)], 2)


table3[1, 1:2] <- "*"
table3[1, 4] <- "*"
table3[2, 3] <- "*"
table3[2, 5] <- "*"


latex_table <- xtable(table3)
print(latex_table, include.rownames = TRUE)

#descriptives table

df <- read.csv("descriptive_analyses/Descriptive Results.csv")

df <- subset(df, select = c("Study.Experiment", "N", "k.per.condition", "mean.effect", "sd.of.effect", "Cohen.s.dz", "sd.trial.by.trial"))
latex_table <- xtable(df)
print(latex_table, include.rownames = TRUE)