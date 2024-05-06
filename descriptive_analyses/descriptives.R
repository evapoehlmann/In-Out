#Set Up
library(tidyverse)
library(psych)
library(haven)
library(httr)


descriptives <- matrix(nrow=15, ncol = 9)
colnames(descriptives) <- c("Study/Experiment", "N", "k per condition", "syllables", "mean effect", "sd of effect", "Cohen's d", "Cohen's dz", "sd overall")

#study1
#Gerten & Topolinski, 2020

study1 <- read_sav("https://osf.io/download/aqsv5/?view_only=750462b8681d4ffcbb48ca9d8dd1f5ae") |>
  filter(type %in% c("inward1000", "outward1000")) |>
  filter(Subj != 328) |>
  filter(!Name %in% c(11, 17, 26, 45, 55, 73)) |>
  group_by(Subj) |>
  filter(n() == 20) |>
  ungroup() |>
  dplyr::select("Subj", "type", "Name") |>
  mutate(
    id = dense_rank(Subj),
    direction = ifelse(type == "inward1000", 0.5, -0.5),
    liking_norm = (as.numeric(Name)-5)/5
  ) |>
  dplyr::select("id", "direction", "liking_norm")

study1$syllables <- 2

#Descriptive analyses
#mean liking per condition
result <- study1 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study1 <- left_join(study1, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study1 <- left_join(study1, wide_result[c("id", "effect")], by = "id")

descriptives[1,1] <- "Gerten & Topolinski, 2020, Experiment 1"
descriptives[1,2] <- length(unique(study1$id))
descriptives[1,3] <- (nrow(study1)/length(unique(study1$id)))/2
descriptives[1,4] <- study1$syllables[1]
descriptives[1,5] <- (describe(study1$effect))$mean
descriptives[1,6] <- (describe(study1$effect))$sd
descriptives[1,7] <- (cohen.d(study1$liking_norm, group = study1$direction))$cohen.d[2]
descriptives[1,8] <- (describe(study1$effect))$mean/(describe(study1$effect))$sd
descriptives[1,9] <- sd(study1$liking_norm)




#study2
#Ingendahl & Vogel, 2022b

study2 <- read_delim(
  "https://osf.io/download/948gw/", 
  locale =  locale(encoding = "UTF-16LE")
) |> 
  filter(!(CASE %in%c(98, 234, 255))) |> 
  mutate(
    CASE = dense_rank(CASE)
  ) |> 
  as.data.frame() |> 
  pivot_longer(
    cols = contains("LI"),
    values_to = "liking",
    names_to = "trial"
  ) |> 
  mutate(
    direction = rep(
      c(
        rep(0.5, times = 27), 
        rep(-0.5, times = 27)
      ),
      298
    )
  ) |> 
  mutate(
    id = dense_rank(CASE),
    liking_norm = (liking - 6) / 5
  ) |> 
  dplyr::select(id, direction, liking_norm) |> 
  arrange(id)

study2$syllables <- 3


#Descriptive analyses
#mean liking per condition
result <- study2 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study2 <- left_join(study2, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study2 <- left_join(study2, wide_result[c("id", "effect")], by = "id")


descriptives[2,1] <- "Ingendahl & Vogel, 2022b"
descriptives[2,2] <- length(unique(study2$id))
descriptives[2,3] <- (nrow(study2)/length(unique(study2$id)))/2
descriptives[2,4] <- study2$syllables[1]
descriptives[2,5] <- (describe(study2$effect))$mean
descriptives[2,6] <- (describe(study2$effect))$sd
descriptives[2,7] <- (cohen.d(study2$liking_norm, group = study2$direction))$cohen.d[2]
descriptives[2,8] <- (describe(study2$effect))$mean/(describe(study2$effect))$sd
descriptives[2,9] <- sd(study2$liking_norm)

#study3
#Ingendahl & Vogel, 2022a

study3 = read_delim("https://osf.io/download/y6pn2/",
           show_col_types = F) |> 
  mutate(id = dense_rank(CASE)) |> 
  select(id, contains("LI")) |> 
  pivot_longer(
    cols = contains("LI"),
    values_to = "rating",
    names_to = "trial"
  ) |> 
  mutate(
    cond = rep(
      c(
        rep("ff", times = 24), 
        rep("fr", times = 24), 
        rep("rf", times = 24), 
        rep("rr", times = 24)
      ),
      349
    )
  ) |> 
  filter(cond %in% c("fr", "rf")) |> 
  mutate(
    direction = ifelse(cond == "fr", .5, -.5),
    liking_norm = (rating - 6) / 5
  )|> 
  select(id, direction, liking_norm)
study3$syllables = 3



#Descriptive analyses
#mean liking per condition
result <- study3 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study3 <- left_join(study3, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study3 <- left_join(study3, wide_result[c("id", "effect")], by = "id")


descriptives[3,1] <- "Ingendahl & Vogel, 2022a"
descriptives[3,2] <- length(unique(study3$id))
descriptives[3,3] <- (nrow(study3)/length(unique(study3$id)))/2
descriptives[3,4] <- study3$syllables[1]
descriptives[3,5] <- (describe(study3$effect))$mean
descriptives[3,6] <- (describe(study3$effect))$sd
descriptives[3,7] <- (cohen.d(study3$liking_norm, group = study3$direction))$cohen.d[2]
descriptives[3,8] <- (describe(study3$effect))$mean/(describe(study3$effect))$sd
descriptives[3,9] <- sd(study3$liking_norm)

#study4
#Ingendahl et al., 2021, Pilot
GET("https://osf.io/download/fnjvq/", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
study4 <- readxl::read_xlsx(tf) |> 
  dplyr::select(starts_with("LI")) |> 
  mutate(sub = 1:n()) |> 
  pivot_longer(
    cols = starts_with("LI"),
    names_prefix = "LI",
    names_to = "item",
    values_to = "Y"
  ) |> 
  mutate(
    item = dense_rank(item),
    cond = ifelse(item%%2, .5, -.5),
    liking_norm = (Y - 6) / 5
  ) |> 
  dplyr::select(sub, cond, liking_norm)
unlink(tf)
study4$syllables <- 3



#Descriptive analyses
#mean liking per condition
result <- study4 %>%
  group_by(sub, cond) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study4 <- left_join(study4, result, by = c("sub", "cond"))

#compute effect
wide_result <- spread(result, key = cond, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study4 <- left_join(study4, wide_result[c("sub", "effect")], by = "sub")


descriptives[4,1] <- "Ingendahl et al., 2021, Pilot"
descriptives[4,2] <- length(unique(study4$sub))
descriptives[4,3] <- (nrow(study4)/length(unique(study4$sub)))/2
descriptives[4,4] <- study4$syllables[1]
descriptives[4,5] <- (describe(study4$effect))$mean
descriptives[4,6] <- (describe(study4$effect))$sd
descriptives[4,7] <- (cohen.d(study4$liking_norm, group = study4$cond))$cohen.d[2]
descriptives[4,8] <- (describe(study4$effect))$mean/(describe(study4$effect))$sd
descriptives[4,9] <- sd(study4$liking_norm)

#study5 
#Ingendahl et al., 2021, Experiment 1

GET("https://osf.io/download/mcak9/", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
study5 <- readxl::read_xlsx(tf) |> 
  filter(!(CASE %in% c(150, 662))) |> 
  pivot_longer(
    cols = contains("LI"),
    names_to = "trial",
    values_to = "Y"
  ) |> 
  mutate(
    phase = rep(rep(c("training", "same", "new"), each = 24), length(unique(CASE))),
    direction = rep(rep(c(.5, -.5), 3 * 12), length(unique(CASE))),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |> 
  filter(phase == "new") |>
  select(id, direction, liking_norm)
unlink(tf)
study5$syllables <- 3


#Descriptive analyses
#mean liking per condition
result <- study5 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study5 <- left_join(study5, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study5 <- left_join(study5, wide_result[c("id", "effect")], by = "id")


descriptives[5,1] <- "Ingendahl et al., 2021, Experiment 1"
descriptives[5,2] <- length(unique(study5$id))
descriptives[5,3] <- (nrow(study5)/length(unique(study5$id)))/2
descriptives[5,4] <- study5$syllables[1]
descriptives[5,5] <- (describe(study5$effect))$mean
descriptives[5,6] <- (describe(study5$effect))$sd
descriptives[5,7] <- (cohen.d(study5$liking_norm, group = study5$direction))$cohen.d[2]
descriptives[5,8] <- (describe(study5$effect))$mean/(describe(study5$effect))$sd
descriptives[5,9] <- sd(study5$liking_norm)

#study6
##Ingendahl, Vogel and Wänke, 2022

study6 <- read.table("https://osf.io/download/ku67e/", 
                     header = T, sep = "\t", 
                     fileEncoding = "UTF-16LE") |>
  filter(IV08_01 == "Liking") |>
  dplyr::select(starts_with("LI")) |> 
  mutate(CASE = 1:n()) |> 
  pivot_longer(cols = contains("LI"),
               names_prefix = "LI",
               names_to = "item",
               values_to = "Y"
  ) |>  
  mutate(
    cond = rep(
      c(
        rep("ff", times = 12), 
        rep("fr", times = 12), 
        rep("rf", times = 12), 
        rep("rr", times = 12)
      ),
      165
    )
  ) |> 
  filter(cond %in% c("fr", "rf")) |>
  mutate(
    direction = ifelse(cond == "fr", 0.5, -0.5),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |>
  dplyr::select(id, direction, liking_norm)
study6$syllables <- 3


#Descriptive analyses
#mean liking per condition
result <- study6 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study6 <- left_join(study6, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study6 <- left_join(study6, wide_result[c("id", "effect")], by = "id")


descriptives[6,1] <- "Ingendahl, Vogel and Wänke, 2022, Experiment 2"
descriptives[6,2] <- length(unique(study6$id))
descriptives[6,3] <- (nrow(study6)/length(unique(study6$id)))/2
descriptives[6,4] <- study6$syllables[1]
descriptives[6,5] <- (describe(study6$effect))$mean
descriptives[6,6] <- (describe(study6$effect))$sd
descriptives[6,7] <- (cohen.d(study6$liking_norm, group = study6$direction))$cohen.d[2]
descriptives[6,8] <- (describe(study6$effect))$mean/(describe(study6$effect))$sd
descriptives[6,9] <- sd(study6$liking_norm)


#study7 

# Topolinski et al., 2024, Experiment 1
url <- "https://osf.io/download/ekdr9/?view_only=6be49bfc6a3d40beada864f74208b6b0"
filename <- "ds1raw.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
study7 <- readxl::read_xlsx(filename)
study7$CASE  <- rownames(study7)
study7 <- cbind(study7, do.call(rbind, apply(study7[c("LI01_01", "LI02_01", "LI03_01", "LI04_01", "LI05_01", "LI06_01", "LI07_01", "LI08_01", "LI09_01", "LI10_01", 
                                                      "LI11_01", "LI12_01", "LI13_01", "LI14_01", "LI15_01", "LI16_01", "LI17_01", "LI18_01", "LI19_01", "LI20_01",
                                                      "LI21_01", "LI22_01", "LI23_01", "LI24_01", "LI25_01", "LI26_01", "LI27_01", "LI28_01", "LI29_01", "LI30_01", 
                                                      "LI31_01", "LI32_01", "LI33_01", "LI34_01", "LI35_01", "LI36_01", "LI37_01", "LI38_01", "LI39_01", "LI40_01", 
                                                      "LI41_01", "LI42_01", "LI43_01", "LI44_01", "LI45_01", "LI46_01", "LI47_01", "LI48_01", "LI49_01", "LI50_01", 
                                                      "LI51_01", "LI52_01", "LI53_01", "LI54_01")], 1, function(x) {
                                                        x1 <- table(x)
                                                        data.frame(var.judgments = max(x1), mode=names(x1)[which.max(x1)])})))
unlink(filename)


study7 <- study7 |>
  dplyr::select(starts_with("LI")) |> 
  mutate(CASE = 1:n()) |> 
  pivot_longer(cols = contains("LI"),
               names_prefix = "LI",
               names_to = "item",
               values_to = "Y"
  ) |>  
  mutate(
    direction = rep(
      c(
        rep("lc", times = 9), 
        rep("cl", times = 9), 
        rep("ld", times = 9), 
        rep("dl", times = 9),
        rep("cd", times = 9),
        rep("dc", times = 9)
      ),
      308
    )
  ) |>
  filter(direction %in% c("ld", "dl")) |>
  mutate(
    direction = ifelse(direction == "ld", 0.5, -0.5),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |>
  dplyr::select(id, direction, liking_norm)
study7$syllables <- 2


#Descriptive analyses
#mean liking per condition
result <- study7 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study7 <- left_join(study7, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study7 <- left_join(study7, wide_result[c("id", "effect")], by = "id")


descriptives[7,1] <- "Topolinski et al., 2024, Experiment 1"
descriptives[7,2] <- length(unique(study7$id))
descriptives[7,3] <- (nrow(study7)/length(unique(study7$id)))/2
descriptives[7,4] <- study7$syllables[1]
descriptives[7,5] <- (describe(study7$effect))$mean
descriptives[7,6] <- (describe(study7$effect))$sd
descriptives[7,7] <- (cohen.d(study7$liking_norm, group = study7$direction))$cohen.d[2]
descriptives[7,8] <- (describe(study7$effect))$mean/(describe(study7$effect))$sd
descriptives[7,9] <- sd(study7$liking_norm)

#study8
#Topolinski et al., 2024, Experiment 2
url <- "https://osf.io/download/dtpnf/?view_only=6be49bfc6a3d40beada864f74208b6b0"
filename <- "ds2raw.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
study8 <- readxl::read_xlsx(filename)

study8 <- cbind(study8, do.call(rbind, apply(study8[c("LI01_01", "LI02_01", "LI03_01", "LI04_01", "LI05_01", "LI06_01", "LI07_01", "LI08_01", "LI09_01", "LI10_01", 
                                                      "LI11_01", "LI12_01", "LI13_01", "LI14_01", "LI15_01", "LI16_01", "LI17_01", "LI18_01", "LI19_01", "LI20_01",
                                                      "LI21_01", "LI22_01", "LI23_01", "LI24_01", "LI25_01", "LI26_01", "LI27_01", "LI28_01", "LI29_01", "LI30_01", 
                                                      "LI31_01", "LI32_01", "LI33_01", "LI34_01", "LI35_01", "LI36_01", "LI37_01", "LI38_01", "LI39_01", "LI40_01", "LI41_01", "LI42_01", "LI43_01", "LI44_01", "LI45_01", "LI46_01", "LI47_01", "LI48_01", "LI49_01", "LI50_01", 
                                                      "LI51_01", "LI52_01", "LI53_01", "LI54_01")], 1, function(x) {
                                                        x1 <- table(x)
                                                        data.frame(var.judgments = max(x1), mode=names(x1)[which.max(x1)])})))


unlink(filename)

study8 <- study8 |>
  dplyr::select(starts_with("LI")) |> 
  mutate(CASE = 1:n()) |> 
  pivot_longer(cols = contains("LI"),
               names_prefix = "LI",
               names_to = "item",
               values_to = "Y"
  ) |>  
  mutate(
    direction = rep(
      c(
        rep("lc", times = 9), 
        rep("cl", times = 9), 
        rep("ld", times = 9), 
        rep("dl", times = 9),
        rep("cd", times = 9),
        rep("dc", times = 9)
      ),
      303
    )
  ) |>
  filter(direction %in% c("ld", "dl")) |>
  mutate(
    direction = ifelse(direction == "ld", 0.5, -0.5),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |> 
  dplyr::select(id, direction, liking_norm) |>
  arrange(id)

study8$syllables <- 2


#Descriptive analyses
#mean liking per condition
result <- study8 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study8 <- left_join(study8, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study8 <- left_join(study8, wide_result[c("id", "effect")], by = "id")


descriptives[8,1] <- "Topolinski et al., 2024, Experiment 2"
descriptives[8,2] <- length(unique(study8$id))
descriptives[8,3] <- (nrow(study8)/length(unique(study8$id)))/2
descriptives[8,4] <- study8$syllables[1]
descriptives[8,5] <- (describe(study8$effect))$mean
descriptives[8,6] <- (describe(study8$effect))$sd
descriptives[8,7] <- (cohen.d(study8$liking_norm, group = study8$direction))$cohen.d[2]
descriptives[8,8] <- (describe(study8$effect))$mean/(describe(study8$effect))$sd
descriptives[8,9] <- sd(study8$liking_norm)

#study9
# Topolinski et al., 2024, Experiment 3
url <- "https://osf.io/download/g6cpj/?view_only=6be49bfc6a3d40beada864f74208b6b0"
filename <- "ds3raw.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
study9 <- readxl::read_xlsx(filename)

study9 <- cbind(study9, do.call(rbind, apply(study9[c("LI01_01", "LI02_01", "LI03_01", "LI04_01", "LI05_01", "LI06_01", "LI07_01", "LI08_01", "LI09_01", "LI10_01", 
                                                         "LI11_01", "LI12_01", "LI13_01", "LI14_01", "LI15_01", "LI16_01", "LI17_01", "LI18_01", "LI19_01", "LI20_01",
                                                         "LI21_01", "LI22_01", "LI23_01", "LI24_01", "LI25_01", "LI26_01", "LI27_01", "LI28_01", "LI29_01", "LI30_01", 
                                                         "LI31_01", "LI32_01", "LI33_01", "LI34_01", "LI35_01", "LI36_01", "LI37_01", "LI38_01", "LI39_01", "LI40_01", "LI41_01", "LI42_01", "LI43_01", "LI44_01", "LI45_01", "LI46_01", "LI47_01", "LI48_01", "LI49_01", "LI50_01", 
                                                         "LI51_01", "LI52_01", "LI53_01", "LI54_01")], 1, function(x) {
                                                           x1 <- table(x)
                                                           data.frame(var.judgments = max(x1), mode=names(x1)[which.max(x1)])})))


unlink(filename)
study9 <- study9 |>
  dplyr::select(starts_with("LI")) |> 
  mutate(CASE = 1:n()) |> 
  pivot_longer(cols = contains("LI"),
               names_prefix = "LI",
               names_to = "item",
               values_to = "Y"
  ) |>  
  mutate(
    direction = rep(
      c(
        rep("lc", times = 9), 
        rep("cl", times = 9), 
        rep("ld", times = 9), 
        rep("dl", times = 9),
        rep("cd", times = 9),
        rep("dc", times = 9)
      ),
      401
    )
  ) |>
  filter(direction %in% c("ld", "dl")) |>
  mutate(
    direction = ifelse(direction == "ld", 0.5, -0.5),
    id = dense_rank(CASE),
    liking_norm = (Y - 6) / 5
  ) |> 
  dplyr::select(id, direction, liking_norm)
study9$syllables <- 3


#Descriptive analyses
#mean liking per condition
result <- study9 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study9 <- left_join(study9, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study9 <- left_join(study9, wide_result[c("id", "effect")], by = "id")


descriptives[9,1] <- "Topolinski et al., 2024, Experiment 3"
descriptives[9,2] <- length(unique(study9$id))
descriptives[9,3] <- (nrow(study9)/length(unique(study9$id)))/2
descriptives[9,4] <- study9$syllables[1]
descriptives[9,5] <- (describe(study9$effect))$mean
descriptives[9,6] <- (describe(study9$effect))$sd
descriptives[9,7] <- (cohen.d(study9$liking_norm, group = study9$direction))$cohen.d[2]
descriptives[9,8] <- (describe(study9$effect))$mean/(describe(study9$effect))$sd
descriptives[9,9] <- sd(study9$liking_norm)


#study 10

#Ingendahl et al., 2023, Experiment 1

study10 <- read.csv("https://osf.io/download/f8xer/")  |>
  mutate(
    wordtype = case_when(
      origin == "voc" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!subject_nr %in% c(29, 82, 85)) |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(subject_nr)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id"))

study10 <- arrange(study10, id)
study10$syllables <- 3


#Descriptive analyses
#mean liking per condition
result <- study10 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study10 <- left_join(study10, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study10 <- left_join(study10, wide_result[c("id", "effect")], by = "id")


descriptives[10,1] <- "Ingendahl et al., 2023, Experiment 1"
descriptives[10,2] <- length(unique(study10$id))
descriptives[10,3] <- (nrow(study10)/length(unique(study10$id)))/2
descriptives[10,4] <- study10$syllables[1]
descriptives[10,5] <- (describe(study10$effect))$mean
descriptives[10,6] <- (describe(study10$effect))$sd
descriptives[10,7] <- (cohen.d(study10$liking_norm, group = study10$direction))$cohen.d[2]
descriptives[10,8] <- (describe(study10$effect))$mean/(describe(study10$effect))$sd
descriptives[10,9] <- sd(study10$liking_norm)


#study11

#Ingendahl et al., 2023, Experiment 2
url <- "https://osf.io/download/vwjax/"
filename <- "studyraw2OS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))


study11 <-readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid %in% c("766109203", "415721123")) |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id"))

study11 <- arrange(study11, id)
study11$syllables <- 3

unlink(filename)


#Descriptive analyses
#mean liking per condition
result <- study11 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study11 <- left_join(study11, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study11 <- left_join(study11, wide_result[c("id", "effect")], by = "id")


descriptives[11,1] <- "Ingendahl et al., 2023, Experiment 2"
descriptives[11,2] <- length(unique(study11$id))
descriptives[11,3] <- (nrow(study11)/length(unique(study11$id)))/2
descriptives[11,4] <- study11$syllables[1]
descriptives[11,5] <- (describe(study11$effect))$mean
descriptives[11,6] <- (describe(study11$effect))$sd
descriptives[11,7] <- (cohen.d(study11$liking_norm, group = study11$direction))$cohen.d[2]
descriptives[11,8] <- (describe(study11$effect))$mean/(describe(study11$effect))$sd
descriptives[11,9] <- sd(study11$liking_norm)


#study12

#Ingendahl et al., 2023, Experiment 3
url <- "https://osf.io/download/uew96/"
filename <- "study3rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))

study12 <- readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid %in% c("766109203", "415721123")) |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id"))

study12 <- arrange(study12, id)
study12$syllables <- 3

unlink(filename)


#Descriptive analyses
#mean liking per condition
result <- study12 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study12 <- left_join(study12, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study12 <- left_join(study12, wide_result[c("id", "effect")], by = "id")


descriptives[12,1] <- "Ingendahl et al., 2023, Experiment 3"
descriptives[12,2] <- length(unique(study12$id))
descriptives[12,3] <- (nrow(study12)/length(unique(study12$id)))/2
descriptives[12,4] <- study12$syllables[1]
descriptives[12,5] <- (describe(study12$effect))$mean
descriptives[12,6] <- (describe(study12$effect))$sd
descriptives[12,7] <- (cohen.d(study12$liking_norm, group = study12$direction))$cohen.d[2]
descriptives[12,8] <- (describe(study12$effect))$mean/(describe(study12$effect))$sd
descriptives[12,9] <- sd(study12$liking_norm)


#study13

# Ingendahl et al., 2023, Experiment 4
url <- "https://osf.io/download/nfwjv/"
filename <- "study4rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))


study13 <- readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid == "276164680") |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id"))

study13 <- arrange(study13, id)
study13$syllables <- 3

unlink(filename)

#Descriptive analyses
#mean liking per condition
result <- study13 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study13 <- left_join(study13, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study13 <- left_join(study13, wide_result[c("id", "effect")], by = "id")


descriptives[13,1] <- "Ingendahl et al., 2023, Experiment 4"
descriptives[13,2] <- length(unique(study13$id))
descriptives[13,3] <- (nrow(study13)/length(unique(study13$id)))/2
descriptives[13,4] <- study13$syllables[1]
descriptives[13,5] <- (describe(study13$effect))$mean
descriptives[13,6] <- (describe(study13$effect))$sd
descriptives[13,7] <- (cohen.d(study13$liking_norm, group = study13$direction))$cohen.d[2]
descriptives[13,8] <- (describe(study13$effect))$mean/(describe(study13$effect))$sd
descriptives[13,9] <- sd(study13$liking_norm)


#study14


##Study 14, Ingendahl et al., 2023, Experiment 5
url <- "https://osf.io/download/qs2kr/"
filename <- "study5rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))

study14 <- readxl::read_xlsx(filename) |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
      origin == "newc"  ~ "New Consonants",
      TRUE              ~ "Conditioned"  
    )
  ) |>
  filter(!OSid %in% c(737495106, 817895977)) |>
  filter(current == "rating") |>
  mutate(
    direction = ifelse(direction == "in", 0.5, -0.5),
    liking_norm = ((as.numeric(response))-5)/4,
    id = dense_rank(OSid)
  ) |>
  filter(wordtype == "New Consonants") |>
  dplyr::select(c("direction", "liking_norm", "id"))

study14 <- arrange(study14, id)
study14$syllables <- 3

unlink(filename)

#Descriptive analyses
#mean liking per condition
result <- study14 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study14 <- left_join(study14, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study14 <- left_join(study14, wide_result[c("id", "effect")], by = "id")


descriptives[14,1] <- "Ingendahl et al., 2023, Experiment 5"
descriptives[14,2] <- length(unique(study14$id))
descriptives[14,3] <- (nrow(study14)/length(unique(study14$id)))/2
descriptives[14,4] <- study14$syllables[1]
descriptives[14,5] <- (describe(study14$effect))$mean
descriptives[14,6] <- (describe(study14$effect))$sd
descriptives[14,7] <- (cohen.d(study14$liking_norm, group = study14$direction))$cohen.d[2]
descriptives[14,8] <- (describe(study14$effect))$mean/(describe(study14$effect))$sd
descriptives[14,9] <- sd(study14$liking_norm)



#study15
#Körner & Rummer, 2021, Experiment 2

study15 <-  read.csv("https://osf.io/download/gcf9s/",
                     sep = ";") |> 
  filter(blockcode == "evalWords") |>
  filter(trialcode %in% c("wordBF", "wordFB")) |>
  mutate(
    direction = ifelse(trialcode == "wordFB", 0.5, -0.5),
    id = dense_rank(subject),
    liking_norm = (as.numeric(response)-5)/5
  ) |>
  dplyr::select(c("id", "direction", "liking_norm"))
study15 <- arrange(study15, id)  
study15$syllables <- 2


#Descriptive analyses
#mean liking per condition
result <- study15 %>%
  group_by(id, direction) %>%
  summarise(mean_liking = mean(liking_norm, na.rm = TRUE))

study15 <- left_join(study15, result, by = c("id", "direction"))

#compute effect
wide_result <- spread(result, key = direction, value = mean_liking)

wide_result <- wide_result %>%
  mutate(effect =  `0.5` - `-0.5`)

study15 <- left_join(study15, wide_result[c("id", "effect")], by = "id")


descriptives[15,1] <- "Körner & Rummer, 2022, Experiment 2"
descriptives[15,2] <- length(unique(study15$id))
descriptives[15,3] <- (nrow(study15)/length(unique(study15$id)))/2
descriptives[15,4] <- study15$syllables[1]
descriptives[15,5] <- (describe(study15$effect))$mean
descriptives[15,6] <- (describe(study15$effect))$sd
descriptives[15,7] <- (cohen.d(study15$liking_norm, group = study15$direction))$cohen.d[2]
descriptives[15,8] <- (describe(study15$effect))$mean/(describe(study15$effect))$sd
descriptives[15,9] <- sd(study15$liking_norm)

write.csv(descriptives, "descriptive_analyses/Descriptive Results.csv", row.names = FALSE)
