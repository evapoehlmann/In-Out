#Set Up
library(dplyr)
library(haven)
library(tidyverse)


#study1
#Gerten & Topolinski, 2020

study1 <- read_sav("https://osf.io/download/aqsv5/?view_only=750462b8681d4ffcbb48ca9d8dd1f5ae") |>
  filter(type %in% c("inward1000", "outward1000")) |>
  filter(Subj != 328) |>
  dplyr::select("Subj", "type", "Name") |>
  filter(!Name %in% c(11, 17, 26, 45, 55, 73)) |>
  group_by(Subj) |>
  filter(n() == 20) |>
  ungroup() |>
  mutate(
    id = dense_rank(Subj),
    direction = ifelse(type == "inward1000", 0.5, -0.5),
    liking_norm = (as.numeric(Name)-5)/5
  ) |>
  dplyr::select("Name","id", "direction", "liking_norm")

study1$syllables <- 2

###Study2 - Ingendahl & Vogel, 2022b

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
  dplyr::select(id, direction, liking, liking_norm) |> 
  arrange(id)

study2$syllables <- 3


###Study 3 - Ingendahl & Vogel, 2022a

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
  select(id, direction, rating, liking_norm)
study3$syllables = 3



##Study 4 - Ingendahl et al., 2021, Pilot data
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
  dplyr::select(sub, cond, Y, liking_norm)
unlink(tf)
study4$syllables <- 3


##Study 5 - Ingendahl et al., 2021 Experiment 1


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
  select(id, direction, Y, liking_norm)
unlink(tf)
study5$syllables <- 3


##Study 6 - Ingendahl, Vogel and Wänke, 2022

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
  dplyr::select(id, direction, Y, liking_norm)
study6$syllables <- 3

#Study 7 - Topolinski et al., 2024, Experiment 1
url <- "https://osf.io/download/ekdr9/?view_only=6be49bfc6a3d40beada864f74208b6b0"
filename <- "ds1raw.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
study7 <- readxl::read_xlsx(filename)
study7$CASE  <- rownames(study7)
dim(study7)
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
  dplyr::select(id, direction, Y, liking_norm)
study7$syllables <- 2

##Study 8, Topolinski et al., 2024, Experiment 2

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
  dplyr::select(id, direction,Y, liking_norm)
study8$syllables <- 2

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
  dplyr::select(id, direction, Y, liking_norm)
study9$syllables <- 3



##Study 10, Ingendahl et al., 2023, Experiment 1

study10 <- read.csv("https://osf.io/download/f8xer/")  |>
  mutate(
    wordtype = case_when(
      origin == "samec" ~ "Same Consonants",
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
  dplyr::select(c("direction", "liking_norm", "response", "id"))

study10 <- arrange(study10, id)
study10$syllables <- 3

##something's wrong here, not all data points?## only 24 trials per subject


#Study 11, Ingendahl et al., 2023, Experiment 2

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
  dplyr::select(c("direction", "liking_norm", "response", "id"))

study11 <- arrange(study11, id)
study11$syllables <- 3

unlink(filename)


##Study 12, Ingendahl et al., 2023, Experiment 3

url <- "https://osf.io/download/uew96/"
filename <- "study3rawOS.xlsx"
GET(url, write_disk(filename, overwrite = TRUE))
study12 <- readxl::read_xlsx(filename)


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
  dplyr::select(c("direction", "liking_norm", "response", "id"))

study12 <- arrange(study12, id)
study12$syllables <- 3

unlink(filename)


#Study 13, Ingendahl et al., 2023, Experiment 4
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
  dplyr::select(c("direction", "liking_norm", "response", "id"))

study13 <- arrange(study13, id)
study13$syllables <- 3

unlink(filename)

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
  dplyr::select(c("direction", "response","liking_norm", "id"))

study14 <- arrange(study14, id)
study14$syllables <- 3

unlink(filename)

##Study 15, Körner & Rummer, 2022, Experiment 2

study15 <-  read.csv("https://osf.io/download/gcf9s/",
                     sep = ";") |> 
  filter(blockcode == "evalWords") |>
  filter(trialcode %in% c("wordBF", "wordFB")) |>
  mutate(
    direction = ifelse(trialcode == "wordFB", 0.5, -0.5),
    id = dense_rank(subject),
    liking_norm = (as.numeric(response)-5)/5
  ) |>
  dplyr::select(c("id", "direction","response", "liking_norm"))
study15 <- arrange(study15, id)  
study15$syllables <- 2

