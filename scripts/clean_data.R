suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(here))

hiv_data <- read_csv(here::here("dataset", "HIV_Data_Original_With_Boroughs_Filled_In.csv"),
                     na = c("*", "NA"),
                     col_types = cols(
                       .default = col_double(),
                       "Borough" = col_character(),
                       "Neighborhood (U.H.F)" = col_character(),
                       "SEX" = col_character(),
                       "RACE/ETHNICITY" = col_character()
                     ))

hiv_data <- hiv_data |>
  mutate(`RACE/ETHNICITY` = str_replace_all(`RACE/ETHNICITY`, "\n", " "), `RACE/ETHNICITY` = str_trim(`RACE/ETHNICITY`))

hiv_clean <- hiv_data |>
  filter(complete.cases(hiv_data))

write_rds(hiv_clean, here::here("dataset", "hiv_clean.rds"))

