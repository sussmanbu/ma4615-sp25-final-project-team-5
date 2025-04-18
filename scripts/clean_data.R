suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(here))

hiv_data <- read_csv(here::here("dataset", "HIV_Data_Original_With_Modifications.csv"),
                     na = c("*", "NA"),
                     col_types = cols(
                       .default = col_double(),
                       "Borough" = col_character(),
                       "Neighborhood (U.H.F)" = col_character(),
                       "SEX" = col_character(),
                       "RACE/ETHNICITY" = col_character()
                     ))

hiv_data <- hiv_data |>
  mutate(
    `RACE/ETHNICITY` = str_replace_all(`RACE/ETHNICITY`, "\n", " "),
    `Borough` = str_replace_all(`Borough`, "\n", " "),
    `Neighborhood (U.H.F)` = str_replace_all(`Neighborhood (U.H.F)`, "\n", " "),
    `RACE/ETHNICITY` = str_replace_all(`RACE/ETHNICITY`, "Latino/Hispanic", "Hispanic"),
    `Neighborhood (U.H.F)` = str_replace_all(`Neighborhood (U.H.F)`, "Union Square - Lower Eastside", "Union Square - Lower East Side"),
    `Neighborhood (U.H.F)` = str_replace_all(`Neighborhood (U.H.F)`, "Upper East Side", "Upper Eastside"),
    `Neighborhood (U.H.F)` = str_replace_all(`Neighborhood (U.H.F)`, "Upper West Side", "Upper Westside"),
    `Neighborhood (U.H.F)` = str_replace_all(`Neighborhood (U.H.F)`, "Greenwich Village - SoHo", "Greenwich Village - Soho")
    )


hiv_clean <- hiv_data |>
  filter(complete.cases(hiv_data))

write_rds(hiv_clean, here::here("dataset", "hiv_clean.rds"))

