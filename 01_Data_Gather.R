library(tidyverse)
library(anyflights)
library(ggview)
library(timetk)
library(modeltime)
library(tidymodels)
library(fastDummies)

# for(y in 2018:2025){
#   get_flights("LAX", year = y, dir = "99_data")
# }

full_flight_df <- tibble()

for (i in list.files("99_data", full.names = TRUE)) {
  load(i)
  full_flight_df <- bind_rows(full_flight_df, flights)
}

rm(flights)
