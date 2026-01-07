library(arrow)
library(tidyverse)
library(sf)
library(lubridate)

inat_pq <- arrow::open_dataset("./data/inat-canada/iNat_non_sensitive_data_Jan2025.parquet")

query <- inat_pq |> 
  filter(place_state_name == "British Columbia") |> 
  filter(scientific_name %in% c("Zostera marina", "Zostera japonica")) |> 
  filter(quality_grade == "research") %>% 
  select(observed_on_string, latitude, longitude, positional_accuracy, place_county_name, scientific_name) %>% 
  collect() 

str(query)

inat_dat <- as_tibble(query)

clean_dates <- inat_dat$observed_on_string %>%
  str_trim() %>%
  # fix double spaces
  str_replace_all("\\s+", " ") %>%
  # fix invalid "15:25:00 PM" style times
  str_replace(
    "(\\b\\d{2}:\\d{2}:\\d{2})\\s*(AM|PM)",
    "\\1"
  )

inat_dat$observed_on <- parse_date_time(
  clean_dates,
  orders = c(
    "ymd HMS",
    "ymd HM",
    "ymd",
    "ymd HMSz",
    "ymd HMz",
    "Ymd HMS",
    "Ymd HM",
    "Ymd",
    "mdY HMS",
    "mdY HM",
    "a b d Y HMS",
    "a b d Y HM",
    "a b d Y HMS z",
    "a b d Y HM z"
  ),
  tz = "America/Vancouver",
  exact = FALSE
)

# fix the one that didn't parse manually 
inat_dat$observed_on[inat_dat$observed_on_string == "Sat Oct 27 2018 06:37:13 GMT+0100 (GMT+1)"] <- "2018-10-26 22:37:13 PDT"

inat_dat <- inat_dat %>% 
  mutate(year_observed = year(observed_on))

# convert to sf (WGS84)
eel_sf <- st_as_sf(inat_dat, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  mutate(scientific_name = factor(
    scientific_name,
    levels = c("Zostera marina", "Zostera japonica")
  )) %>% 
  mutate(
    observed_on_local = with_tz(observed_on, "America/Vancouver"),
    observed_on_label = format(observed_on_local, "%Y-%m-%d %H:%M:%S %Z")
  ) %>% 
  filter(positional_accuracy < 1000)

saveRDS(eel_sf, "./data/eel_sf_clean.rds")
