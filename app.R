#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(shinyLP)
library(readxl)
library(highcharter)
library(tidyverse)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(tigris)
library(janitor)
library(fredr)
library(sf)
library(zoo)

fredr_set_key("22c6ffaa111781ee88df344a4f120eef")

blank <- "https://api.mapbox.com/styles/v1/mrw03b/cji21nw3e03ht2rqz4kgte0ea/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibXJ3MDNiIiwiYSI6IlYwb2FiOWcifQ.RWUm2a87fEC9XrDxzvZKKg"
map_attr <- "<a href='https://www.mapbox.com/map-feedback/'>© MAPBOX</a> | <a href='http://texas2036.org'> MAP © TEXAS 2036</a>"

thumbnail_label <- function (title, label, content, button_link, button_label) {
  div(class = "row", div(class = "col-sm-14 col-md-12",
                         div(class = "thumbnail", 
                             HTML(title),
                             h3(label), 
                             p(content), 
                             div(class = "caption",
                                 p(a(href = button_link, 
                                     class = "btn btn-primary", 
                                     role = "button", button_label))))))
}

# DATA PREP CODE ----------------------------------------------------------


# **Coronavirus Data -----------------------------------------------------------


# ~~JHU Data --------------------------------------------------------------


jhu_cases_state_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_state.csv") %>% 
  janitor::clean_names() %>% 
  filter(country_region=="US") %>%
  group_by(last_update) %>% 
  mutate(cases_rank=dense_rank(desc(confirmed)),
         deaths_rank=dense_rank(desc(deaths)),
         recovered_rank=dense_rank(desc(recovered)),
         tested_rank=dense_rank(desc(people_tested)),
         hospitalized_rank=dense_rank(desc(people_hospitalized)),
         incident_rank=dense_rank(desc(incident_rate)),
         testrate_rank=dense_rank(desc(testing_rate)),
         mortality_rank=dense_rank(desc(mortality_rate)),
         hosprate_rank=dense_rank(desc(hospitalization_rate))) %>% 
  ungroup()

jhu_cases_state <- jhu_cases_state_us %>% 
  filter(province_state=="Texas") %>%
  mutate(fips=as.character(fips))

jhu_cases_county <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases.csv") %>% 
  janitor::clean_names() %>% 
  rename(county=admin2,
         state=province_state,
         cases=confirmed) %>% 
  separate(last_update, into=c("date","time"), sep=" ") %>% 
  mutate(fips=as.character(fips),
         mort_rate=round(deaths/cases))
  
jhu_tx_cases_today <- jhu_cases_county %>% 
  mutate(date = ymd(date)) %>%
  group_by(state) %>% 
  filter(date == max(date)) %>% 
  ungroup()

tx_county_cases <- jhu_cases_county %>%
  filter(state=="Texas")

tx_today <- tx_county_cases %>% 
  mutate(date = ymd(date)) %>%
  group_by(state) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-county)

# ~~NYT Data --------------------------------------------------------------

nyt_county_cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(state=="Texas") %>% 
  mutate(min = min(cases),
         max = max(cases)) %>% 
  group_by(county) %>% 
  mutate(prev_day_cases = lag(cases,1),
         prev_week_cases = lag(cases,7),
         prev_day_deaths = lag(deaths,1),
         prev_week_deaths = lag(deaths,7),
         new_cases_1day = cases-prev_day_cases,
         new_cases_7day= cases-prev_week_cases,
         new_deaths_1day = deaths-prev_day_deaths,
         new_deaths_7day= deaths-prev_week_deaths) %>% 
  ungroup()
         
nyt_state_cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  group_by(state) %>% 
  mutate(date = ymd(date),
         min = min(cases),
         max = max(cases),
         prev_day_deaths = lag(deaths,1),
         new_deaths_1day = deaths-prev_day_deaths,
         prev_day_cases = lag(cases,1),
         prev_week_cases = lag(cases,7),
         new_cases_1day = cases-prev_day_cases,
         new_cases_7day= cases-prev_week_cases) %>% 
  ungroup()

nyt_state_cases_tx <- nyt_state_cases %>% 
  filter(state=="Texas")

tigris_cntys <- tigris::counties(state="48", cb = TRUE) %>% 
  as_tibble() %>% 
  select(GEOID,county=NAME)

tx_counties <- tidycensus::county_laea %>% 
  filter(str_detect(GEOID, "^48")) %>% 
  left_join(tigris_cntys, by="GEOID") %>% 
  st_as_sf() %>% 
  st_transform(crs="+init=epsg:4326")

county_list <- nyt_county_cases %>% 
  as_tibble() %>% 
  select(`County Names`=county) %>%
  distinct() %>% 
  arrange(`County Names`) %>% 
  as.list()

tx_county_sf <- tx_counties %>%
  left_join(tx_today, by=c("GEOID"="fips")) %>% 
  st_as_sf() %>% 
  st_transform(crs="+init=epsg:4326") %>% 
  fill(c("date"),.direction="downup") %>% 
  select(-state, -country_region, -time, -combined_key, -iso3) %>% 
  mutate(cases_label=case_when(
    is.na(cases) ~ "No Reported",
    is.numeric(cases) ~ scales::comma(cases)
  ),
  active_label=case_when(
    is.na(active) ~ "No Reported",
    is.numeric(active) ~ scales::comma(active)
  ))

# ~~COVID Tracking Data --------------------------------------------------------------

test_daily <- read_csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv") %>% 
  filter(state=="TX") %>%
  mutate(date=as.character(date)) %>%
  mutate(date=str_replace(date,"(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3")) %>% 
  mutate(date = ymd(date))

test_positive <- test_daily %>% 
  arrange(date) %>% 
  select(date, state, positive, totalTestResults, positiveIncrease, negativeIncrease,totalTestResultsIncrease) %>% 
  mutate(daily_test_pos_rate = round(positive/totalTestResultsIncrease, digits=4),
         daily_test_pos_rate_7day_avg = rollmean(daily_test_pos_rate, 5, 
                                                 fill=0, align = "right"),
         test_pos_label = daily_test_pos_rate,
         test_pos_7day_label = daily_test_pos_rate_7day_avg) %>% 
  mutate_at(vars(test_pos_label,test_pos_7day_label),scales::percent_format(accuracy = .11, scale=100)) %>% 
  gather(increase_type,increase,5:6)

tex_today_tests <- test_daily %>% 
  arrange(date) %>% 
  select(date, state, positive, totalTestResults, positiveIncrease, negativeIncrease,totalTestResultsIncrease) %>% 
  mutate(per_capita = round((totalTestResults/27885195)*100000, digits=2),
         per_capita_per_day = round((totalTestResultsIncrease/27885195)*100000, digits=2),
         daily_test_pos_rate = round(positiveIncrease/totalTestResultsIncrease, digits=4),
         daily_test_pos_rate_7day_avg = rollmean(daily_test_pos_rate, 5, 
                                                 fill=0, align = "right"),
         test_pos_label = daily_test_pos_rate,
         test_pos_7day_label = daily_test_pos_rate_7day_avg) %>% 
  mutate_at(vars(totalTestResults,per_capita), scales::comma) %>% 
  mutate_at(vars(test_pos_label,test_pos_7day_label),scales::percent_format(accuracy = .11, scale=100)) %>% 
  # gather(increase_type,increase,5:6) %>% 
  filter(date==max(date))

# twc_claims_cnty <- read_excel("data/twc/weekly-claims-by-county-twc.xlsx", skip=2) %>%
#   slice(1:254) %>% 
#   clean_names() %>% 
#   filter(!is.na(county)) %>% 
#   remove_empty("cols") %>% 
#   # select_at(vars(county, ends_with("2020"))) %>% 
#   pivot_longer(-county, names_to = "date", values_to = "value") %>% 
#   mutate(date=gsub("^x","0",x=date),
#          date=gsub("_","-",x=date)) %>% 
#   mutate(date=lubridate::mdy(date))


# ~~DSHS Data ----

    
dshs_state_case_and_fatalities <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/cases/state_cases_and_fatalities.csv?token=AB6K4YWRCYOHXCJPGKVP7JC6VTSUI") %>% 
  mutate(state="Texas",
         fips="48")

dshs_state_hospitalizations <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/hospitals/state_hospitalizations.csv?token=AB6K4YVDEHZTYS2MTMKM6RS6VTTDG") %>% 
  mutate(state="Texas",
         fips="48")

dshs_state_tests <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/testing/state_tests.csv?token=AB6K4YUH4D2UGQDBONHNB226VTTFC") %>% 
  mutate(fips=as.character(fips))

dshs_state_demographics <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/cases/state_case_demographics.csv?token=AB6K4YSQ2E653FBQVK4UFHS6VTSSS") %>% 
  mutate(fips=as.character(fips))

dshs_county_data <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/cases/county_cases.csv?token=AB6K4YQQIQZPRY7XZZCOJCS6VTSRM")

dshs_county_test_data <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/testing/county_tests.csv?token=AB6K4YRPQNYC2TAXYYWBF226VTTE4")

dshs_tsa_hosp_data <- read_rds("clean_data/dshs/hospitals/tsa_bed_data_with_counties_sf.rds") %>% 
  st_transform(crs="+init=epsg:4326")

dshs_tsa_vent_data <- read_rds("clean_data/dshs/hospitals/tsa_vent_data_with_counties_sf.rds") %>% 
  st_transform(crs="+init=epsg:4326")

dshs_syndromic_tx <- read_csv("https://raw.githubusercontent.com/mrworthington/covid_tracker/master/clean_data/dshs/syndromic_tx.csv?token=AB6K4YRYQFKSNX2HEV2H2D26VTTWA")

## TODO - We need to automate this scraping to build a timeseries, and make sure that
##        We have the correct data associated with each spreadsheet. 

# dshs_tsa_hospital_data = read_excel("data/dshs/Copy of TSA COVID Bed Reports_04.19.20.xlsx") %>%
#   clean_names() %>% 
#   mutate(tsa_clean = substring(tsa, 5))

# ** Population Data ---------------------------------------------------------

state_pop <- read_rds("data/population/state_pop.rds")
county_pop <- read_rds("data/population/county_pop.rds")

tsa_shps <- dshs_tsa_hosp_data %>%
  filter(tsa!="Total") %>% 
  select(tsa,tsa_counties,geometry) %>% 
  group_by(tsa) %>% 
  summarise(tsa_counties = toString(tsa_counties)) %>% 
  ungroup()

## ** Crosswalk Data ----

crosswalk_data = read_excel("data/dshs/Crosswalk TX Counties RACs PHRs.xlsx", skip=2) %>%
  rename(
    county_name=1,
    public_health_region=2,
    tsa=3
  ) %>%
  mutate(
    tsa_clean = substring(tsa, 1, 1)
  )

# ** Economic Data -----------------------------------------------------------


# ^^^Homebase Data -----------------------------------------------------


hb_hours_worked_all <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1930671010&single=true&output=csv", skip=2) %>% 
  fill(X2) %>%
  slice(-1,-3,-4) %>%
  select(-1) %>%
  mutate(
    geo_type = case_when(
      str_detect(X2, "MSA") ~ "MSA",
      str_detect(X2, "industry") ~ "US Industry",
      str_detect(X2, "state") ~ "State",
      TRUE ~ "Nationwide"
    )
  ) %>%
  select(geo_type, area = X3, everything(),-X2) %>%
  filter(!is.na(area)) %>% 
  filter(area == "Texas") %>%
  select(-geo_type) %>% 
  pivot_longer(-area,
               names_to="date",
               values_to = "pct") %>% 
  mutate(date=gsub(pattern="/",replacement="-",x=date),
         pct=gsub(pattern="%",replacement="",x=pct),
         pct=as.numeric(pct),
         date=paste0("2020-",date),
         date=ymd(date)) %>% 
  arrange(date) %>% 
  mutate(prev_date=lag(pct,1),
         prev_day=lag(date,1),
         change=pct-prev_date,
         change_lbl=case_when(
           change > 0 ~ "+",
           change < 0 ~ "",
           TRUE ~ ""
         ),
         color=case_when(
           change > 0 ~ "#66E8C6",
           change < 0 ~ "#F26852",
           TRUE ~ "#FFD100"
         )) 

hb_hours_worked <- hb_hours_worked_all %>%
  filter(date == max(date))

hb_businesses_open_all <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1102464531&single=true&output=csv", skip=2) %>% 
  fill(X2) %>% 
  slice(-1,-3,-4) %>% 
  select(-1) %>% 
  mutate(geo_type=case_when(
    str_detect(X2, "MSA") ~ "MSA",
    str_detect(X2, "industry") ~ "US Industry",
    str_detect(X2, "state") ~ "State",
    TRUE ~ "Nationwide"
  )) %>% 
  select(geo_type, area=X3,everything(),-X2) %>% 
  filter(!is.na(area)) %>% 
  filter(area == "Texas") %>%
  select(-geo_type) %>% 
  pivot_longer(-area,
               names_to="date",
               values_to = "pct") %>% 
  mutate(date=gsub(pattern="/",replacement="-",x=date),
                                        pct=gsub(pattern="%",replacement="",x=pct),
                                        pct=as.numeric(pct),
                                        date=paste0("2020-",date),
                                        date=ymd(date)) %>% 
  arrange(date) %>% 
  mutate(prev_date=lag(pct,1),
         prev_day=lag(date,1),
         change=pct-prev_date,
         change_lbl=case_when(
           change > 0 ~ "+",
           change < 0 ~ "",
           TRUE ~ ""
         ),
         color=case_when(
           change > 0 ~ "#66E8C6",
           change < 0 ~ "#F26852",
           TRUE ~ "#FFD100"
         )) 

hb_businesses_open <- hb_businesses_open_all %>%
  filter(date == max(date))

hb_employees_working_all <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1658358543&single=true&output=csv", skip=2) %>% 
  fill(X2) %>% 
  slice(-1,-3,-4) %>% 
  select(-1) %>% 
  mutate(geo_type=case_when(
    str_detect(X2, "MSA") ~ "MSA",
    str_detect(X2, "industry") ~ "US Industry",
    str_detect(X2, "state") ~ "State",
    TRUE ~ "Nationwide"
  )) %>% 
  select(geo_type, area=X3,everything(),-X2) %>% 
  filter(!is.na(area)) %>% 
  filter(area == "Texas") %>%
  select(-geo_type) %>% 
  pivot_longer(-area,
               names_to="date",
               values_to = "pct") %>% 
  mutate(date=gsub(pattern="/",replacement="-",x=date),
         pct=gsub(pattern="%",replacement="",x=pct),
         pct=as.numeric(pct),
         date=paste0("2020-",date),
         date=ymd(date)) %>% 
  arrange(date) %>% 
  mutate(prev_date=lag(pct,1),
         prev_day=lag(date,1),
         change=pct-prev_date,
         change_lbl=case_when(
           change > 0 ~ "+",
           change < 0 ~ "",
           TRUE ~ ""
         ),
         color=case_when(
           change > 0 ~ "#52F2A9",
           change < 0 ~ "#F26852",
           TRUE ~ "#FFD100"
         )) 

hb_employees_working <- hb_employees_working_all %>%
  filter(date == max(date))

# ^^^FREDR Data -----------------------------------------------------

 tx_series_all <-fredr(
  series_id = "TXICLAIMS",
  observation_start = as.Date("2020-01-01")
) %>% 
  mutate(date=as.character(date)) %>%
  # add_row(date="2020-04-08", series_id="TXICLAIMS", value=313832) %>%
  add_row(date="2020-04-16", series_id="TXICLAIMS", value=273567) %>%
  mutate(date=as.Date(date)) %>%
  arrange(date)

tx_series <- tx_series_all %>%
  filter(date >= as.Date("2020-03-21")) %>% 
  group_by(series_id) %>% 
  summarise(running_claims=sum(value)) %>% 
  rename(value=running_claims) %>% 
  mutate_at(vars(value),scales::comma)

tx_urn <-fredr(
  series_id = "TXURN",
  observation_start = as.Date("2020-01-01")
) %>% 
  mutate(date=as.Date(date)) %>%
  filter(date == max(date))



# **DERIVED METRICS -----------------------------------

# 
# ** State -----------------------------------

# Tests Per 100,000

total_population <- state_pop %>%
  clean_names() %>% 
  rename(state=name, total_population=estimate, fips=geoid) %>% 
  filter(state=="Texas")

total_tests <- dshs_state_tests %>%
  group_by(state,fips) %>%
  summarise(total_tests = sum(tests)) %>%
  ungroup() %>%
  left_join(total_population, by=c("state","fips")) %>%
  left_join(dshs_state_hospitalizations, by=c("state","fips")) %>% 
  left_join(jhu_cases_state, by=c("state"="province_state","fips")) %>% 
  mutate(tests_per_100k = round((total_tests/total_population)*100000,digits=4))

  
hosp_capacity <- total_tests %>% 
  mutate(beds_to_active = avail_hospital_beds/active,
         icu_beds_to_hosp = avail_icu_beds/people_hospitalized,
         vents_to_hosp = avail_ventilators/people_hospitalized)

# ^^^Case Growth Rate ----------------------------------------

daily_growth_rates <- nyt_state_cases %>%
  arrange(date) %>%
  filter(state=="Texas") %>% 
  mutate(daily_growth_rate = (cases/lag(cases))) %>%
  mutate(daily_growth_rate_7day_avg = rollmean(daily_growth_rate, 7, 
                                               fill=0, align = "right"))

# Doubling Every X days

## Get total cases today, find the date that was half of that

today <- ((nyt_state_cases %>% 
            filter(state=="Texas") %>% 
            arrange(desc(date)))[1, 1])$date

cases_today <- ((nyt_state_cases %>% 
                  filter(state=="Texas") %>% 
                  arrange(desc(date)))[1, 4])$cases

last_half_day <- ((nyt_state_cases %>% 
                    filter(state=="Texas") %>% 
                    arrange(desc(date)) %>% 
                    filter(cases < (cases_today / 2)))[1, 1])$date



# 1 and 7-day rates of change for cumulative cases, daily new cases, daily new deaths, and daily new hospitalized


rates_of_change <- nyt_state_cases %>%
  arrange(date) %>%
  mutate(
    new_cases = case_when(is.na(cases - lag(cases)) ~ 0,
                          TRUE ~ cases - lag(cases)),
    new_deaths = case_when(is.na(deaths - lag(deaths)) ~ 0,
                           TRUE ~ deaths - lag(deaths))
  ) %>%
  mutate(
    new_cases_7day_avg = rollmean(new_cases, 7, fill = 0, align = "right"),
    new_deaths_7day_avg = rollmean(new_deaths, 7, fill = 0, align = "right")
  )

# print("Rates of Change.")
# print(rates_of_change %>% arrange(desc(date)))

# Positive/Negative Testing. Current and TS.

# 
#  ** County ----------
# 

# Crosswalk the NCHS and Trauma Service Area Data.

# county_tsa_data  = merge(crosswalk_data, dshs_tsa_hospital_data, by="tsa_clean")

# print("Crosswalk county hospital data.")
# print(county_tsa_data)

# Integrate DSHS Tests Per County Data

total_cnty_population <- county_pop %>%
  clean_names() %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(county_name=name, total_population=estimate, fips=geoid) %>% 
  mutate(county_name=gsub(pattern=" County, Texas", replacement="",x=county_name))

total_cnty_tests <- dshs_county_test_data %>%
  left_join(total_cnty_population, by="county_name") %>%
  mutate(tests_per_100k = round((total_tests/total_population)*100000,digits=1)) %>% 
  group_by(state) %>% 
  mutate(state_ranking = dense_rank(desc(tests_per_100k))) %>% 
  ungroup() %>% 
  filter(county_name!="TOTAL")

# Available Ventilators Per COVID-19 Case

# dshs_county_data = dshs_county_data %>%
#   mutate(
#     ventilators_per_case = adult_icu / confirmed_cases
#   )
# 
# print("Available Ventilators Per COVID-19 Case (Calculated at TSA level)")
# print(dshs_county_data %>% select("county_name", "ventilators_per_case"))
# 
# 
# # Available ICU Beds Per COVID-19 Case
# 
# dshs_county_data = dshs_county_data %>%
#   mutate(
#     icu_beds_per_case = adult_icu / confirmed_cases
#   )
# 
# print("Available ICU Beds COVID-19 Case (Calculated at TSA level)")
# print(dshs_county_data %>% select("county_name", "icu_beds_per_case"))
# 
# # Case Growth Rate
# 
# daily_county_growth_rates = nyt_county_cases %>%
#   group_by(county) %>%
#   arrange(date) %>%
#   mutate(
#     daily_growth_rate = case_when(
#       is.na(cases / lag(cases)) ~ 0,
#       TRUE ~ cases / lag(cases)
#     )
#   ) %>%
#   mutate(
#     daily_growth_rate_7day_avg = rollmean(daily_growth_rate, 7, fill=0, align="right")
#   )
# 
# print("County growth rate, e.g. Austin County:")
# print(daily_county_growth_rates %>% filter(county=="Austin") %>% arrange(desc(date)))
# 
# # Doubling Every X days
# 
# # 1 and 7-day rates of change for cumulative cases, daily new cases, daily new deaths, and daily new hospitalized
# 
# county_rates_of_change = nyt_county_cases %>%
#   group_by(county) %>%
#   arrange(date) %>%
#   mutate(
#     new_cases = case_when(
#       is.na(cases - lag(cases)) ~ 0,
#       TRUE ~ cases - lag(cases)
#     ),
#     new_deaths = case_when(
#       is.na(deaths - lag(deaths)) ~ 0,
#       TRUE ~ deaths - lag(deaths)
#     )
#   ) %>% 
#   mutate(
#     new_cases_7day_avg = rollmean(new_cases, 7, fill=0, align="right"),
#     new_deaths_7day_avg = rollmean(new_deaths, 7, fill=0, align="right")
#   )
# 
# print("County rates of Change.")
# print(county_rates_of_change %>% arrange(desc(date)))


# Positive/Negative Testing. Current and TS. (If we can get comprehensive data on testing at the county level. To my knowledge, COVID-tracking only produces this at a statewide-level).
# 


# HEADER CODE-----------------------------------------------------------

header <- dashboardHeader(disable = FALSE,
  title = tags$a(href='http://www.texas2036.org',
                 HTML('<svg viewBox="0 0 227.4 83.5" style="height:4.5vh;padding-bottom:1.1vh;margin-top:7px"><path fill="#3a4a9f" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
                 tags$title('Texas COVID-19 Resource Kit'))
  # dropdownMenu(type = "notifications",
  #              headerText=NULL,
  #              badgeStatus = "info",
  #              notificationItem(
  #                text = "New County Data Has Been Added",
  #                icon("users")
  #              ))
  )

# SIDEBAR CODE-----------------------------------------------------------

sidebar <- dashboardSidebar(disable = FALSE,
                            collapsed = FALSE,
                            sidebarMenu(
                              id = "tabs",
                              menuItem("Introduction",
                                       tabName = "intro", 
                                       icon = icon("square")),
                              menuItem("Reopening",
                                       tabName = "reopening", 
                                       icon = icon("door-open")),
                              menuItem("State Explorer",
                                       tabName = "state_profiles", 
                                       icon = icon("landmark")),
                              menuItem("County Explorer",
                                       tabName = "county_profiles", 
                                       icon = icon("city")),
                              # menuItem("Credits",
                              #          tabName = "credits", 
                              #          icon = icon("circle")),
                              # menuItem("Data",
                              #          tabName = "data", 
                              #          icon = icon("circle")),
                              actionButton("about", "About"),
                              actionButton("learn", "Learn More")
                              # actionButton("show", "Learn More", icon = icon("info-circle", class = "fa-pull-left"), style="color: #152934"),
                              
                            )
)



# BODY CODE -----------------------------------------------------

body <- dashboardBody(
  tags$head(
    tags$script(src="https://kit.fontawesome.com/5272d94c6c.js", crossorigin="anonymous"),
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom2.css"),
    includeHTML(("google_analytics.html")),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")  ),

# **Landing Page ----------------------------------------------------------

  
  tabItems(
    tabItem(tabName = "intro",
      jumbotron("Texas COVID-19 Data Resource", 
                "A Comprehensive Look At Our Current Moment",
                button = FALSE),
      hr(style="border-top: 48px solid #fff;"),
      HTML("<i style='color:#F26852;display: block;text-align: center;margin-top:-82px;margin-bottom: 20px;font-size: 112px;}' class='fas fa-2x fa-hands-helping'></i>"),
      br(),
      fluidRow(
        column(4, thumbnail_label(title="<i class='fas fa-door-open'></i>",
                                  label = 'Reopening Analysis',
                                  content = includeMarkdown("markdown/intro/reopening.md"),
                                  button_link = '#shiny-tab-reopening', button_label = 'Explore')
        ),
        column(4, thumbnail_label(title="<i class='fas fa-landmark'></i>", 
                                  label = 'State Explorer',
                                  content = includeMarkdown("markdown/intro/state.md"),
                                  button_link = '#shiny-tab-state_profiles', button_label = 'Explore')),
        column(4, thumbnail_label(title="<i class='fas fa-city'></i>",
                                  label = 'County Explorer',
                                  content = includeMarkdown("markdown/intro/county.md"),
                                  button_link = '#shiny-tab-county_profiles', button_label = 'Explore'))
    )),
    tabItem(tabName = "reopening",
            fluidRow(
              HTML("<iframe width='100%' height=1200vh' style='border-top-width: 0px;border-right-width: 0px;border-bottom-width: 0px;border-left-width: 0px;' src='https://staging.convex.design/texas-2036/texas-covid-live-report/?currentCounty=Anderson'></iframe>"))),
    tabItem(tabName = "state_profiles",
# **STATEWIDE PROFILE UI ----------------------------------------------------
            h1(style="font-weight:800;", "STATEWIDE COVID-19 PROFILE", span="id='statewide-profile'"),      

# ~~Public Health ---------------------------------------------------------

            
              h3(class="covid-topic", "Current Case Data"),
            fluidRow(
              infoBoxOutput("tx_cases", width=4),
              infoBoxOutput("tx_mort", width=4),
              infoBoxOutput("tx_recover", width=4)
              ),
            fluidRow(
              highchartOutput("state_growth_rate_hchart", height = 350)
              ),

# ~~Population Health ---------------------------------------------------------


# ~~Testing ---------------------------------------------------------

            h3(class="covid-topic", "Current Testing Data"),
            fluidRow(
              column(width = 3, class="economic-grid",
                     h2(class="economic-tile",
                        p(class="tile-header", "Total Tests"),
                        hr(),
                        p(style = "text-align:center;font-size:1em;font-weight:800", paste0(tex_today_tests$totalTestResults)),
                        p(style = "text-align:center;font-size:.6em;font-weight:600;color:#DBDCDD;", 
                          paste0("Since: Mar 4, 2020")),
                        p(style = "text-align:center;font-size:.4em;font-weight:400", 
                          tags$a(href="https://github.com/COVID19Tracking/covid-tracking-data","Source: COVID-Tracking Project")))),
              column(width = 3, class="economic-grid",
                     h2(class="economic-tile",
                        p(class="tile-header", "Total Tests Per Capita"),
                        hr(),
                        p(style = "text-align:center;font-size:1em;font-weight:800", paste0(tex_today_tests$per_capita)),
                        p(style = "text-align:center;font-size:.6em;font-weight:600;color:#DBDCDD;", 
                          paste0("Per 100,000 People")),
                        p(style = "text-align:center;font-size:.4em;font-weight:400", 
                          tags$a(href="https://github.com/COVID19Tracking/covid-tracking-data","Source: COVID-Tracking Project")))),
              column(width = 3, class="economic-grid",
                     h2(class="economic-tile",
                        p(class="tile-header", "Daily Tests Per Capita"),
                        hr(),
                        p(style = "text-align:center;font-size:1em;font-weight:800", paste0(tex_today_tests$per_capita_per_day)),
                        p(style = "text-align:center;font-size:.6em;font-weight:600;color:#DBDCDD;", 
                          paste0("Per 100,000 People")),
                        p(style = "text-align:center;font-size:.4em;font-weight:400", 
                          tags$a(href="https://github.com/COVID19Tracking/covid-tracking-data","Source: COVID-Tracking Project")))),
              
              column(width = 3, class="economic-grid",
                     h2(class="economic-tile",
                        p(class="tile-header", "Test Positive Rate"),
                        hr(),
                        p(style = "text-align:center;font-size:1em;font-weight:800", paste0(tex_today_tests$test_pos_label)),
                        p(style = "text-align:center;font-size:.6em;font-weight:600;color:#DBDCDD;", 
                          paste0("Tests Were Positive (", format(tex_today_tests$date, format="%b %d"),")")),
                        p(style = "text-align:center;font-size:.4em;font-weight:400", 
                          tags$a(href="https://github.com/COVID19Tracking/covid-tracking-data","Source: COVID-Tracking Project"))))),

# ~~Syndromic Data ---------------------------------------------------------

h3(class="covid-topic", "Current Syndromic Data"),
fluidRow(
  fluidRow(
    column(width = 6,
           highchartOutput("cli_hchart", height = 350)),
    column(width = 6,
           highchartOutput("ili_hchart", height = 350))
  ),
),

# ~~Serology ---------------------------------------------------------



# ~~Hospital Data -----------------------------------------------------------


h3(class="covid-topic", "Current Hospital Data"),
            fluidRow(
              infoBoxOutput("hospitalized_rate", width=3),
              infoBoxOutput("tx_beds", width=3),
              infoBoxOutput("tx_icu_beds", width=3),
              infoBoxOutput("tx_vents", width=3)
              ),


# ~~Trends Over Time --------------------------------------------------------

            h3(class="covid-topic", "Trends Over Time", span="id='statewide_tot"),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_curves_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_new_cases_hchart", height = 350))
              ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_new_deaths_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_new_tests_hchart", height = 350))
            ),


# ~~Economic Data -----------------------------------------------------------

            h3(class="covid-topic", "Economy + Society"),
            fluidRow(
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",
                          paste0(tx_series$value)),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;",
                          paste0("Since: Mar 21, 2020")),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Jobless Claims"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXICLAIMS","Source: BLS via FREDr")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(tx_urn$value,"%")),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;",
                          paste0("As of: ", format(tx_urn$date, format="%b %d"))),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Unemployment Rate"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXURN","Source: BLS via FREDr")))),
                     column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_businesses_open$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_businesses_open$color,";"),
                          paste0(hb_businesses_open$change_lbl, hb_businesses_open$change, "% From ", format(hb_businesses_open$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Local Businesses Open"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/covid-19/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_hours_worked$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_hours_worked$color,";"),
                          paste0(hb_hours_worked$change_lbl, hb_hours_worked$change, "% From ", format(hb_hours_worked$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Reduction In Hours Worked"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/covid-19/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_employees_working$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_employees_working$color,";"),
                          paste0(hb_employees_working$change_lbl, hb_employees_working$change, "% From ", format(hb_employees_working$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Hourly Employees Working"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/covid-19/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800","TBD"),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;","TBD"),
                        p(style="text-align:center;font-size:.5em;font-weight:300","Monthly $ Loss Per Employee"),
                        p(style="text-align:center;font-size:.4em;font-weight:400","Source: Homebase")))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_claims_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_businesses_hchart", height = 350))
              ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_hours_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_employees_hchart", height = 350))
              )
),

# **COUNTY PROFILE UI  ------------------------------------------------------
tabItem(tabName = "county_profiles",
            fluidRow(
              column(width = 6,
                     h2(style="font-weight:800;","COUNTY COVID-19 PROFILE", span="id='county-profile'")),
              column(width = 6,
                     selectizeInput(inputId = "countyname", label =NULL, choices = county_list,
                                    selected="Harris", multiple = FALSE, width="100%",
                                    options = list(maxItems=1,placeholder = 'Select Your County...')))
              ),
            fluidRow(
              column(width = 6, 
                     # div(style = 'overflow-y: scroll; position: fixed; width: 700px',
                     h4("Data As of:",textOutput("currentTime", inline=TRUE)),
                     leafletOutput("map", width = "100%", height = 600)
                    ),
              column(width = 6,
              # h3(style="font-weight:700;",textOutput("countyname", inline = TRUE)),
              h3(class="covid-topic", "Public Health"),
              fluidRow( 
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("county_active_text")),
                       p(style="text-align:center","Active Cases")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("county_mort_rate")),
                       p(style="text-align:center","Case Fatality Rate")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("county_test_text")),
                       p(style="text-align:center","Tests Per 100,000")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("county_incident_text")),
                       p(style="text-align:center","Cases Per 100,000"))
                ),
              h3(class="covid-topic", "Hospital Capacity"),
              fluidRow( 
                column(width = 12,
                       tags$div( class="tsa-paragraph",
                       br(),
                       h4(style="font-weight:800;color:#FFD100;display:inline;", textOutput("county_name",inline=TRUE)),
                       h4(style="font-weight:400;display:inline;", "is a part of "),
                       h4(style="font-weight:800;text-align:left;display:inline;color:#FFD100;", textOutput("tsa_name",inline=TRUE)),
                       h4(style="font-weight:400;display:inline;"," Trauma Service Areas are geographical clusters defined by where people are most likely to receive trauma care when they need it. For COVID-19 patients, it's the area where they are most likely to receive care. "),
                       h4(style="font-weight:800;color:#FFD100;display:inline;", "On April 24th "),
                       h4(style="font-weight:400;display:inline;", ", the latest date for which we have data, hospitals in this Trauma Service Area reported the following:"),
                       ))),
              fluidRow( 
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("hosp_beds_tsa")),
                       p(style="text-align:center","Beds Occupied (%)")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("hosp_covid_er_visits")),
                       p(style="text-align:center","COVID-19 Related ER Visits")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("hosp_susp_covid")),
                       p(style="text-align:center","Suspected COVID-19 Patients")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", 
                          textOutput("hosp_lab_covid")),
                       p(style="text-align:center","Lab Confirmed COVID-19 Patients"))
              ),
              h3(class="covid-topic", "Trends Over Time"),
              # highchartOutput("cnty_compare_hchart", height = 100),
              highchartOutput("cnty_curves_hchart", height = 350),
              highchartOutput("cnty_new_cases_hchart", height = 350),
              highchartOutput("cnty_new_deaths_hchart", height = 350),
              box(solidHeader = TRUE, 
                  width=12, 
                  height = 600,
                  background = "navy",
                  collapsible = FALSE))
              )
            ),
    tabItem(tabName = "credits",
            fluidRow(class="collateral",
              column(width=3),
              column(width=6,
                     withMathJax(includeMarkdown("markdown/sidebar/credits.md"))
                     ),
              column(width=3)
              
              )
            ),
    tabItem(tabName = "data",
        fluidRow(class="collateral",
          column(width=2),
          column(width=8,
                 withMathJax(includeMarkdown("markdown/sidebar/data.md"))
                 ),
          column(width=2)
          )
        )
),
hr(),
tags$footer(includeMarkdown("footer.md"), align = "center")
)


# DASHBOARD PAGE CODE -----------------------------------------------------

ui <- dashboardPage(title="Texas 2036 | COVID-19 Resource Kit",
                    header = header,
                    sidebar = sidebar,
                    body = body

)


# SERVER FUNCTIONS ------------------------------------------------

server <- function (input, output, session) {
  
  options(digits.secs = 0) # Include milliseconds in time display
  
  
  

# {Latest NYT Update Date} ----------------------------------------------------
    
  output$currentTime <- renderText({

    latest_update <- tx_today %>% 
      distinct(date) %>% 
      mutate(date = as.character(date)) %>%
      parse_date_time("Ymd")
    
    format(latest_update, format="%B %d, %Y")
      
    })

# MODAL - Sign-Up -----------------------------------------------------------

  observeEvent(input$learn, {
    showModal(
      modalDialog(
        title = "Connect with Texas 2036",
        size = "l",
        footer = HTML("<a href='http://www.unitedwayaustin.org/our-work/2gen/'>© United Way of Greater Austin</a>"),
        includeHTML("markdown/sidebar/signup.html"),
        easyClose = TRUE
      ))
  })

# MODAL - About -----------------------------------------------------------

  observeEvent(input$about, {
    showModal(
      modalDialog(
      title = "About This Dashboard",
      size = "l",
      footer = HTML("<a href='http://www.unitedwayaustin.org/our-work/2gen/'>© United Way of Greater Austin</a>"),
      includeMarkdown("markdown/sidebar/learnmore.md"),
      easyClose = TRUE
    ))
  })
  
# INFO BOXES - STATEWIDE--------------------------------------------------------------

# --{InfoBox - PH - Total Cases} -----------------------------------------------
    
  output$tx_cases <- renderInfoBox({
    
    tot_pos <- jhu_cases_state %>% 
      select(confirmed, cases_rank) %>% 
      mutate_at(vars(confirmed), scales::comma) %>% 
      mutate_at(vars(cases_rank), scales::label_ordinal())
    
    infoBox(
       value=paste0(tot_pos$confirmed), title="Total Cases",
       subtitle=paste0(tot_pos$cases_rank, " Most in US"),
      icon = icon("lungs-virus"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  # {InfoBox  - PH - Mortality} -----------------------------------------------
  
  output$tx_mort <- renderInfoBox({
    
    tot_pos <- jhu_cases_state %>% 
      select(mortality_rate,  mortality_rank) %>% 
      mutate_at(vars(mortality_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
      mutate_at(vars(mortality_rank), scales::label_ordinal())
    
    infoBox(
      title="Deaths (% of All Cases)", value=paste0(tot_pos$mortality_rate, "%"),
      subtitle=paste0(tot_pos$mortality_rank, " Most in US"),
      icon = icon("biohazard"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  
  # {InfoBox - PH - Recovered} -----------------------------------------------
  
  output$tx_recover <- renderInfoBox({
    
    tot_pos <- jhu_cases_state %>% 
      select(recovered,  recovered_rank) %>% 
      mutate_at(vars(recovered), scales::number_format(accuracy=1, scale=1, big.mark = ",")) %>% 
      mutate_at(vars(recovered_rank), scales::label_ordinal())
    
    infoBox(
      title="Recovered", value=paste0(tot_pos$recovered),
      subtitle=paste0(tot_pos$recovered_rank, " Most in US"),
      icon = icon("hand-holding-medical"), color = "navy", href=NULL
    )
  })
  
  # {InfoBox - TTD - Total Tests} -----------------------------------------------
  
  output$test_rate <- renderText({
    
    tot_pos <- total_tests %>% 
      select(tests_per_100k) %>% 
      mutate_at(vars(tests_per_100k), scales::number_format(accuracy=.01, scale=1)) 
    
    test_rank <- jhu_cases_state %>% 
      select(testrate_rank) %>% 
      mutate_at(vars(testrate_rank), scales::label_ordinal())
    
    infoBox(
      title=paste0("Tests Per 100,000"), value=paste0(tot_pos$tests_per_100k),
      subtitle=paste0(test_rank$testrate_rank, " Most in US"),
      icon = icon("vials"), color = "navy", href="https://www.dshs.state.tx.us/coronavirus/additionaldata/"
    )
  })
  
  
# {Total COVID-19 Tests} -----------------------------------------------
  
  output$tx_tests <- renderInfoBox({
    
    tot_pos <- hosp_capacity %>% 
      select(total_tests, tested_rank) %>% 
      mutate_at(vars(total_tests), scales::comma) %>% 
      mutate_at(vars(tested_rank), scales::label_ordinal())
    
    infoBox(
      title="Total People Tested", 
      value=paste0(tot_pos$total_tests), 
      icon = icon("microscope"),
      subtitle=paste0(tot_pos$tested_rank, " in US"),
      color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
# {Total Deaths} -----------------------------------------------
  
  output$tx_deaths <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(deaths, deaths_rank) %>% 
      mutate_at(vars(deaths), scales::comma) %>% 
      mutate_at(vars(deaths_rank), scales::label_ordinal())
    
    
    infoBox(
      title="Total Confirmed Deaths", value=paste0(tot_pos$deaths), icon=icon("heartbeat"),
      subtitle=paste0(tot_pos$deaths_rank, " in US"),
      color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })

# {Hosp Beds Per Confirmed Case - State}-------------------------------------------------

  output$tx_beds <- renderInfoBox({
    
    tot_pos <- dshs_tsa_hosp_data %>%
      as_tibble() %>% 
      select(tsa,tsa_counties,available_beds,bed_capacity) %>% 
      # filter(tsa_counties==input$countyname) %>%
      filter(tsa=="Total") %>%
      mutate(bed_utilization=round(available_beds/bed_capacity,digits=3)) %>%
      mutate_at(vars(bed_utilization),scales::percent)
    
    
    infoBox(
      title="All Beds Availability", value=paste0(tot_pos$bed_utilization), icon=icon("bed"),
      subtitle="of All Beds Available",
      color = "navy", href=NULL
    )
  })
  
# {ICU Beds Availability - State}-------------------------------------------------
  
  output$tx_icu_beds <- renderInfoBox({
    
    tot_pos <- dshs_tsa_hosp_data %>%
      as_tibble() %>% 
      select(tsa,tsa_counties,adult_icu,total_icu_beds) %>% 
      # filter(tsa_counties==input$countyname) %>%
      filter(tsa=="Total") %>%
      mutate(bed_utilization=round((total_icu_beds-adult_icu)/total_icu_beds,digits=3)) %>%
      mutate_at(vars(bed_utilization),scales::percent)
    
    infoBox(
      title="ICU Beds Availability", value=paste0(tot_pos$bed_utilization), icon=icon("procedures"),
      subtitle="of ICU Beds Available",
      color = "navy", href=NULL
    )
  })
  
# {Ventilators Availability - State}-------------------------------------------------
  
  output$tx_vents <- renderInfoBox({
    
    tot_pos <- dshs_tsa_vent_data %>% 
      filter(tsa=="Total") %>% 
      mutate(vent_availability=round(total_vents_avail/(total_vents_avail+total_vents_in_use), digits=2)) %>% 
      select(vent_availability) %>% 
      mutate_at(vars(vent_availability), scales::percent_format(accuracy=.1))
    
    
    infoBox(
      title="Ventilator Availability", value=paste0(tot_pos$vent_availability), icon=icon("lungs"),
      subtitle="of Ventilators Available",
      color = "navy", href=NULL
    )
  })

  
  # {Hospitalization Rate - State} -----------------------------------------------
  
  output$hospitalized_rate <- renderInfoBox({
    
    tot_pos <- hosp_capacity %>% 
      select(hospitalization_rate, hosprate_rank) %>% 
      mutate_at(vars(hospitalization_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
      mutate_at(vars(hosprate_rank), scales::label_ordinal())
    
    
    infoBox(
      title="% Hospitalized", value=paste0(tot_pos$hospitalization_rate, "%"),
      subtitle=paste0(tot_pos$hosprate_rank, " Most in US"),
      icon = icon("hospital-user"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  
  
  # {State Growth Rate Charts}  --------------------------------------------------
  
  output$state_growth_rate_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    nyt_tx_growth_rate_hchart <- nyt_state_cases_tx %>% 
      filter(date > as.Date("2020-03-06")) %>% 
      arrange(date) %>% 
      mutate(daily_growth_rate = round(((cases/lag(cases))-1)*100, digits=1)) %>%
      mutate(daily_growth_rate_7day_avg = round(rollmean(daily_growth_rate, 7,
                                                   fill=0, align = "right"), digits=1)) %>%
      mutate(min_new = min(daily_growth_rate, na.rm = TRUE),
             max_new = max(daily_growth_rate, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup()
    
    nyt_tx_hchart <- nyt_tx_growth_rate_hchart %>% 
      hchart("column", hcaes(x = date, y = daily_growth_rate), animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(nyt_tx_growth_rate_hchart, type = "area", hcaes(x = date, y = daily_growth_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_growth_rate_7day_avg:,.0f}%"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Case Growth Rate, By Day | <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text = "Growth Rate From Previous Day"),
               min = round(mean(nyt_tx_growth_rate_hchart$min_new), 2), 
               max = round(mean(nyt_tx_growth_rate_hchart$max_new), 2),
               format = "{value}%") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Growth Rate: {point.y:.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: New York Times County-Level COVID-19 Data (Github)",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    nyt_tx_hchart
  })
  
  # {State CLI Charts}  --------------------------------------------------
  
  output$cli_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    nyt_tx_hchart <- dshs_syndromic_tx %>% 
      mutate(min = min(ili_from_syndromic_surveillance, na.rm = TRUE),
             max = max(ili_from_syndromic_surveillance, na.rm = TRUE)) 
    
    nyt_tx_hchart %>% 
      hchart("area", hcaes(x = date, y = cli_from_syndromic_surveillance), animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas Coronavirus-Like Illnesses (CLI)",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text="Reported Cases of CLI"),
               min = 0, 
               max = 3000) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported CLI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
  })
  
  # {State ILI Charts}  --------------------------------------------------
  
  output$ili_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    nyt_tx_hchart <- dshs_syndromic_tx %>% 
      mutate(min = min(ili_from_syndromic_surveillance, na.rm = TRUE),
             max = max(ili_from_syndromic_surveillance, na.rm = TRUE)) 
    
    nyt_tx_hchart %>% 
      hchart("area", hcaes(x = date, y = ili_from_syndromic_surveillance), animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas Influenza-Like Illnesses (ILI)",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Reported Cases of ILI"),
               min = 0, 
               max = 3000) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported ILI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
  })
  
  # {State Curve Charts}  --------------------------------------------------
  
  output$state_curves_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
    #               name = "Fit", id = "fit") 
    
    # min <- nyt_state_cases_tx %>% 
    #   select(min) %>% 
    #   distinct() %>% 
    #   # filter(!is.na(1)) %>% 
    #   as.character()
    
    nyt_tx_hchart <- nyt_state_cases_tx %>% 
      hchart("area", hcaes(x = date, y = cases), animation=FALSE,
             color = "#fff") %>% 
      hc_title(
        text ="Texas COVID-19 Cumulative Cases",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Total Cases"),
               min = round(mean(nyt_state_cases_tx$min), 2), 
               max = round(mean(nyt_state_cases_tx$max), 2)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: New York Times County-Level COVID-19 Data (Github)",
        href = "https://github.com/nytimes/covid-19-data") %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    nyt_tx_hchart
  })
  
  # {State New Cases Charts}  --------------------------------------------------
  
  output$state_new_cases_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)

    nyt_tx_new_cases_hchart <- nyt_state_cases_tx %>% 
      arrange(date) %>% 
      mutate(daily_growth_rate = (new_cases_1day/lag(new_cases_1day))) %>%
      mutate(daily_growth_rate_7day_avg = rollmean(new_cases_1day, 7, 
                                               fill=0, align = "right")) %>% 
      mutate(min_new = min(new_cases_1day, na.rm = TRUE),
             max_new = max(new_cases_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 

      ungroup()
    
    nyt_tx_new_cases_hchart %>% 
     hchart("column", hcaes(x = date, y = new_cases_1day), 
             animation=FALSE,
             color = "#fff") %>% 
     hc_add_series(nyt_tx_new_cases_hchart, type = "area", hcaes(x = date, y = daily_growth_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_growth_rate_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Cases",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="LEGEND - <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="New Cases Per Day"),
               min = mean(nyt_tx_new_cases_hchart$min_new),
               max = mean(nyt_tx_new_cases_hchart$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Cases: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: New York Times County-Level COVID-19 Data (Github)",
        href = "https://github.com/nytimes/covid-19-data") %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", 
                                             fontWeight = "bold",
                                             color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  

# {State New Deaths  Charts} ----------------------------------------------
  
  output$state_new_deaths_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
    #               name = "Fit", id = "fit") 
    
    nyt_tx_new_cases_hchart <- nyt_state_cases_tx %>% 
      mutate(daily_death_growth_rate_7day_avg = rollmean(new_deaths_1day, 7, 
                                               fill=0, align = "right")) %>% 
      mutate(min_new = min(new_deaths_1day, na.rm = TRUE),
             max_new = max(new_deaths_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup()

    nyt_tx_new_cases_hchart %>% 
      hchart("column", hcaes(x = date, y = new_deaths_1day), 
             animation=FALSE,
             color = "#fff") %>% 
     hc_add_series(nyt_tx_new_cases_hchart, type = "area", hcaes(x = date, y = daily_death_growth_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_death_growth_rate_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Deaths",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="LEGEND - <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="New Deaths Per Day"),
               min = mean(nyt_tx_new_cases_hchart$min_new),
               max = mean(nyt_tx_new_cases_hchart$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Deaths: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: New York Times County-Level COVID-19 Data (Github)",
        href = "https://github.com/nytimes/covid-19-data") %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  

# {State New Tests Chart} -------------------------------------------------

  
  output$state_new_tests_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
    #               name = "Fit", id = "fit") 
    
    tx_new_tests_hchart <- test_daily %>%
      arrange(date) %>%
      mutate(daily_test_growth_rate = (totalTestResultsIncrease/lag(totalTestResultsIncrease))) %>%
      mutate(daily_test_growth_rate_7day_avg = rollmean(totalTestResultsIncrease, 7, 
                                                   fill=0, align = "right")) %>% 
      select(date,state,totalTestResultsIncrease,daily_test_growth_rate_7day_avg) %>% 
      mutate(min_new = min(totalTestResultsIncrease, na.rm = TRUE),
             max_new = max(totalTestResultsIncrease, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup()
    
    tx_new_tests_hchart %>% 
      hchart("column", hcaes(x = date, y = totalTestResultsIncrease), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(tx_new_tests_hchart, type = "area", hcaes(x = date, y = daily_test_growth_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_test_growth_rate_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Tests",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="LEGEND - <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="New Tests Per Day"),
               min = mean(tx_new_tests_hchart$min_new),
               max = mean(tx_new_tests_hchart$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Tests: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: The COVID Tracking Project (Github)",
        href = "https://github.com/nytimes/covid-19-data") %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  

 
  

# {State Jobless Claims Chart} -------------------------------------------------
  
  
  output$state_claims_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    tx_series_all %>% 
      hchart("column", hcaes(x = date, y = value), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth=10)) %>% 
      hc_title(
        text ="Texas Jobless Claims",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Claims Filed Weekly"),
               min = mean(tx_series_all$min_new),
               max = mean(tx_series_all$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Weekly Claims Filed: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: The Federal Reserve Bank of St. Louis + Department of Labor",
        href = "https://fred.stlouisfed.org/series/TXICLAIMS") %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  
  
  
  
  
# {State Businesses Open Chart} -------------------------------------------------
  
  
  output$state_businesses_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    hb_businesses_open_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Change in Businesses Open",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Business Open"),
               min = mean(tx_series_all$min_new),
               max = mean(tx_series_all$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Weekly Claims Filed: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  
  
  
  
  # {State Hours Worked} -------------------------------------------------
  
  
  output$state_hours_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    hb_hours_worked_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Change in Hours Worked By Hourly Employees",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Hours Worked"),
               min = mean(tx_series_all$min_new),
               max = mean(tx_series_all$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Hours Worked: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  
  
  
  
  # {State Employees Working} -------------------------------------------------
  
  
  output$state_employees_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    hb_employees_working_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text = "Change in Number of Hourly Employees Working",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Employees Working"),
               min = mean(tx_series_all$min_new),
               max = mean(tx_series_all$max_new)) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Employees Working: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = NULL) %>%
      hc_add_theme(
        hc_theme_merge(
          hc_theme_smpl(),
          hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                             color="#fff",fontWeight="500", textTransform="uppercase")),
                   title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                align = "left"), 
                   subtitle = list(style = list(fontFamily = "Montserrat", 
                                                color="#fff",
                                                fontSize = "12px"),
                                   align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = 0,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = .5), 
                   yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")), 
                                gridLineWidth = .5,
                                gridLineColor = "#F3F3F3", 
                                lineColor = "#fff", 
                                minorGridLineColor = "#F3F3F3", 
                                tickColor = "#F3F3F3", 
                                tickWidth = 1))))
    
    
  })
  
  
  
  
 # {MAP_SHAPE_CLICK CODE FOR MAP} -----------------------------------------------------------
  
output$map <- renderLeaflet({
   
  click_cnty <- eventReactive(input$map_shape_click, {
      
      x <- input$map_shape_click  
      
      y <- x$id  
      
      print(x)  
      
      return(y)  
      
    })
    
    observe({
      
      # req(input$countyname) # do this if click_tract() is not null
      
      tx_highlight_shp <- tx_county_sf %>% 
        filter(county==input$countyname)
      
      tx_highlight_shp_tsa <- dshs_tsa_hosp_data %>% 
        filter(tsa_counties==input$countyname)
      
      # Add the clicked tract to the map in aqua, and remove when a new one is clicked
      map <- leafletProxy('map') %>%
        removeShape('hcounty') %>%
        removeShape('harea') %>%
        addPolygons(data=tx_highlight_shp_tsa,
                    fill = TRUE,
                    fillColor = "#2d2d2d",
                    fillOpacity = .5,
                    stroke = TRUE,
                    weight = 3,
                    highlightOptions = highlightOptions(bringToFront = FALSE),
                    color = '#5d5d5d', 
                    opacity = .5, 
                    layerId = 'harea') %>% 
        addPolygons(data = tx_highlight_shp,
                    fill = FALSE,
                    stroke = TRUE,
                    weight = 2,
                    color = '#3A4A9F', 
                    opacity = 1, 
                    layerId = 'hcounty')
      
    })
    
    metro <- reactive({

      m <- tx_county_sf[tx_county_sf$county == click_cnty(), ]
      m <- m %>%
        as.data.frame()

      return(m)

    })
    # 
    county_data <- reactive({

      # Fetch data for the clicked zipcode
      return(metro()[metro()$county == click_cnty(), ])

    })
    

# REACTIVE TEXT -----------------------------------------------------------
    
# State - Hours Worked ----------------------------------------------------

output$state_hours_worked <- renderText({
  
  tx_county_cases %>%
    filter(county==input$countyname) %>% 
    mutate_at(vars(cases), scales::comma) %>% 
    distinct(cases) %>% 
    as.character()
  
})
    

# County Name -------------------------------------------------------------

    output$county_name <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      name <- input$countyname %>% 
        as.character()
      
      paste0(name," County")
    })   
    
# TSA Name -------------------------------------------------------------
    
    output$tsa_name <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tsa_name <- dshs_tsa_hosp_data %>%
        as_tibble() %>%
        filter(tsa_counties==input$countyname) %>%
        # filter(tsa_counties=="El Paso") %>%
        separate(tsa, into=c("tsa_init","tsa_letter")) %>% 
        distinct(tsa_letter) %>% 
        as.character()
        
      paste0("Trauma Service Area ",tsa_name, ".")
    })   
    
    # TSA Date -------------------------------------------------------------
    
    output$tsa_date <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tsa_name <- dshs_tsa_hosp_data %>%
        as_tibble() %>%
        filter(tsa_counties==input$countyname) %>%
        distinct(date) %>% 
        as.character()
      
      paste0("Trauma Service Area ",tsa_name, ".")
    })   

# County Confirmed Cases --------------------------------------------------

    output$county_cases_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tx_county_cases %>%
        filter(county==input$countyname) %>% 
        mutate_at(vars(cases), scales::comma) %>% 
        distinct(cases) %>% 
        as.character()
    })
    
# County Active Cases --------------------------------------------------
    
    output$county_active_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tx_county_cases %>%
        filter(county==input$countyname) %>% 
        mutate_at(vars(active), scales::comma) %>% 
        distinct(active) %>% 
        as.character()
    })
    
# County Incident Rate ----------------------------------------------------
  
    output$county_incident_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tx_county_cases %>%
        filter(county==input$countyname) %>% 
        distinct(incident_rate) %>% 
        round(digits=2) %>% 
        as.character()
    })
    
# County Confirmed Deaths -------------------------------------------------

    output$county_deaths_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      tx_county_cases %>%
        filter(county==input$countyname) %>% 
        distinct(deaths) %>% 
        as.character()
    })
    
# County Mortality Rate ---------------------------------------------------

output$county_mort_rate <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        filter(date == max(date)) %>% 
        filter(county==input$countyname) %>%
        mutate(mort_rate=round(deaths/cases,digits=4)) %>% 
        mutate_at(vars(mort_rate),scales::percent) %>% 
        distinct(mort_rate) %>% 
        as.character()
    })

# County Testing Rate -----------------------------------------------------

 output$county_test_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      total_cnty_tests %>%
        filter(county_name==input$countyname) %>% 
        distinct(tests_per_100k) %>% 
        as.character()
    })

# County Testing Total -----------------------------------------------------
    
    output$county_test_tots <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      total_cnty_tests %>%
        filter(county_name==input$countyname) %>% 
        distinct(total_tests) %>% 
        as.character()
    })
    
# TSA Beds Occupation -----------------------------------------------------
    
    output$hosp_beds_tsa <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        as_tibble() %>% 
        select(tsa,tsa_counties,available_beds,bed_capacity) %>% 
        filter(tsa_counties==input$countyname) %>%
        # filter(tsa_counties=="El Paso") %>%
        mutate(bed_utilization=round(available_beds/bed_capacity,digits=3)) %>%
        mutate_at(vars(bed_utilization),scales::percent) %>% 
        distinct(bed_utilization) %>% 
        as.character()
    })
  
# TSA ICU Bed Availability -----------------------------------------------------
    
    output$hosp_icu_beds_tsa <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        as_tibble() %>% 
        select(tsa,tsa_counties,adult_icu,total_icu_beds) %>% 
        filter(tsa_counties==input$countyname) %>%
        # filter(tsa_counties=="El Paso") %>%
        mutate(bed_utilization=round((total_icu_beds-adult_icu)/total_icu_beds,digits=3)) %>%
        mutate_at(vars(bed_utilization),scales::percent) %>% 
        distinct(adult_icu) %>% 
        as.character()
    })
    
# TSA Hosp Covid ER Visits -----------------------------------------------------
    
    output$hosp_covid_er_visits <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        distinct(covid19_er_visits_24h) %>% 
        as.character()
    })
    
# TSA Suspected COVID Patients -----------------------------------------------------
    
    output$hosp_susp_covid <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        distinct(suspected_in_hosp) %>% 
        as.character()
    })
    
# TSA Lab Confirmed COVID Patients -----------------------------------------------------
    
    output$hosp_lab_covid <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        distinct(lab_con_covid19_gen) %>% 
        as.character()
    })

# CHARTS ------------------------------------------------------------------
    

# # {County Compare Chart} ----------------------------------------------------
# 
#     output$cnty_compare_hchart <- renderHighchart({
#       
#       # Make sure requirements are met
#       req(input$countyname)
#       
#       nyt_county_cases_chart <- nyt_county_cases %>% 
#         filter(county==input$countyname) %>% 
#         mutate(date = ymd(date)) 
#       
#       nyt_county_cases_today <- nyt_county_cases %>%
#         mutate(date = ymd(date)) %>% 
#         group_by(state) %>% 
#         filter(date == max(date)) %>% 
#         ungroup()
#       
#       nyt_highlight_cnty <- nyt_county_cases_today %>% 
#         filter(county==input$countyname)
#       
#       nyt_county_cases_today %>% 
#         hchart(type="scatter", hcaes(x = cases, y = 0, name = county), animation=FALSE,
#              color = "#fff",showInLegend=FALSE) %>%
#         hc_add_series(data=nyt_highlight_cnty, type="scatter",  hcaes(x = cases, y = 0,name = county), 
#                       color = "#FFD100", marker = list(radius=9, enabled=TRUE), animation = FALSE,
#                       showInLegend=FALSE, dataLabels = list(enabled = TRUE,
#                                                             padding=10,
#                                                             format = '{point.name} County')) %>%
#         hc_xAxis(labels=list(format = "{value:,.0f} Cases")) %>%
#         hc_yAxis(title=FALSE,
#           showFirstLabel = FALSE,
#           showLastLabel = FALSE
#         ) %>% 
#         hc_tooltip(table = TRUE, sort = TRUE, borderWidth = 0,
#                    pointFormat = "<b>{this.name}</b><br>
#                                   Confirmed Cases: <b> {point.x:,.0f}</b>") %>%
#         hc_add_theme(
#           hc_theme_merge(
#             hc_theme_smpl(),
#             hc_theme(chart = list(backgroundColor = "#3A4A9F",
#                                   style = list(fontFamily = "Montserrat")),
#                      plotOptions = list(series = list(dataLabels = list(style = list(fontFamily = "Montserrat", 
#                                                                                      color="#FFD100",
#                                                                                      fontSize = "1.4em",
#                                                                                      textOutline = FALSE)))),
#                      yAxis = list(labels = list(style = list(fontFamily = "Montserrat", color="#fff")), 
#                                   title = list(style = list(color = "#fff", fontSize = "12px", 
#                                                             color="#fff",fontWeight="500")), 
#                                   gridLineWidth = 0,
#                                   gridLineColor = "#F3F3F3", 
#                                   lineColor = "#fff", 
#                                   minorGridLineColor = "#F3F3F3", 
#                                   tickColor = "#F3F3F3", 
#                                   tickWidth = 0),
#                      xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
#                                   title = list(enabled=FALSE),
#                                   gridLineWidth = 0,
#                                   gridLineColor = "#F3F3F3", 
#                                   lineColor = "#fff", 
#                                   minorGridLineColor = "transparent", 
#                                   tickColor = "#F3F3F3", 
#                                   tickWidth = .5)))
#           )
#       
#     })
#     
    
# {County Curve Charts}  --------------------------------------------------
    
    output$cnty_curves_hchart <- renderHighchart({
      
      # Make sure requirements are met
     req(input$countyname)
      
      nyt_county_cases_chart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>% 
        mutate(min_new = min(cases, na.rm = TRUE),
               max_new = max(cases, na.rm = TRUE)) %>% 
        mutate(date = ymd(date))
      
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      nyt_county_cases_chart %>% 
      hchart("area", hcaes(x = date, y = cases), animation=FALSE,
             color = "#fff") %>% 
        hc_title(
          text = paste0(input$countyname, " County COVID-19 Cases"),
          useHTML = TRUE) %>% 
        hc_yAxis(title=list(text="Total Cases"),
                 min = round(mean(nyt_county_cases_chart$min_new), 1), 
                 max = round(mean(nyt_county_cases_chart$max_new), 1)) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: New York Times County-Level COVID-19 Data (Github)",
          href = "https://github.com/nytimes/covid-19-data") %>%
        hc_add_theme(
              hc_theme_merge(
                hc_theme_smpl(),
                hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                      style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                                   color="#fff",fontWeight="500", textTransform="uppercase")),
                         title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                      align = "left"), 
                         title = list(style = list(fontFamily = "Montserrat", color="#fff"),
                                         align = "left"), 
                         legend = list(align = "right", 
                                       style = list(fontFamily = "Montserrat", color="white"), 
                                       verticalAlign = "bottom"),
                         credits = list(style = list(color = "#fff")),
                         xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                      title = list(style = list(color = "#fff", fontSize = "12px", 
                                                                color="#fff",fontWeight="500")),
                                      gridLineWidth = 0,
                                      gridLineColor = "#F3F3F3", 
                                      lineColor = "#fff", 
                                      minorGridLineColor = "#F3F3F3", 
                                      tickColor = "#F3F3F3", 
                                      tickWidth = .5), 
                         yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                      title = list(style = list(color = "#fff", fontSize = "12px", 
                                                                color="#fff",fontWeight="500")), 
                                      gridLineWidth = .5,
                                      gridLineColor = "#F3F3F3", 
                                      lineColor = "#fff", 
                                      minorGridLineColor = "#F3F3F3", 
                                      tickColor = "#F3F3F3", 
                                      tickWidth = 1))))
    })
    
# {County New Cases Charts}  --------------------------------------------------
    
    output$cnty_new_cases_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      nyt_cnty_new_cases_hchart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>%
        arrange(date) %>% 
        mutate(daily_growth_rate = (new_cases_1day/lag(new_cases_1day))) %>%
        mutate(daily_growth_rate_7day_avg = rollmean(new_cases_1day, 7, 
                                                     fill=0, align = "right")) %>% 
        mutate(min_new = min(new_cases_1day, na.rm = TRUE),
               max_new = max(new_cases_1day, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup()
      
      nyt_cnty_new_cases_hchart %>% 
        hchart("column", hcaes(x = date, y = new_cases_1day), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(nyt_cnty_new_cases_hchart, type = "area", hcaes(x = date, y = daily_growth_rate_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_growth_rate_7day_avg:,.0f}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(column = list(pointWidth = 6),
                       area = list(fillOpacity=.3)) %>% 
        hc_title(
          text =paste0(input$countyname, " County | COVID-19 Daily New Cases"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="LEGEND - <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title=list(text="New Cases Per Day"),
                 min = mean(nyt_cnty_new_cases_hchart$min_new),
                 max = mean(nyt_cnty_new_cases_hchart$max_new)) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   New Cases: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: New York Times County-Level COVID-19 Data (Github)",
          href = "https://github.com/nytimes/covid-19-data") %>%
        hc_add_theme(
          hc_theme_merge(
            hc_theme_smpl(),
            hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                  style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                               color="#fff",fontWeight="500", textTransform="uppercase")),
                     title = list(style = list(fontFamily = "Montserrat", 
                                               fontWeight = "bold",
                                               color="white"),
                                  align = "left"), 
                     subtitle = list(style = list(fontFamily = "Montserrat", 
                                                  color="#fff",
                                                  fontSize = "12px"),
                                     align = "left"), 
                     legend = list(align = "right", 
                                   style = list(fontFamily = "Montserrat", color="white"), 
                                   verticalAlign = "bottom"),
                     credits = list(style = list(color = "#fff")),
                     xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(style = list(color = "#fff", fontSize = "12px", 
                                                            color="#fff",fontWeight="500")),
                                  gridLineWidth = 0,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "#F3F3F3", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = .5), 
                     yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(style = list(color = "#fff", fontSize = "12px", 
                                                            color="#fff",fontWeight="500")), 
                                  gridLineWidth = .5,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "#F3F3F3", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = 1))))
      
      
    })
    
    
    # {County New Deaths  Charts} ----------------------------------------------
    
    output$cnty_new_deaths_hchart <- renderHighchart({
      
      # Make sure requirements are met
      # req(input$countyname)
      
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
      #               name = "Fit", id = "fit") 
      
      nyt_cnty_new_deaths_hchart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>%
        arrange(date) %>% 
        mutate(daily_death_growth_rate_7day_avg = rollmean(new_deaths_1day, 7, 
                                                           fill=0, align = "right")) %>% 
        mutate(min_new = min(new_deaths_1day, na.rm = TRUE),
               max_new = max(new_deaths_1day, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup()
      
      nyt_cnty_new_deaths_hchart %>% 
        hchart("column", hcaes(x = date, y = new_deaths_1day), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(nyt_cnty_new_deaths_hchart, type = "area", hcaes(x = date, y = daily_death_growth_rate_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_death_growth_rate_7day_avg:,.0f}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_title(
          text =paste0(input$countyname, " County | COVID-19 Daily New Deaths"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="LEGEND - <span style='color: #FFD100'>7-Day Rolling Avg.</span>",
          useHTML = TRUE) %>% 
        hc_plotOptions(column = list(pointWidth = 6),
                       area = list(fillOpacity=.3)) %>% 
        hc_yAxis(title=list(text="New Deaths Per Day"),
                 min = mean(nyt_cnty_new_deaths_hchart$min_new),
                 max = mean(nyt_cnty_new_deaths_hchart$max_new)) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   New Deaths: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: New York Times County-Level COVID-19 Data (Github)",
          href = "https://github.com/nytimes/covid-19-data") %>%
        hc_add_theme(
          hc_theme_merge(
            hc_theme_smpl(),
            hc_theme(chart = list(backgroundColor = "#3A4A9F", 
                                  style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                               color="#fff",fontWeight="500", textTransform="uppercase")),
                     title = list(style = list(fontFamily = "Montserrat", fontWeight = "bold",color="white"),
                                  align = "left"), 
                     subtitle = list(style = list(fontFamily = "Montserrat", 
                                                  color="#fff",
                                                  fontSize = "12px"),
                                     align = "left"), 
                     legend = list(align = "right", 
                                   style = list(fontFamily = "Montserrat", color="white"), 
                                   verticalAlign = "bottom"),
                     credits = list(style = list(color = "#fff")),
                     xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(style = list(color = "#fff", fontSize = "12px", 
                                                            color="#fff",fontWeight="500")),
                                  gridLineWidth = 0,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "#F3F3F3", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = .5), 
                     yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(style = list(color = "#fff", fontSize = "12px", 
                                                            color="#fff",fontWeight="500")), 
                                  gridLineWidth = .5,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "#F3F3F3", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = 1))))
      
      
    })
    
# TEST CODE ---------------------------------------------------------------
    # p <- c(0.5, 0.75, 0.9, .99, 1)
    # 
    # p_names <- map_chr(p, ~paste0(.x*100, "%"))
    # 
    # p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    #   set_names(nm = p_names)
    # 
    # avgs <- tx_today %>% 
    #   group_by(date) %>% 
    #   mutate(sqrt=sqrt(cases)) %>% 
    #   summarize_at(vars(sqrt), funs(!!!p_funs)) %>% 
    #   gather(percentile,value, 2:6) %>% 
    #   mutate(size=round((value*value),digits = 0)) %>% 
    #   mutate(label=as.character(size))
    
# COUNTY MAP --------------------------------------------------------------

    pal <- colorNumeric(palette = "Reds", na.color = "#DBDCDD", 
                        domain = tx_county_sf$incident_rate)
    
    
    labels_clean <- sprintf("<a style = 'font-family: Montserrat; font-size: 22px; font-weight: 700; color:#3a4a9f'>%s County</a></br><a style = 'font-family: Montserrat; font-size: 16px; font-weight: 400; color:#6B6D6F'>%s Active Cases</a><br/><a style = 'font-family: Montserrat; font-size: 12px; font-weight: 400; color:#8C8F93'>%s Total Confirmed Cases</a>",
                            tx_county_sf$county,
                            tx_county_sf$active_label,
                            tx_county_sf$cases_label) %>%
   lapply(htmltools::HTML)
    
    map <- leaflet(tx_county_sf, width = "100%", height = "600px", 
                   options = leafletOptions(zoomControl = FALSE, minZoom = 6, 
                                            maxZoom = 6, dragging=FALSE, 
                                            doubleClickZoom= FALSE)) %>%
      addTiles(urlTemplate = blank, 
               options = providerTileOptions(detectRetina = TRUE), 
               attribution = map_attr) %>% 
      addPolygons(data = tx_county_sf,
                  stroke = TRUE, 
                  color = "#3A4A9F", 
                  weight = 1,
                  smoothFactor = 0, 
                  fillColor = ~pal(incident_rate),
                  fillOpacity = 1,
                  group="Incidence Rate",
                  layerId = ~county,
                  label = labels_clean,
                  labelOptions = labelOptions(
                    style = list("font-family" = "Montserrat", 
                                 "font-weight" = "normal",
                                 "text-align" = "left",
                                 "line-height" = "1.3",
                                 padding = "3px 8px"),
                    textsize = "18px",
                    direction = "auto")) %>% 
      addPolygons(data= tsa_shps,
                  fill = FALSE,
                  stroke = TRUE,
                  weight = 2,
                  color = '#2d2d2d',
                  layerId = ~tsa,
                  label = labels_clean,
                  labelOptions = labelOptions(
                    style = list("font-family" = "Montserrat", 
                                 "font-weight" = "normal",
                                 "text-align" = "left",
                                 "line-height" = "1.3",
                                 padding = "3px 8px"),
                    textsize = "18px",
                    direction = "auto"),
                  opacity = 1, 
                  group="Trauma Service Areas") %>% 
      addCircleMarkers(~long, ~lat,
                       radius = ~sqrt(cases*.95),
                       color = "#F26852",
                       stroke = TRUE,
                       weight=2,
                       fillOpacity = 0.5,
                       group="Confirmed Cases",
                       label = labels_clean,
                       labelOptions = labelOptions(
                         style = list("font-family" = "Montserrat", 
                                      "font-weight" = "normal",
                                      "text-align" = "left",
                                      "line-height" = "1.3",
                                      padding = "3px 8px"),
                         textsize = "18px",
                         direction = "auto")) %>%
      addLayersControl(
        overlayGroups = c("Confirmed Cases","Trauma Service Areas"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      leaflet::addLegend("bottomleft",
                data = tx_county_sf,
                pal = pal,
                values = ~incident_rate,
                title = "Cases<br>(Per 100k People)",
                opacity = 1) %>%
      # setView(31.9686, -99.9018, zoom = 6) #%>%
      fitBounds(-106.64585, 25.83706, -93.50782, 36.50045)
    
    map 
    
    
  })
  
}
shiny::shinyApp(ui, server)
