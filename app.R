#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(vroom)
library(shiny)
library(shinyLP)
library(readxl)
library(gt)
library(metathis)
library(highcharter)
library(tidyverse)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(janitor)
library(waiter)
library(sever)
library(sf)
library(zoo)

blank <- "https://api.mapbox.com/styles/v1/mrw03b/ck9k6odnd1hqd1it49c80p11z/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibXJ3MDNiIiwiYSI6IlYwb2FiOWcifQ.RWUm2a87fEC9XrDxzvZKKg"
map_attr <- "<a href='https://www.mapbox.com/map-feedback/'>© MAPBOX</a> | <a href='http://texas2036.org'> MAP © TEXAS 2036</a>"

thumbnail_label <- function (title, label, content, button_link, button_label) {
  div(class = "row", div(class = "col-sm-14 col-md-12",
                         div(class = "thumbnail", 
                             HTML(title),
                             h3(label), 
                             p(content), 
                             actionButton(inputId=button_link, 
                                          label=button_label)
                         )))
}


tx2036_hc <- hc_theme_merge(
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
                                        textTransform="initial",
                                        fontWeight="400",
                                        fontSize = "14px"),
                           align = "left"), 
           legend = list(align = "right", 
                         style = list(fontFamily = "Montserrat", color="white"),
                         itemStyle = list(fontFamily = 'Montserrat', color = 'white'),
                         itemHoverStyle = list(color = 'gray'),   
                         verticalAlign = "top"),
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
                        tickWidth = 1)))


sever_default <- function (title = "Whoops!", subtitle = "You have been disconnected", 
                           button = "Reload", button_class = "default") 
{
  tagList(tags$a(href='http://www.texas2036.org',
                 HTML('<svg viewBox="0 0 227.4 83.5" style="height:7vh;padding-bottom:1.1vh;margin-top:7px"><path fill="#fff" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
                 tags$title('Texas COVID-19 Resource Kit')),
          tags$h1(title), tags$p(subtitle), reload_button(button, class = button_class))
}


# Waiting Screen ----------------------------------------------------------

disconnected <- sever_default(
  # logo = "www/logo_short_w.png",
  title = "Howdy!", 
  subtitle = "There's a lot of data here, so this app has been resting while you were away.", 
  button = "Push to Wake", 
  button_class = "info"
)

# DATA PREP CODE ----------------------------------------------------------


# **Coronavirus Data -----------------------------------------------------------


# ~~JHU Data --------------------------------------------------------------


jhu_cases_state_us <- vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_state.csv") %>% 
  janitor::clean_names() %>%
  filter(country_region=="US") %>%
  group_by(last_update) %>% 
  mutate(cases_rank=dense_rank(desc(confirmed)),
         deaths_rank=dense_rank(desc(deaths)),
         recovered_rank=dense_rank(desc(recovered)),
         active_rank=dense_rank(desc(active)),
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

jhu_cases_county <- vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases.csv") %>% 
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

nyt_county_cases <- vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
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

nyt_state_cases <- vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
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

tx_counties <- read_rds("clean_data/population/county_pop.rds")

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
  )) %>% 
  rename(county=NAME)

# ~~COVID Tracking Data --------------------------------------------------------------

test_daily <- vroom("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv") %>% 
  filter(state=="TX") %>%
  mutate(date=as.character(date)) %>%
  mutate(date=str_replace(date,"(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3")) %>% 
  mutate(date = ymd(date))

test_positive <- test_daily %>% 
  arrange(date) %>% 
  select(date, state, positive, totalTestResults, positiveIncrease, negativeIncrease,totalTestResultsIncrease) %>% 
  mutate(daily_test_pos_rate = round((positiveIncrease/totalTestResultsIncrease), digits=4),
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
         daily_test_pos_rate_7day_avg = rollmean(daily_test_pos_rate, 7, 
                                                 fill=0, align = "right"),
         test_pos_label = daily_test_pos_rate,
         test_pos_7day_label = daily_test_pos_rate_7day_avg) %>% 
  mutate_at(vars(totalTestResults), scales::comma_format(accuracy=1)) %>% 
  mutate_at(vars(per_capita), scales::comma_format(accuracy = .1)) %>%
  mutate_at(vars(test_pos_label,test_pos_7day_label),scales::percent_format(accuracy = .01, scale=100)) %>%
  filter(totalTestResultsIncrease!=0) %>%
  filter(date==max(date))


# ~~DSHS Data ----


dshs_state_case_and_fatalities <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/cases/state_cases_and_fatalities.csv") %>% 
  mutate(state="Texas",
         fips="48")

dshs_state_hospitalizations <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/hospitals/state_hospitalizations.csv") %>% 
  mutate(state="Texas",
         fips="48")

dshs_state_tests <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/testing/state_tests.csv") %>% 
  mutate(fips=as.character(fips))

dshs_state_demographics <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/cases/time_series_demographics.csv") %>% 
  mutate(fips=as.character(fips))

dshs_county_data <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/cases/county_cases.csv")

dshs_county_active <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/cases/cnty_active.csv")

dshs_county_test_data <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/testing/county_tests.csv")

dshs_tsa_hosp_data <- read_rds("clean_data/dshs/hospitals/texas_hosp_bed_ts_data.rds") %>% 
  st_transform(crs="+init=epsg:4326")

dshs_tsa_hosp_data_ts <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/hospitals/dshs_hosp_ts_data.csv")

dshs_tsa_vent_data <- read_rds("clean_data/dshs/hospitals/texas_hosp_vent_ts_data.rds") %>% 
  st_transform(crs="+init=epsg:4326")

dshs_tsa_24hr_data <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/hospitals/texas_hosp_bed_ts_24hr_data.csv") %>% 
  ungroup()

dshs_test_pos <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/testing/test_pos_tx_ts.csv")

dshs_syndromic_tx <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/syndromic_tx.csv")

# ** Population Data ---------------------------------------------------------

state_pop <- read_rds("clean_data/population/state_pop.rds")

tsa_shps <- dshs_tsa_hosp_data %>%
  filter(date==max(date)) %>% 
  filter(!str_detect(tsa,"Total|total")) %>%
  select(tsa,tsa_counties,geometry) %>% 
  group_by(tsa) %>% 
  summarise(tsa_counties = toString(tsa_counties)) %>% 
  ungroup()

# ** NPI Data ----------------------------------------------------------------


# ~~Google Mobility -------------------------------------------------------

google_mobility <- vroom("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
                            col_types = cols(sub_region_2 = col_character())) %>% 
  filter(sub_region_1=="Texas") %>% 
  mutate(sub_region_2=gsub(pattern=" County", replacement="",x=sub_region_2))

google_mobil_tx <- google_mobility %>%
  filter(is.na(sub_region_2)) %>% 
  mutate(date = ymd(date))

google_mobil_tx_cnties <- google_mobility %>%
  filter(!is.na(sub_region_2))

# google_tx_state <- vroom("clean_data/npi/google/google_mobility_tx_state.csv")
# 
# google_tx_cnties <- vroom("clean_data/npi/google/google_mobility_tx_cnties.csv")

# ** Economic Data -----------------------------------------------------------


# ^^^TWC County UI Claims Data ------------------------------------------------------

twc_ui_by_county <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/twc/county_claims_by_industry_detail.csv") %>% 
  filter(rank <= 5) %>% 
  rename(county=1) %>% 
  select(county,rank,naics_title,pct_label)

twc_claims_cnty_raw <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/twc/county_claims_data.csv")

twc_claims_cnty <- twc_claims_cnty_raw %>% 
  filter(str_detect(date, "^2020")) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(date=as.Date(date))

twc_claims_date <- twc_claims_cnty_raw %>% 
  filter(str_detect(date, "^2020")) %>% 
  mutate(date=ymd(date)) %>% 
  select(date) %>% 
  filter(date==max(date)) %>% 
  distinct()

twc_claims_cnty_summ <- twc_claims_cnty_raw %>% 
  mutate(date=as.Date(date)) %>%
  mutate(date=ymd(date)) %>%
  filter(date >= as.Date("2020-03-21")) %>% 
  group_by(county) %>% 
  summarise(all_claims=sum(value)) %>% 
  mutate_at(vars(all_claims), scales::comma_format(accuracy=1))

# ^^^Homebase Data -----------------------------------------------------

hb_hours_worked_all <- vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1930671010&single=true&output=csv", skip=2) %>% 
  fill(...2) %>%
  rename(X2=...2) %>% 
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
  select(geo_type, area = ...3, everything(),-X2) %>%
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
  mutate(prev_date=lag(pct,7),
         prev_day=lag(date,7),
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

hb_businesses_open_all <- vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1102464531&single=true&output=csv", skip=2) %>% 
  fill(...2) %>% 
  rename(X2=...2) %>% 
  slice(-1,-3,-4) %>% 
  select(-1) %>% 
  mutate(geo_type=case_when(
    str_detect(X2, "MSA") ~ "MSA",
    str_detect(X2, "industry") ~ "US Industry",
    str_detect(X2, "state") ~ "State",
    TRUE ~ "Nationwide"
  )) %>% 
  select(geo_type, area=...3,everything(),-X2) %>% 
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
  mutate(prev_date=lag(pct,7),
         prev_day=lag(date,7),
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

hb_employees_working_all <- vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1658358543&single=true&output=csv", skip=2) %>% 
  fill(...2) %>% 
  rename(X2=...2) %>% 
  slice(-1,-3,-4) %>% 
  select(-1) %>% 
  mutate(geo_type=case_when(
    str_detect(X2, "MSA") ~ "MSA",
    str_detect(X2, "industry") ~ "US Industry",
    str_detect(X2, "state") ~ "State",
    TRUE ~ "Nationwide"
  )) %>% 
  select(geo_type, area=...3,everything(),-X2) %>% 
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
  mutate(prev_date=lag(pct,7),
         prev_day=lag(date,7),
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

tx_series_all <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/fredr/ui_claims_ts.csv")

tx_series <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/fredr/tx_series_summ.csv", delim=",") %>% 
  mutate_at(vars(value),scales::comma)

tx_urn <- vroom("https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/fredr/unemploy_rate.csv")


# **DERIVED METRICS -----------------------------------

# 
# ** State -----------------------------------


# ~~Testing Metrics ----------------------------------------------------------

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

active_cases <- jhu_cases_state %>% 
  select(state=province_state, active, active_rank) 

hosp_rate <- dshs_tsa_hosp_data %>% 
  as_tibble() %>% 
  filter(date==max(date)) %>% 
  filter(str_detect(tsa,"Total|total")) %>% 
  select(lab_con_covid19_gen, lab_con_covid19_icu) %>% 
  mutate(state="Texas") %>% 
  left_join(active_cases, by="state") %>% 
  mutate(hospitalization_rate=100*((lab_con_covid19_gen+lab_con_covid19_icu)/active)) %>% 
  mutate_at(vars(hospitalization_rate), scales::number_format(accuracy=.01, scale=1))

test_pos_today <- dshs_test_pos %>% 
  filter(date==max(date)) %>% 
  mutate(test_pos_label=test_pos) %>% 
  mutate_at(vars(test_pos_label),scales::percent_format(accuracy=.01, scale=100))

# **STATE EXPLORER METRICS ------------------------------------------------------


# ~~Current Case Data -----------------------------------------------------


tx_cases <- jhu_cases_state %>% 
  select(confirmed, cases_rank) %>% 
  mutate_at(vars(confirmed), scales::comma) %>% 
  mutate_at(vars(cases_rank), scales::label_ordinal())

tx_mort <- jhu_cases_state %>% 
  select(mortality_rate,  mortality_rank) %>% 
  mutate_at(vars(mortality_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
  mutate_at(vars(mortality_rank), scales::label_ordinal())

tx_active <- active_cases %>% 
  mutate_at(vars(active), scales::number_format(accuracy=1, scale=1, big.mark = ",")) %>% 
  mutate_at(vars(active_rank), scales::label_ordinal())

tx_recover <-  jhu_cases_state %>% 
  select(recovered,  recovered_rank) %>% 
  mutate_at(vars(recovered), scales::number_format(accuracy=1, scale=1, big.mark = ",")) %>% 
  mutate_at(vars(recovered_rank), scales::label_ordinal())

date1 <- nyt_state_cases_tx %>% 
  filter(date==max(date))

date_filter <- date1$date - days(30)

state_case_growth <- nyt_state_cases_tx %>% 
  filter(date >= as.Date(date_filter)) %>% 
  arrange(date) %>% 
  mutate(daily_growth_rate = round(((cases/lag(cases))-1)*100, digits=1)) %>%
  mutate(daily_growth_rate_7day_avg = round(rollmean(daily_growth_rate, 7,
                                                     fill=0, align = "right"), digits=1)) %>%
  mutate(min_new = min(daily_growth_rate, na.rm = TRUE),
         max_new = max(daily_growth_rate, na.rm = TRUE)) %>% 
  mutate(min_new = as.numeric(min_new),
         max_new = as.numeric(max_new)) %>% 
  ungroup()

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

total_cnty_population <- tx_counties %>%
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

waiting_screen <- tagList(
  spin_flower(),
  h4("Pulling the latest data from 6 different sources...")
)



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
                              menuItem("Credits",
                                       tabName = "credits",
                                       icon = icon("circle")),
                              menuItem("Data",
                                       tabName = "data",
                                       icon = icon("circle")),
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
    # includeHTML("social_tags.html"),
    includeHTML(("google_analytics.html")),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")),
  
  # **Landing Page ----------------------------------------------------------
  use_sever(),
  meta() %>%
    meta_social(
      title = "Texas 2036 | Texas COVID-19 Data Resource",
      description = "A comprehensive look at Texas COVID-19 Health and Economic Data",
      url = "http://covid19.texas2036.org",
      image = "https://texas-2036.github.io/covid-pages/images/trends_cover.png",
      image_alt = "Texas 2036 | Texas COVID-19 Data Resource",
      twitter_creator = "@mrworthington",
      twitter_card_type = "summary",
      twitter_site = "@texas2036"
    ),
  tabItems(
    tabItem(tabName = "intro",
            jumbotron("Texas COVID-19 Data Resource", 
                      "A Comprehensive Look At Our Current Moment",
                      button = FALSE),
            # hr(style="border-top: 48px solid #fff;"),
            # HTML("<i style='color:#F26852;display: block;text-align: center;margin-top:-82px;margin-bottom: 20px;font-size: 112px;}' class='fas fa-2x fa-hands-helping'></i>"),
            hr(),
            br(),
            fluidRow(
              column(4, thumbnail_label(title="<i class='fas fa-door-open'></i>",
                                        label = 'Reopening Analysis',
                                        content = includeMarkdown("markdown/intro/reopening.md"),
                                        button_link ='explore_reopen', 
                                        button_label = 'Explore')),
              column(4, thumbnail_label(title="<i class='fas fa-landmark'></i>", 
                                        label = 'State Explorer',
                                        content = includeMarkdown("markdown/intro/state.md"),
                                        button_link ='explore_state', 
                                        button_label = 'Explore')),
              column(4, thumbnail_label(title="<i class='fas fa-city'></i>",
                                        label = 'County Explorer',
                                        content = includeMarkdown("markdown/intro/county.md"),
                                        button_link = 'explore_county', 
                                        button_label = 'Explore'))
            )),
    tabItem(tabName = "reopening",
            fluidRow(
              HTML("<iframe width='100%' height=1200vh' style='border-top-width: 0px;border-right-width: 0px;border-bottom-width: 0px;border-left-width: 0px;' src='https://staging.convex.design/texas-2036/texas-covid-live-report/?currentCounty=Anderson'></iframe>"))),
    
    tabItem(tabName = "state_profiles",
            use_waiter(include_js = FALSE),
            use_hostess(),
            waiter_show_on_load(html = tagList(spin_flower(),h4("Thanks for being patient while we get everything set up.")),
                                color = "#3A4A9F",
                                logo = "logo_short_w.png"),
            # waiter_show_on_load(
            #   color = "#f7fff7",
            #   hostess_loader(
            #     "loader", 
            #     preset = "circle", 
            #     text_color = "black",
            #     class = "label-center",
            #     center_page = TRUE
            #   )
            # ),
            
            # **STATEWIDE PROFILE UI ----------------------------------------------------
            h1(style="font-weight:800;", "STATEWIDE PROFILE", span="id='statewide-profile'"), 
            
            
            
            # ~~Current Cases Data ---------------------------------------------------------
            
            h3(class="covid-topic", "Current Case Data"),
            fluidRow(
              infoBoxOutput("tx_cases", width=3),
              infoBoxOutput("tx_mort", width=3),
              infoBoxOutput("tx_recover", width=3),
              infoBoxOutput("tx_active", width=3)
            ),
            
            # ~~Trends Over Time --------------------------------------------------------
            
            h3(class="covid-topic", "Trends Over Time", span="id='statewide_tot"),
            fluidRow(
              column(width=12, 
                     highchartOutput("state_new_cases_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_curves_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_growth_rate_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_new_deaths_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_new_tests_hchart", height = 350))
            ),

            
            # ~~Population Health ---------------------------------------------------------
            h3(class="covid-topic", "Population Health", span="id='pop_health"),
            column(width = 12,
                   tags$div(class="tsa-paragraph",
                            br(),
                            h4(style="font-weight:700;display:inline;","NOTE:"),
                            h4(style="font-weight:400;display:inline;","These charts were developed by recording each day's value from the DSHS daily report published on their 'Accessible Dashboard Data' report posted on their site every day, saving a copy of that information in a timestamped file on our github, and then organizing each of the individual timestamped files into a single time-series dataset that allows us to track trends in population health as the completes more cases and fatality investigations."),
                   ),
                   br(),
                   hr()
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_cases_race", height = 350)),
              column(width = 6,
                     highchartOutput("state_fatalities_race", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_cases_age", height = 350)),
              column(width = 6,
                     highchartOutput("state_fatalities_age", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_cases_gender", height = 350)),
              column(width = 6,
                     highchartOutput("state_fatalities_gender", height = 350))
            ),
            
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
                        p(style = "text-align:center;font-size:1em;font-weight:800", paste0(test_pos_today$test_pos_label)),
                        p(style = "text-align:center;font-size:.6em;font-weight:600;color:#DBDCDD;", 
                          paste0("Last 7 Days From ", format(test_pos_today$date, format="%b %d"))),
                        p(style = "text-align:center;font-size:.4em;font-weight:400", 
                          tags$a(href="https://www.dshs.state.tx.us/coronavirus/additionaldata/","Source: Texas Department of State Health Services"))))),
            
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
            
            
            # ~~Hospital Data -----------------------------------------------------------
            
            
            h3(class="covid-topic", "Current Hospital + Trends Data"),
            fluidRow(
              infoBoxOutput("hospitalized_rate", width=3),
              infoBoxOutput("tx_beds", width=3),
              infoBoxOutput("tx_icu_beds", width=3),
              infoBoxOutput("tx_vents", width=3)
            ),
            hr(),
            fluidRow(
              column(width = 6,
                     highchartOutput("total_conf_covid_gen_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("total_conf_covid_icu_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("daily_covid_gen_admits_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("daily_covid_icu_admits_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("daily_covid_ers_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("daily_covid_of_total_er_hchart", height = 350))
            ),
            
            # ~~Mobility ---------------------------------------------------------
            
            # ~~Trends Over Time --------------------------------------------------------
            
            h3(class="covid-topic", "Mobility Trends", span="id='statewide_mob"),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_grocery_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_parks_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_transit_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_retail_rec_hchart", height = 350))
            ),
            
            fluidRow(
              column(width = 6,
                     highchartOutput("state_workplace_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("state_residential_hchart", height = 350))
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
                        p(style="text-align:center;font-size:.45em;font-weight:300","Jobless Claims"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXICLAIMS","Source: BLS via FREDr")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(tx_urn$value,"%")),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;",
                          paste0("As of: ", format(tx_urn$date, format="%b %d"))),
                        p(style="text-align:center;font-size:.45em;font-weight:300","Unemployment Rate"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXURN","Source: BLS via FREDr")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_businesses_open$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_businesses_open$color,";"),
                          paste0(hb_businesses_open$change_lbl, hb_businesses_open$change, "% From ", format(hb_businesses_open$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.43em;font-weight:500","Est. Local Businesses Open"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_hours_worked$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_hours_worked$color,";"),
                          paste0(hb_hours_worked$change_lbl, hb_hours_worked$change, "% From ", format(hb_hours_worked$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.43em;font-weight:500","Est. Reduction In Hours Worked"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",paste0(hb_employees_working$pct,"%")),
                        p(style=paste0("text-align:center;font-size:.6em;font-weight:600;color:",hb_employees_working$color,";"),
                          paste0(hb_employees_working$change_lbl, hb_employees_working$change, "% From ", format(hb_employees_working$prev_day, format="%b %d"))),
                        p(style="text-align:center;font-size:.43em;font-weight:500","Est. Hourly Employees Working"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://joinhomebase.com/data/","Source: Homebase")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800","TBD"),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;","TBD"),
                        p(style="text-align:center;font-size:.45em;font-weight:300","Monthly $ Loss Per Employee"),
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
                     h2(style="font-weight:800;","COUNTY PROFILE", span="id='county-profile'")),
              column(width = 6,
                     selectizeInput(inputId = "countyname", label =NULL, choices = county_list,
                                    selected="Dallas", multiple = FALSE, width="100%",
                                    options = list(maxItems=1,placeholder = 'Select Your County...')))
            ),
            fluidRow(
              column(width = 6, 
                     # div(style = 'overflow-y: scroll; position: fixed; width: 700px',
                     h4("Data As of:",textOutput("currentTime", inline=TRUE)),
                     
                     # ~~County + TSA Map --------------------------------------------------------
                     
                     leafletOutput("map", width = "100%", height = 600)
              ),
              column(width = 6,
                     # h3(style="font-weight:700;",textOutput("countyname", inline = TRUE)),
                     
                     # ~~Current Case Data -----------------------------------------------------
                     
                     
                     h3(class="covid-topic", "Current Case Data"),
                     fluidRow( 
                       column(width = 3,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("county_active_text")),
                              p(style="text-align:center","Active Cases")),
                       column(width = 3,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("county_mort_rate")),
                              p(style="text-align:center","Deaths (as % of All Cases)")),
                       column(width = 3,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("county_test_text")),
                              p(style="text-align:center","Tests Per 100,000")),
                       column(width = 3,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("county_incident_text")),
                              p(style="text-align:center","Cases Per 100,000"))
                     ),
                     
                     
                     # ~~Current Hospital Data ---------------------------------------------------
                     
                     h3(class="covid-topic", "Current Hospital Data"),
                     fluidRow( 
                       column(width = 12,
                              tags$div( class="tsa-paragraph",
                                        br(),
                                        h4(style="font-weight:800;color:#FFD100;display:inline;", textOutput("county_name",inline=TRUE)),
                                        h4(style="font-weight:400;display:inline;", "is a part of "),
                                        h4(style="font-weight:800;text-align:left;display:inline;color:#FFD100;", textOutput("tsa_name",inline=TRUE)),
                                        h4(style="font-weight:400;display:inline;"," Trauma Service Areas are geographical clusters defined by where people are most likely to receive trauma care when they need it. For COVID-19 patients, it's the area where they are most likely to receive care. "),
                                        h4(style="font-weight:800;color:#FFD100;display:inline;", textOutput("tsa_date",inline=TRUE)),
                                        h4(style="font-weight:400;display:inline;", "the latest date for which we have data, hospitals in this Trauma Service Area reported the following:"),
                              ))),
                     fluidRow( 
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("hosp_covid_er_visits")),
                              p(style="text-align:center","COVID-19 Related ER Visits")),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("hosp_susp_covid")),
                              p(style="text-align:center","Suspected COVID-19 Patients")),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("hosp_lab_covid")),
                              p(style="text-align:center","Lab Confirmed COVID-19 Patients"))
                     ),
                     fluidRow( 
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_beds")),
                              p(style="text-align:center","All Beds Availability")),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_icu_beds")),
                              p(style="text-align:center","ICU Beds Availability")),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_vents")),
                              p(style="text-align:center","Ventilator Availability"))
                     ),
                     br())
            ),
            
            # ~~Economic Data ------------------------------------------------------
            h3(class="covid-topic", "Economic Data"),
            fluidRow(column(width=6,
                            fluidRow(
                              column(width = 4, class="economic-grid",
                                     h2(class="economic-tile-cnty",
                                        p(style="text-align:center;font-size:.6em;font-weight:600;text-transform:uppercase;letter-spacing:2px", textOutput("county_name2", inline=TRUE)),
                                        p(style="text-align: center;font-size: .5em;color:#FFD100;font-weight:400;text-transform: uppercase;letter-spacing: 2px;","Jobless Claims"),
                                        hr(style="width:80%;"),
                                        p(style="text-align:center;font-size:1em;font-weight:800", 
                                          textOutput("county_all_claims", inline = TRUE, container=span)),
                                        p(style="text-align:center;font-size:.5em;font-weight:600;color:#DBDCDD;",
                                          paste0("Total Since: Mar 21, 2020")),
                                        p(class="cnty_latest_date",paste0("Latest Data: ", format(twc_claims_date$date, format="%b %d"))),
                                        p(style="text-align:center;font-size:.4em;font-weight:400",
                                          tags$a(href="https://www.twc.texas.gov/news/unemployment-claims-numbers#claimsByCounty","Source: Texas Workforce Commission")))),
                              column(width = 8, class="economic-grid",
                                     gt_output(outputId = "county_claims_table"))
                            )),
                     column(width=6,
                            highchartOutput("cnty_claims_hchart", height = 285))),
            
            # ~~Trends Over Time ----------------------------------------------------
            
            h3(class="covid-topic", "Trends Over Time"),
            fluidRow(
              column(width=12,
                     highchartOutput("cnty_new_cases_hchart", height = 350))),
            fluidRow(
              column(width=6,
                     highchartOutput("cnty_curves_hchart", height = 350)),
              column(width=6,
                     highchartOutput("cnty_new_deaths_hchart", height = 350))
            ),
            

# ~~ Hospital Capacity Trends ---------------------------------------------

            
            h3(class="covid-topic", "Hospital Capacity Trends"),
column(width = 12,
       tags$div(class="tsa-paragraph",
                 br(),
                 h4(style="font-weight:700;display:inline;","IMPORTANT NOTE:"),
                 h4(style="font-weight:400;display:inline;","As described above, county-level hospital capacity data is represented at the Trauma Service Area level instead of the county. Trauma Service Areas are geographical clusters defined by where people are most likely to receive trauma care when they need it. For COVID-19 patients, it's the area where they are most likely to receive care. The trend data shown below represents data for this county's assigned Trauma Service Area."),
                 ),
       br(),
       hr()
       ),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_total_conf_covid_gen_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_total_conf_covid_icu_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_daily_covid_gen_admits_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_daily_covid_icu_admits_hchart", height = 350))
            ),            
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_daily_covid_ers_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_daily_covid_of_total_er_hchart", height = 350))
            ),
  
            # ~~Mobility Trends Data --------------------------------------------------
            
            h3(class="covid-topic", "Mobility Trends", span="id='cnty_mob"),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_grocery_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_parks_hchart", height = 350))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_transit_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_retail_rec_hchart", height = 350))
            ),
            
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_workplace_hchart", height = 350)),
              column(width = 6,
                     highchartOutput("cnty_residential_hchart", height = 350))
            )
    ),
    
    # **CREDITS PAGE ----------------------------------------------------------
    
    tabItem(tabName = "credits",
            HTML("<iframe width='110%' height=1200vh' style='margin-left:-15px;border-top-width: 0px;border-right-width: 0px;border-bottom-width: 0px;border-left-width: 0px;' src='https://texas-2036.github.io/covid-pages/covid_credits_page.html'></iframe>")),
    
    # **DATA PAGE -------------------------------------------------------------
    
    tabItem(tabName = "data",
            HTML("<iframe width='110%' height=1200vh' style='margin-left:-15px;border-top-width: 0px;border-right-width: 0px;border-bottom-width: 0px;border-left-width: 0px;' src='https://texas-2036.github.io/covid-pages/covid_data_page.html'></iframe>"))
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
  
  Sys.sleep(3) # do something that takes time
  waiter_hide()
  
  # Waiter + Waitress Functions ---------------------------------------------
  
  w <- Waiter$new(html = spin_facebook(), 
                  id = c("tx_cases","state_growth_rate_hchart", "cli_hchart", "ili_hchart", "state_curves_hchart",
                         "state_new_cases_hchart", "state_new_deaths_hchart", "state_new_tests_hchart",
                         "state_cases_race", "state_fatalities_race", "state_cases_age",
                         "state_fatalities_age", "state_cases_gender", "state_fatalities_gender",
                         "state_claims_hchart", "state_businesses_hchart", "state_hours_hchart", 
                         "state_employees_hchart", "state_grocery_hchart", "state_parks_hchart",
                         "state_transit_hchart", "state_retail_rec_hchart", "state_workplace_hchart",
                         "state_residential_hchart", "cnty_new_deaths_hchart", "cnty_new_cases_hchart", 
                         "daily_covid_ers_hchart","daily_covid_of_total_er_hchart",
                         "daily_covid_gen_admits_hchart","daily_covid_icu_admits_hchart",
                         "total_conf_covid_gen_hchart","total_conf_covid_icu_hchart",
                         "cnty_daily_covid_of_total_er_hchart","cnty_daily_covid_ers_hchart",
                         "cnty_daily_covid_gen_admits_hchart","cnty_daily_covid_icu_admits_hchart",
                         "cnty_total_conf_covid_gen_hchart","cnty_total_conf_covid_icu_hchart",
                         "cnty_curves_hchart", "cnty_grocery_hchart", "cnty_parks_hchart",
                         "cnty_transit_hchart", "cnty_retail_rec_hchart", "cnty_workplace_hchart", 
                         "cnty_residential_hchart","cnty_claims_hchart"))
  
  dataset <- reactive({
    input$draw
    
    w$show()
    
    Sys.sleep(3)
    
    # head(cars)
  })
  
  # App Disconnect Dialogue ----------------------------------------------------------
  
  sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
  
  # Tab Switching Functions -------------------------------------------------
  
  observeEvent(input$explore_reopen, {
    
    
    updateTabItems(session, "tabs", "reopening")
    
  })
  
  observeEvent(input$explore_state, {
    
    
    updateTabItems(session, "tabs", "state_profiles")
    
  })
  
  observeEvent(input$explore_county, {
    
    updateTabItems(session, "tabs", "county_profiles")    
    
    Sys.sleep(3) # do something that takes time
    waiter_hide()
    
    
  })
  
  options(digits.secs = 0) # Include milliseconds in time display
  
  
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)
  
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
        footer = HTML("<a href='http://www.texas2036.org//'>© Texas 2036</a>"),
        includeHTML("markdown/sidebar/signup.html"),
        easyClose = TRUE
      ))
  })
  
  # MODAL - About -----------------------------------------------------------
  
  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = "About Texas 2036",
        size = "l",
        footer = HTML("<a href='http://www.texas2036.org//'>© Texas 2036</a>"),
        includeMarkdown("markdown/sidebar/learnmore.md"),
        easyClose = TRUE
      ))
  })
  
  # INFO BOXES - STATEWIDE--------------------------------------------------------------
  
  # --{InfoBox - PH - Total Cases} -----------------------------------------------
  
  output$tx_cases <- renderInfoBox({
    
    infoBox(
      value=paste0(tx_cases$confirmed), title="Total Cases",
      subtitle=paste0(tx_cases$cases_rank, " Most in US"),
      icon = icon("chart-line"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  # {InfoBox  - PH - Deaths} -----------------------------------------------
  
  output$tx_mort <- renderInfoBox({
    
    infoBox(
      title="Deaths (% of All Cases)", value=paste0(tx_mort$mortality_rate, "%"),
      subtitle=paste0(tx_mort$mortality_rank, " Most in US"),
      icon = icon("virus"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  
  # {InfoBox - PH - Recovered} -----------------------------------------------
  
  output$tx_recover <- renderInfoBox({
    
    infoBox(
      title="Est. Recovered", value=paste0(tx_recover$recovered),
      subtitle=paste0(tx_recover$recovered_rank, " Most in US"),
      icon = icon("hand-holding-medical"), color = "navy", href=NULL
    )
  })
  
  # {InfoBox - PH - Active} -----------------------------------------------
  
  output$tx_active <- renderInfoBox({
    
    infoBox(
      title="Est. Active", value=paste0(tx_active$active),
      subtitle=paste0(tx_active$active_rank, " Most in US"),
      icon = icon("lungs-virus"), color = "navy", href=NULL
    )
  })
  
  
  # {InfoBox - HC - Hospitalization Rate - State} -----------------------------------------------
  
  output$hospitalized_rate <- renderInfoBox({
    
    dataset()
    
    tot_pos <- hosp_rate
    
    infoBox(
      title="% Hospitalized", 
      value=paste0(tot_pos$hospitalization_rate, "%"),
      subtitle="% of Active Cases",
      icon = icon("hospital-user"), color = "navy", href=NULL
    )
  })
  
  # {InfoBox - HC - Bed Availability - State}-------------------------------------------------
  
  output$tx_beds <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_hosp_data_ts %>%
      as_tibble() %>% 
      filter(date==max(date)) %>% 
      select(tsa,tsa_counties,bed_avail_rate) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      mutate_at(vars(bed_avail_rate),scales::percent)
    
    
    infoBox(
      title="All Beds Availability", value=paste0(tot_pos$bed_avail_rate), icon=icon("bed"),
      subtitle="of All Beds Available",
      color = "navy", href=NULL
    )
  })
  
  # {InfoBox - HC - ICU Beds Availability - State}-------------------------------------------------
  
  output$tx_icu_beds <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_hosp_data_ts %>%
      as_tibble() %>%
      filter(date==max(date)) %>% 
      select(tsa,tsa_counties,icu_avail_rate) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      mutate_at(vars(icu_avail_rate),scales::percent)
    
    infoBox(
      title="ICU Beds Availability", 
      value=paste0(tot_pos$icu_avail_rate),
      icon=icon("procedures"),
      subtitle="of ICU Beds Available",
      color = "navy", href=NULL)
  })
  
  # {InfoBox - HC - Ventilators Availability - State}-------------------------------------------------
  
  output$tx_vents <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_hosp_data_ts %>%
      filter(date==max(date)) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      select(tsa,tsa_counties, vent_avail_rate) %>% 
      mutate_at(vars(vent_avail_rate), scales::percent_format(accuracy=.1))
    
    
    infoBox(
      title="Ventilator Availability", value=paste0(tot_pos$vent_avail_rate), icon=icon("lungs"),
      subtitle="of Ventilators Available",
      color = "navy", href=NULL)
  })
  
  # CHARTS - STATE ----------------------------------------------------------
  
  # {State Growth Rate Charts}  --------------------------------------------------
  
  output$state_growth_rate_hchart <- renderHighchart({
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    state_case_growth %>% 
      hchart("column", hcaes(x = date, y = daily_growth_rate), animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(state_case_growth, type = "area", hcaes(x = date, y = daily_growth_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.daily_growth_rate_7day_avg:,.0f}%"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Case Growth Rate, By Day",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Last 30-Days of Data Shown Here | LEGEND - <span style='color: #FFD100'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text = "Growth Rate From Previous Day"),
               min = round(mean(state_case_growth$min_new), 2), 
               max = 8,
               format = "{value}%") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Growth Rate: {point.y:.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: New York Times County-Level COVID-19 Data (Github)",
        href = NULL) %>%
      hc_add_theme(tx2036_hc)
    
    
  })
  
  # {State CLI Charts}  --------------------------------------------------
  
  output$cli_hchart <- renderHighchart({
    
    dataset()
    
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
               max = 1000) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported CLI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  # {State ILI Charts}  --------------------------------------------------
  
  output$ili_hchart <- renderHighchart({
    
    dataset()
    
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
               max = 1000) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported ILI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  # {State Curve Charts}  --------------------------------------------------
  
  output$state_curves_hchart <- renderHighchart({
    
    dataset()
    
    nyt_tx_hchart <- nyt_state_cases_tx %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16")) %>% 
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
      hc_add_theme(tx2036_hc)
    
    nyt_tx_hchart
    
  })
  
  # {State Race - Cases}  --------------------------------------------------
  
  output$state_cases_race <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="cases",
             demographic_type=="race") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Cases, by Race",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Race")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list("#FEDA26","#CCB233", "#CCBE7A", "#D9D2AD", "#6CB6D9", "#3091BF","#0072A6","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State Race - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_race <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="fatalities",
             demographic_type=="race") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Race",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Race")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list("#FEDA26","#CCB233", "#CCBE7A", "#D9D2AD", "#6CB6D9", "#3091BF","#0072A6","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State Age - Cases}  --------------------------------------------------
  
  output$state_cases_age <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="cases",
             demographic_type=="ages") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Cases, by Age",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Age")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      # hc_colors(colors = list("#002D74","#2A7DE1","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State Age - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_age <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="fatalities",
             demographic_type=="ages") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Age",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Age")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      # hc_colors(colors = list("#002D74","#2A7DE1","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State Gender - Cases}  --------------------------------------------------
  
  output$state_cases_gender <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="cases",
             demographic_type=="gender") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Cases, by Gender",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Gender")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list("#002D74","#2A7DE1","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State Gender - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_gender <- renderHighchart({
    
    dataset()
    
    dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"),
             report_type=="fatalities",
             demographic_type=="gender") %>% 
      hchart("area", hcaes(x = date, y = pct, group=group, colors=group), animation=FALSE,
              tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.0f}%")) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Gender",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Gender")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list("#002D74","#2A7DE1","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State New Cases Charts}  --------------------------------------------------
  
  output$state_new_cases_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    nyt_tx_new_cases_hchart <- nyt_state_cases_tx %>% 
      mutate(daily_growth_rate = (new_cases_1day/lag(new_cases_1day))) %>%
      mutate(daily_growth_rate_7day_avg = rollmean(new_cases_1day, 7, 
                                                   fill=0, align = "right")) %>% 
      mutate(min_new = min(new_cases_1day, na.rm = TRUE),
             max_new = max(new_cases_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
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
        text ="LEGEND - <span style='color: #FFD100'>7-DAY ROLLING AVG.</span>",
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
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  # {State New Deaths  Charts} ----------------------------------------------
  
  output$state_new_deaths_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    nyt_tx_new_cases_hchart <- nyt_state_cases_tx %>% 
      mutate(daily_death_growth_rate_7day_avg = rollmean(new_deaths_1day, 7, 
                                                         fill=0, align = "right")) %>% 
      mutate(min_new = min(new_deaths_1day, na.rm = TRUE),
             max_new = max(new_deaths_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
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
        text ="LEGEND - <span style='color: #FFD100'>7-DAY ROLLING AVG.</span>",
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
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  # {State New Tests Chart} -------------------------------------------------
  
  
  output$state_new_tests_hchart <- renderHighchart({
    
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
      ungroup() %>% 
      filter(date >= as.Date("2020-03-16"))
    
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
        href = "https://github.com/COVID19Tracking/covid-tracking-data") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  
  
  
  # {State Jobless Claims Chart} -------------------------------------------------
  
  
  output$state_claims_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    tx_series_all %>% 
      hchart("column", hcaes(x = date, y = value), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth=10)) %>% 
      hc_title(
        text ="Texas Jobless Claims",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Claims Filed Weekly")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Weekly Claims Filed: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: The Federal Reserve Bank of St. Louis + Department of Labor",
        href = "https://fred.stlouisfed.org/series/TXICLAIMS") %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  # {State New COVID-Related ER Visits}  --------------------------------------------------
  
  output$daily_covid_ers_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(min_new = min(covid19_er_visits_24h, na.rm = TRUE),
             max_new = max(covid19_er_visits_24h, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date)
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid19_er_visits_24h), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_er_visits_24h_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_er_visits_24h_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="COVID-19-Related Daily ER Visits",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this shows the total number of ER visits in the last 24 hours for suspected COVID-19 related illness <br><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="New Visits Per Day"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value:,.0f}") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Visits: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  # {State COVID-Related ER Visits of All ER Visits Charts} ----------------------------------------------
  
  output$daily_covid_of_total_er_hchart <- renderHighchart({

    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(covid_share_of_new_er_visits=round(covid_share_of_new_er_visits*100, digits=1),
             covid_share_of_new_er_visits_7day_avg=round(covid_share_of_new_er_visits_7day_avg*100, digits=1)) %>% 
      mutate(min_new = min(covid_share_of_new_er_visits, na.rm = TRUE),
             max_new = max(covid_share_of_new_er_visits, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid_share_of_new_er_visits), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid_share_of_new_er_visits_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid_share_of_new_er_visits_7day_avg}%"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="COVID-19-Related ER Visits As % of Total New ER Visits",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this shows the total number of ER visits in the last 24 hours for suspected COVID-19 related illness as a share of all ER visits that day.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Daily COVID-19 Share of All ER Visits"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}%") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Share: {point.y}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  # {State New COVID-19-Related General Bed Admits} -------------------------------------------------

  output$daily_covid_gen_admits_hchart <- renderHighchart({

    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(covid_share_of_new_er_visits=covid19_admitted_gen_24h,
             covid_share_of_new_er_visits_7day_avg=covid19_admitted_gen_24h_7day_avg) %>% 
      mutate(min_new = min(covid19_admitted_gen_24h, na.rm = TRUE),
             max_new = max(covid19_admitted_gen_24h, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid19_admitted_gen_24h), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_gen_24h_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_gen_24h_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Daily COVID-19-Related Admits to General Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to a MedSurg (General) or isolation bed in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Daily Admits to Gen. Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)

  })
  
  # {State New COVID-19-Related ICU Bed Admits} -------------------------------------------------
  
  output$daily_covid_icu_admits_hchart <- renderHighchart({

    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(covid_share_of_new_er_visits=covid19_admitted_icu_24h,
             covid_share_of_new_er_visits_7day_avg=covid19_admitted_icu_24h_7day_avg) %>% 
      mutate(min_new = min(covid19_admitted_icu_24h, na.rm = TRUE),
             max_new = max(covid19_admitted_icu_24h, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid19_admitted_icu_24h), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_icu_24h_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_icu_24h_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Daily COVID-19-Related Admits to ICU Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to adult or pediatric ICU beds in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Daily Admits to ICU Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)

  })
  
  # {State Total Confirmed COVID-19 General Bed Admits} -------------------------------------------------
  
  output$total_conf_covid_gen_hchart <- renderHighchart({

    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(covid_share_of_new_er_visits=lab_con_covid19_gen,
             covid_share_of_new_er_visits_7day_avg=lab_con_covid_gen_tot_7day_avg) %>% 
      mutate(min_new = min(lab_con_covid19_gen, na.rm = TRUE),
             max_new = max(lab_con_covid19_gen, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = lab_con_covid19_gen), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = lab_con_covid_gen_tot_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.lab_con_covid_gen_tot_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Total Confirmed COVID-19 in General Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of lab confirmed COVID-19 patients admitted to MedSurg (general) or isolation beds at time of report. It's important to note these patients are <i>not</i> suspected of having COVID-19. They are lab confirmed.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Total COVID-19 in Gen Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Total Admits : {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)

  })
  
  # {State Total Confirmed COVID-19 ICU Bed Admits} -------------------------------------------------
  
  output$total_conf_covid_icu_hchart <- renderHighchart({

    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      mutate(covid_share_of_new_er_visits=lab_con_covid19_icu,
             covid_share_of_new_er_visits_7day_avg=lab_con_covid_icu_tot_7day_avg) %>% 
      mutate(min_new = min(lab_con_covid19_icu, na.rm = TRUE),
             max_new = max(lab_con_covid19_icu, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = lab_con_covid19_icu), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = lab_con_covid_icu_tot_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.lab_con_covid_icu_tot_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Total Confirmed COVID-19 in ICU Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of lab confirmed COVID-19 patients admitted to adult or pediatric ICU beds at time of report. It's important to note these patients are <i>not</i> suspected of having COVID-19. They are lab confirmed.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Total COVID-19 in ICU Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc)

  })
  
  # {State Google Mobility - Grocery Chart} -------------------------------------------------
  
  
  output$state_grocery_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Grocery & Pharmacy Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-shopping-basket mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  
  
  # {State Google Mobility - Parks Chart} -------------------------------------------------
  
  
  output$state_parks_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = parks_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Parks Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-tree mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  
  
  # {State Google Mobility - Transit Chart} -------------------------------------------------
  
  
  output$state_transit_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = transit_stations_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Transit Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-bus-alt mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
    
    
  })
  
  
  
  
  # {State Google Mobility - Retail/Rec Chart} -------------------------------------------------
  
  
  output$state_retail_rec_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = retail_and_recreation_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Retail & Recreation Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-shopping-bag mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  
  
  # {State Google Mobility - Workplaces Chart} -------------------------------------------------
  
  
  output$state_workplace_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = workplaces_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Workplace Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-building mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
    
  })
  
  
  
  
  # {State Google Mobility - Residential Chart} -------------------------------------------------
  
  
  output$state_residential_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    google_mobil_tx %>% 
      hchart("area", hcaes(x = date, y = residential_percent_change_from_baseline), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Residential Mobility Trends",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<i class='fas fa-home mobility-icon'></i>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  
  
  
  
  
  
  # {State Businesses Open Chart} -------------------------------------------------
  
  
  output$state_businesses_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    hb_businesses_open_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Est. Change in Businesses Open",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Business Open")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Businesses Open: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = "https://joinhomebase.com/data/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  
  
  
  # {State Hours Worked} -------------------------------------------------
  
  
  output$state_hours_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    hb_hours_worked_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Est. Change in Hours Worked By Hourly Employees",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Hours Worked")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Hours Worked: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = "https://joinhomebase.com/data/") %>%
      hc_add_theme(tx2036_hc)
    
  })
  
  
  # {State Employees Working} -------------------------------------------------
  
  
  output$state_employees_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    hb_employees_working_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text = "Est. Change in Number of Hourly Employees Working",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Employees Working")) %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Employees Working: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Homebase",
        href = "https://joinhomebase.com/data/") %>%
      hc_add_theme(tx2036_hc)
    
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
                    fill = FALSE,
                    # fillColor = "#2d2d2d",
                    # fillOpacity = .5,
                    stroke = TRUE,
                    weight = 8,
                    highlightOptions = highlightOptions(bringToFront = FALSE),
                    color = '#5d5d5d', 
                    opacity = .8, 
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
    
    output$county_name2 <- renderText({
      
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
      
      tsa_date <- dshs_tsa_24hr_data %>%
        as_tibble() %>%
        filter(date==max(date)) %>% 
        distinct(date)
      
      paste0("On ",format(tsa_date$date, format="%b %d"),",")
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
      
      dshs_county_active %>%
        filter(county==input$countyname,
               date==max(date)) %>% 
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
        round(digits=1) %>% 
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
        mutate_at(vars(mort_rate),scales::percent_format(accuracy=.01)) %>% 
        distinct(mort_rate) %>% 
        as.character()
    })
    
    # County Testing Rate -----------------------------------------------------
    
    output$county_test_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      total_cnty_tests %>%
        filter(county_name==input$countyname) %>% 
        mutate_at(vars(tests_per_100k), scales::comma) %>% 
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
    
    # County UI Claims --------------------------------------------------------
    
    output$county_all_claims <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      twc_claims_cnty_summ %>%
        filter(county==input$countyname) %>%
        distinct(all_claims) %>% 
        as.character()
    })
    
    # County Top 5 Industries -------------------------------------------------
    
    output$county_claims_table <- gt::render_gt({
      
      req(input$countyname)
      
      gt_tbl <-  twc_ui_by_county %>%
        filter(county==input$countyname) %>%
        # filter(county=="Harris") %>%
        gt() %>%
        tab_header(title = paste0("Top Claims in ", input$countyname, " County")) %>%
        cols_hide(contains("county")) %>% 
        cols_label(rank=md("**Rank**"),
                   naics_title=md("**Industry**"),
                   pct_label=md("**% of All Claims**")) %>% 
        cols_align(align="center",
                   columns=vars(rank, pct_label)) %>% 
        tab_options(table.background.color = "#002D74",
                    table.font.color = "#fff",
                    table.border.top.color = "#002D74",
                    table.border.left.color = "#002D74",
                    table.border.right.color = "#002D74",
                    table.border.bottom.color = "#002D74",
                    table_body.border.bottom.color = "#002D74")
      
      gt_tbl
    })
    
    
    output$county_ui_1 <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      twc_ui_by_county %>%
        filter(county==input$countyname) %>% 
        distinct(naics_title1) %>% 
        as.character()
    })
    
    output$county_ui_2 <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      twc_ui_by_county %>%
        filter(county==input$countyname) %>% 
        distinct(naics_title2) %>% 
        as.character()
    })
    
    output$county_ui_3 <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      twc_ui_by_county %>%
        filter(county==input$countyname) %>% 
        distinct(naics_title3) %>% 
        as.character()
    })
    
    # TSA Beds Availability -----------------------------------------------------
    
    output$cnty_beds <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data_ts %>%
        filter(date==max(date)) %>% 
        as_tibble() %>% 
        select(tsa,tsa_counties,bed_avail_rate) %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate_at(vars(bed_avail_rate), scales::percent_format(accuracy=.1)) %>% 
        distinct(bed_avail_rate) %>% 
        as.character()
    })
    
    # TSA ICU Beds Availability -----------------------------------------------------
    
    output$cnty_icu_beds <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data_ts %>%
        filter(date==max(date)) %>% 
        as_tibble() %>%
        select(tsa,tsa_counties,icu_avail_rate) %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate_at(vars(icu_avail_rate), scales::percent_format(accuracy=.1)) %>% 
        distinct(icu_avail_rate) %>% 
        as.character()
      
    })
    
    # TSA Ventilator Availability -----------------------------------------------------
    
    output$cnty_vents <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data_ts %>% 
        filter(date==max(date)) %>% 
        as_tibble() %>% 
        select(tsa,tsa_counties,vent_avail_rate) %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate_at(vars(vent_avail_rate), scales::percent_format(accuracy=.1)) %>% 
        distinct(vent_avail_rate) %>% 
        as.character()
      
    })
    
    # TSA Hosp Covid ER Visits -----------------------------------------------------
    
    output$hosp_covid_er_visits <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        filter(date==max(date)) %>% 
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        mutate_at(vars(covid19_er_visits_24h), scales::comma) %>% 
        distinct(covid19_er_visits_24h) %>% 
        as.character()
    })
    
    # TSA Suspected COVID Patients -----------------------------------------------------
    
    output$hosp_susp_covid <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_hosp_data %>%
        filter(date==max(date)) %>%
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
        filter(date==max(date)) %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        distinct(lab_con_covid19_gen) %>% 
        as.character()
    })
    
    # CHARTS - COUNTY------------------------------------------------------------------
    
    # {TSA New COVID-Related ER Visits}  --------------------------------------------------
    
    output$cnty_daily_covid_ers_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>% 
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate(min_new = min(covid19_er_visits_24h, na.rm = TRUE),
               max_new = max(covid19_er_visits_24h, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date)
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = covid19_er_visits_24h), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_er_visits_24h_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_er_visits_24h_7day_avg:,.0f}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | COVID-19-Related Daily ER Visits"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this shows the total number of ER visits in the last 24 hours for suspected COVID-19 related illness.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="New Visits Per Day"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value:,.0f}") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   New Visits: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    
    # {TSA COVID-Related ER Visits of All ER Visits Charts} ----------------------------------------------
    
    output$cnty_daily_covid_of_total_er_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate(covid_share_of_new_er_visits=round(covid_share_of_new_er_visits*100, digits=1),
               covid_share_of_new_er_visits_7day_avg=round(covid_share_of_new_er_visits_7day_avg*100, digits=1)) %>% 
        mutate(min_new = min(covid_share_of_new_er_visits, na.rm = TRUE),
               max_new = max(covid_share_of_new_er_visits, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = covid_share_of_new_er_visits), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid_share_of_new_er_visits_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid_share_of_new_er_visits_7day_avg}%"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | COVID-19-Related ER Visits (% of All Visits)"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this shows the total number of ER visits in the last 24 hours for suspected COVID-19 related illness as a share of all ER visits that day.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Daily COVID-19 Share of All ER Visits"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value}%") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Today's Share: {point.y}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
      
    })
    
    # {TSA New COVID-19-Related General Bed Admits} -------------------------------------------------
    
    output$cnty_daily_covid_gen_admits_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate(covid_share_of_new_er_visits=covid19_admitted_gen_24h,
               covid_share_of_new_er_visits_7day_avg=covid19_admitted_gen_24h_7day_avg) %>% 
        mutate(min_new = min(covid19_admitted_gen_24h, na.rm = TRUE),
               max_new = max(covid19_admitted_gen_24h, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = covid19_admitted_gen_24h), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_gen_24h_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_gen_24h_7day_avg}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Daily COVID-19-Related Admits to General Beds"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to a MedSurg (General) or isolation bed in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Daily Admits to Gen. Beds"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value}") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    # {TSA New COVID-19-Related ICU Bed Admits} -------------------------------------------------
    
    output$cnty_daily_covid_icu_admits_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
        as_tibble() %>%
        filter(tsa_counties==input$countyname) %>%
        mutate(covid_share_of_new_er_visits=covid19_admitted_icu_24h,
               covid_share_of_new_er_visits_7day_avg=covid19_admitted_icu_24h_7day_avg) %>% 
        mutate(min_new = min(covid19_admitted_icu_24h, na.rm = TRUE),
               max_new = max(covid19_admitted_icu_24h, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = covid19_admitted_icu_24h), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_icu_24h_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_icu_24h_7day_avg}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Daily COVID-19-Related Admits to ICU Beds"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to adult or pediatric ICU beds in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Daily Admits to ICU Beds"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value}") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    # {TSA Total Confirmed COVID-19 General Bed Admits} -------------------------------------------------
    
    output$cnty_total_conf_covid_gen_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>%
        mutate(covid_share_of_new_er_visits=lab_con_covid19_gen,
               covid_share_of_new_er_visits_7day_avg=lab_con_covid_gen_tot_7day_avg) %>% 
        mutate(min_new = min(lab_con_covid19_gen, na.rm = TRUE),
               max_new = max(lab_con_covid19_gen, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = lab_con_covid19_gen), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = lab_con_covid_gen_tot_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.lab_con_covid_gen_tot_7day_avg}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Total Confirmed COVID-19 in General Beds"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this chart shows trends for the daily total of lab confirmed COVID-19 patients admitted to MedSurg (general) or isolation beds at time of report. It's important to note these patients are <i>not</i> suspected of having COVID-19. They are lab confirmed.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Total COVID-19 in Gen Beds"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value}") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Total Admits : {point.y}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    # {TSA Total Confirmed COVID-19 ICU Bed Admits} -------------------------------------------------
    
    output$cnty_total_conf_covid_icu_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        mutate(covid_share_of_new_er_visits=lab_con_covid19_icu,
               covid_share_of_new_er_visits_7day_avg=lab_con_covid_icu_tot_7day_avg) %>% 
        mutate(min_new = min(lab_con_covid19_icu, na.rm = TRUE),
               max_new = max(lab_con_covid19_icu, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      dshs_tsa_24hr_data_hchart %>% 
        hchart("column", hcaes(x = date, y = lab_con_covid19_icu), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = lab_con_covid_icu_tot_7day_avg),
                      tooltip = list(pointFormat = "<br>7-Day Avg.: {point.lab_con_covid_icu_tot_7day_avg}"),
                      color = "#FFD100", name="7-Day Avg.") %>%
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Total Confirmed COVID-19 in ICU Beds"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this chart shows trends for the daily total of lab confirmed COVID-19 patients admitted to adult or pediatric ICU beds at time of report. It's important to note these patients are <i>not</i> suspected of having COVID-19. They are lab confirmed.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Total COVID-19 in ICU Beds"),
                 min = mean(dshs_tsa_24hr_data_hchart$min_new),
                 max = mean(dshs_tsa_24hr_data_hchart$max_new),
                 format = "{value}") %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    
    # {County Google Mobility - Grocery Chart} -------------------------------------------------
    
    output$cnty_grocery_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Grocery & Pharmacy Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-shopping-basket mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
      
      
    })
    
    # {County Google Mobility - Parks Chart} -------------------------------------------------
    
    
    output$cnty_parks_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = parks_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Parks Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-tree mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    
    
    
    # {County Google Mobility - Transit Chart} -------------------------------------------------
    
    
    output$cnty_transit_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = transit_stations_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Transit Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-bus-alt mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
    
      
    })
    
    
    
    
    # {County Google Mobility - Retail/Rec Chart} -------------------------------------------------
    
    
    output$cnty_retail_rec_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = retail_and_recreation_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Retail & Recreation Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-shopping-bag mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
      
      
    })
    
    
    
    
    # {County Google Mobility - Workplaces Chart} -------------------------------------------------
    
    
    output$cnty_workplace_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = workplaces_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Workplace Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-building mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
      
      
    })
    
    
    
    
    # {County Google Mobility - Residential Chart} -------------------------------------------------
    
    
    output$cnty_residential_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      google_mobil_tx_cnties %>% 
        filter(sub_region_2==input$countyname) %>% 
        hchart("area", hcaes(x = date, y = residential_percent_change_from_baseline), 
               animation=FALSE,
               color = "#FFD100") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_title(
          text = paste0(input$countyname," County | Residential Mobility Trends"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="<i class='fas fa-home mobility-icon'></i>",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% Change in Mobility")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Google COVID-19 Community Mobility Reports",
          href = "https://www.google.com/covid19/mobility/") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    
    
    
    
    
    # {County Jobless Claims Chart} -------------------------------------------
    
    output$cnty_claims_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      twc_claims_cnty %>% 
        filter(county==input$countyname) %>%
        # filter(county=="Dallas") %>% 
        hchart("column", hcaes(x = date, y = value), 
               animation=FALSE,
               color = "#fff") %>% 
        hc_plotOptions(column = list(pointWidth=10)) %>% 
        hc_title(
          text =paste0(input$countyname," County | Jobless Claims"),
          # text = "Dallas County | Jobless Claims",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="Claims Filed Weekly")) %>% 
        hc_xAxis(title=NULL) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Weekly Claims Filed: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: The Texas Workforce Commission",
          href = "https://www.twc.texas.gov/news/unemployment-claims-numbers#claimsByCounty") %>%
        hc_add_theme(tx2036_hc)
      
    })
    
    
    # {County Curve Charts}  --------------------------------------------------
    
    output$cnty_curves_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      nyt_county_cases_chart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>% 
        mutate(min_new = min(cases, na.rm = TRUE),
               max_new = max(cases, na.rm = TRUE)) %>% 
        mutate(date = ymd(date)) %>% 
        arrange(date) %>% 
        filter(date >= as.Date("2020-03-16"))
      
      nyt_county_cases_chart %>% 
        hchart("area", hcaes(x = date, y = cases), animation=FALSE,
               color = "#fff") %>% 
        hc_title(
          text = paste0(input$countyname, " County | COVID-19 Cases"),
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
        hc_add_theme(tx2036_hc)
      
    })
    
    # {County New Cases Charts}  --------------------------------------------------
    
    output$cnty_new_cases_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
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
        ungroup() %>% 
        filter(date >= as.Date("2020-03-16"))
      
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
        hc_add_theme(tx2036_hc)
      
    })
    
    
    # {County New Deaths  Charts} ----------------------------------------------
    
    output$cnty_new_deaths_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dataset()
      
      nyt_cnty_new_deaths_hchart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>%
        arrange(date) %>% 
        mutate(daily_death_growth_rate_7day_avg = rollmean(new_deaths_1day, 7, 
                                                           fill=0, align = "right")) %>% 
        mutate(min_new = min(new_deaths_1day, na.rm = TRUE),
               max_new = max(new_deaths_1day, na.rm = TRUE)) %>% 
        mutate(min_new = as.numeric(min_new),
               max_new = as.numeric(max_new)) %>% 
        ungroup() %>% 
        filter(date >= as.Date("2020-03-16"))
      
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
        hc_add_theme(tx2036_hc)
      
    })
    
    # COUNTY MAP --------------------------------------------------------------
    
    pal <- colorNumeric(palette = "Reds", na.color = "#DBDCDD", 
                        domain = tx_county_sf$incident_rate)
    
    
    labels_clean <- sprintf("<a style = 'font-family: Montserrat; font-size: 22px; font-weight: 700; color:#3a4a9f'>%s County</a></br><a style = 'font-family: Montserrat; font-size: 16px; font-weight: 400; color:#6B6D6F'>%s Active Cases</a><br/><a style = 'font-family: Montserrat; font-size: 12px; font-weight: 400; color:#8C8F93'>%s Total Confirmed Cases</a>",
                            tx_county_sf$county,
                            tx_county_sf$active_label,
                            tx_county_sf$cases_label) %>%
      lapply(htmltools::HTML)
    
    map <- leaflet(tx_county_sf, width = "100%", height = "600px", 
                   options = leafletOptions(zoomControl = TRUE, dragging=TRUE, 
                                            minZoom = 5, maxZoom = 7,
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
                       radius = ~sqrt((cases*.11)),
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
      hideGroup("Trauma Service Areas") %>% 
      leaflet::addLegend("bottomleft",
                         data = tx_county_sf,
                         pal = pal,
                         values = ~incident_rate,
                         title = "Cases<br>(Per 100k People)",
                         opacity = 1) %>%
      # setView(31.9686, -99.9018, zoom = 6) #%>%
      fitBounds(-106.64585, 25.83706, -93.50782, 36.50045) %>%
      suspendScroll(sleepNote = FALSE, hoverToWake = TRUE, sleepOpacity = 1) %>%
      addResetMapButton()
    
    map 
    
    
  })
  
}
shiny::shinyApp(ui, server)
