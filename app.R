#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# R Packages --------------------------------------------------------------

library(shinydashboard)
library(vroom)
library(shiny)
library(shinyLP)
library(readxl)
library(gt)
library(reactable)
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
library(scales)
library(shinyalert)

# Helper Functions --------------------------------------------------------

blank <- "https://api.mapbox.com/styles/v1/datatx2036/ckc2gkem302hd1io7aaw54a09/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGF0YXR4MjAzNiIsImEiOiJja2J4cmJkMWUwYWh1MnNwamQ1a3NxMDlnIn0.pz06mEDLOJOhh4EyGar6Lg"
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
                        lineColor = 'rgba(255,255,255,0.7)', 
                        minorGridLineColor = 'rgba(243,243,243,0.7)', 
                        tickColor = "#F3F3F3", 
                        tickWidth = 1), 
           yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                        title = list(style = list(color = "#fff", fontSize = "12px", 
                                                  color="#fff",fontWeight="500")), 
                        gridLineWidth = .5,
                        gridLineColor = 'rgba(243,243,243,0.15)', 
                        lineColor = 'rgba(255,255,255,0.15)', 
                        minorGridLineColor = 'rgba(243,243,243,0.15)', 
                        tickColor = "#F3F3F3", 
                        tickWidth = 2)))


sever_default <- function (title = "Whoops!", subtitle = "You have been disconnected", 
                           button = "Reload", button_class = "default") 
{
  tagList(tags$a(href='http://www.texas2036.org',
                 HTML('<svg viewBox="0 0 227.4 83.5" style="height:7vh;padding-bottom:1.1vh;margin-top:7px"><path fill="#fff" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
                 tags$title('Texas COVID-19 Resource Kit')),
          tags$h1(title), tags$p(subtitle), reload_button(button, class = button_class))
}

bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

rating_cols <- c("pct_of_all")


rating_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
def_rating_color <- make_color_pal(c("#AAB9D0", "#5572A2", "#002D74", "#001E4D", "#000F26"), bias = 1)
# knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)


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


# ~~Key Events Data -------------------------------------------------------

key_events <- vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2q_c5RpywszCamM3VINgAwZ51OJoPfBFflEvXpuAqAZrw9SDovcGnfDOlF7uwzCnZf5XMkEluhlUb/pub?output=csv") %>% 
  clean_names() %>% 
  mutate(date=as_date(date))


# ~~JHU Data --------------------------------------------------------------


jhu_cases_state_us <- vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_state.csv") %>% 
  janitor::clean_names() %>%
  filter(country_region=="US",
         !str_detect(province_state, "Diamond Princess|Grand Princess|Guam|Puerto Rico|Virgin Islands|Northern Mariana Islands|American Samoa")) %>%
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

r_naught <- vroom("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv") %>% 
  filter(region=="TX")

county_lat_long <- vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases.csv") %>%
  janitor::clean_names() %>% 
  filter(province_state=="Texas") %>%
  select(fips,county=admin2,last_update,lat,long) %>% 
  separate(last_update, into=c("date","time"), sep=" ") %>%
  mutate(fips=as.character(fips)) %>% 
  select(-time)

# ~~NYT Data --------------------------------------------------------------

nyt_county_cases <- vroom("clean_data/dshs/testing/county_table.csv") %>%
  rename(cases=total_confirmed_cases,
         deaths=total_fatalities) %>%
  mutate(date=gsub("2020-07-30","	2020-06-30",x=date)) %>% 
  mutate(min = min(cases),
         max = max(cases),
         incident_rate=round((cases/population)*100000, digits=2),
         mort_rate=round(deaths/cases, digits=2),
         tests_per_100k=round((total_tests/population)*100000, digits=2)) %>% 
  group_by(county) %>% 
  mutate(prev_day_cases = lag(cases,1),
         prev_week_cases = lag(cases,7),
         prev_day_deaths = lag(deaths,1),
         prev_week_deaths = lag(deaths,7),
         new_cases_1day = cases-prev_day_cases,
         new_cases_7day= cases-prev_week_cases,
         new_deaths_1day = deaths-prev_day_deaths,
         new_deaths_7day= deaths-prev_week_deaths) %>% 
  ungroup() %>% 
  mutate(date=as_date(date))

date1 <- nyt_county_cases %>% 
  filter(date==max(date)) 

date_filter <- date1$date - days(5)

if (is.na(date1$active_cases)) {
  
  nyt_county_cases_today <- nyt_county_cases %>%
    filter(date>=date_filter) %>% 
    group_by(county) %>% 
    fill(active_cases) %>% 
    ungroup() %>% 
    filter(date==max(date))
  
} else {

  nyt_county_cases_today <- date1
}

dshs_hosp_rate_ts <- vroom("clean_data/dshs/hospitals/hosp_rate_ts.csv") %>% 
  rename(hospitalizations=hospitalized)

dshs_state_trends <- vroom("clean_data/dshs/cases/state_test_trends.csv")

# nyt_state_cases_tx <- vroom("https://texas-2036.github.io/covid-data/state.csv") #%>%
nyt_state_cases_tx <- vroom("clean_data/dshs/cases/state_cases_trends.csv") %>%
  left_join(dshs_hosp_rate_ts,by=c("date","state","fips")) %>% 
  left_join(dshs_state_trends, by=c("date","state","fips")) %>% 
  # select(-x1) %>% 
  rename(cases=cumulative_cases,
         # daily_new_fatalities=fatalities_by_date_of_death,
         deaths=cumulative_fatalities) %>%
  mutate(deaths=replace(deaths,date=="2020-08-03",7261)) %>% #Adjusts for Aug 3 Absence in Reporting
  mutate(population=27885195,
         min = min(cases),
         max = max(cases),
         # total_tests=total_public_lab_tests+total_private_lab_tests,
         tests_per_100k=round((total_tests_reported/population)*100000, digits=2),
         incident_rate=round((cases/population)*100000, digits=2),
         mort_rate=round(deaths/cases, digits=4),
         # recovered=round(recovered, digits=0),
         test_rate=round((cases/population)*100000, digits=2),
         hosp_rate=round((hospitalizations/active)*100, digits=2),
         date = ymd(date),
         prev_day_deaths = lag(deaths,1),
         new_deaths_1day = deaths-prev_day_deaths,
         prev_day_cases = lag(cases,1),
         prev_week_cases = lag(cases,7),
         new_cases_1day = cases-prev_day_cases,
         new_cases_7day= cases-prev_week_cases) %>% 
  ungroup()

date2 <- nyt_state_cases_tx %>% 
  filter(date==max(date)) 

date_filter <- date2$date - days(60)

hosp_rate_hchart <- nyt_state_cases_tx %>% 
  arrange(date) %>%
  select(date, hospitalizations,active) %>% 
  mutate(state="TX") %>% 
  group_by(state) %>% 
  mutate(hosp_rate_7day_avg = rollmean(hospitalizations, 7, 
                                       fill=0, align = "right")) %>% 
  filter(date >= as.Date(date_filter)) %>% 
  # mutate(min_new = min(hosp_rate, na.rm = TRUE),
  #        max_new = max(hosp_rate, na.rm = TRUE)) %>% 
  ungroup()

hospt_rate_hchart <- nyt_state_cases_tx %>% 
  arrange(date) %>%
  select(date,hosp_rate) %>% 
  mutate(hosp_rate_7day_avg = rollmean(hosp_rate, 7, 
                                       fill=0, align = "right")) %>% 
  filter(date >= as.Date(date_filter),
         !is.na(hosp_rate)) %>% 
  mutate(min_new = min(hosp_rate, na.rm = TRUE),
         max_new = max(hosp_rate, na.rm = TRUE))

nyt_state_cases_text <- nyt_state_cases_tx %>%
  fill(c(active,recovered, mort_rate), .direction="down") %>% 
  filter(date==max(date)) %>% 
  select(cases,mort_rate,active,recovered) %>% 
  mutate_at(vars(cases,active,recovered), scales::comma) %>% 
  mutate_at(vars(mort_rate), scales::percent_format(scale=100,accuracy=.01))

tx_counties <- read_rds("clean_data/population/county_pop.rds") %>% 
  mutate(NAME=gsub(" County, Texas","",x=NAME)) %>% 
  left_join(county_lat_long, by=c("GEOID"="fips", "NAME"="county")) %>% 
  select(-date)

county_list <- nyt_county_cases_today %>% 
  as_tibble() %>% 
  select(`County Names`=county) %>%
  distinct() %>% 
  arrange(`County Names`) %>% 
  as.list()

tx_county_sf <- tx_counties %>%
  left_join(nyt_county_cases_today, by=c("NAME"="county")) %>% 
  st_as_sf() %>% 
  st_transform(crs="+init=epsg:4326") %>% 
  fill(c("date"),.direction="downup") %>% 
  # select(-state, -country_region, -time, -combined_key, -iso3) %>% 
  mutate(cases_label=case_when(
    is.na(cases) ~ "No Reported",
    is.numeric(cases) ~ scales::comma(cases)
  ),
  active_label=case_when(
    is.na(active_cases) ~ "No Reported",
    is.numeric(active_cases) ~ scales::comma(active_cases)
  )) %>% 
  rename(county=NAME,
         active=active_cases)

# ~~COVID Tracking Data --------------------------------------------------------------

test_daily <- vroom("https://api.covidtracking.com/v1/states/daily.csv") %>% 
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


# COVID Relief Data -------------------------------------------------------

covid_relief <- vroom("clean_data/covid_relief/crf_data.csv")


# ~~DSHS Data ----

dshs_state_demographics <- vroom("clean_data/dshs/cases/time_series_demographics.csv") %>% 
  mutate(fips=as.character(fips))

dshs_case_trends <- vroom("clean_data/dshs/cases/state_cases_trends.csv")

dshs_case_fatality_totals <- dshs_case_trends %>% 
  select(date,cases=cumulative_cases, fatalities=cumulative_fatalities) %>% 
  gather(report_type,totals, 2:3)

dshs_completed_investigation <- dshs_state_demographics %>% 
  group_by(date, report_type) %>% 
  summarise(completed_investigations=sum(count)) %>% 
  ungroup() %>% 
  left_join(dshs_case_fatality_totals, by=c("date","report_type")) %>% 
  mutate(completed_investigations=completed_investigations/3,
         diff=totals-completed_investigations) %>% 
  select(-totals)

dshs_new_hosp_data <- vroom("clean_data/dshs/hospitals/new_hosp_data_dshs.csv") %>% 
  clean_names() %>% 
  filter(date >= as_date("2020-07-25"))

dshs_tsa_hosp_data <- read_rds("clean_data/dshs/hospitals/texas_hosp_bed_ts_data.rds") %>% 
  st_transform(crs="+init=epsg:4326")

dshs_tsa_24hr_data <- vroom("clean_data/dshs/hospitals/texas_hosp_bed_ts_24hr_data.csv") %>% 
  ungroup() %>% 
  mutate(date=ymd(date),
         date=as.Date(date))

dshs_tsa_avail_latest <- dshs_tsa_24hr_data %>%
  filter(!is.na(icu_avail_rate)) %>% 
  filter(date==max(date)) %>% 
  as_tibble() %>%
  distinct(date)

dshs_tsa_num_latest <- dshs_tsa_24hr_data %>%
  filter(!is.na(total_susp_covid_in_hosp_at_time_of_report)) %>% 
  filter(date==max(date)) %>% 
  as_tibble() %>%
  distinct(date)

dshs_test_pos <- vroom("clean_data/dshs/testing/test_pos_tx_ts.csv")

dshs_syndromic_tx <- vroom("clean_data/dshs/syndromic_tx.csv") %>% 
  rename(influenza_like_illness=2,covid_like_illness=3)

# ** Population Data ---------------------------------------------------------

state_pop <- read_rds("clean_data/population/state_pop.rds")

tsa_shps <- dshs_tsa_hosp_data %>%
  filter(date==max(date)) %>% 
  filter(!str_detect(tsa,"Total|total")) %>%
  select(tsa,tsa_counties,geometry) %>% 
  group_by(tsa) %>% 
  summarise(tsa_counties = toString(tsa_counties)) %>%
  ungroup()

dshs_cases_text <- dshs_case_trends %>% 
  filter(date==max(date)) %>% 
  select(cumulative_cases) %>% 
  mutate_at(vars(cumulative_cases), scales::comma)

# ** NPI Data ----------------------------------------------------------------


# ~~Google Mobility -------------------------------------------------------

google_mobil_tx <- vroom("https://texas-2036.github.io/covid-data/google_mobility_tx_state.csv")
  
google_mobil_tx_cnties <- vroom("https://texas-2036.github.io/covid-data/google_mobility_tx_cnty.csv")

# ** Economic Data -----------------------------------------------------------


# ~~Affinity Spending Data --------------------------------------------------

dd <- vroom("clean_data/opportunity_tracker/affinity_tables.csv") 

# ~~Homebase Data ---------------------------------------------------------

hb_state_only <- vroom("https://texas-2036.github.io/covid-data/homebase_state_summary.csv", na = " ") %>% 
  filter(!is.na(prev_day)) %>% 
  mutate(change=as.numeric(change),
         change=round(change,digits=1)) %>% 
  mutate(prev_day=as_date(prev_day))

# hb_state_industry <- vroom("https://texas-2036.github.io/covid-data/homebase_state_industry_breakdown.csv")

hb_hours_worked_all <- hb_state_only %>%
  filter(variable=="hours_worked")

hb_businesses_open_all <- hb_state_only %>%
  filter(variable=="businesses_open")

hb_employees_working_all <- hb_state_only %>%
  filter(variable=="employees_working")

hb_hours_worked <- hb_state_only %>%
  filter(variable=="hours_worked",
         date == max(date))

hb_businesses_open <- hb_state_only %>%
  filter(variable=="businesses_open",
         date == max(date))

hb_employees_working <- hb_state_only %>%
  filter(variable=="employees_working",
         date == max(date))

# ~~TWC County UI Claims Data ------------------------------------------------------

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


twc_ui_top_25_tbl <- vroom("https://lmci.state.tx.us/shared/dashboarddata/UI_BY_NAICS_TOP25.csv") %>% 
  clean_names() %>% 
  mutate(pct_of_all = round(naics_count/texas_total_ui_count, digits = 4),
         index = 1:25) %>% 
  select(index, 1,2,4) 

# ~~FREDR Data -----------------------------------------------------

tx_series_all <- vroom("clean_data/fredr/ui_claims_ts.csv")

tx_cont_series <- vroom("clean_data/fredr/cont_claims_ts.csv", delim=",") %>% 
  mutate_at(vars(value),scales::comma) %>% 
  filter(date==max(date))

tx_series <- vroom("clean_data/fredr/tx_series_summ.csv", delim=",") %>% 
  mutate_at(vars(value),scales::comma)

tx_urn <- vroom("clean_data/fredr/unemploy_rate.csv")


# DERIVED METRICS -----------------------------------

# ~~Testing Metrics ----------------------------------------------------------

# total_population <- state_pop %>%
#   clean_names() %>% 
#   rename(state=name, total_population=estimate, fips=geoid) %>% 
#   filter(state=="Texas")
# 
# total_tests <- dshs_state_tests %>%
#   group_by(state,fips) %>%
#   summarise(total_tests = sum(tests)) %>%
#   ungroup() %>%
#   left_join(total_population, by=c("state","fips")) %>%
#   left_join(dshs_state_hospitalizations, by=c("state","fips")) %>% 
#   left_join(jhu_cases_state, by=c("state"="province_state","fips")) %>% 
#   mutate(tests_per_100k = round((total_tests/total_population)*100000,digits=4))

active_cases <- jhu_cases_state %>% 
  select(state=province_state, active, active_rank) 

test_pos_today <- dshs_state_trends %>% 
  filter(!is.na(covid_19_positivity_rate)) %>% 
  filter(date==max(date)) %>% 
  mutate(test_pos_label=covid_19_positivity_rate) %>% 
  select(date,test_pos_label) %>% 
  mutate_at(vars(test_pos_label),scales::percent_format(accuracy=.01, scale=100))

# **STATE EXPLORER METRICS ------------------------------------------------------


# ~~Current Case Data -----------------------------------------------------

tx_cases <- jhu_cases_state %>% 
  select(cases_rank) %>% 
  mutate_at(vars(cases_rank), scales::label_ordinal())

tx_mort <- jhu_cases_state %>% 
  select( mortality_rank) %>% 
  mutate_at(vars(mortality_rank), scales::label_ordinal())

tx_active <- active_cases %>% 
  mutate_at(vars(active_rank), scales::label_ordinal())

tx_recover <-  jhu_cases_state %>% 
  select(recovered_rank) %>% 
  mutate_at(vars(recovered_rank), scales::label_ordinal())

date1 <- dshs_case_trends %>% 
  filter(date==max(date))

date_filter <- date1$date - days(30)

state_case_growth <- dshs_case_trends %>% 
  select(date,cumulative_cases) %>% 
  arrange(date) %>% 
  mutate(daily_growth_rate = round(((cumulative_cases/lag(cumulative_cases))-1)*100, digits=1)) %>%
  mutate(daily_growth_rate_7day_avg = round(rollmean(daily_growth_rate, 7,
                                                     fill=0, align = "right"), digits=1)) %>%
  mutate(min_new = min(daily_growth_rate, na.rm = TRUE),
         max_new = max(daily_growth_rate, na.rm = TRUE)) %>% 
  mutate(min_new = as.numeric(min_new),
         max_new = as.numeric(max_new)) %>% 
  filter(date >= as.Date(date_filter)) %>% 
  mutate(date=as_date(date)) %>% 
  ungroup()


# HEADER CODE  ------------------------------------------------------------

header <- dashboardHeader(disable = FALSE,
                          title = tags$a(href='http://www.texas2036.org',
                                         HTML('<svg viewBox="0 0 227.4 83.5" style="height:4.5vh;padding-bottom:1.1vh;margin-top:7px"><path fill="#3a4a9f" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
                                         tags$title('Texas COVID-19 Resource Kit')))

# SIDEBAR CODE-----------------------------------------------------------

sidebar <- dashboardSidebar(disable = FALSE,
                            collapsed = FALSE,
                            sidebarMenu(
                              id = "tabs",
                              menuItem("Introduction",
                                       tabName = "intro", 
                                       icon = icon("square", class="fad fa-square")),
                              menuItem("Reopening",
                                       tabName = "reopening", 
                                       icon = icon("door-open", class="fad fa-door-open")),
                              menuItem("State Explorer",
                                       tabName = "state_profiles", 
                                       icon = icon("landmark", class="fad fa-landmark")),
                              menuItem("County Explorer",
                                       tabName = "county_profiles", 
                                       icon = icon("city", class="fad fa-city")),
                              menuItem("Credits",
                                       tabName = "credits",
                                       icon = icon("circle", class="fad fa-circle")),
                              menuItem("Data",
                                       tabName = "data",
                                       icon = icon("circle", class="fad fa-circle")),
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
    tags$link(rel = "stylesheet", type = "text/css", href = "tbl_css.css"),
    # includeHTML("social_tags.html"),
    includeHTML(("google_analytics.html")),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")),
  
  # **Landing Page ----------------------------------------------------------
  use_sever(),
  useShinyalert(),
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
              column(4, thumbnail_label(title="<i class='fad fa-door-open fa-3x'></i>",
                                        label = 'Reopening Analysis',
                                        content = includeMarkdown("markdown/intro/reopening.md"),
                                        button_link ='explore_reopen', 
                                        button_label = 'Explore')),
              column(4, thumbnail_label(title="<i class='fad fa-landmark fa-3x'></i>", 
                                        label = 'State Explorer',
                                        content = includeMarkdown("markdown/intro/state.md"),
                                        button_link ='explore_state', 
                                        button_label = 'Explore')),
              column(4, thumbnail_label(title="<i class='fad fa-city fa-3x'></i>",
                                        label = 'County Explorer',
                                        content = includeMarkdown("markdown/intro/county.md"),
                                        button_link = 'explore_county', 
                                        button_label = 'Explore'))
            )),
    tabItem(tabName = "reopening",
            fluidRow(
              HTML("<iframe width='100%' height=1200vh' style='border-top-width: 0px;border-right-width: 0px;border-bottom-width: 0px;border-left-width: 0px;' src='https://staging.convex.design/texas-2036/texas-covid-live-report/?currentCounty=Anderson'></iframe>"))),
    
    tabItem(tabName = "state_profiles",
            use_waiter(),
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
            # fluidRow(
            #   br(),
            #   column(width = 12,
            #          tags$div(class="source-notes",
            #                   br(),
            #                   h4(style="font-weight:700;display:inline;","Source Notes:"),
            #                   h4(style="font-weight:400;display:inline;","Before July, Texas 2036 sourced figures for confirmed cases, deaths as a percent of all cases, estimated recoveries, and estimated active cases from Johns Hopkins University's COVID-19 Data Repository. As of July 2014, we have switched to sourcing figures from the Texas Department of State Health Services' 'COVID-19 Accessible Dashboard Data'. In addiiton, rankings are still being produced by Texas 2036 using the Johns Hopkins University dataset (which records data from other territorities states), but rankings have been updated to only reflect comparisons between Texas, other US States, and DC only. Prior to July, comparisons included other US Territories."),
            #          ),
            #          hr()
            #   )
            # ),
            
            
            # ~~Trends Over Time --------------------------------------------------------
            
            h3(class="covid-topic", "Trends Over Time", span="id='statewide_tot"),
            fluidRow(
              column(width=6, 
                     highchartOutput("state_new_cases_hchart", height = 375)),
              column(width=6, 
                     highchartOutput("infection_chart", height = 375)),
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
                            h4(style="font-weight:400;display:inline;","These charts were developed by recording each day's value from the DSHS daily report published on their 'Accessible Dashboard Data' report posted on their site every day, saving a copy of that information in a timestamped file on our github, and then organizing each of the individual timestamped files into a single time-series dataset that allows us to track trends in population health as the completes more cases and fatality investigations. As of July 27th, fatality demographics have shifted from being processed using data from completed epidemiological fatality investigations to being processed using data from death certificates."),
                   ),
                   br(),
                   hr()
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_cases_report_rate", height = 225)),
              column(width = 6,
                     highchartOutput("state_fatalities_report_rate", height = 225))
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
                          tags$a(href="https://www.dshs.state.tx.us/coronavirus/additionaldata/","Source: Texas Department of State Health Services")))),
              fluidRow(
                column(width = 12,
                       highchartOutput("test_pos_chart", height = 350))
              ),
              ),
            
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
                     highchartOutput("state_hosp_chart", height = 375)),
              column(width = 6,
                     highchartOutput("state_hosp_rate_chart", height = 375))
            ),
            fluidRow(
              column(width = 12,
                     highchartOutput("state_hosp_resource_hchart", height = 400))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("total_conf_covid_gen_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("total_conf_covid_icu_hchart", height = 375))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("daily_covid_gen_admits_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("daily_covid_icu_admits_hchart", height = 375))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("daily_covid_ers_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("daily_covid_of_total_er_hchart", height = 375))
            ),
            
            # ~~COVID Relief Funds Data ---------------------------------------------------------
            # h3(class="covid-topic", "COVID-19 Relief Funds Data"),
            # fluidRow(
            #   column(width=12, class="economic-grid",
            #          gt_output(outputId = "state_related_crf"))
            # ),
            # ~~Mobility Data --------------------------------------------------------
            
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
                          paste0("Since: Mar 21")),
                        p(style="text-align:center;font-size:.45em;font-weight:300","Jobless Claims"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXICLAIMS","Source: BLS via FREDr")))),
              column(width = 2, class="economic-grid",
                     h2(class="economic-tile",
                        p(style="text-align:center;font-size:1em;font-weight:800",
                          paste0(tx_cont_series$value)),
                        p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;",
                          paste0("As of: ", format(tx_cont_series$date, format="%b %d"))),
                        p(style="text-align:center;font-size:.45em;font-weight:300","Continued Claims"),
                        p(style="text-align:center;font-size:.4em;font-weight:400",
                          tags$a(href="https://fred.stlouisfed.org/series/TXCCLAIMS","Source: BLS via FREDr")))),
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
                          tags$a(href="https://joinhomebase.com/data/","Source: Homebase"))))
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
            ),
            fluidRow(
              column(width = 6,
                     h3("Top 25 Unemployment Claims, by Industry", style="text-align:left;font-size:2em;font-weight:700"),
                     h3(style="text-align:left;font-size:1em;font-weight:500", includeMarkdown("markdown/charts/unemployment_top25.md")),
                     reactableOutput("top25_ui_claims", height = 775)),
              column(width = 6,
                     h3("Consumer Spending Trends, by Industry", style="text-align:left;font-size:2em;font-weight:700"),
                     h3(style="text-align:left;font-size:1em;font-weight:500", includeMarkdown("markdown/charts/spending_trends.md")),
                     tabBox(
                       title = NULL,
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1", width = 12,
                       tabPanel("By Industry", 
                                uiOutput("spending_charts_lowinc")),
                       tabPanel("By Consumer ZIP Code",  
                                uiOutput("spending_charts_all")))
                     )
            )
    ),
    
    # **COUNTY PROFILE UI  ------------------------------------------------------
    tabItem(tabName = "county_profiles",
            fluidRow(
              column(width = 6,
                     h2(style="font-weight:800;","COUNTY PROFILE", span="id='county-profile'")),
              column(width = 6,
                     selectizeInput(inputId = "countyname", label =NULL, choices = county_list,
                                    selected="Harris", multiple = FALSE, width="100%",
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
                              p(style="text-align:center","COVID-19 Related ER Visits"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_num_latest$date, format="%b %d"))),
                       ),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("hosp_susp_covid")),
                              p(style="text-align:center","Suspected COVID-19 Patients"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_num_latest$date, format="%b %d"))),
                       ),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("hosp_lab_covid")),
                              p(style="text-align:center","Confirmed COVID-19 Patients"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_num_latest$date, format="%b %d"))),
                       )
                     ),
                     fluidRow( 
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_beds")),
                              p(style="text-align:center","All Beds Availability"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_avail_latest$date, format="%b %d"))),
                              ),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_icu_beds")),
                              p(style="text-align:center","ICU Beds Availability"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_avail_latest$date, format="%b %d"))),
                       ),
                       column(width = 4,
                              h2(style="font-weight:800;text-align:center;", 
                                 textOutput("cnty_vents")),
                              p(style="text-align:center","Ventilator Availability"),
                              p(class="hosp_latest_date",paste0("As of: ", format(dshs_tsa_avail_latest$date, format="%b %d"))),
                       )
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
              column(width = 12,
                     highchartOutput("cnty_hosp_resource_hchart", height = 400))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_total_conf_covid_gen_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("cnty_total_conf_covid_icu_hchart", height = 375))
            ),
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_daily_covid_gen_admits_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("cnty_daily_covid_icu_admits_hchart", height = 375))
            ),            
            fluidRow(
              column(width = 6,
                     highchartOutput("cnty_daily_covid_ers_hchart", height = 375)),
              column(width = 6,
                     highchartOutput("cnty_daily_covid_of_total_er_hchart", height = 375))
            ),


            # ~~COVID Relief Funds ------------------------------------------------------

            h3(class="covid-topic", "COVID-19 Relief Funds Data"),
            fluidRow(
              column(width=12, class="economic-grid",
                     gt_output(outputId = "county_related_crf"))
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
  
  shinyalert(
    title = "New Economic Data +</br> Hospital Data Update",
    text = includeMarkdown("markdown/intro/shinyalert.md"),
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#F26852",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")

  Sys.sleep(3) # do something that takes time
  waiter_hide()
  
  # Waiter + Waitress Functions ---------------------------------------------
  
  w <- Waiter$new(html = spin_facebook(), 
                  id = c("tx_cases","state_growth_rate_hchart", "infection_chart","cli_hchart", "ili_hchart", "state_curves_hchart",
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
    
    latest_update <- nyt_county_cases %>% 
      filter(county=="Travis",
             date==max(date)) %>% 
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
      value=paste0(dshs_cases_text$cumulative_cases), title="Total Cases",
      subtitle=paste0(tx_cases$cases_rank, " Most in US"),
      icon = icon("chart-line", class = "fad fa-chart-line"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  # {InfoBox  - PH - Deaths} -----------------------------------------------
  
  output$tx_mort <- renderInfoBox({
    
    infoBox(
      title="Deaths (% of All Cases)", value=paste0(nyt_state_cases_text$mort_rate),
      subtitle=paste0(tx_mort$mortality_rank, " Most in US"),
      icon = icon("virus", class="fad fa-virus"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  
  # {InfoBox - PH - Recovered} -----------------------------------------------
  
  output$tx_recover <- renderInfoBox({
    
    infoBox(
      title="Est. Recovered", value=paste0(nyt_state_cases_text$recovered),
      subtitle=paste0(tx_recover$recovered_rank, " Most in US"),
      icon = icon("hand-holding-medical", class="fad fa-hand-holding-medical"), color = "navy", href=NULL
    )
  })
  
  # {InfoBox - PH - Active} -----------------------------------------------
  
  output$tx_active <- renderInfoBox({
    
    infoBox(
      title="Est. Active", value=paste0(nyt_state_cases_text$active),
      subtitle=paste0(tx_active$active_rank, " Most in US"),
      icon = icon("lungs-virus", class="fad fa-lungs-virus"), color = "navy", href=NULL
    )
  })
  
  
  # {InfoBox - HC - Hospitalization Rate - State} -----------------------------------------------
  
  output$hospitalized_rate <- renderInfoBox({
    
    dataset()
    
    tot_pos <- nyt_state_cases_tx %>% 
      filter(!is.na(hosp_rate)) %>% 
      filter(date==max(date))
    
    infoBox(
      title="% Hospitalized", 
      value=paste0(tot_pos$hosp_rate, "%"),
      subtitle="% of Active Cases",
      icon = icon("hospital-user", class = "fad fa-hospital-user"), color = "navy", href=NULL
    )
  })
  
  # {InfoBox - HC - Bed Availability - State}-------------------------------------------------
  
  output$tx_beds <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_24hr_data %>%
      as_tibble() %>% 
      filter(!is.na(bed_avail_rate)) %>% 
      filter(date==max(date)) %>%
      select(tsa,tsa_counties,bed_avail_rate) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      mutate_at(vars(bed_avail_rate),scales::percent)
    
    
    infoBox(
      title="All Beds Availability", value=paste0(tot_pos$bed_avail_rate), 
      icon=icon("bed", class = "fad fa-bed"),
      subtitle="of All Beds Available",
      color = "navy", href=NULL
    )
  })
  
  # {InfoBox - HC - ICU Beds Availability - State}-------------------------------------------------
  
  output$tx_icu_beds <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_24hr_data %>%
      as_tibble() %>%
      filter(!is.na(icu_avail_rate)) %>% 
      filter(date==max(date)) %>% 
      select(tsa,tsa_counties,icu_avail_rate) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      mutate_at(vars(icu_avail_rate),scales::percent)
    
    infoBox(
      title="ICU Beds Availability", 
      value=paste0(tot_pos$icu_avail_rate),
      icon=icon("procedures", class = "fad fa-procedures"),
      subtitle="of ICU Beds Available",
      color = "navy", href=NULL)
  })
  
  # {InfoBox - HC - Ventilators Availability - State}-------------------------------------------------
  
  output$tx_vents <- renderInfoBox({
    
    dataset()
    
    tot_pos <- dshs_tsa_24hr_data %>%
      filter(!is.na(vent_avail_rate)) %>% 
      filter(date==max(date)) %>% 
      filter(str_detect(tsa,"Total|total")) %>%
      select(tsa,tsa_counties, vent_avail_rate) %>% 
      mutate_at(vars(vent_avail_rate), scales::percent_format(accuracy=.1))
    
    
    infoBox(
      title="Ventilator Availability", value=paste0(tot_pos$vent_avail_rate), 
      icon=icon("lungs", class = "fad fa-lungs"),
      subtitle="of Ventilators Available",
      color = "navy", href=NULL)
  })
  
  # CHARTS - STATE ----------------------------------------------------------
  
  # {State Infection Rate Charts}  --------------------------------------------------
  
  output$infection_chart <- renderHighchart({
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    dates <- key_events_fltr$date
    events <- key_events_fltr$event
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    r_naught %>% 
      hchart("line", hcaes(x = date, y = mean), animation=FALSE,
             tooltip = FALSE,
             threshold = 1, negativeColor = "#FFD100",  color = "#F26852") %>%
      hc_add_series(r_naught, type = "arearange",
                    hcaes(x = date, low = lower_80,
                          high = upper_80),
                    threshold = 1, negativeColor = "#FFD100", color = "#F26852",
                    linkedTo = "r_naught") %>%
      hc_plotOptions(arearange = list(fillOpacity=.3)) %>%
      hc_title(
        text ="Texas Effective Reproduction Rate · R<sub>t</sub>",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="<span>From R<sub>t</sub> Live: R<sub>t</sub>  is the average number of people who become infected by an infectious person.</span><br/><span style='color: #F26852'>  If it’s above 1.0, COVID-19 will spread quickly.</span> <span style='color: #FFD100'> If it’s below 1.0, infections will slow.</span>",
        useHTML = TRUE) %>%
      hc_yAxis(title = list(text="Effective Reproduction Rate (R<sub>t</sub>)"),
               min = min(r_naught$lower_80), 
               max = max(r_naught$upper_80)) %>% 
      hc_xAxis(title=NULL,
               plotLines = plotLines) %>% 
      hc_tooltip(table = FALSE, sort = FALSE,
                 pointFormat = "Effective Reproduction Rate · R<sub>t</sub>: {point.mean:,.2f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: rt.live Analysis",
        href = "https://rt.live") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Growth Rate Charts}  --------------------------------------------------
  
  output$state_growth_rate_hchart <- renderHighchart({
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_legend(enabled=FALSE) %>%
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Growth Rate: {point.y:.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
  })
  
  # {State CLI Charts}  --------------------------------------------------
  
  output$cli_hchart <- renderHighchart({
    
    dataset()
    
    nyt_tx_hchart <- dshs_syndromic_tx %>% 
      mutate(min = min(influenza_like_illness, na.rm = TRUE),
             max = max(influenza_like_illness, na.rm = TRUE)) 
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    dates <- key_events_fltr$date
    events <- key_events_fltr$event
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    nyt_tx_hchart %>% 
      hchart("area", hcaes(x = date, y = covid_like_illness), animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas Coronavirus-Like Illnesses (CLI)",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text="Reported Cases of CLI"),
               min = 0, 
               max = 1100) %>% 
      hc_xAxis(title=NULL,
               plotLines = plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported CLI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  # {State ILI Charts}  --------------------------------------------------
  
  output$ili_hchart <- renderHighchart({
    
    dataset()
    
    nyt_tx_hchart <- dshs_syndromic_tx %>% 
      mutate(min = min(influenza_like_illness, na.rm = TRUE),
             max = max(influenza_like_illness, na.rm = TRUE)) 
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    nyt_tx_hchart %>% 
      hchart("area", hcaes(x = date, y = influenza_like_illness), animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas Influenza-Like Illnesses (ILI)",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Reported Cases of ILI"),
               min = 0, 
               max = 1100) %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Reported ILI Cases: {point.y:,.0f}<br>") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Department of State Health Services (DSHS)",
        href = NULL) %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  # {State Test Positive Charts}  --------------------------------------------------
  
  output$test_pos_chart <- renderHighchart({
    
    dataset()
    
    state_test_trends <- dshs_state_trends %>% 
      select(date,test_pos=covid_19_positivity_rate) %>%
      mutate(test_pos=test_pos*100) %>% 
      mutate(min = min(dshs_state_trends$test_pos, na.rm = TRUE),
             max = max(dshs_state_trends$test_pos, na.rm = TRUE))
    
    # state_test_trends <- dshs_state_trends %>% 
    #   select(date,test_pos=covid_19_positivity_rate) %>%
    #   mutate(test_pos=test_pos*100) %>% 
    #   mutate(min = min(dshs_state_trends$test_pos, na.rm = TRUE),
    #          max = max(dshs_state_trends$test_pos, na.rm = TRUE))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    band_df <- list(
      band_text = c("No Rate Reported","No Rate Reported"),
      band_from = c("2020-05-22","2020-05-29"),
      band_to = c("2020-05-24","2020-05-31"))
    
    draw_bands <- function(band_text, band_from, band_to, ...) {
      list(label = list(text = band_text, rotation = 90, y = 100, x=-5,
                        style = list(color = "rgba(255,255,255, 0.5)", fontWeight = '300',
                                     fontSize='16px',textTransform='uppercase')),
           color = "rgba(0, 45, 116, 0.7)",
           width = 2,
           from = datetime_to_timestamp(as.Date(band_from, tz="UTC")),
           to = datetime_to_timestamp(as.Date(band_to, tz="UTC")))
    }
    
    plotBands <- pmap(band_df,draw_bands)
    
    state_test_trends %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16")) %>% 
      hchart("area", hcaes(x = date, y = test_pos), animation=FALSE, name = "7-Day Avg. Test Positivty",
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>7-Day Avg. Positivty</b>: {point.y:,.2f}%"),
             color='rgba(255,255,255, 0.6)') %>% 
      hc_title(
        text ="Texas COVID-19 Test Positive Rate | 7-Day Avg. History",
        useHTML = TRUE) %>% 
      hc_subtitle(text="This data comes from DSHS Accessible Data Dashboard. DSHS Calculates their test positive 7-day Avg. History as follows: 'Positivity rate' (previous 7 days) = New cases (previous 7 days) / New test results (previous 7 days), excluding antibody tests in rate reported 19-May onwards. Gaps indicate missing reported data.") %>% 
      hc_yAxis(title = list(text ="Test Positive Rate (%)"),
               min = round(min(state_test_trends$test_pos), 2), 
               max = round(max(state_test_trends$test_pos), 2)) %>% 
      hc_xAxis(title=NULL,plotLines=plotLines,plotBands=plotBands) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Test Positive Rate: {point.y:,.0f}<br>") %>% 
      hc_plotOptions(area = list(fillOpacity=.2)) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       title = "Export This Chart",
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  
  
  # {State Curve Charts}  --------------------------------------------------
  
  output$state_curves_hchart <- renderHighchart({
    
    dataset()
    
    state_case_trends <- dshs_case_trends %>% 
      select(date,cases=cumulative_cases) %>% 
      mutate(min = min(dshs_case_trends$cumulative_cases, na.rm = TRUE),
             max = max(dshs_case_trends$cumulative_cases, na.rm = TRUE)) %>% 
      mutate(date=as_date(date))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    nyt_tx_hchart <-  state_case_trends %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16")) %>% 
      hchart("area", hcaes(x = date, y = cases), animation=FALSE,
             color = "#fff") %>% 
      hc_title(
        text ="Texas COVID-19 Cumulative Cases",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Total Cases"),
               min = round(min(state_case_trends$min), 2), 
               max = round(min(state_case_trends$max), 2)) %>% 
      hc_xAxis(title=NULL,plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    nyt_tx_hchart
    
  })
  
  # {State Cases - Reporting Rate}  --------------------------------------------------
  
  output$state_cases_report_rate <- renderHighchart({
    
    dataset()
    
    dshs_completed_investigation_chart <- dshs_completed_investigation %>% 
      arrange(date) %>% 
      rename(Remaining=diff,`Completed`=completed_investigations) %>% 
      filter(date==max(date),
             report_type=="cases") %>% 
      gather(group,value,3:4) %>% 
      mutate(report_type =gsub("cases",paste0("As of: ", format(date, format="%B %d, %Y")), x = report_type))
    
    dshs_completed_investigation_chart <- within(dshs_completed_investigation_chart, cat2 <- factor(group, levels=c("Remaining","Completed")))
    
    dshs_completed_investigation_chart %>% 
      hchart("bar", hcaes(x = report_type, y = value, group = cat2), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.value:,.0f}")) %>% 
      hc_title(
        text ="Status of Completed Case Investigations",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="As of today, this is the breakdown of completed and remaing investigations received by DSHS from local and regional health departments.",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of All Investigations")) %>%
      hc_xAxis(title = NULL,
               labels = FALSE) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, 
                 sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.2),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list('rgba(242,104,82,0.3)','rgba(242,104,82,1)')) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Fatalities - Reporting Rate}  --------------------------------------------------
  
  output$state_fatalities_report_rate <- renderHighchart({
    
    dataset()
    
    dshs_completed_investigation_chart <- dshs_completed_investigation %>% 
      arrange(date) %>% 
      rename(Remaining=diff,`Completed`=completed_investigations) %>% 
      filter(date==max(date),
             report_type=="fatalities") %>% 
      gather(group,value,3:4) %>% 
      mutate(report_type = gsub("fatalities",paste0("As of: ", format(date, format="%B %d, %Y")), x = report_type))
    
    dshs_completed_investigation_chart <- within(dshs_completed_investigation_chart, cat2 <- factor(group, levels=c("Remaining","Completed")))
    
    dshs_completed_investigation_chart %>% 
      hchart("bar", hcaes(x = report_type, y = value, group = cat2), animation=FALSE,
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.value:,.0f}")) %>% 
      hc_title(
        text ="Percent of Death Certificates Processed",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="As of today, this is the breakdown of death certificates that have been processed for demographic data by DSHS.",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of All Investigations")) %>%
      hc_xAxis(title = NULL,
               labels = FALSE) %>%
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, 
                 sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.2),
                     series = list(stacking = "percent")) %>%
      hc_colors(colors = list('rgba(242,104,82,0.3)','rgba(242,104,82,1)')) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
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
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.2f}%")) %>% 
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
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Race - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_race <- renderHighchart({
    
    dataset()
    
    race_dem <- dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date == max(date),
             report_type=="fatalities",
             demographic_type=="race") 
    
    hchart(data_to_hierarchical(race_dem, group_vars=group, size_var = pct,
                                colors = list("rgba(59, 154, 178,0.8)",
                                              "rgba(120, 183, 197,0.8)",
                                              "rgba(235, 204, 42,0.8)",
                                              "rgba(225, 175, 0,0.8)",
                                              "rgba(242, 26, 0,0.8)",
                                              "rgba(196, 203, 106,0.8)")),
           type = "treemap",
           tooltip = list(pointFormat = "<br>Share of Deaths: {point.value:,.2f}%"),
           animated = FALSE) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Race",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text="This chart breaks data that has been processed from death certificates by DSHS, by race. Each box represents the share of each group's representation among all recorded COVID-19 deaths in Texas."
      ) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_plotOptions(series = list(setOpacity=.5)) %>%
      hc_colors(colors = list("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
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
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.2f}%")) %>% 
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
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Age - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_age <- renderHighchart({
    
    dataset()
    
    age_dem <- dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date == max(date),
             report_type=="fatalities",
             demographic_type=="ages") 
    
    hchart(data_to_hierarchical(age_dem, group_vars=group, size_var = pct,
                                colors = list("rgba(124,181,236,0.8)",
                                              "rgba(67,67,72,0.8)",
                                              "rgba(144,237,125,0.8)",
                                              "rgba(247,163,92,0.8)",
                                              "rgba(128,133,233,0.8)",
                                              "rgba(241,92,128,0.8)",
                                              "rgba(228,211,84,0.8)",
                                              "rgba(43,144,143,0.8)",
                                              "rgba(244,91,91,0.8)",
                                              "rgba(145,232,225,0.8)")),
           type = "treemap",
           tooltip = list(pointFormat = "<br>Share of Deaths: {point.value:,.2f}%"),
           animated = FALSE) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Age",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text="This chart breaks data that has been processed from death certificates by DSHS, by age. Each box represents the share of each group's representation among all recorded COVID-19 deaths in Texas."
      ) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_plotOptions(treemap = list(fillOpacity=.5)) %>%
      hc_colors(colors = list("#FEDA26","#CCB233", "#CCBE7A", "#D9D2AD", "#6CB6D9", "#3091BF","#0072A6","#DBDCDD")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
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
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.2f}%")) %>% 
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
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Gender - Fatalities}  --------------------------------------------------
  
  output$state_fatalities_gender <- renderHighchart({
    
    dataset()
    
    gender_dem <- dshs_state_demographics %>% 
      mutate(pct=round(pct*100,digits=2)) %>% 
      arrange(date) %>% 
      filter(date == max(date),
             report_type=="fatalities",
             demographic_type=="gender") 
    
    hchart(data_to_hierarchical(gender_dem, group_vars=group, size_var = pct,
                                colors = list("rgba(0,45,116,0.8)",
                                              "rgba(42,125,225,0.8)",
                                              "rgba(219,220,221,0.8)")),
           type = "treemap",
           tooltip = list(pointFormat = "<br>Share of Deaths: {point.value:,.2f}%"),
           animated = FALSE) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities, by Gender",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text="This chart breaks data that has been processed from death certificates by DSHS, by gender. Each box represents the share of each group's representation among all recorded COVID-19 deaths in Texas."
      ) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State New Cases Charts}  --------------------------------------------------
  
  output$state_new_cases_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    nyt_tx_new_cases_hchart <- dshs_case_trends %>% 
      mutate(min_new = min(daily_new_cases, na.rm = TRUE),
             max_new = max(daily_new_cases, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16")) %>% 
      mutate(date=as_date(date))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    nyt_tx_new_cases_hchart %>% 
      hchart("column", hcaes(x = date, y = daily_new_cases), 
             animation=FALSE,
             color = "#fff") %>% 
      # hc_add_series(key_events, type="line", hcaes(x = date, y = event)) %>% 
      hc_add_series(nyt_tx_new_cases_hchart, type = "area", hcaes(x = date, y = new_cases_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.new_cases_7day_avg:,.0f}"),
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Cases: {point.y:,.0f}") %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  
  # {State New Deaths  Charts} ----------------------------------------------
  
  output$state_new_deaths_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    nyt_tx_new_cases_hchart <- dshs_case_trends %>% 
      mutate(min_new = min(daily_new_fatalities, na.rm = TRUE),
             max_new = max(daily_new_fatalities, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16")) %>% 
      mutate(date=as_date(date))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    nyt_tx_new_cases_hchart %>% 
      hchart("column", hcaes(x = date, y = daily_new_fatalities), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(nyt_tx_new_cases_hchart, type = "area", hcaes(x = date, y = new_deaths_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.new_deaths_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Texas COVID-19 Fatalities by Date of Death",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="LEGEND - <span style='color: #FFD100'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Deaths By Date"),
               min = mean(nyt_tx_new_cases_hchart$min_new),
               max = mean(nyt_tx_new_cases_hchart$max_new)) %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Deaths: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
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
      filter(date >= as.Date("2020-03-16")) %>% 
      mutate(date=as_date(date))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Tests: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: The COVID Tracking Project (Github)",
        href = "https://github.com/COVID19Tracking/covid-tracking-data") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  
  
  
  
  # {State Jobless Claims Chart} -------------------------------------------------
  
  
  output$state_claims_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    tx_series_all %>% 
      hchart("column", hcaes(x = date, y = value), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth=10)) %>% 
      hc_title(
        text ="Texas Jobless Claims",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Claims Filed Weekly")) %>% 
      hc_xAxis(title=NULL, plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Weekly Claims Filed: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: The Federal Reserve Bank of St. Louis + Department of Labor",
        href = "https://fred.stlouisfed.org/series/TXICLAIMS") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  # {State Hospital Resource Availability Charts}  --------------------------------------------------
  
  output$state_hosp_resource_hchart <- renderHighchart({
    
    hosp_resource_avail <- dshs_tsa_24hr_data %>% 
      ungroup() %>% 
      select(date,tsa,location,
             `All Bed Availability`=bed_avail_rate,
             `ICU Bed Availability`=icu_avail_rate, 
             `Ventilator Availabiliity`=vent_avail_rate) %>% 
      arrange(date) %>% 
      filter(location=="Texas",
             !is.na(`All Bed Availability`),
             date >= as.Date("2020-03-16")) %>% 
      gather(type, value, 4:6) %>% 
      mutate(value=value*100)
    
    hosp_resource_avail_avg <- dshs_tsa_24hr_data %>% 
      ungroup() %>% 
      filter(location=="Texas",
             !is.na(bed_avail_rate_7day_avg),
             bed_avail_rate_7day_avg!=0,
             date >= as.Date("2020-03-16")) %>% 
      select(date,tsa,location,
             `All Bed Availability | 7-Day Avg.`=bed_avail_rate_7day_avg,
             `ICU Bed Availability | 7-Day Avg.`=icu_avail_rate_7day_avg,
             `Ventilator Availability | 7-Day Avg.`=vent_avail_rate_7day_avg) %>% 
      arrange(date) %>% 
      gather(type, value, 4:6) %>% 
      mutate(value=value*100)
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hosp_resource_avail %>% 
      hchart("line", hcaes(x = date, y = value, group=type), color = c("rgba(255, 255, 255, 0.5)","rgba(242, 104, 82, 0.5)", "rgba(255, 209, 0, 0.5)"),
             tooltip = list(pointFormat = "<br><b>{series.name}</b>: {point.y:,.1f}%"),
             animation=FALSE) %>% 
      hc_add_series(hosp_resource_avail_avg, type = "spline", hcaes(x = date, y = value, group=type),
                    tooltip = list(pointFormat = "<br><b>{series.name}</b>: {point.y:,.1f}%"),
                    dashStyle = "Dash", color = c("#FFF","#F26852", "#FFD100")) %>%
      hc_plotOptions(line = list(lineWidth=5)) %>%
      hc_title(
        text ="Texas Hospital Resource Availability",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="This chart shows representations of hospital resource availability for All Beds, ICU Beds, and Ventilators. <br> [COLOR LEGEND] - <span style='color: #FFF;font-weight:bold;'>All Beds</span> | <span style='color: #F26852;font-weight:bold;'>ICU Beds</span> | <span style='color: #FFD100;font-weight:bold;'>Ventilators</span><br>[LINE LEGEND] - <span style='color: #FFF;font-weight:bold;'>Solid Line</span> = Daily Rate | <span style='color: #FFF;font-weight:bold;'>Dashed Line</span> = 7-Day Avg.",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Resource Available"),
               min = 0,
               max = 100) %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE, crosshairs = TRUE) %>%
      hc_legend(enabled=FALSE) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State Hospitalizations}  --------------------------------------------------
  
  output$state_hosp_chart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
      
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hosp_rate_hchart %>% 
      hchart("column", hcaes(x = date, y = hospitalizations), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(hosp_rate_hchart, type = "area", hcaes(x = date, y = hosp_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.hosp_rate_7day_avg:,.0f}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>%
      hc_title(
        text ="Texas COVID-19 Daily Total Hospitalizations",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this shows the number of total hospitalized patients in Texas for COVID-19 <br><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Total Hospitalizations Per Day"),
               min = min(hosp_rate_hchart$hospitalizations),
               max = max(hosp_rate_hchart$hospitalizations),
               format = "{value:,.0f}") %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Total Hospitalizations: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  # {State Hospitalized Rate}  --------------------------------------------------
  
  output$state_hosp_rate_chart <- renderHighchart({
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hospt_rate_hchart %>% 
      hchart("column", hcaes(x = date, y = hosp_rate), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(hospt_rate_hchart, type = "area", hcaes(x = date, y = hosp_rate_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.hosp_rate_7day_avg:,.2f}%"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>%
      hc_title(
        text ="Texas COVID-19 Daily Hospitalization Rate",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this shows the reported day's hospitalization rate in Texas for COVID-19. Hospitalization rate is calculated by taking each day's number of reported hospitalizations and dividing it by the number of estimated active cases. <br><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Hospitalization
                            Rate (%)"),
               min = 0,
               max = max(hospt_rate_hchart$hosp_rate),
               format = "{value:,.2f}%") %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Hospitalization Rate: {point.y:,.2f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Accessible Dashboard Data",
        href = "https://www.dshs.state.tx.us/coronavirus/additionaldata/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
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
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Visits: {point.y:,.0f}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
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
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Share: {point.y}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  # {State New COVID-19-Related General Bed Admits} -------------------------------------------------
  
  output$daily_covid_gen_admits_hchart <- renderHighchart({
    
    dshs_tsa_24hr_data_hchart <- dshs_tsa_24hr_data %>%
      filter(str_detect(tsa,"Total|total")) %>%
      # mutate(covid_share_of_new_er_visits=covid19_admitted_gen_24h,
      #        covid_share_of_new_er_visits_7day_avg=covid19_admitted_gen_24h_7day_avg) %>% 
      mutate(min_new = min(covid19_admitted_gen_24h, na.rm = TRUE),
             max_new = max(covid19_admitted_gen_24h, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup() %>% 
      arrange(date) %>% 
      filter(date >= as.Date("2020-03-16"))
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid19_admitted_gen_24h), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_gen_24h_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_gen_24h_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>%
      hc_title(
        text ="Daily COVID-19-Suspected Admits to General Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to a MedSurg (General) or isolation bed in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Daily Admits to Gen. Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
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
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    dshs_tsa_24hr_data_hchart %>% 
      hchart("column", hcaes(x = date, y = covid19_admitted_icu_24h), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_add_series(dshs_tsa_24hr_data_hchart, type = "area", hcaes(x = date, y = covid19_admitted_icu_24h_7day_avg),
                    tooltip = list(pointFormat = "<br>7-Day Avg.: {point.covid19_admitted_icu_24h_7day_avg}"),
                    color = "#FFD100", name="7-Day Avg.") %>%
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Daily COVID-19-Suspected Admits to ICU Beds",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Covering all of Texas, this chart shows trends for the daily total of patients with suspected COVID-19 related illness admitted to adult or pediatric ICU beds in 24-hour intervals.<br/><br/>LEGEND - <span style='color: #FFD100;font-weight:bold'>7-DAY ROLLING AVG.</span>",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="Daily Admits to ICU Beds"),
               min = mean(dshs_tsa_24hr_data_hchart$min_new),
               max = mean(dshs_tsa_24hr_data_hchart$max_new),
               format = "{value}") %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
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
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Total Admits : {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))

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
    
    key_events_fltr <- key_events %>%
      filter(!str_detect(event,"Stimulus"))
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Today's Admits: {point.y}") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
        href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))

  })
  
  # {State Google Mobility - Grocery Chart} -------------------------------------------------
  
  
  output$state_grocery_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  
  
  
  # {State Google Mobility - Parks Chart} -------------------------------------------------
  
  
  output$state_parks_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  
  
  
  # {State Google Mobility - Transit Chart} -------------------------------------------------
  
  
  output$state_transit_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
    
  })
  
  
  
  
  # {State Google Mobility - Retail/Rec Chart} -------------------------------------------------
  
  
  output$state_retail_rec_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  
  
  
  
  # {State Google Mobility - Workplaces Chart} -------------------------------------------------
  
  
  output$state_workplace_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
    
  })
  
  
  
  
  # {State Google Mobility - Residential Chart} -------------------------------------------------
  
  
  output$state_residential_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
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
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Change From Baseline Activity: {point.y:,.0f}%") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Source: Google COVID-19 Community Mobility Reports",
        href = "https://www.google.com/covid19/mobility/") %>%
      hc_add_theme(tx2036_hc) %>% 
      hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                   allowHTML = TRUE,
                   buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                       symbol = 'menuball', 
                                                       symbolStrokeWidth = 1,
                                                       symbolFill = 'rgba(255,209, 0, 0.9)',
                                                       symbolStroke ='#ffffff',
                                                       theme = list(fill='#3A4A9F'))),
                   chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                       subtitle =  list(style = list(fontSize = '14px')),
                                       chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
  })
  

# State Top 25 Industries UI Claims -----------------------------------------------------------

  output$top25_ui_claims <- renderReactable({
    
      reactable(twc_ui_top_25_tbl,
        searchable = FALSE,
        striped = FALSE,
        highlight = TRUE,
        bordered = FALSE,
        pagination = FALSE,
        showSortIcon = FALSE,
        # height = 350,
        fullWidth = TRUE,
        compact = TRUE,
        theme = reactableTheme(
          color = "#ffffff",
          backgroundColor = "rgba(58,74,159,1)",
          borderColor = "rgba(255,255,255,.2)",
          headerStyle = list(background = "#002D74", fontWeight="700", letterSpacing="1px", 
                             textTransform="Uppercase",
                             fontSize="16px"),
          stripedColor = "rgba(58,74,159,.6)",
          highlightColor = "#2A7DE1",
          cellPadding = "8px 12px",
          style = list(fontFamily = "'Montserrat', -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          searchInputStyle = list(width = "100%")),
        columns = list(
          index = colDef(name = "#",
                         width = 35, 
                         align = "right"),
          naics_title = colDef(name = "Industry",
                               class = "border-right"),
          naics_count = colDef(name = "UI Claims",
                               class = "border-left",
                               header = NULL,
                               defaultSortOrder = "desc",
                               cell = function(value) {
                                 width <- paste0(value * 100 / max(twc_ui_top_25_tbl$naics_count), "%")
                                 value <- format(value, big.mark = ",")
                                 value <- format(value, width = 9, justify = "right")
                                 value <- format(value, width = 5, justify = "right")
                                 bar_chart(value, width = width, fill = "#F26852")
                               },
                               align = "left",
                               style = list(fontFamily = "SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono','Courier New', monospace", whiteSpace = "pre")),
          pct_of_all = rating_column(name = "% of All",
                                     defaultSortOrder = "desc",
                                     width = 100, 
                                     cell = function(value) {
                                       scaled <- (value/max(twc_ui_top_25_tbl$pct_of_all))
                                       color <- def_rating_color(scaled)
                                       value <- paste0(format(round(value * 100,digits = 1), nsmall = 1), "")
                                       div(style = list(background = color, border = "1px solid rgba(255,255,255, .3)"), value)
                                     },
                                     style = list(color = "#ffffff",whiteSpace = "pre",
                                                  fontFamily = "SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono','Courier New', monospace"))))
    
  })
  
  # State Spending Charts - All -----------------------------------------------------------------------

  output$spending_charts_all <- renderUI({
    
    color_theme <- "#FFD100" # extracted with chrome extension
    
    df2 <- dd %>%
      # rename(Year = date) %>% 
      # group_by(type) %>%
      filter(group!="Industry") %>% 
      mutate(order = case_when(
        type == "All ZIP Codes" ~ 1,
        type == "Low Income ZIP Codes" ~ 2,
        type == "Middle Income ZIP Codes" ~ 3,
        type == "High Income ZIP Codes" ~ 4,
      )) %>% 
      arrange(order)
    
    df <- map(unique(df2$type), function(x){
      df2 %>% filter(type == x) %>% 
        hchart(., "areaspline", 
               hcaes(x = date, y = rate, group = type), 
               animation=FALSE,
               # threshold = 0,
               # negativeColor = "#F26852",
               # color = "#fff",
               tooltip = list(
                 pointFormat = "<b>{series.name}</b>: <b>{point.y:,.0f}%</b>"),
               showInLegend = FALSE) %>% 
        hc_chart(spacingBottom =  0,
                 spacingLeft =  -36,
                 spacingRight =  -10) %>%  # just plying to get these numbers
        hc_title(
          text = x,
          align = 'left',
          verticalAlign = 'top',
          floating = TRUE,
          x = 50,
          style = list(fontWeight = "800", 
                       background = "rgba(58,74,159,0.4)",
                       border = "0px solid rgba(255,255,255,0.6)",
                       color = "rgba(255,255,255,0.6)",
                       useHTML = TRUE),
          useHTML = TRUE) %>%  
        hc_xAxis(title = list(text = NULL),
                 opposite = FALSE,
                 gridLineWidth = 1,
                 gridLineColor = "rgba(255,209,0,0.3)", # vertical lines
                 tickColor = "rgba(255,209,0,0.3)",
                 lineColor = "transparent",  # horizontal line,
                 labels = list(rotation = 90, y=-35, x=-7, style = list(color = "rgba(255,255,255,.5)", fontSize = "10px")),
                 tickInterval = 16 * 24 * 7200 * 1000) %>%  # interval of 1 day (in your case = 60)
        hc_yAxis(title = list(text = ""),
                 opposite = TRUE,
                 plotLines = list(list(
                   # label = list(text="0"),
                   value = 0,
                   width = 1,
                   dashStyle = "Dash",
                   color = "rgba(255,255,255,.5)",
                   zIndex = 10
                 )),
                 gridLineColor = "transparent",
                 showFirstLabel = FALSE,
                 labels = list(enabled=FALSE)) %>%
        hc_plotOptions(series = list(color = color_theme,
                                     fillColor = hex_to_rgba(color_theme, 0.20),
                                     marker = list(enabled = FALSE))) %>% 
        hc_tooltip(pointFormat = "<b>{point.y:,.0f}%</b>") %>% 
        hc_legend(enabled=FALSE) %>% 
        hc_add_theme(tx2036_hc)
    }) %>% 
      hw_grid(ncol = 1, rowheight = 123) %>% 
      htmltools::browsable()
    
    df
    
  })
  
  output$spending_charts_lowinc <- renderUI({
    
    color_theme <- "#FFD100" # extracted with chrome extension
    
    df2 <- dd %>%
      # rename(Year = date) %>% 
      # group_by(type) %>%
      filter(group=="Industry")
    
   df <- map(unique(df2$type), function(x){
     df2 %>% filter(type == x) %>% 
       hchart(., "areaspline", 
              hcaes(x = date, y = rate, group = type), 
              animation=FALSE,
              # threshold = 0,
              # negativeColor = "#F26852",
              # color = "#fff",
              tooltip = list(
                pointFormat = "<b>{series.name}</b>: <b>{point.y:,.0f}%</b>"),
              showInLegend = FALSE) %>% 
       hc_chart(spacingBottom =  0,
                spacingLeft =  -36,
                spacingRight =  -10) %>%  # just plying to get these numbers
       hc_title(
         text = x,
         align = 'left',
         verticalAlign = 'top',
         floating = TRUE,
         x = 50,
         style = list(fontWeight = "800", 
                      background = "rgba(58,74,159,0.4)",
                      border = "0px solid rgba(255,255,255,0.6)",
                      color = "rgba(255,255,255,0.6)",
                      useHTML = TRUE),
         useHTML = TRUE) %>%  
       hc_xAxis(title = list(text = NULL),
                opposite = FALSE,
                gridLineWidth = 1,
                gridLineColor = "rgba(255,209,0,0.3)", # vertical lines
                tickColor = "rgba(255,209,0,0.3)",
                lineColor = "transparent",  # horizontal line,
                labels = list(rotation = 90, y=-35, x=-7, style = list(color = "rgba(255,255,255,.5)", fontSize = "10px")),
                tickInterval = 16 * 24 * 7200 * 1000) %>%  # interval of 1 day (in your case = 60)
       hc_yAxis(title = list(text = ""),
                opposite = TRUE,
                plotLines = list(list(
                  # label = list(text="0"),
                  value = 0,
                  width = 1,
                  dashStyle = "Dash",
                  color = "rgba(255,255,255,.5)",
                  zIndex = 10
                )),
                gridLineColor = "transparent",
                showFirstLabel = FALSE,
                labels = list(enabled=FALSE)) %>%
       hc_plotOptions(series = list(color = color_theme,
                                    fillColor = hex_to_rgba(color_theme, 0.20),
                                    marker = list(enabled = FALSE))) %>% 
       hc_tooltip(pointFormat = "<b>{point.y:,.0f}%</b>") %>% 
       hc_legend(enabled=FALSE) %>% 
       hc_add_theme(tx2036_hc)
   }) %>% 
     hw_grid(ncol = 1, rowheight = 123) %>% 
             htmltools::browsable()
   
   df
    
  })
  
  
  # {State Businesses Open Chart} -------------------------------------------------
  
  
  output$state_businesses_hchart <- renderHighchart({
    
    # Make sure requirements are met
    # req(input$countyname)
    
    dataset()
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = '#fff', fontSize='11px', textTransform='initial')),
                            color = "#fff",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hb_businesses_open_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Est. Change in Businesses Open",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Business Open")) %>% 
      hc_xAxis(title=NULL,plotLines=plotLines) %>% 
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
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hb_hours_worked_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text ="Est. Change in Hours Worked By Hourly Employees",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Hours Worked")) %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
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
    
    key_events_fltr <- key_events
    
    plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                      ~list(label = list(text = .y,
                                         style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                      fontSize='12px',textTransform='initial')),
                            color = "rgba(255, 255, 255, 0.6)",
                            width = 1,
                            dashStyle = "Dash",
                            value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
    
    hb_employees_working_all %>% 
      hchart("area", hcaes(x = date, y = pct), 
             animation=FALSE,
             color = "#FFD100") %>% 
      hc_plotOptions(area = list(fillOpacity=.3)) %>% 
      hc_title(
        text = "Est. Change in Number of Hourly Employees Working",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% Change in Employees Working")) %>% 
      hc_xAxis(title=NULL,
               plotLines=plotLines) %>% 
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
    
    # output$state_hours_worked <- renderText({
    #   
    #   tx_county_cases %>%
    #     filter(county==input$countyname) %>% 
    #     mutate_at(vars(cases), scales::comma) %>% 
    #     distinct(cases) %>% 
    #     as.character()
    #   
    # })
    
    
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
      
      nyt_county_cases %>%
        filter(county==input$countyname,
               date==max(date)) %>% 
        mutate_at(vars(cases), scales::comma) %>% 
        distinct(cases) %>% 
        as.character()
    })
    
    # County Active Cases --------------------------------------------------
    
    output$county_active_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        filter(!is.na(active_cases)) %>% 
        filter(county==input$countyname,
               date==max(date)) %>% 
        mutate_at(vars(active_cases), scales::comma) %>% 
        distinct(active_cases) %>% 
        as.character()
    })
    
    # County Incident Rate ----------------------------------------------------
    
    output$county_incident_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        filter(county==input$countyname,
               date==max(date)) %>% 
        distinct(incident_rate) %>% 
        round(digits=1) %>% 
        as.character()
    })
    
    # County Confirmed Deaths -------------------------------------------------
    
    output$county_deaths_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        filter(county==input$countyname,
               date==max(date)) %>% 
        distinct(deaths) %>% 
        as.character()
    })
    
    # County Mortality Rate ---------------------------------------------------
    
    output$county_mort_rate <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        fill(deaths, .direction="down") %>% 
        filter(county==input$countyname,
               date==max(date)) %>%
        mutate(mort_rate=round(deaths/cases,digits=4)) %>% 
        mutate_at(vars(mort_rate),scales::percent_format(accuracy=.01)) %>% 
        distinct(mort_rate) %>% 
        as.character()
    })
    
    # County Testing Rate -----------------------------------------------------
    
    output$county_test_text <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases %>%
        filter(!is.na(tests_per_100k)) %>% 
        filter(county==input$countyname,
               date==max(date)) %>% 
        mutate_at(vars(tests_per_100k), scales::comma) %>% 
        distinct(tests_per_100k) %>% 
        as.character()
    })
    
    # County Testing Total -----------------------------------------------------
    
    # output$county_test_tots <- renderText({
    #   
    #   # Make sure requirements are met
    #   req(input$countyname)
    #   
    #   total_cnty_tests %>%
    #     filter(county==input$countyname) %>% 
    #     distinct(total_tests) %>% 
    #     as.character()
    # })
    
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
    

    # COVID-Relief State ------------------------------------------------------

    output$state_related_crf <- gt::render_gt({
      
      gt_tbl <-  covid_relief %>%
        filter(place=="State of Texas") %>%
        gt() %>%
        tab_header(title = paste0("COVID-19 Related Relief Funds Related To The State of Texas"),
                   subtitle = "This table shows the distribution of COVID-19 related relief funds that were initially allocated to the State Government in Texas. At the State level, 100% of funds are released to the state from the federal government and then allocated from the State of Texas to smaller governments. The state's larger cities applied for and received money directly. These areas are can be identified on the county page by looking for entities in a county who have '100%' of funds released.") %>%
        cols_hide(c("county", "place_type")) %>%
        fmt_percent(vars(pct_released), decimals=1, drop_trailing_zeros = TRUE) %>% 
        fmt_currency(vars(allocated,curr_released),decimals=0,currency="USD",sep_mark = ",") %>% 
        cols_label(place=md("**Entity Name**"),
                   allocated=md("**Total Allocated**"),
                   curr_released=md("**Currently Released to Texas**"),
                   pct_released=md("**% Released of All Allocated**")) %>%
        cols_align(align="center",
                   columns=vars(allocated, curr_released,pct_released)) %>% 
        tab_options(table.background.color = "#3A4A9F",
                    table.font.color = "#fff",
                    table.font.size = "24px",
                    table.width = pct(95),
                    table.align = "center",
                    row_group.background.color = "#3A4A9F",
                    table.border.top.color = "#3A4A9F",
                    table.border.left.color = "#3A4A9F",
                    table.border.right.color = "#3A4A9F",
                    table.border.bottom.color = "#3A4A9F",
                    table_body.border.bottom.color = "#3A4A9F")
      
      gt_tbl
    })
    
    
    # COVID-Relief County -------------------------------------------------
    
    output$county_related_crf <- gt::render_gt({
      
      req(input$countyname)
      
      gt_tbl <-  covid_relief %>%
        filter(county==input$countyname) %>%
        gt() %>%
        tab_header(title = paste0("COVID-19 Related Relief Funds Related To ", input$countyname, " County"),
                   subtitle = "This report shows the distribution of COVID-19 related relief funds that were initially allocated to specific governments in Texas and how much of those funds have been released to each entity. Not all funds are administered by the county government, which is why you may see multiple city or municipal governments associated with a county. Entities showing 100% of funds released represent those entities who received their relief funds directly from the federal government instead of through the State. Areas with less than 100% rely on the state to act as a fiduciary of federal relief funds.") %>%
        cols_hide("county") %>%
        fmt_percent(vars(pct_released), decimals=1, drop_trailing_zeros = TRUE) %>% 
        fmt_currency(vars(allocated,curr_released),currency="USD",sep_mark = ",") %>% 
        cols_label(place=md("**Entity Name**"),
                   place_type=md("**Entity Type**"),
                   allocated=md("**Total Allocated**"),
                   curr_released=md("**Currently Released**"),
                   pct_released=md("**% Released of All Allocated**")) %>% 
        cols_align(align="center",
                   columns=vars(allocated, curr_released,pct_released)) %>% 
        tab_options(table.background.color = "#3A4A9F",
                    table.font.color = "#fff",
                    table.font.size = "22px",
                    table.width = pct(95),
                    table.align = "center",
                    row_group.background.color = "#3A4A9F",
                    table.border.top.color = "#3A4A9F",
                    table.border.left.color = "#3A4A9F",
                    table.border.right.color = "#3A4A9F",
                    table.border.bottom.color = "#3A4A9F",
                    table_body.border.bottom.color = "#3A4A9F")
      
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
      
      dshs_tsa_24hr_data %>%
        filter(!is.na(bed_avail_rate)) %>% 
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
      
      dshs_tsa_24hr_data %>%
        filter(!is.na(icu_avail_rate)) %>% 
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
      
      dshs_tsa_24hr_data %>% 
        filter(!is.na(vent_avail_rate)) %>% 
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
      
      dshs_tsa_24hr_data %>%
        filter(date==max(date)) %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>%
        # filter(tsa_counties=="Harris") %>% 
        mutate_at(vars(total_susp_covid_in_hosp_at_time_of_report), scales::comma) %>% 
        distinct(total_susp_covid_in_hosp_at_time_of_report) %>% 
        as.character()
    })
    
    # TSA Lab Confirmed COVID Patients -----------------------------------------------------
    
    output$hosp_lab_covid <- renderText({
      
      # Make sure requirements are met
      req(input$countyname)
      
      dshs_tsa_24hr_data %>%
        filter(date==max(date)) %>%
        as_tibble() %>% 
        filter(tsa_counties==input$countyname) %>% 
        mutate_at(vars(total_laboratory_confirmed), scales::comma) %>% 
        distinct(total_laboratory_confirmed) %>% 
        as.character()
    })
    
    # CHARTS - COUNTY------------------------------------------------------------------
    
    # {TSA Hospital Resource Availability}  --------------------------------------------------
    
    output$cnty_hosp_resource_hchart <- renderHighchart({
      
      hosp_resource_avail <- dshs_tsa_24hr_data %>% 
        ungroup() %>% 
        select(date,tsa,tsa_counties,
               `All Bed Availability`=bed_avail_rate,
               `ICU Bed Availability`=icu_avail_rate, 
               `Ventilator Availabiliity`=vent_avail_rate) %>% 
        arrange(date) %>% 
        filter(tsa_counties==input$countyname,
               !is.na(`All Bed Availability`),
               date >= as.Date("2020-03-16")) %>% 
        gather(type, value, 4:6) %>% 
        mutate(value=value*100)
      
      hosp_resource_avail_avg <- dshs_tsa_24hr_data %>% 
        ungroup() %>% 
        filter(tsa_counties==input$countyname,
               !is.na(bed_avail_rate_7day_avg),
               bed_avail_rate_7day_avg!=0,
               date >= as.Date("2020-03-16")) %>% 
        select(date,tsa,tsa_counties,
               `All Bed Availability | 7-Day Avg.`=bed_avail_rate_7day_avg,
               `ICU Bed Availability | 7-Day Avg.`=icu_avail_rate_7day_avg,
               `Ventilator Availability | 7-Day Avg.`=vent_avail_rate_7day_avg) %>% 
        arrange(date) %>% 
        gather(type, value, 4:6) %>% 
        mutate(value=value*100)
      
      key_events_fltr <- key_events %>%
        filter(!str_detect(event,"Stimulus"))
      
      plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                        ~list(label = list(text = .y,
                                           style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                        fontSize='12px',textTransform='initial')),
                              color = "rgba(255, 255, 255, 0.6)",
                              width = 1,
                              dashStyle = "Dash",
                              value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
      
      hosp_resource_avail %>% 
        hchart("line", hcaes(x = date, y = value, group=type), color = c("rgba(255, 255, 255, 0.5)","rgba(242, 104, 82, 0.5)", "rgba(255, 209, 0, 0.5)"),
               tooltip = list(pointFormat = "<br><b>{series.name}</b>: {point.y:,.1f}%"),
               animation=FALSE) %>% 
        hc_add_series(hosp_resource_avail_avg, type = "spline", hcaes(x = date, y = value, group=type),
                      tooltip = list(pointFormat = "<br><b>{series.name}</b>: {point.y:,.1f}%"),
                      dashStyle = "Dash", color = c("#FFF","#F26852", "#FFD100")) %>%
        hc_plotOptions(line = list(lineWidth=5)) %>%
        hc_title(
          text =paste0(input$countyname, " County | Hospital Resource Availability"),
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Covering only this TSA, this chart shows representations of hospital resource availability for All Beds, ICU Beds, and Ventilators. <br> [COLOR LEGEND] - <span style='color: #FFF;font-weight:bold;'>All Beds</span> | <span style='color: #F26852;font-weight:bold;'>ICU Beds</span> | <span style='color: #FFD100;font-weight:bold;'>Ventilators</span><br>[LINE LEGEND] - <span style='color: #FFF;font-weight:bold;'>Solid Line</span> = Daily Rate | <span style='color: #FFF;font-weight:bold;'>Dashed Line</span> = 7-Day Avg.",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% of Resource Available"),
                 min = 0,
                 max = 100) %>% 
        hc_xAxis(title=NULL,
                 plotLines=plotLines) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   shared=TRUE, crosshairs = TRUE) %>%
        hc_legend(enabled=FALSE) %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://texas-2036.github.io/covid-pages/Creating-COVID-Hospitalization-TS-Data.html") %>%
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
    })
    
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
      
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
          text = paste0(input$countyname," County | Daily COVID-19-Suspected Admits to General Beds"),
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
          text = paste0(input$countyname," County | Daily COVID-19-Suspected Admits to ICU Beds"),
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
    
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
      
      key_events_fltr <- key_events %>%
        filter(!str_detect(event,"Stimulus"))
      
      plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                        ~list(label = list(text = .y,
                                           style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                        fontSize='12px',textTransform='initial')),
                              color = "rgba(255, 255, 255, 0.6)",
                              width = 1,
                              dashStyle = "Dash",
                              value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
      
      nyt_county_cases_chart %>% 
        hchart("area", hcaes(x = date, y = cases), animation=FALSE,
               color = "#fff") %>% 
        hc_title(
          text = paste0(input$countyname, " County | COVID-19 Cases"),
          useHTML = TRUE) %>% 
        hc_xAxis(title=NULL,plotLines=plotLines) %>% 
        hc_yAxis(title=list(text="Total Cases"),
                 min = round(mean(nyt_county_cases_chart$min_new), 1), 
                 max = round(mean(nyt_county_cases_chart$max_new), 1)) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
        hc_plotOptions(area = list(fillOpacity=.3)) %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://tabexternal.dshs.texas.gov/t/THD/views/COVIDExternalQC/COVIDTrends?%3AisGuestRedirectFromVizportal=y&%3Aembed=y") %>%
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
      
      key_events_fltr <- key_events %>%
        filter(!str_detect(event,"Stimulus"))
      
      plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                        ~list(label = list(text = .y,
                                           style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                        fontSize='12px',textTransform='initial')),
                              color = "rgba(255, 255, 255, 0.6)",
                              width = 1,
                              dashStyle = "Dash",
                              value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
      
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
        hc_xAxis(title=NULL,plotLines=plotLines) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   New Cases: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://tabexternal.dshs.texas.gov/t/THD/views/COVIDExternalQC/COVIDTrends?%3AisGuestRedirectFromVizportal=y&%3Aembed=y") %>%
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
      
      key_events_fltr <- key_events %>%
        filter(!str_detect(event,"Stimulus"))
      
      plotLines <- map2(key_events_fltr$date,key_events_fltr$event,
                        ~list(label = list(text = .y,
                                           style = list(color = "rgba(255, 255, 255, 0.6)", 
                                                        fontSize='12px',textTransform='initial')),
                              color = "rgba(255, 255, 255, 0.6)",
                              width = 1,
                              dashStyle = "Dash",
                              value = datetime_to_timestamp(as.Date(.x, tz="UTC"))))
      
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
        hc_xAxis(title=NULL,plotLines=plotLines) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   New Deaths: {point.y:,.0f}") %>% 
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas DSHS - COVID-19 Hospital Bed Reporting",
          href = "https://tabexternal.dshs.texas.gov/t/THD/views/COVIDExternalQC/COVIDTrends?%3AisGuestRedirectFromVizportal=y&%3Aembed=y") %>%
        hc_add_theme(tx2036_hc) %>% 
        hc_exporting(enabled=TRUE, scale=2, sourceWidth= 1200, sourceHeight = 600, 
                     allowHTML = TRUE,
                     buttons = list(contextButton = list(menuItems = myMenuItems, 
                                                         title = "Export This Chart",
                                                         symbol = 'menuball', 
                                                         symbolStrokeWidth = 1,
                                                         symbolFill = 'rgba(255,209, 0, 0.9)',
                                                         symbolStroke ='#ffffff',
                                                         theme = list(fill='#3A4A9F'))),
                     chartOptions = list(title =  list(style = list(fontWeight = '800', fontSize = '22px', textTransform = "uppercase")),
                                         subtitle =  list(style = list(fontSize = '14px')),
                                         chart = list(events = list(load = JS('function() {
                                         this.renderer.image("https://texas-2036.github.io/reference-files/logo_short_w.png",
                                                             1135, 
                                                             5,
                                                             50,
                                                             41.7 
                                         ).add();
                                       }')))))
      
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
                       radius = ~sqrt((cases*.03)),
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
