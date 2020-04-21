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
library(highcharter)
library(tidyverse)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(tigris)
library(fredr)
library(sf)

fredr_set_key("22c6ffaa111781ee88df344a4f120eef")

blank <- "https://api.mapbox.com/styles/v1/mrw03b/cji21nw3e03ht2rqz4kgte0ea/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibXJ3MDNiIiwiYSI6IlYwb2FiOWcifQ.RWUm2a87fEC9XrDxzvZKKg"
map_attr <- "<a href='https://www.mapbox.com/map-feedback/'>© MAPBOX</a> | <a href='http://texas2036.org'> MAP © TEXAS 2036</a>"

# DATA PREP CODE ----------------------------------------------------------


# **Coronavirus Data -----------------------------------------------------------


# ~~JHU Data --------------------------------------------------------------


jhu_cases_state_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_state.csv") %>% 
  janitor::clean_names() %>% 
  filter(country_region=="US") %>%
  group_by(last_update) %>% 
  mutate(cases_rank=dense_rank(desc(confirmed)),
         deaths_rank=dense_rank(desc(deaths)),
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
         max = max(cases),
         prev_day_cases = lag(cases,1),
         prev_week_cases = lag(cases,7),
         new_cases_1day = cases-prev_day_cases,
         new_cases_7day= cases-prev_week_cases)
         
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


# **Economic Data -----------------------------------------------------------

hb_summary <-  read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pub?gid=1178059516&single=true&output=csv",
                        skip=2)
  
tx_series <-fredr(
  series_id = "TXICLAIMS",
  observation_start = as.Date("2020-01-01")
) %>% 
  mutate(date=as.character(date)) %>%
  add_row(date="2020-04-08", series_id="TXICLAIMS", value=313832) %>% 
  mutate(date=as.Date(date))


# HEADER CODE-----------------------------------------------------------

header <- dashboardHeader(disable = FALSE,
  title = tags$a(href='http://www.texas2036.org',
                 HTML('<svg viewBox="0 0 227.4 83.5" style="height:4.5vh;padding-bottom:1vh;margin-top:9px"><path fill="currentColor" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
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
                              menuItem("State Profile",
                                       tabName = "state_profiles", icon = icon("square")),
                              menuItem("County Profile",
                                       tabName = "county_profiles", icon = icon("square"))
                              # menuItem("About", icon = icon("circle"), tabName = "about")
                            )
)


# BODY CODE -----------------------------------------------------

body <- dashboardBody(
  tags$head(
    tags$script(src="https://kit.fontawesome.com/5272d94c6c.js", crossorigin="anonymous"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom2.css"),
    includeHTML(("google_analytics.html")),
    tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")
    
  ),
  tabItems(
    tabItem(tabName = "state_profiles",
# **Statewide Profile UI ----------------------------------------------------
            h2(style="font-weight:800;", "STATEWIDE COVID-19 PROFILE", span="id='statewide-profile'"),            
            h3(class="covid-topic", "Public Health Indicators"),
            fluidRow(
              infoBoxOutput("tx_cases", width=3),
              infoBoxOutput("tx_tests", width=3),
              # infoBoxOutput("incident_rate", width=3),
              # infoBoxOutput("tx_deaths", width=3),
              infoBoxOutput("test_rate", width=3),
              infoBoxOutput("mort_rate", width=3)
              ),
            # fluidRow(
            #   # valueBoxOutput("tx_tests", width=3),
            #   # valueBoxOutput("test_rate", width=3),
            #   valueBoxOutput("days_since_mandate", width=3),
            #   valueBoxOutput("social_distancing_score", width=3)
            #   ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_curves_hchart", height = 300)),
              column(width = 6,
                     highchartOutput("state_new_cases_hchart", height = 300))
              ),
            fluidRow(
              column(width = 6,
                     highchartOutput("state_new_deaths_hchart", height = 300)),
              column(width = 6,
                     highchartOutput("state_new_tests_hchart", height = 300))
            )
            # h3(class="covid-topic", "Economy + Society"),
            # fluidRow(
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#F26852;","-4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Jobless Claims"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Texas Workforce Commission"))),
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#F26852;","-4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Unemployment Rate"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Texas Workforce Commission"))),
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;","+4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Local Businesses Open"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Homebase"))),
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;","+4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Reduction In Hours Worked"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Homebase"))),
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#F26852;","-4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Hourly Employees Working"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Homebase"))),
            #   column(width = 2, class="economic-grid",
            #          h2(class="economic-tile",
            #             p(style="text-align:center;font-size:1em;font-weight:800","3,261"),
            #             p(style="text-align:center;font-size:.6em;font-weight:600;color:#00A9C5;","+4%"),
            #             p(style="text-align:center;font-size:.5em;font-weight:300","Monthly $ Loss Per Employee"),
            #             p(style="text-align:center;font-size:.3em;font-weight:400","Source: Homebase")))
            # )
),

# **County Profile UI  ------------------------------------------------------
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
                     h4("Data As of:",textOutput("currentTime", inline=TRUE)),
                     leafletOutput("map", width = "100%", height = 600)),
              column(width = 6,
              # h3(style="font-weight:700;",textOutput("countyname", inline = TRUE)),
              # h3(class="covid-topic", "Public Health"),
              fluidRow(
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", textOutput("county_cases_text")),p(style="text-align:center","Confirmed Cases")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", textOutput("county_deaths_text")),p(style="text-align:center","Confirmed Deaths")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", textOutput("county_incident_text")),p(style="text-align:center","Cases Per Capita")),
                column(width = 3,
                       h2(style="font-weight:800;text-align:center;", textOutput("county_active_text")),p(style="text-align:center","Active Cases"))
    
                ),
              # h3(class="covid-topic", "Economy + Society"),
              highchartOutput("cnty_compare_hchart", height = 100),
              highchartOutput("cnty_curves_hchart", height = 300),
              box(solidHeader = TRUE, 
                  width=12, 
                  height = 600,
                  background = "navy",
                  collapsible = FALSE))
              )
            ),
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "Calls & Calls Per 100 Households", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
                ),
              box(
                title = "Median Household Values & The Gini Index", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
                )
              ),
            fluidRow(
              box(
                title = "The Map Indicators & Indicator Percentile", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
              ),
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
              )
            ),
            fluidRow(
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
              ),
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE
              )
            )
            )
    )
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
  

# INFO BOXES - STATEWIDE--------------------------------------------------------------

# --{Total Confirmed Cases} -----------------------------------------------
    
  output$tx_cases <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(confirmed, cases_rank) %>% 
      mutate_at(vars(confirmed), scales::comma) %>% 
      mutate_at(vars(cases_rank), scales::label_ordinal())
    
    infoBox(
       value=paste0(tot_pos$confirmed), title="Total Confirmed Cases",
       subtitle=paste0(tot_pos$cases_rank, " Most in US"),
      icon = icon("lungs-virus"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
# {Total COVID-19 Tests} -----------------------------------------------
  
  output$tx_tests <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(people_tested, tested_rank) %>% 
      mutate_at(vars(people_tested), scales::comma) %>% 
      mutate_at(vars(tested_rank, scales::label_ordinal()))
    
    infoBox(
      title="Total People Tested", value=paste0(tot_pos$people_tested), icon = icon("microscope"),
      subtitle=paste0("(",tot_pos$tested_rank, " in US)"),
      color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
# {Total Deaths} -----------------------------------------------
  
  output$tx_deaths <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(deaths, deaths_rank) %>% 
      mutate_at(vars(deaths), scales::comma) %>% 
      mutate_at(vars(deaths_rank, scales::label_ordinal()))
    
    
    infoBox(
      title="Total Confirmed Deaths", value=paste0(tot_pos$deaths), icon=icon("heartbeat"),
      subtitle=paste0(tot_pos$deaths_rank, " in US"),
      color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
# {Hospitalization Rate} -----------------------------------------------
  
  output$incident_rate <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(incident_rate, incident_rank) %>% 
      mutate_at(vars(incident_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
      mutate_at(vars(incident_rank), scales::label_ordinal())
    
    
    infoBox(
      title="Cases Per Capita", value=paste0(tot_pos$incident_rate),
      subtitle=paste0(tot_pos$incident_rank, " Most in US"),
      icon = icon("procedures"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  # {Mortality Rate} -----------------------------------------------
  
  output$mort_rate <- renderInfoBox({
    tot_pos <- jhu_cases_state %>% 
      select(mortality_rate,  mortality_rank) %>% 
      mutate_at(vars(mortality_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
      mutate_at(vars(mortality_rank), scales::label_ordinal())
    
    infoBox(
      title="Mortality Rate", value=paste0(tot_pos$mortality_rate),
      subtitle=paste0(tot_pos$mortality_rank, " Most in US"),
      icon = icon("biohazard"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
  })
  
  # {Testing Rate} -----------------------------------------------
  
  output$test_rate <- renderInfoBox({
    
    tot_pos <- jhu_cases_state %>% 
      select(testing_rate, testrate_rank) %>% 
      mutate_at(vars(testing_rate), scales::number_format(accuracy=.01, scale=1)) %>% 
      mutate_at(vars(testrate_rank), scales::label_ordinal())
    
    infoBox(
      title=paste0("Tests Per Capita"), value=paste0(tot_pos$testing_rate),
      subtitle=paste0(tot_pos$testrate_rank, " Most in US"),
      icon = icon("vials"), color = "navy", href="https://github.com/CSSEGISandData/COVID-19?target=_blank"
    )
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
      hc_yAxis(min = round(mean(nyt_state_cases_tx$min), 2), 
               max = round(mean(nyt_state_cases_tx$max), 2)) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
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
                                gridLineWidth = .5,
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
    
    # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
    #               name = "Fit", id = "fit") 
    
    nyt_tx_new_cases_hchart <- nyt_state_cases_tx %>% 
      mutate(min_new = min(new_cases_1day, na.rm = TRUE),
             max_new = max(new_cases_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
        ungroup()
    
    nyt_tx_new_cases_hchart %>% 
      hchart("column", hcaes(x = date, y = new_cases_1day), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth = 6)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Cases",
        useHTML = TRUE) %>% 
      hc_yAxis(min = mean(nyt_tx_new_cases_hchart$min_new),
               max = mean(nyt_tx_new_cases_hchart$max_new)) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Cases: {point.y:,.0f}<br>") %>% 
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
                                gridLineWidth = .5,
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
      mutate(min_new = min(new_deaths_1day, na.rm = TRUE),
             max_new = max(new_deaths_1day, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup()
    
    nyt_tx_new_cases_hchart %>% 
      hchart("column", hcaes(x = date, y = new_deaths_1day), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth = 6)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Deaths",
        useHTML = TRUE) %>% 
      hc_yAxis(min = mean(nyt_tx_new_cases_hchart$min_new),
               max = mean(nyt_tx_new_cases_hchart$max_new)) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Deaths: {point.y:,.0f}<br>") %>% 
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
                                gridLineWidth = .5,
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
      mutate(min_new = min(totalTestResultsIncrease, na.rm = TRUE),
             max_new = max(totalTestResultsIncrease, na.rm = TRUE)) %>% 
      mutate(min_new = as.numeric(min_new),
             max_new = as.numeric(max_new)) %>% 
      ungroup()
    
    tx_new_tests_hchart %>% 
      hchart("column", hcaes(x = date, y = totalTestResultsIncrease), 
             animation=FALSE,
             color = "#fff") %>% 
      hc_plotOptions(column = list(pointWidth = 6)) %>% 
      hc_title(
        text ="Texas COVID-19 Daily New Tests",
        useHTML = TRUE) %>% 
      hc_yAxis(min = mean(tx_new_tests_hchart$min_new),
               max = mean(tx_new_tests_hchart$max_new)) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 pointFormat = "<b>{point.name}</b><br>
                   New Tests: {point.y:,.0f}<br>") %>% 
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
                   title = list(style = list(fontFamily = "Montserrat", color="#fff"),
                                align = "left"), 
                   legend = list(align = "right", 
                                 style = list(fontFamily = "Montserrat", color="white"), 
                                 verticalAlign = "bottom"),
                   credits = list(style = list(color = "#fff")),
                   xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                title = list(style = list(color = "#fff", fontSize = "12px", 
                                                          color="#fff",fontWeight="500")),
                                gridLineWidth = .5,
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
      
      # req(click_cnty()) # do this if click_tract() is not null
      
      tx_highlight_shp <- tx_county_sf %>% 
        filter(county==input$countyname)
      
      # Add the clicked tract to the map in aqua, and remove when a new one is clicked
      map <- leafletProxy('map') %>%
        removeShape('htract') %>%
        addPolygons(data = tx_highlight_shp,
                    fill = FALSE,
                    stroke = TRUE,
                    weight = 3,
                    color = '#3A4A9F', 
                    opacity = 1, 
                    layerId = 'htract')
      
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
    

# County Persons Tested ---------------------------------------------------


# County Testing Rate -----------------------------------------------------


# CHARTS ------------------------------------------------------------------
    

# {County Compare Chart} ----------------------------------------------------

    output$cnty_compare_hchart <- renderHighchart({
      
      # Make sure requirements are met
      req(input$countyname)
      
      nyt_county_cases_chart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>% 
        mutate(date = ymd(date)) 
      
      nyt_county_cases_today <- nyt_county_cases %>%
        mutate(date = ymd(date)) %>% 
        group_by(state) %>% 
        filter(date == max(date)) %>% 
        ungroup()
      
      nyt_highlight_cnty <- nyt_county_cases_today %>% 
        filter(county==input$countyname)
      
      nyt_county_cases_today %>% 
        hchart(type="scatter", hcaes(x = cases, y = 0, name = county), animation=FALSE,
             color = "#fff",showInLegend=FALSE) %>%
        hc_add_series(data=nyt_highlight_cnty, type="scatter",  hcaes(x = cases, y = 0,name = county), 
                      color = "#FFD100", marker = list(radius=9, enabled=TRUE), animation = FALSE,
                      showInLegend=FALSE, dataLabels = list(enabled = TRUE,
                                                            padding=10,
                                                            format = '{point.name} County')) %>%
        hc_xAxis(labels=list(format = "{value:,.0f} Cases")) %>%
        hc_yAxis(title=FALSE,
          showFirstLabel = FALSE,
          showLastLabel = FALSE
        ) %>% 
        hc_tooltip(table = TRUE, sort = TRUE, borderWidth = 0,
                   pointFormat = "<b>{this.name}</b><br>
                                  Confirmed Cases: <b> {point.x:,.0f}</b>") %>%
        hc_add_theme(
          hc_theme_merge(
            hc_theme_smpl(),
            hc_theme(chart = list(backgroundColor = "#3A4A9F",
                                  style = list(fontFamily = "Montserrat")),
                     plotOptions = list(series = list(dataLabels = list(style = list(fontFamily = "Montserrat", 
                                                                                     color="#FFD100",
                                                                                     fontSize = "1.4em",
                                                                                     textOutline = FALSE)))),
                     yAxis = list(labels = list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(style = list(color = "#fff", fontSize = "12px", 
                                                            color="#fff",fontWeight="500")), 
                                  gridLineWidth = 0,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "#F3F3F3", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = 0),
                     xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                                  title = list(enabled=FALSE),
                                  gridLineWidth = 0,
                                  gridLineColor = "#F3F3F3", 
                                  lineColor = "#fff", 
                                  minorGridLineColor = "transparent", 
                                  tickColor = "#F3F3F3", 
                                  tickWidth = .5)))
          )
      
    })
    
# {County Curve Charts}  --------------------------------------------------
    
    output$cnty_curves_hchart <- renderHighchart({
      
      # Make sure requirements are met
     req(input$countyname)
      # 
      # req(exists(input$countyname, "package:datasets", inherits = FALSE),
      #     )
      
      nyt_county_cases_chart <- nyt_county_cases %>% 
        filter(county==input$countyname) %>% 
        mutate(date = ymd(date))
      
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      # hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
      #               name = "Fit", id = "fit") 
      
      nyt_county_cases_chart %>% 
      hchart("area", hcaes(x = date, y = cases), animation=FALSE,
             color = "#fff") %>% 
        hc_title(
          text = paste0(input$countyname, " County COVID-19 Cases"),
          useHTML = TRUE) %>% 
        hc_yAxis(min = round(mean(nyt_county_cases$min), 1), 
                 max = round(mean(nyt_county_cases$max), 1)) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   pointFormat = "<b>{point.name}</b><br>
                   Confirmed Cases: {point.y:,.0f}<br>") %>% 
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
                                      gridLineWidth = .5,
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
    p <- c(0.5, 0.75, 0.9, .99, 1)
    
    p_names <- map_chr(p, ~paste0(.x*100, "%"))
    
    p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
      set_names(nm = p_names)
    
    avgs <- tx_today %>% 
      group_by(date) %>% 
      mutate(sqrt=sqrt(cases)) %>% 
      summarize_at(vars(sqrt), funs(!!!p_funs)) %>% 
      gather(percentile,value, 2:6) %>% 
      mutate(size=round((value*value),digits = 0)) %>% 
      mutate(label=as.character(size))
    
# COUNTY MAP --------------------------------------------------------------

    pal <- colorNumeric(palette = "Reds", na.color = "#DBDCDD", 
                        domain = tx_county_sf$incident_rate)
    
    labels_clean <- sprintf("<a style = 'font-family: Montserrat; font-size: 22px; font-weight: 700; color:#3a4a9f'>%s County</a> <br/><a style = 'font-family: Montserrat; font-size: 16px; font-weight: 400; color:#6B6D6F'>%s Active Cases</a><br/><a style = 'font-family: Montserrat; font-size: 12px; font-weight: 400; color:#8C8F93'>%s Total Confirmed Cases</a>",
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
      addCircleMarkers(~long, ~lat,
                       radius = ~sqrt(cases),
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
        # baseGroups = c("Incidence Rate"),
        overlayGroups = c("Confirmed Cases"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addLegend("bottomleft",
                data = tx_county_sf,
                pal = pal,
                values = ~incident_rate,
                title = "Incident Rate<br>(Per 100k People)",
                # labFormat = labelFormat(suffix = ""),
                opacity = 1) %>%
      # setView(31.9686, -99.9018, zoom = 6) %>% 
      fitBounds(-106.64585, 25.83706, -93.50782, 36.50045)
    
    map 
    
    
  })
  
}
shiny::shinyApp(ui, server)
