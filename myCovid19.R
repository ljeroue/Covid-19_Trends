library(tidyverse) 


# # Get today's date
# (today <- format(Sys.Date(), "%m/%d/%y"))
# today <- if(substr(today, 1, 1) == "0") substr(today, 2, nchar(today)) # remove leading zero in month

# # Pull data
# deaths <- read_csv("C:/LACEY/CODE/gitHub/Covid19/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

# Load the dataset directly from github
gitURL <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
deaths <- read_csv(gitURL)


# Convert to long dataset
deaths <- deaths %>% 
  gather(date, deaths, `1/22/20`:`3/21/20`)
deaths


# Format date
deaths <- deaths %>% 
  mutate(Date = as.POSIXct(strptime(date, format = "%m/%d/%y")))

typeof(deaths$Date)
deaths


# State Lookup table
USState_Lookup <- tibble(`Province/State` = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                                  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                                  "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                                  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                                  "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                  "New York", "North Carolina", "North Dakota", "Ohio",
                                  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                                  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                              State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                                       "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                       "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                                       "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
                                       "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# Add up all county data for each us state
# On 3/10, Johns Hopkins switched from reporting by county to reporting by state
TotalCounty <-
  deaths %>% 
  filter(`Country/Region` %in% c("US") &
           str_detect(`Province/State`, "County,")) %>%  # subset to just US Counties 
  mutate(State = unlist(lapply(str_split(`Province/State`, ", "), "[[", 2))) %>% # extract state
  group_by(`Country/Region`, State, Date) %>% 
  summarise(deaths = sum(deaths)) %>% 
  left_join(USState_Lookup)
  
# Add county totals to state totals
TotalState <- deaths %>% 
  filter(`Country/Region` %in% c("US") &
           !str_detect(`Province/State`, ",")) %>%  # remove all except just state (and ship) totals)
  bind_rows(TotalCounty) %>% 
  group_by(Date, `Country/Region`, `Province/State`) %>%
  summarize(deaths = sum(deaths))


# Replace original state totals, with newly calculated state total
deaths <- deaths %>% 
  mutate(stateCount = ifelse(`Country/Region` == "US" &  # disignate which are original state totals
                               !str_detect(`Province/State`, ","), 1, 0)) %>% 
  filter(stateCount == 0) %>%  # remove original state totals
  bind_rows(TotalState) %>% 
  select(-stateCount)


# Sum over states and provinces to get country totals for US and China
TotalCountry <- deaths %>% 
  filter(`Country/Region` %in% c("US", "China") &
           !str_detect(`Province/State`, ",")) %>%  # remove all except just state (and ship) totals)
  group_by(Date, `Country/Region`) %>%
  summarize(deaths = sum(deaths))

# Add country totals to main df
deaths <- bind_rows(deaths, TotalCountry)



# What do we have just for USA? -includes states, counties and total
deaths %>% 
  filter(`Country/Region` == "US")

# Total in the US
deaths %>% 
  filter(`Country/Region` == "US" &
           is.na(`Province/State`))


# Just US Counties?
deaths %>% 
  filter(`Country/Region` == "US" &
           str_detect(`Province/State`, "County"))


# What do we have for the state of CA
deaths %>% 
  filter(`Country/Region` == "US" &
           `Province/State` == "California")


# What do we have for just Italy?
deaths %>% 
  filter(`Country/Region` == "Italy")

# What do we have for just South Korea?
deaths %>% 
  filter(`Country/Region` == "Korea")

# What about for china - many province/states
deaths %>% 
  filter(`Country/Region` == "China")


# Visualize US
deaths %>% 
  filter(`Country/Region` == "US" &
           is.na(`Province/State`)) %>% 
  ggplot(aes(Date, deaths, color = `Country/Region`)) +
  geom_line()


# Visualize top US States
deaths %>% 
  filter(`Country/Region` == "US" &
           `Province/State` %in% c("California", "Washington", "New York")) %>% 
  ggplot(aes(Date, deaths, color = `Province/State`)) +
  geom_line()


# Visualize top countries
deaths %>% 
  filter(`Country/Region` %in% c("China", "Italy", "US", "Korea, South", "Iran", "Spain") & 
           is.na(`Province/State`)) %>% 
  ggplot(aes(Date, deaths, color = `Country/Region`)) +
  geom_line()


