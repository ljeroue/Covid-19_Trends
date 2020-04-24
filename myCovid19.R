library(tidyverse) 

# GET DATA
# Pull data from my directory which has been pulled from Johns Hopkins Github
# Make column names consistant

fileDirectory <- "C:/LACEY/CODE/gitHub/Covid19/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"
dailyReports <- paste0(fileDirectory, dir(fileDirectory, pattern = "\\.csv$"))

deaths <- NULL
for(x in dailyReports){
  report <- read_csv(x)
  names(report) <- str_replace( names(report), "\\bLat\\b", "Latitude")
  names(report) <- gsub("Long_", "Longitude", names(report))
  names(report) <- gsub("Last Update", "Last_Update", names(report))
  names(report) <- gsub("Province/State", "Province_State", names(report))
  names(report) <- gsub("Country/Region", "Country_Region", names(report))
  report$Date <- str_sub(x, start = -14, end = -5)  # the 'last update' column is inconsitent
  deaths <- plyr::rbind.fill(deaths, report)
}


# Convert to tibble
deaths <- as_tibble(deaths)
deaths


# Add log deaths and cases
deaths <- deaths %>% 
  mutate(Deaths_log = log10(Deaths),
         Confirmed_log = log10(Confirmed))


# Format date
deaths <- deaths %>%
  mutate(Date = as.POSIXct(strptime(Date, format = "%m-%d-%Y")))


# State Lookup table
USState_Lookup <- tibble(Province_State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
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



# Remove '(From Diamond Princess)'
deaths$Province_State <- gsub("\\(From Diamond Princess\\)", "", deaths$Province_State)


# Standardize 'China'
deaths$Country_Region <- gsub("Mainland China", "China", deaths$Country_Region)



# Pull out US Counties
USCounty <- deaths %>% 
  filter(Country_Region == "US" &
         str_detect(Province_State, "County,")) 


# Remove County from main df becuase it was not reported consistantly
deaths <- deaths %>% 
  mutate(Remove = ifelse(Country_Region == "US" &
                          str_detect(Province_State, "County,"), 1, 0)) %>% 
  filter(Remove == 0) %>% 
  select(-Remove)


# Extract City info
USCity <- deaths %>% 
  filter(Country_Region == "US" &
         str_detect(Province_State, ", ")) %>% 
  mutate(State = unlist(lapply(str_split(Province_State, ", "), "[[", 2)),
         City = Province_State) %>% 
  select(-Province_State) %>% 
  left_join(USState_Lookup)
    

# Sum over cities
USState_sum <- USCity %>% 
  group_by(Province_State, Country_Region, Date) %>% 
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active))



# Add my calculated state totals to the main dataset
deaths <- deaths %>% 
  bind_rows(USState_sum)



# On 3/22 Hopkins switches format again, *shrug*
# city info is now stored in the Adimin2 column
# Sum over cities for state totals
USCity3_22 <- deaths %>% 
  filter(Country_Region == "US" &
           Date >= "2020-03-22")

USState3_22 <- USCity3_22 %>% 
  group_by(Province_State, Country_Region, Date) %>% 
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered))


# Replace all US city data after 3/21 with state data
deaths <- deaths %>% 
  mutate(Remove = ifelse(Country_Region == "US" &
                         Date >= "2020-03-22", 1, 0)) %>% 
  filter(Remove == 0) %>% 
  select(-Remove) %>% 
  bind_rows(USState3_22)


# Sum over Province_State to get country totals for US and China
USChina_totals <- deaths %>%
  filter(Country_Region %in% c("China", "US") &
           !str_detect(Province_State, ", ")) %>%  # remove city
  group_by(Date, Country_Region) %>%
  summarize(Confirmed = sum(Confirmed, na.rm = T),
            Deaths = sum(Deaths, na.rm = T),
            Recovered = sum(Recovered, na.rm = T),
            Active = sum(Active, na.rm = T)
            )


# Add US & China totals to main df
deaths <- bind_rows(deaths, USChina_totals)


# VISUALIZE THE CURVES!  ######################################################

# Visualize US
deaths %>% 
  filter(Country_Region == "US" &
           is.na(Province_State)) %>% 
  ggplot(aes(Date, Deaths, color = Country_Region)) +
  geom_line()


# Visualize top US States
deaths %>% 
  filter(Country_Region == "US" &
           Date > "2020-03-14" &
         Province_State %in% c("California", "Washington",  "Florida", 
                                   "Louisiana", "Connecticut", "New Jersey", "Michigan",
                               "Massachusetts", "Illinois", "New York", "Colorado")) %>% 
  ggplot(aes(Date, Deaths, color = Province_State)) +
  # ggplot(aes(Date, Deaths_log, color = Province_State)) +
  
  geom_line()


# Visualize top countries
deaths %>% 
  filter(Country_Region %in% c("China", "Italy", "US", "Korea, South", "Iran", 
                               "Spain", "Germany", "France") & 
           is.na(Province_State)) %>% 
  # ggplot(aes(Date, Deaths, color = Country_Region)) +
  ggplot(aes(Date, Deaths_log, color = Country_Region)) +
  
  geom_line()



