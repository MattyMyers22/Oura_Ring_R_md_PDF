# Oura Ring Analysis Dashboard

# Matthew Myers

# load csv of choice
data <- read.csv(file.choose())

# assess structure of data
str(data)

# packages
library(lubridate) #load lubridate for working with dates
library(dplyr) #dplyr for manipulated data
library(ggplot2) #for visualizations

# convert date to date type
data$date <- as.Date(data$date, format="%m/%d/%Y")

# select desired columns
sel_data <- data %>%
  select(date, Total.Sleep.Duration, REM.Sleep.Duration, Light.Sleep.Duration, 
         Deep.Sleep.Duration, Sleep.Efficiency, Average.Resting.Heart.Rate,
         Lowest.Resting.Heart.Rate, Average.HRV, Respiratory.Rate, 
         Activity.Burn, Steps, Inactive.Time, Long.Periods.of.Inactivity)

# check structure of selected dataframe
str(sel_data)

# seconds to hours function
sec_to_hour <- function(x) x/3600

# apply seconds to hours function to time duration columns
sel_data[c('Total.Sleep.Duration', 'REM.Sleep.Duration', 'Light.Sleep.Duration',
           'Deep.Sleep.Duration', 'Inactive.Time')] <- lapply(sel_data[
             c('Total.Sleep.Duration', 'REM.Sleep.Duration', 
               'Light.Sleep.Duration', 'Deep.Sleep.Duration', 'Inactive.Time')],
             sec_to_hour)

# create % of hours slept for each sleep cycle columns
sel_data$perc_REM <- sel_data$REM.Sleep.Duration / 
  sel_data$Total.Sleep.Duration * 100

sel_data$perc_Light <- sel_data$Light.Sleep.Duration / 
  sel_data$Total.Sleep.Duration * 100

sel_data$perc_Deep <- sel_data$Deep.Sleep.Duration / 
  sel_data$Total.Sleep.Duration * 100

# check descriptive statistics of sel_data
summary(sel_data)

# create quarter with year column
sel_data$qtr_yr <- paste(quarters(sel_data$date), year(sel_data$date))

# initialize empty vector for ordered quarter and year pairs
factored_qtrs <- c()

# nested for loops to create ordered vector of quarter + year
for (yr in unique(year(sel_data$date)))
{
  
  for (qtr in unique(quarters(sel_data$date)))
  {
    factored_qtrs = c(factored_qtrs, paste(qtr, yr))
  }
}

# set qtr_yr as a factor and set factor levels
sel_data$qtr_yr <- as.factor(sel_data$qtr_yr)
sel_data$qtr_yr <- factor(sel_data$qtr_yr, levels = factored_qtrs)

# look at total sleep over time
ggplot(sel_data, aes(qtr_yr, Total.Sleep.Duration)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(Total.Sleep.Duration, na.rm=TRUE)), 
             linetype = "dashed") +
  labs(title = "Total Sleep Duration by Quarters", x = "Quarter by Year",
       y = "Total Sleep in Hours",
       caption = paste("Dashed line represents the average time slept of",
                       round(mean(sel_data$Total.Sleep.Duration, na.rm=TRUE), 
                             digits = 2), "hours."))

# create dataframe for REM cycle stats
rem_sleep <- sel_data %>%
  select(date, qtr_yr, REM.Sleep.Duration, perc_REM) %>%
  mutate(cycle = 'REM') %>%
  rename(cycle_duration = REM.Sleep.Duration, perc_duration = perc_REM)

# create dataframe for light cycle stats
light_sleep <- sel_data %>%
  select(date, qtr_yr, Light.Sleep.Duration, perc_Light) %>%
  mutate(cycle = 'Light') %>%
  rename(cycle_duration = Light.Sleep.Duration, perc_duration = perc_Light)

# create dataframe for deep cycle stats
deep_sleep <- sel_data %>%
  select(date, qtr_yr, Deep.Sleep.Duration, perc_Deep) %>%
  mutate(cycle = 'Deep') %>%
  rename(cycle_duration = Deep.Sleep.Duration, perc_duration = perc_Deep)

# union all from sleep cycle stat df's into one dataframe
sleep_cycles <- union_all(rem_sleep, deep_sleep)

# create dataframe of sleep cycle averages by qtr
avg_qtr_cycles <- sleep_cycles %>% 
  group_by(cycle, qtr_yr) %>%
  summarize(avg_cycle_duration = mean(cycle_duration, na.rm = TRUE),
            avg_perc_duration = mean(perc_duration, na.rm = TRUE))

# visualize percentage of sleep in each sleep cycle
ggplot(avg_qtr_cycles, aes(qtr_yr, avg_cycle_duration, fill = cycle)) +
  geom_col(position = 'dodge') +
  labs(title = "Average Nightly Sleep Cycle Duration by Quarter",
       x = "Quarter by Year", y = "Duration in Hours", caption = 
       "As per the Oura ring app, on average the optimal amount of REM sleep 
       hours starts with 1.5. For Deep sleep, it is 1 - 1.5 hours. Both are 
       expected to decrease with age.")

# view mean sleep efficiency by quarter
sel_data %>%
  group_by(qtr_yr) %>%
  summarize(avg_sleep_eff = mean(Sleep.Efficiency, na.rm = TRUE))

# look at avg HRV over time
ggplot(sel_data, aes(x = qtr_yr, y = Average.HRV)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(Average.HRV, na.rm=TRUE)), 
             linetype = "dashed") +
  labs(title = "Average HRV by Quarters", x = "Quarter by Year",
       y = "Average HRV at Night",
       caption = paste("Dashed line represents overall average HRV of",
                       round(mean(sel_data$Average.HRV, na.rm=TRUE), 
                             digits = 2)))

# steps over time
ggplot(sel_data, aes(date, Steps)) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Steps, na.rm=TRUE)), 
             linetype = "dashed") +
  labs(title = "Daily Steps Over Time", x = "Date",
       y = "Daily Steps",
       caption = paste("Dashed line represents overall average daily steps of",
                       round(mean(sel_data$Steps, na.rm=TRUE), 
                             digits = 2)))

# Inactive Time by Date
ggplot(sel_data, aes(date, Inactive.Time)) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Inactive.Time, na.rm=TRUE)), 
             linetype = "dashed") +
  labs(title = "Daily Inactive Hours by Date", x = "Date",
       y = "Inactive Hours",
       caption = paste("Dashed line represents overall average daily 
                       inactive hours of", round(mean(sel_data$Inactive.Time, 
                                                   na.rm=TRUE), digits = 2)))

# Get data for past 3 months
past_3_months <- sel_data %>%
  filter(date >= max(date) - months(3))