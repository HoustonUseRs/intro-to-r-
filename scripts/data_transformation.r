
# Load library ------------------------------------------------------------

library(nycflights13)
library(tidyverse)
library(janitor)

# Introduce data ----------------------------------------------------------

# introduce tibble format
flights
?flights

# Dplyr basics ------------------------------------------------------------

# filter() ----
flights %>% 
            filter(month == 1, day == 1)

# flights in Nov or Dec
flights %>% 
  filter(month == 11 | month == 12)

flights %>% 
  filter(month %in% c(11, 12))

# flights NOT in Nov or Dec
flights %>% 
  filter(!(month == 11 | month == 12))

# find missing values
flights %>% 
  filter(is.na(year))

flights %>% 
  filter(is.na(dep_time))

# EXERCISES: filtering ----
# 1. Find flights with arrival delays of more than 2 hrs
flights %>% 
  filter(arr_delay > 120)
# 2. Find flights that flew to Houston (IAH or HOU).
flights %>% 
  filter(dest == 'HOU' | dest == 'IAH')

flights %>% 
  filter(dest %in% c('HOU', 'IAH'))
# 3. Flights operated by United, American, or Delta
flights %>% 
  filter(carrier %in% c('UA', 'AA', 'DL'))
# 4. Flights departed in summer (June, July, August)
flights %>% 
  filter(between(month, 6, 8))
# 5. Delayed for at least an hour, but made up over 30 minutes in flight
flights %>% 
  filter(dep_delay >= 60,
         arr_delay <= -30)
# not sure if this is correct!

# 7. Departed between midnight and 6 am (inclusive).
flights %>% 
 filter(between(dep_time, 1, 600) | dep_time == 2400)


# arrange() ----

flights %>% 
  arrange(year, month, day)

flights %>% 
  arrange(desc(dep_delay))

#EXERCISES: arrange ----
# 1. Use arrange to sort all missing values to the start? (Use is.na())
# not sure how to answer this one

# 2. Sort flights to find fastest flights
flights %>% 
  arrange(air_time) %>% 
  select(carrier, flight, air_time)

# 3. Which flights traveled the longest? Which traveled the shortest?
flights %>% 
  arrange(desc(distance)) %>% 
  select(carrier, flight, origin, dest, distance)

flights %>% 
  arrange(distance) %>% 
  select(carrier, flight, origin, dest, distance)

# select() ----
flights %>% 
  select(year, month, day)

flights %>% 
  select(year:day)

flights %>% 
  select(-(year:day))

flights %>% 
  select(starts_with('dep'))

flights %>% 
  select(ends_with('time'))

flights %>% 
  select(contains('dep'))

# renaming variables with select() and rename()
flights %>% 
  select(departure_delay = dep_delay)

flights %>% 
  rename(departure_delay = dep_delay)

flights %>% 
  select(time_hour, air_time, everything()) # useful for rearranging columns

# mutate() ----
# used to create new variables
# added to end of dataset
flights_sml <- flights %>% 
  select(
    year:day,
    ends_with('delay'),
    distance,
    air_time
  )

flights_sml %>% 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time *
  )

flights_sml %>% 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gains_per_hour = gain / hours
  )

# can use transmute() if you only want to keep the variables being created

# EXERCISES: mutate() ----
# 1. Compare air_time with arr_time - dep_time. 
# What do you expect to see? What do you see? What needs to be fixed?
flights %>% 
  transmute(
    reported_air_time = air_time,
    calculated_air_time = arr_time - dep_time
  )

# arr_time and dep_time are in local tz

# 2. Compare dep_time, sched_dep_time, dep_delay.
flights %>% 
  select(contains('dep')) %>% 
  mutate(
    dep_delay_2 = dep_time - sched_dep_time
  )

# rememeber that dep_time is in hours and minutes
# need to convert them to more meaningful date types 

# summarize() ----
# collapses data into a single row

flights %>% 
  summarize(avg_delay = mean(dep_delay, na.rm = T))

flights %>% 
  group_by(year, month, day) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = T))

# visualize average delays by day
flights %>% 
  mutate(
    month2 = ifelse(nchar(month) != 2, 
                    str_pad(month, width = 2, side = 'left', pad = '0'),
                    month),
    day2 = ifelse(nchar(day) != 2,
                  str_pad(day, width = 2, side = 'left', pad = '0'),
                  day),
    date = as.Date(paste(year, month2, day2, sep = '-'))
  ) %>% 
  group_by(date) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = T)) %>% 
  ggplot(aes(x = date, y = avg_delay)) +
    geom_jitter() +
    geom_line()

# visualize average flight delays by destination and distance
flights %>% 
  group_by(dest) %>% 
  summarise(count = n(),
            avg_dist = mean(distance, na.rm = T),
            avg_delay =  mean(dep_delay, na.rm = T)) %>% 
  filter(count > 20,
         dest != 'HNL') %>% 
  ggplot(aes(x = avg_dist, avg_delay)) +
    geom_point(aes(size = count), alpha = .30) +
    geom_smooth(se = F)

# measures of spread to use with group_by() and mutate()
# sd()
# IQR()
# mad()

# measures of rank
# min()
# max()
# quantile()

# measures of position 
# fist()
# last()
# nth()

# EXERCISE: summarize()
# 1. Find the first and last departure time for each day
flights %>% 
  filter(!is.na(dep_time)) %>% 
  group_by(year, month, day) %>% 
  summarize(first_dep = first(dep_time),
            last_dep = last(dep_time))
)

# 2. Which destinations have the most carriers?
flights %>% 
  group_by(dest) %>% 
  summarize(no_carriers = n_distinct(carrier)) %>% 
  arrange(desc(no_carriers))

# 3. Which destinations have the most arrivals?
flights %>% 
  filter(!is.na(dep_time)) %>% 
  count(dest) %>% 
  arrange(desc(n))

# 4. Find the number of flights per month.
flights %>% 
  filter(!is.na(dep_time)) %>% 
  group_by(month) %>% 
  count() %>% 
  arrange(desc(n))

# 5. Look at the number of cancelled flights per day. Is the proportion of cancelled flights related to the average delay?
flights %>% 
  mutate(cancelled = ifelse(is.na(dep_time), T, F)) %>% # flag cancelled flights
  group_by(year, month, day) %>% 
  summarise(no_cancelled = sum(cancelled),
            n_flights = n(),
            prop_cancelled = no_cancelled / n_flights,
            avg_delay = mean(dep_delay, na.rm = T)) %>% 
  # arrange(desc(prop_cancelled)) %>% 
  print()

# spread() and gather() ----
# separate() and unite() ----

# Relational Data ---------------------------------------------------------

airlines %>% glimpse()
airports %>% glimpse()
planes %>% glimpse()
weather %>% glimpse()

# primary key vs foreign key
planes$tailnum # primary key: uniquely identifies each plane in the table
flights$tailnum # foreign key: uniquely identifies an observation in the planes table

# mutating joins: left_join() and inner_join() ----

# left join
flights %>% 
  left_join(airports, by = c('origin' = 'faa')) %>% 
  select(origin, name) %>% 
  # distinct() %>% 
  print()


flights %>% 
  left_join(planes, by = 'tailnum', suffix = c('.flights', '.planes'))

# inner join
# returns all rows from x where there are matching values in y, and all columns from x and y
# will return all combinations if multiple matches are present
flights %>% 
  inner_join(airports, by = c('origin' = 'faa')) %>% 
  count(origin)

airports %>% 
  inner_join(flights, by = c('faa' = 'origin')) %>% 
  count(name)

# outer joins: left_join(), right_join(), full_join() ----

# full join
# returns all columns from x and y, fills in missing valus with NA 
flights %>% 
  full_join(airports, by = c('origin' = 'faa')) %>% 
  count(origin)

# filtering joins: semi_join() and anti_join() ----

# semi join
# returns all rows of x where there are matching values in y, keeping only the columns from x
flights %>% 
  semi_join(airlines, by = 'carrier') %>% 
  count(carrier)

airlines %>% # airlines that appear in the flights dataset
  semi_join(flights, by = 'carrier') %>% 
  count(name)


# anti join
# returns all rows of x that do not have a match in y, keeping only the columns in x
airlines %>% 
  anti_join(flights, by = 'carrier') %>% 
  count(name)

airports %>% 
  anti_join(flights, by = c('faa' = 'origin')) %>% 
  count(name)

# EXERCISES: relational data ----
# 1. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
  
  airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# 2. What does it mean for a flight to have a missing tailnum? What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)

# 3. Filter flights to only show flights with planes that have flown at least 100 flights.

# 4. Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

# 5. What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
  
  