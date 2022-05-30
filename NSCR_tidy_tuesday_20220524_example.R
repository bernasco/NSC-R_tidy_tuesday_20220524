# Tidy Tuesday May 24, 2022 
# Wim Bernasco, May 24, 2022
#   Acknowledgement: Franziska Yasrebi-de Kom corrected errors and provided
#                    fruitfull suggestions for improvement of the first draft.


# 1. Create a new folder on the computer where you run R, 
#    and store the two datafiles crime.csv and residents.csv 
#    in the folder.
#
# 2. Read both files from disk and assign each a name. 
#    Any name will do, but I suggest "crime" and " residents".
# 
# 3. Explore the structure of the files. How many rows (observations), 
#    how many columns (variables), what are the variable names? 
#    What are their types: Are they integers, characters, dates, 
#    factors, .? keys?
#   
# 4. Visualize (plot) the development of the population size of 
#    the Netherlands between January 2012 and April 2022, 
#    and do the same for the frequencies of residential burglary, 
#    bike theft and assault.
# 
# 5. Create a residential burglary rate by relating number of 
#    residential burglaries to the size of the population, and 
#    think about how you can adjust for different months having 
#    a different numbers of days (28-31). To do this, you will need 
#    to merge (join) the "crime" dataframe with the "residents" 
#    dataframe by year and month.
# 
# 6. Visualize the development of the three crime rates.
# 
# 7. What can we say about the development of crime since 
#    February 2020, relative to the developments between 2012 
#    and 2020? How can you quantify this with the data at hand?
#   
# 8. Do anything else with the data that you find fun or seems 
#    to make sense

# You will normally load libraries here at the top of your script,
# but in this example I will load libraries at the point where I need
# their functionality. This shows when and why they are
# needed.

# If your project is reasonably small and all files (data, scripts,
# and output files) can be stored in a single folder without
# creating confusion, setting a working folder is a good idea. All
# reading from and writing to disk will be done from and to this 
# folder.
setwd("X:/YYY/ZZZ") # for example: setwd("C:/Users/bernasco/crime_trends_NL")

# Note: In this script I will use the new "|>" piping symbol. 
# It is equivalent to "%>%" but has two advantages, which are
# (1) it is shorter (2 instead of 3 symbols), and
# (2) it does not require you to load a library, as it has been
#     built into base R

# 2. Read data using the read_csv function in package 'readr'
#    read_csv has a few advantages over read.csv but the latter
#    will also do. 

# read_csv is in the 'readr' library, so we load
#    readr first.
library(readr)     # read_csv function

# Note: in the live workshop we stumbled upon an error caused
#          by our assumption that "crimes.csv" and "population.csv"
#          had dates coded with dashes (like in "2022-05-24")  
#          but my Excel version had written it with slashes
#          (like in "2022/05/24"). Verify that after reading
#          the files, their 'date' column has type 'date',
#          not type 'character'. You can use 'glimpse' to
#          verify this.

crimes <- read_csv("crimes.csv")
population <- read_csv("population.csv")

# 3. Explore data

# First explore the crime data.
# How many observations and how many variables (rows and columns)?
crimes |> dim()        # dim(crimes)

# For glimpse, slice-sample and count, we need library dplyr
library(dplyr)

# Display the names and types of the variables, and show values
#   for the first few observations column-wise
# Note: verify that colmn date has type 'date'
crimes |> glimpse()

# Show the first few cases row-wise
crimes |> head()

# Show a random sample of 10 cases rather than the first ones
#   This will usually have more variation in values
crimes |> slice_sample(n=10) |> head()

# Frequency table of crime types
crimes |> count(crime_type)
# You will see we have data for 124 months:
# 10 full years (2012-2021) + 4 months (Jan-April 2022)

# Next explore the population data
population |> dim()
#Note: verify that column date has type 'date'
population |> glimpse()
population |> head()
population |> slice_sample(n=10) |> head()

# 4. Visualize (plot) population and crime development

# The ggplot function is in the ggplot2 library
library(ggplot2)

# Plot population as a scatterplot
  population |> 
    ggplot() + 
    geom_point(mapping = aes(x = date, y = population))
  # or as a line graph
  population |> 
    ggplot() + 
    geom_line(mapping = aes(x = date, y = population)) 
# Keep it simple first. You can finetune later.
  
# Plot burglary frequencies
  crimes %>%
    filter(crime_type == "burglary") |>
    ggplot() + 
    geom_line(mapping = aes(x=date, y=frequency)) 
# You should see a seasonal pattern with highs in Winter (nov-jan)
# and lows in summer 
  
# Plot bike theft
  crimes %>%
    filter(crime_type == "bike theft") |>
    ggplot() + 
    geom_line(mapping = aes(x = date, y = frequency)) 
# You should also see a seasonal pattern, but with peaks in autumn
# (dep-nov)
  
# Plot assault
  crimes %>%
    filter(crime_type == "assault") |>
    ggplot() + 
    geom_line(mapping = aes(x = date, y = frequency)) 
# The seasonal pattern for assault shows peaks in spring (apr-jun)    
  
# Plot the three crime types in a single graph
#   and distinguish them by color
  crimes |>
    ggplot() + 
    geom_line(aes(x = date, y = frequency, color = crime_type)) 

# Plot them in separate panels
  crimes |>
    ggplot() + 
    geom_line(aes(x = date, y = frequency)) +
    # scales = "free_y" allows each panel to have its own
    #  Y-axis scaling
    facet_wrap(facets = "crime_type", scales = "free_y", ncol = 1)

  # I would not advise it, but you can combine panels and color
  #   (if you do, hide the legend to prevent redundancy)
  crimes |>
    ggplot() + 
    geom_line(aes(x = date, y = frequency, color = crime_type)) +
    facet_wrap(facets = "crime_type", scales = "free_y", ncol = 1) +
    theme(legend.position="none")
  
# 5. Calculate corrected rate
  
  # merge crime and population by date (year and month)
  crime_population <- 
    # merge with crime type category labels
    left_join(crimes, population, by = "date")

  # To calculate the number of days in the month of a date,
  #   you can use the 'day_in_month' function which is in
  #   the 'lubridate' library
  
  # Divide monthly crime frequency by number of days in the
  #   month to obtain the daily crime frequency, and divide
  #   by 100,000 to obtain daily crime rates per 100,000 
  #   populatio. 
  # Further, I multiply the result by mean days per month 
  #  to obtain the MONTHLY crime rate, but this is arbitrary
  #  (daily, weekly, of annual rates would be fine as well)
  library(lubridate)
  crime_population_corrected <-
    crime_population |>
    mutate(
           # corrected for different month lengths
           rate = frequency / days_in_month(date) * 
             (365.25/12) / (population / 100000)) 
  
# 6. Visualize development of corrected crime rates
    crime_population_corrected |>
    ggplot() + 
    geom_line(aes(x=date, y=rate, color=crime_type)) +
    facet_wrap(facets = "crime_type", scales="free_y", ncol=1)
#  You will get this warning:
#    Warning message:
#    Removed 3 row(s) containing missing values    
      
# So where are these three missing values?
  crime_population_corrected |>
    # select observations with NA (missing) values on 'rate'
    filter(is.na(rate))
  # # A tibble: 3 x 5
  # date          frequency crime_type    population  rate
  # <date>            <dbl>  <chr>           <dbl>   <dbl>
  # 1 2022-04-01      1645   burglary          NA    NA
  # 2 2022-04-01      7096   bike theft        NA    NA
  # 3 2022-04-01      3416   assault           NA    NA  
  
# Aha! The population data of April 2022 were not yet online! 
#      This means we have valid data on both crime and population
#      Jan 2012 to March 2022
  

# 7. Crime before and after covid
  
# Let's look at burglary and start with making a plot that 
#    highlights the distinction between the periods before and 
#    during the pandemic
  
  
# First we create a new variable/column that contains an indicator
#   for whether the month is a pandemic month or not.

# Define the date the pandemic started (a single number of the type 'date')
covid_start <- as_date("2020-02-01")       

# Note:
#  The expression:
#    date >= covid_start
#  returns TRUE (1)  for 'covid_start' and all later dates, and
#  returns FALSE (0) for all dates before 'covid_start'
# The labels parameter assigns 'before' to 0 and 'during' to 1    
crime_population_corrected_covid <-  
  crime_population_corrected |>
  mutate(covid = factor(date >= covid_start , 
                        labels=c("before", "during")))
  
  
  
# We could just add a vertical line at the month that the pandemic
#   started (February 2022)  
  crime_population_corrected_covid |>
    filter(crime_type == "burglary") |>
    ggplot() + 
    geom_line(aes(x=date, y=rate)) +
    # add a red vertical line at given point on X-axis
    geom_vline(xintercept = covid_start, color="red")

# Alternatively, we could create a two-category variable indicating 
#   the before-after distinction, and then plot by color  
crime_population_corrected_covid |>
  filter(crime_type == "burglary") |>
  ggplot() + 
  geom_line(aes(x=date, y=rate, color=covid))

# A combined graph for the three crime types
crime_population_corrected_covid |>
  ggplot() + 
  geom_line(aes(x=date, y=rate, color=covid)) +
  facet_wrap(facets = "crime_type", scales="free_y", ncol=1)

# Note that  there is a 'hole' in de line graphs between January
#   2020 and February 2020. This is because we are actually drawing 
#   two separate line graphs here, one ending Jan 2022 and one 
#   starting Feb 2022. When we create a line graph, we drawing an
#   an individual point graph and then connect consecutive points
#   with lines. So there really is nothing between Jan 2020 and Feb 
#   2020.
# 
# As we think of time as continuous (by day, or even hour), we may
#   want to create a 'before covid' line that continues to Feb 2020.
#   One way to do this is to explicitly draw two line graphs in the
#   same plot. As you see this quickly becomes complicated ....
#
#   Note: I used "#F8766D" and "#00BFC4" to get the same colors that
#   ggplot uses by default when there are two categories in a 
#   discrete variable

crime_population_corrected |>
  mutate(precovid_rate = if_else(date <= as_date("2020-02-01"),
                                 rate, as.numeric(NA)),
         postcovid_rate =if_else(date >= as_date("2020-02-01"),
                                 rate, as.numeric(NA))) |>
  ggplot() + 
  geom_line(aes(x=date, y=precovid_rate), color="#F8766D") +
  geom_line(aes(x=date, y=postcovid_rate), color="#00BFC4") +
  xlab("rate") +
  facet_wrap(facets = "crime_type", scales="free_y", ncol=1)  


# Now let us quantify the trends. We first calculate annual rates 
#  as we are presently not interested in the seasonal variations.

annual_rates_changes <-
  crime_population_corrected |>
  # create a variable indicating the year
  mutate(year = year(date)) |>
  # select only years 2012-2021 because we have not full 2022 data
  filter(year < 2022) |>
  # for each year and crime type, calculate annual crime rate
  group_by(year, crime_type) |>
  summarize(annual_rate = sum(rate)) |>
  # Now calculate relative change: 
  #   By what proportion does this year's rate differ
  #   from previous year's rate?
  # We do this for each crime type separately
  group_by(crime_type) |>
  # sort by year 
  arrange(year) |>
  # Copy last year's rate
  mutate(lag_annual_rate = lag(annual_rate)) |>
  # Relative change
  # See, e.g. https://www.youtube.com/watch?v=muAkepkjpZI
  mutate(annual_change = ((annual_rate - lag_annual_rate) / lag_annual_rate))

# Just to check that what you did generates the intended result:
#   lag_annual_rate(2015) == annual_rate(2014)
#   lag_annual_rate(2014) == annual_rate(2013)
#   lag_annual_rate(2013) == annual_rate(2012)
#   lag_annual_rate(2012) == NA (because annual_rate(2011) is unknown)
annual_rates_changes |>
  filter(crime_type == "assault") |>
  head()

# Plot relative changes
annual_rates_changes |>
  ggplot() +
  geom_line(aes(x=year, y=annual_rate, color=crime_type))

# Define the covid years
covid_years <- c(2020, 2021)

# Compare annual change before and during the pandemic per crime type
annual_rates_changes %>%
  mutate(covid = factor(year %in% covid_years, labels = c("before", "during"))) |>
  group_by(crime_type, covid) |>
  summarize(mean_change = mean(annual_change, na.rm=TRUE))
# You can see that there is an overall decline for each crime type
#   (mean_change is always negative) and the decline is more 
#   pronounced (mean_change is more negative) during the pandemic 
#   than before the pandemic, in particular for assault and burglary
#
# For more rigorous statistical tests, we would need to dive into
#   time series analysis



