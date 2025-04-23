# Cristina's Scratchpad 
# Data Exploration and Cleaning
f <- "Data/Crime_Incidents_in_2024.csv"
d <- read_csv(f, col_names = TRUE)


#Step 1: Clean Names of Columns and Select Only Relevant Columns {janitor}
d1 <- d %>%
  clean_names() %>%
  select("report_dat", "shift", "offense", "block", "neighborhood_cluster", "latitude", "longitude")


#Step 2: Combining various offense types to have a few clear categories and formatting naming conventions for data rows
  # Step 2.A: Understanding the unique offense and shift types
offences <- tibble(
  Offences = unique(d1$offense)
)
shifts <- tibble(
  shifts = unique(d1$shift)
)

  # Step 2.B: Combine all types of Theft and format names for Offenses and Shifts {stringr}
d2 <- d1 %>%
  mutate(
    offense = case_when(
      str_detect(tolower(offense), "theft") ~ "Theft",
      str_detect(tolower(offense), "sex abuse") ~ "Sex Abuse",
      str_detect(tolower(offense), "robbery") ~ "Robbery",
      str_detect(tolower(offense), "homicide") ~ "Homicide",
      str_detect(tolower(offense), "assault") ~ "Assault",
      str_detect(tolower(offense), "burglary") ~ "Burglary",
      str_detect(tolower(offense), "arson") ~ "Arson"
      ),
    shift = case_when(
      str_detect(tolower(shift), "day") ~ "Day",
      str_detect(tolower(shift), "midnight") ~ "Midnight",
      str_detect(tolower(shift), "evening") ~ "Evening"
      )
    )


#Step 3: Format columns to the correct class (report_dat to date format and others to factor class)
  # Step 3.A: looking at the class of each variable:
classes <- tibble(
  column = names(d2),
  class = sapply(d2, class)
)

  # Step 3.B: Formatting columns to correct class
d3 <- d2 %>%
  mutate(
    report_dat = gsub("\\+00$", "+0000", report_dat),
    report_dat = as.POSIXct(report_dat, format = "%Y/%m/%d %H:%M:%S%z", tz = "UTC"),
    shift = as.factor(shift),
    offense = as.factor(offense),
    neighborhood_cluster = as.factor(neighborhood_cluster)
    )


#Step 4: Removing Duplicates
  # Step 4.A: Looking for duplicates
duplicates <- d3 %>%
  count(report_dat, shift, offense, block, neighborhood_cluster, latitude, longitude, sort = TRUE) %>%
  filter(n > 1)
      # 5 rows appear to be duplicated once or more than once. Total of 11 rows of duplicate data. Keep one of each for a total of 6 removed rows.

  # Step 4.B: Remove Duplicates
d4 <- d3 %>%
  group_by(report_dat, shift, offense, block, neighborhood_cluster, latitude, longitude) %>%
  slice(1) %>%
  ungroup()
  
  # Step 4.C: Check to make sure there are no more duplicates
dup_check <- d4 %>%
  count(report_dat, shift, offense, block, neighborhood_cluster, latitude, longitude, sort = TRUE) %>%
  filter(n > 1)
     # empty tibble = duplicates removed


# Step 5: Remove outliers in coordenate and dates
  # Step 5.A: Check for geographic outliers
geo_full_plot <- ggplot(d4, aes(x = longitude, y = latitude)) +
  geom_point()
      # Visually, all points fall within the map boundary of Washington DC.
  
  # Step 5.B: Remove statistical outliers
d5 <- d4 %>%
  filter(
    between(latitude,
            quantile(latitude, 0.25) - 1.5 * IQR(latitude),
            quantile(latitude, 0.75) + 1.5 * IQR(latitude)
    ),
    between(longitude,
            quantile(longitude, 0.25) - 1.5 * IQR(longitude),
            quantile(longitude, 0.75) + 1.5 * IQR(longitude)
    )
  )

  # Step 5.C: Check new distribution
geo_stat_plot <- ggplot(d5, aes(x = longitude, y = latitude )) +
  geom_point()
  

  # Step 5.D: Check for date outliers
out_date <- range(d4$report_dat)
      # The range ends with 2025 dates.  

  # Step 5.E: Filtering to remove any dates that do not belong to 2024.
d6 <- d5 %>%
  filter(
    report_dat >= as.POSIXct("2024-01-01 00:00:00", tz = "UTC") &
    report_dat <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
    )


# Step 6: Create columns for "month" and "day of the week" from the report_dat column {lubridate}
d7 <- d6 %>%
  mutate(
    month = month(report_dat, label = TRUE, abbr = FALSE),
    week_day = wday(report_dat, label = TRUE, abbr = FALSE)
  )

# Step 7: Reorder columns 
d7 <- d7 %>%
  select(
    "report_dat", "month", "week_day", "shift", "offense", "block", "neighborhood_cluster", "latitude", "longitude"
  )

# Step 8: Export as new .csv file {readr}
write_csv(d7, "Data/Clean_CrimeData_2024.csv")


