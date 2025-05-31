# This code is to create datasets for movement visulation in QGIS
# Select data point per week from each collar

#Use this in the 2025 analysis

library(sf)
library(amt)
#library(dplyr) #There is a conflict with amt select if dply is loaded
library (lubridate)

library(ggplot2)



setwd("D:/FERIT_1/Webequie Access/R/Caribou/data")
dfCollarData_wRange <- st_read("dfCollarData_3161_wRanges.gpkg")

# Track data function
getTrackData <- function (inData, sampleRate) {
  trk <- mk_track(inData, x, y, t, id = AnimID, crs = 3161,  AnimID = AnimID,
                  Source = Source, Location = Location, Season=Season, Range=Range, geometry=geom)
  # Nest data by id to account for multiple animals
  trk_N <- trk |> amt::nest(data = -"id")
  #Make Tracks by burst for Winter Season
  trk_N_RS  <- trk_N |>
    mutate(steps = map(data, function(x)
      x |> track_resample(rate = hours(sampleRate), tolerance = minutes(20)) |> steps_by_burst()))
  trk_UN_RS <- trk_N_RS |> select(id, steps) |> unnest(cols = steps)
  # Subset data by step length.  See comments above for rationale
  # Track, winter, unnested, random sample, subsetted
 # trk_UN_RS_sub <- trk_UN_RS |> filter(sl_ > 150 & sl_ < 1200)
  #trl_UN_RS_Sub$Season <- Seas
}


track36hr <- getTrackData(dfCollarData_wRange,36)
sftrack36hr = st_as_sf(track36hr, coords = c("x1_", "y1_"), crs = 3161)
#write_sf(dfCollarData, "dfCollarData_3161.gpkg", delete_layer = TRUE)

write_sf(sftrack36hr, "sftrack36hr.shp", delete_layer = TRUE)
dfMissOz <- dfCollarData_wRange |> filter(Source == "AtkinsRealis" | Source == "MNRF" )
trkMissOz36hr <- getTrackData(dfMissOz,36)
write_sf(trkMissOz36hr, "trkMissOz36hr.shp", delete_layer = TRUE)

trkMissOz168hr <- getTrackData(dfMissOz,168)
sftrkMissOz168hr = st_as_sf(trkMissOz168hr, coords = c("x1_", "y1_"), crs = 3161)
write_sf(sftrkMissOz168hr, "trkMissOz168hr.shp", delete_layer = TRUE)


trackAll168hr <- getTrackData(dfCollarData_wRange,168)
sftrackAll168hr = st_as_sf(trackAll168hr, coords = c("x1_", "y1_"), crs = 3161)

write_sf(sftrackAll168hr, "trkAll168hr.shp", delete_layer = TRUE)
getwd()

#############################################
#Step length analysis
setwd("D:/FERIT_1/Webequie Access/R/Caribou/data")
dfCollarData_wRange <- st_read("dfCollarData_3161_wRanges.gpkg")
dfMissOz <- dfCollarData_wRange |> filter(Source == "AtkinsRealis" | Source == "MNRF" )
trkMissOz24hr <- getTrackData(dfMissOz,24)

# Calculate the daily average step length
avg_sl_by_day <- trkMissOz24hr %>%
  mutate(JulianDay = yday(as.Date(t1_))) %>%
  group_by(JulianDay) %>%
  summarize(avg_sl = mean(sl_, na.rm = TRUE))


# Define the dates
dates <- as.Date(c("2024-02-09", "2024-03-21","2024-05-01", "2024-06-09", "2024-09-09", "2024-10-19" ))
dates <- as.Date(c("2024-02-09", "2024-02-10","2024-03-21", "2024-05-01", "2024-06-09", "2024-7-23", "2024-8-31", "2024-9-9", "2024-10-19" ))
dates <- as.Date(c("2024-02-09", "2024-03-21", "2024-05-01", "2024-06-09", "2024-7-23", "2024-8-31", "2024-9-9", "2024-10-19" ))

# Calculate the day of the year for each date
day_of_year <- as.numeric(format(dates, "%j"))
# Print the day of the year for each date
for (i in 1:length(dates)) {
  cat("Date:", dates[i], "- Day of year:", day_of_year[i], "\n")
}

for (i in 1:length(dates)) {
  cat("Date:", dates[i], "- Month of year:", months[i], "\n")
}


# Define the start of each season (adjust these as necessary for your specific context)
season_starts <- c(Spring = 80, Summer = 172, Fall = 264, Winter = 355)
season_starts <- c(Spring = 105, Summer = 173, Fall = 265, Winter = 355)
season_starts <- c(Spring = 80, Summer = 150, Fall = 275, Winter = 355)

season_starts <- c(S1=1, S2 = 40, S3 = 81, S4 = 122, S5 = 161, S6 = 205, S7 = 244, S8 = 253, S9 = 293)


# Create the plot
p <- ggplot(avg_sl_by_day, aes(x = JulianDay, y = avg_sl)) +
  geom_line() +  # Plot average step lengths
  geom_vline(xintercept = season_starts, col = "blue", linetype = "dashed") +  # Marking seasons
  scale_x_continuous(
    name = "Julian Day",
    breaks = c(1, season_starts),
   # labels = c("Jan 1", names(season_starts))
    labels = c("S1", names(season_starts))
  ) +
  scale_y_continuous(name = "Average Step Length (sl_)") +
  theme_minimal()

# Print the plot
print(p)

##########################

# Pre-compute month starts (non-leap year)
days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
month_starts  <- c(1, 1 + cumsum(days_in_month)[-12])
# month_starts: 1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335



p <- ggplot(avg_sl_by_day, aes(x = JulianDay, y = avg_sl)) +
  geom_line() +


  # vertical lines at month boundaries
  geom_vline(xintercept = month_starts,
             linetype   = "dashed",
             color      = "grey50") +

  # primary axis: month names; secondary axis: JulianDay numbers
  scale_x_continuous(
    breaks   = month_starts,
    labels   = month.abb,
    expand   = expansion(add = c(0, 0)),
    sec.axis = sec_axis(
      ~ .,
      name   = "Julian Day",
      breaks = month_starts,
      labels = month_starts
    )
  ) +

  labs(
    x     = "Month",
    y     = "Average SL",
    title = "Daily Average SL by Julian Day"
  ) +

  theme_minimal() +
  theme(
    axis.ticks.x     = element_blank(),            # remove bottom ticks
    axis.text.x      = element_text(angle = 45,    # tilt month labels
                                    hjust = 1),
    axis.title.x.top = element_text(margin = margin(b = 5))  # space for top title
  )


plot (p)


#










season_starts <- c(Spring = 105, Summer = 173, Fall = 265, Winter = 355)
season_data <- data.frame(
  JulianDay = season_starts,
  Label = names(season_starts)
)


p <- ggplot(avg_sl_by_day, aes(x = JulianDay, y = avg_sl)) +
  geom_line() +  # Plot average step lengths
  geom_vline(xintercept = season_starts, col = "blue", linetype = "dashed") +  # Marking seasons
  scale_x_continuous(
    name = "Julian Day",
    breaks = c(1, seq(10, 365, by = 10)),  # Add every 10 Julian Days
    labels = c("Jan 1", rep("", length(seq(10, 355, by = 10))))  # Empty labels for regular days, keeping season names
  ) +
  scale_y_continuous(name = "Average Step Length (sl_)") +
  theme_minimal()

print(p)



p <- ggplot(avg_sl_by_day, aes(x = JulianDay, y = avg_sl)) +
  geom_line() +  # Plot average step lengths
  geom_vline(data = season_data, aes(xintercept = JulianDay), col = "blue", linetype = "dashed") +  # Marking seasons
  geom_text(data = season_data, aes(x = JulianDay, y = Inf, label = Label), vjust = -0.5, angle = 90, color = "blue") +  # Adding season labels
  scale_x_continuous(
    name = "Julian Day",
    breaks = c(1, season_starts),
    labels = c("Jan 1", names(season_starts))
  ) +
  scale_y_continuous(name = "Average Step Length (sl_)") +
  theme_minimal()

print(p)






# Assuming 'season_starts' is a named vector with Julian days for season starts
# Create a combined vector of breaks
all_breaks <- sort(c(1, seq(10, 365, by = 10), unname(season_starts)))

# Create corresponding labels, starting with 'Jan 1' and then empty labels for each 10-day interval
all_labels <- c("Jan 1", rep("", length(seq(10, 355, by = 10))))

# Now add the names of the seasons to the labels at the correct positions
for (season in names(season_starts)) {
  index <- which(all_breaks == season_starts[season])
  all_labels[index] <- season
}

# Now plot with corrected breaks and labels
p <- ggplot(avg_sl_by_day, aes(x = JulianDay, y = avg_sl)) +
  geom_line() +  # Plot average step lengths
  geom_vline(xintercept = season_starts, col = "blue", linetype = "dashed") +  # Marking seasons
  scale_x_continuous(
    name = "Julian Day",
    breaks = all_breaks,
    labels = all_labels
  ) +
  scale_y_continuous(name = "Average Step Length (sl_)") +
  theme_minimal()

# Print the plot
print(p)


