library(lubridate)

qtime <- as_datetime(1687553950303/1000, tz = "America/Los_Angeles")
formatted_time <- format(qtime, "%Y-%m-%d %I:%M:%S %p %Z")
print(formatted_time)
