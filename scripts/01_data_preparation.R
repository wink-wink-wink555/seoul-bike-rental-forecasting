raw_path <- path_for("data", "raw", "SeoulBikeData.csv")
if (tolower(unname(tools::md5sum(raw_path))) != "bf632307dcb419f67df589faaa0318e1")
  stop("The official frozen source checksum changed. Do not reuse cached experiments with another dataset.")
raw <- utils::read.csv(raw_path, fileEncoding = "latin1", check.names = FALSE)
stopifnot(ncol(raw) == 14L, nrow(raw) == 8760L)
original_names <- names(raw)
names(raw) <- c("date", "count", "hour", "temperature", "humidity", "wind_speed", "visibility",
  "dew_point", "solar_radiation", "rainfall", "snowfall", "season", "holiday", "functioning_day")
d <- raw
d$date <- as.Date(d$date, "%d/%m/%Y")
d$timestamp <- as.POSIXct(paste(d$date, sprintf("%02d:00:00", d$hour)), tz = config$timezone)
d <- d[order(d$timestamp), ]
d$index <- seq_len(nrow(d))
d$operating <- as.integer(d$functioning_day == "Yes")
d$holiday_flag <- as.integer(d$holiday == "Holiday")
d$weekday <- as.integer(format(d$date, "%u"))
d$weekend <- as.integer(d$weekday >= 6)
d$month <- format(d$date, "%Y-%m")
d$split <- ifelse(d$date <= config$initial_end, "initial_training",
  ifelse(d$date <= config$validation_end, "validation", "test"))
stopifnot(!anyNA(d), !anyDuplicated(d$timestamp), all(diff(as.numeric(d$timestamp)) == 3600),
  all(d$count >= 0), all(d$count == floor(d$count)), all(d$hour %in% 0:23),
  all(d$count[d$operating == 0] == 0), sum(d$count == 0) == 295,
  all(d$count[d$operating == 1] > 0), min(d$date) == as.Date("2017-12-01"),
  max(d$date) == as.Date("2018-11-30"))
export <- d
export$timestamp <- format(d$timestamp, "%Y-%m-%d %H:%M:%S", tz = config$timezone)
write_csv(export, "data", "processed", "seoul_hourly.csv")
units <- c("date", "rentals/hour", "hour of day", "degrees C", "%", "m/s", "10 m",
  "degrees C", "MJ/m2", "mm", "cm", "category", "category", "category")
write_csv(data.frame(original_name = original_names, clean_name = names(raw), unit = units,
  role = c("time", "target", "calendar", rep("observed_weather", 8), "calendar", "calendar",
    "retrospective_operating_status")), "results", "variable_dictionary.csv")
write_csv(data.frame(check = c("rows", "columns_raw", "missing_cells", "duplicate_timestamps",
  "non_hourly_steps", "zeros", "non_operating_hours", "operating_zeros", "raw_md5"),
  value = c(nrow(d), ncol(raw), sum(is.na(d)), sum(duplicated(d$timestamp)),
    sum(diff(as.numeric(d$timestamp)) != 3600), sum(d$count == 0), sum(d$operating == 0),
    sum(d$count == 0 & d$operating == 1), unname(tools::md5sum(raw_path)))),
  "results", "data_quality.csv")
closed <- which(d$operating == 0)
gr <- split(closed, cumsum(c(TRUE, diff(closed) != 1)))
write_csv(do.call(rbind, lapply(gr, function(i) data.frame(start = d$timestamp[min(i)],
  end = d$timestamp[max(i)], hours = length(i)))), "results", "closure_intervals.csv")
splits <- do.call(rbind, lapply(split(d, d$split), function(g) data.frame(split = g$split[1],
  start = min(g$timestamp), end = max(g$timestamp), n = nrow(g), days = length(unique(g$date)),
  closure_hours = sum(g$operating == 0), closure_percent = 100 * mean(g$operating == 0))))
write_csv(splits, "results", "split_information.csv")
daily <- do.call(rbind, lapply(split(d, d$date), function(g) data.frame(date = g$date[1],
  rentals = sum(g$count), operating_hours = sum(g$operating), temperature = mean(g$temperature),
  rainfall = sum(g$rainfall), split = g$split[1])))
write_csv(daily, "data", "processed", "seoul_daily.csv")
message("Validated 8760 consecutive hours; 295 structural zero records retained.")
