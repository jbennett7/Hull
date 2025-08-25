suppressMessages(require(fredr))
suppressMessages(require(tidyverse))
suppressMessages(require(tis))

key <- readLines('~/.fred_key')
fredr_set_key(key)
#sofr <- fredr(series_id = "SOFR",
#              observation_start = (as.Date("2024-08-22")-30))
#write.csv(sofr, file="./work/sofr.csv", row.names=F)
#sofr.30day <- fredr(series_id = "SOFR30DAYAVG",
#                    observation_start = (as.Date("2024-08-22")))
#write.csv(sofr.30day, file="./work/sofr_30day.csv", row.names=F)


is.holiday <- function(x) {
   isHoliday(goodFriday=T, inaug=T, x)
}

weight <- function(data) {
  data %>%
    mutate(
      iday = 1,
      day_of_week = wday(date, label = T, abbr = F),
      iday = case_when(
          day_of_week == "Friday" &
          !is.na(value) ~ 3,
          is.na(value) ~ 0,
          TRUE ~ 1
      ),
      iday = case_when(
        is.na(lead(value)) ~ iday + 1,
        TRUE ~ iday
      ),
    ) %>%
    select(!day_of_week)
}

annualize <- function(data) {
  data %>%
    mutate(
      arate = (value/100) * iday + 1
    )
}


sofr <- as_tibble(read.csv("./work/sofr.csv"))
sofr.30day <- as_tibble(read.csv("./work/sofr_30day.csv"))
#head(as.data.frame(sofr), 30)
#head(as.data.frame(sofr.30day), 20)

sofr <- sofr %>%
  weight() %>%
  select(!c(realtime_start, realtime_end)) %>%
  drop_na() %>%
  annualize() %>%
  filter(
    date >= (as.Date('2024-08-22')-30) &
    date <= as.Date('2024-08-22')
  )

prod(1 + sofr$value/100)^(1/30) - 1
(prod(1 + sofr$value/100))^(1/30) - 1
sofr$value
