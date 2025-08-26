suppressMessages(require(zoo))
suppressMessages(require(tidyverse))
suppressMessages(require(fredr))
fredr_set_key(readLines('~/.fred_key'))

# Fred data - from the St. Louis Federal Reserve

## Market yield on U.S. Treasury securities, constant maturity, quoted on an investment basis
interest.rates <- c(
    'DGS1MO',  # 1-Month
    'DGS3MO',  # 3-Month
    'DGS6MO',  # 6-Month
    'DGS1',    # 1-Year
    'DGS2',    # 2-Year
    'DGS3',    # 3-Year
    'DGS5',    # 5-Year
    'DGS7',    # 7-Year
    'DGS10',   # 10-Year
    'DGS20',   # 20-Year
    'DGS30'    # 30-Year
)

## Download each series from fred and save locally
download_interest_rate_reference_data <- function() {
    sapply(interest.rates, function(i) {
        tmp <- fredr(series_id = i, observation_start = ymd(20250101))
        write.csv(tmp, file=paste0('./work/fred/', tolower(i), '.csv'), row.names=F)
    })
}

## Read each of the fred reference data from file and create a zoo object from
## them.
series_list <- suppressMessages(lapply(interest.rates, function(i) {
    tmp <- read_csv(
        paste0('./work/fred/', tolower(i), '.csv')) %>% filter(!is.na(value)
    )
    zoo(tmp$value, tmp$date)
}))
reference.rates <- do.call(merge, series_list)
colnames(reference.rates) <- tolower(interest.rates)

# Coupon bearing U.S. Treasury bonds
files <- c(
           './work/bonds/3m.csv',
           './work/bonds/6m.csv',
           './work/bonds/9m.csv',
           './work/bonds/1yr.csv',
           './work/bonds/1_5yr.csv',
           './work/bonds/2yr.csv',
           './work/bonds/3yr.csv',
           './work/bonds/4yr.csv',
           './work/bonds/5yr.csv',
           './work/bonds/10yr.csv',
           './work/bonds/20yr.csv',
           './work/bonds/30yr.csv'
)

# U.S. Treasury zeros
zeros <- c(
           './work/zeros/z3m.csv',
           './work/zeros/z6m.csv',
           './work/zeros/z9m.csv',
           './work/zeros/z1yr.csv',
           './work/zeros/z1_5yr.csv',
           './work/zeros/z2yr.csv',
           './work/zeros/z3yr.csv',
           './work/zeros/z4yr.csv',
           './work/zeros/z5yr.csv',
           './work/zeros/z10yr.csv',
           './work/zeros/z20yr.csv',
           './work/zeros/z30yr.csv'
)

## Load U.S. Bond data (downloaded from )
#bonds <- function(files){
#    # Downloaded data has a trailing comma, which
#    # `read_csv` complains about because it thinks there
#    # are more variables than the headers suggest.
#    suppressWarnings(do.call(rbind,
#         lapply(files,
#         read_csv,
#         show_col_types=F,
#         na=c("--")))) %>%
#    select(-Action, -Quote, -`Estimated Total`) %>%
#    filter(
#        !is.na(Price)
#    ) %>%
#    mutate(
#      Maturity = as.Date(Maturity, format="%m/%d/%Y"),
#      TTM = time_length(interval(today(), Maturity), "years"),
#    )
#}

## An attempt to figure out my own zero rates (not used).
zero.rate <- function(data){
  func <- function(r, price, ttm) price * exp(r * ttm) - 100

  data |>
  rowwise() |>
  mutate(
      Zero = uniroot(func, c(-1, 1), Price, TTM)$root*100
  )
}

attm <- function(data) {
  reference <- c(1/4, 1/2, 3/4, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  labels <- c('3m', '6m', '9m', '1y', '1.5y', '2y', '2.5y', '3y',
              '3.5y', '4y', '4.5y', '5')
  #reference <- c(1/4, 1/2, 3/4, 1, 1.5, 2)
  #labels <- c('3m', '6m', '9m', '1y', '1.5y', '2y')

  data |>
  rowwise() |>
  mutate(
      ATTM = reference[which.min(abs(reference - TTM))],
      ATTM = factor(
          ATTM,
          levels = reference,
          labels = labels
      )
  )
}

bonds <- suppressMessages(suppressWarnings(
    read_csv(
        './data/bonds_2025-08-26.csv',
        na=c('--')
    )
)) %>%
    select(-Action, -Quote, -`Estimated Total`, -YTW1) %>%
    filter(
        !is.na(Price)
    ) %>%
    mutate(
        Maturity = mdy(Maturity),
        TTM = time_length(interval(today(), Maturity), "years")
    ) %>%
    attm() %>%
    select(-Min, -Max, -`Accrued Interest`) %>%
    filter(
        Coupon > 0
    )

zeros <- suppressMessages(suppressWarnings(
    read_csv(
        './data/zeros_2025-08-26.csv',
        na=c('--')
    ) %>%
    select(
        -Action,
        -Quote,
        -`Estimated Total`
    ) %>%
    filter(
        !is.na(Price)
    ) %>%
    mutate(
        Maturity = mdy(Maturity),
        TTM = time_length(interval(today(), Maturity), "years"),
    ) %>%
    attm() %>%
    select(-Min, -Max)
))

# Determine the present value of a US coupon bond using zero
# rates as the discount rate. Current rates are only for
# 6m, 1y, 1.5y, and 2y. 2 year max bond.
present.value <- function(data, zrates) {
   data %>% mutate(
        PV = {
            dates = sort(Maturity %m-%
                months(seq(0, by=6, length.out = floor(TTM*2))))
            ttm = time_length(interval(today(), dates), "years")
            zrates = zrates[1:(length(ttm))]
            Coupon * sum(exp(-ttm*zrates)) + 100 * exp(-last(ttm)*last(zrates))
        }
   )
}

zrates <- zeros %>%
    zero.rate() %>%
    group_by(ATTM) %>%
    summarize(
        YTM = max(Zero)
    ) %>%
    filter(ATTM %in% c('6m', '1y', '1.5y', '2y'))

df <- data.frame(bonds %>% filter(ATTM %in% c('6m', '1y', '1.5', '2y')) |>
    filter(floor(TTM*2) > 0) |>
    rowwise() |>
    present.value(zrates$YTM/100))[57,]

#                                Description Coupon   Maturity Quantity   Price
#57 US Treasury 6.125% 11/15/2027, 912810FB9  6.125 2027-11-15      300 105.344
#     YTM      TTM ATTM       PV
#57 3.593 2.221311   2y 115.4958

#df


coupon <- 3
r <- c(.05, .058, .064, .068)
ttm <- c(.5, 1, 1.5, 2)
price <- 98.39

coupon <- 6.125
r <- zrates$YTM/100
    dates <- sort(as.Date("2027-11-15") %m-% months(seq(0, by=6, length.out = 4)))
ttm <- time_length(interval(today(), dates), "years")
price <- 105.344


f <- function(r, c, t, p) c * sum(exp(-t*r)) + 100 * exp(-last(t)*last(r)) - p
uniroot(f, c(-1, 1), c=coupon, t=ttm, p=price)$root

### ATTM is a factor that aggregates bonds by maturity
### type: 3-month 6-month, etc...
#reference <- c(1/4, 1/2, 3/4, 1, 1.5, 2, 3, 4, 5, 10, 20, 30)
#labels <- c( '3m', '6m', '9m', '1y', '1.5y', '2y', '3y', '4y', '5y', '10y', '20y', '30y')

# Using zero bonds, get the max YTM and use them for zero rates
# for 6m, 1y, 1.5y, and 2y.
#tzeros <- bonds(zeros) %>%
#    select(Maturity, Price, YTM, TTM) %>%
#    zero.rate() %>%
#    mutate(
#       ATTM = reference[which.min(abs(reference - TTM))],
#       ATTM = factor(
#           ATTM,
#           levels = reference,
#           labels = labels
#       )
#    ) %>%
#    group_by(ATTM) %>%
#    summarize(
#      YTM = max(YTM)
#   ) %>%
#   filter(ATTM %in% c('6m', '1y', '1.5y', '2y'))

#tbonds <- bonds(files) %>%
#    filter(Coupon > 0) %>%
#    select(Maturity, Coupon, Price, YTM, TTM) |>
#    rowwise() |>
#    mutate(
#        ATTM = reference[which.min(abs(reference - TTM))],
#        ATTM = factor(
#            ATTM,
#            levels = reference,
#            labels = labels
#        )
#    )

#zero.rates <- tzeros$YTM/100
# 2026-02-28  4.62  100.   4.06 0.510 6m    116.

#dates <- sort(as.Date("2026-02-28") %m-% months(seq(0, by=6, length.out = floor(0.510*2))))

#ttm = time_length(interval(today(), dates), "years")

#4.62 * exp(-ttm*zero.rates[1]) + 100 * exp(-ttm*zero.rates[1])

#bonds %>%
#    select(Description, Maturity, Coupon, Price, YTM, TTM) %>%
#    filter(
#        Coupon > 0
#    )
#
#zeros %>%
#    select(Description, Maturity, Price, YTM, TTM)
