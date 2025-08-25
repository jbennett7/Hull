suppressMessages(require(tidyverse))

files <- './work/bonds/5yr.csv'

data <- suppressWarnings(read_csv(files,
         col_types = cols_only(
             Description = col_character(),
             Coupon = col_double(),
             Maturity = col_datetime(format="%m/%d/%Y"),
             Quantity = col_double(),
             Price = col_double(),
             Min = col_integer(),
             Max = col_integer(),
             YTM = col_double()
         ),
         na = c("--")
)) %>% filter(Coupon > 0)

#wday(data$Maturity, label=T)
#month(data$Maturity, label=T)

d30d <-  duration(num=30, units='days')
d90d <-  duration(num=90, units='days')
d180d <- duration(num=180, units='days')
d6m <- duration(num=6, units='months')

#wday(data$Maturity[1] - d30d, label=T)
#data$Maturity[1] - d30d

#ceiling_date(today(), unit="month")-1

#maturity <- data$Maturity[1]
#n_coupons = floor(time_length(interval(today(), maturity), "years"))*2
#coupon_dates <- sort(maturity %m-% months(seq(0, by = 6, length.out = n_coupons)))
#coupon_dates

f <- function(r) 2*exp(-0.02010*.5) + 2*exp(-0.02225*1) + 102*exp(-r*1.5) - 102.5
uniroot(f, c(-1, 1))$root
