suppressMessages(require(tidyverse))
suppressMessages(require(fredr))
fredr_set_key(readLines('~/.fred_key'))

#dgs3mo <- fredr(series_id = 'DGS3MO',
#      observation_start = ymd(20250101)
#)
#write.csv(dgs3mo, file='./work/fred/dgs3mo.csv', row.names=F)
dgs3mo <- suppressMessages(read_csv('./work/fred/dgs3mo.csv')) %>%
    filter(!is.na(value))

#dgs6mo <- fredr(series_id = 'DGS6MO',
#    observation_start = ymd(20250101)
#)
#write.csv(dgs6mo, file='./work/fred/dgs6mo.csv', row.names=F)
dgs6mo <- suppressMessages(read_csv('./work/fred/dgs6mo.csv')) %>%
    filter(!is.na(value))

reference <- c(1/4, 1/2, 3/4, 1, 1.5, 2, 3, 4, 5, 10, 20, 30)
labels <- c( '3m', '6m', '9m', '1y', '1.5y', '2y', '3y', '4y', '5y', '10y', '20y', '30y')

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

d30d <- duration(num=30, units='days')
d90d <- duration(num=90, units='days')
d180d <- duration(num=180, units='days')

bonds <- function(files){
    suppressWarnings(do.call(rbind,
         lapply(files,
         read_csv,
         show_col_types=F,
         na=c("--")))) %>%
    select(-Action, -Quote, -`Estimated Total`) %>%
    filter(
        !is.na(Price)
    ) %>%
    mutate(
      Maturity = as.Date(Maturity, format="%m/%d/%Y"),
      TTM = time_length(interval(today(), Maturity), "years"),
    )
}

zero.rate <- function(data){
  func <- function(r, price, ttm) price * exp(r * ttm) - 100

  data |>
  rowwise() |>
  mutate(
      Zero = uniroot(func, c(-1, 1), Price, TTM)$root*100
  )
}

tzeros <- bonds(zeros) %>%
    select(Maturity, Price, YTM, TTM) %>%
    zero.rate() %>%
    mutate(
       ATTM = reference[which.min(abs(reference - TTM))],
       ATTM = factor(
           ATTM,
           levels = reference,
           labels = labels
       )
    )

tbonds <- bonds(files) %>%
    select(Maturity, Coupon, Price, YTM, TTM) |>
    rowwise() |>
    mutate(
        ATTM = reference[which.min(abs(reference - TTM))],
        ATTM = factor(
            ATTM,
            levels = reference,
            labels = labels
        )
    )

tzeros %>%
  group_by(ATTM) %>%
  summarize(
     YTM = max(YTM)
  )

tbonds %>%
    group_by(ATTM) %>%
    summarize(
        YTM = max(YTM)
    )

#ttm <- tbonds$TTM
#reference[which.min(abs(reference - ttm))]

#sapply(ttm, function(val) reference[which.min(abs(reference - ttm))])
#sapply(ttm, function(val) print(val))
#sapply(ttm, function(x) reference[which.min(abs(reference - x))])

#data.frame(
#tzeros %>% 
#    group_by(ATTM) %>%
#    summarize(MaxYield = max(YTM))
#)

#data.frame(
#tbonds %>%
#    select(TTM, ATTM)
#    group_by(ATTM) %>%
#    summarize(MaxYield = max(YTM))
#)

#tbonds <- tbonds[1,]
#ncoupons = tbonds$TTM*2
#dates <- sort(tbonds$Maturity %m-%
#    months(seq(0, by=6, length.out = ncoupons)))
#zrates <- c(3.50, 3.62, 3.60, 3.66, 3.66, 3.67, 3.70, 3.76, 3.82, 3.89, 4.09)/100
#ttm <- time_length(interval(today(), dates), "years")
#
#zcr <- head(zrates, -1)
#ctm <- head(ttm, -1)
#
#zlr <- last(zrates)
#ltm <- last(ttm)
#
#coupon <- tbonds$Coupon
#lcoupon <- coupon + 100
#coupon

#f <- function(r) sum(coupon * exp(-ctm*zcr)) + lcoupon * exp(-ltm*r) - tbonds$Price
#uniroot(f, c(-1, 1))$root

#sum(coupon * exp(-zcr*ctm)) + lcoupon*exp(-zlr*ltm)

#coupons <- tbonds$Coupon
#l <- 5
#price <- 104.929
#zrates <- c(2.01, 2.225)/100
#ttm <- c(.5, 1)
#l <- 1.5
#coupons <- 2
#price <- 102.5

#p <- 100 + coupons
#f <- function(r) sum(coupons * exp(-ttm*zrates)) + p * exp(-l*r)
#uniroot(function(r) f(r) - price, c(-1, 1))$root

#tbonds <- bonds(files) %>%
#    select(Description, Coupon, Maturity, Price, YTM, TTM) %>%
#    filter(Coupon > 0) %>%
#    mutate(
#      N_Coupons = TTM * 2,
#    ) |>
#    rowwise() |>
#    mutate(
#        Rate = {
#            dates <- sort(Maturity %m-%
#                months(seq(0, by=6, length.out = N_Coupons)))
#            ttm <- time_length(interval(today(), dates), "years")
#            f <- function(r) Coupon * sum(exp(zrates[-1]*ttm[-1])) +
#                100 * exp(zrates[length(zrates)]*ttm[length(zrates)]) - Price
#            uniroot(f, c(-1,1))$root
#        }
#    )
