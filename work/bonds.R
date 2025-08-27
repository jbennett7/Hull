suppressMessages(require(zoo))
suppressMessages(require(tidyverse))
suppressMessages(require(fredr))
fredr_set_key(readLines('~/.fred_key'))

start.date = as.Date("2025-08-26")
zero.file = './data/zeros_2025-08-26.csv'
bond.file = './data/bonds_2025-08-26.csv'

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

download_interest_rate_reference_data <- function() {
    sapply(interest.rates, function(i) {
        tmp <- fredr(series_id = i, observation_start = ymd(20250101))
        write.csv(tmp, file=paste0('./work/fred/', tolower(i), '.csv'), row.names=F)
    })
}

series_list <- suppressMessages(lapply(interest.rates, function(i) {
    tmp <- read_csv(
        paste0('./work/fred/', tolower(i), '.csv')) %>% filter(!is.na(value)
    )
    zoo(tmp$value, tmp$date)
}))
reference.rates <- do.call(merge, series_list)
colnames(reference.rates) <- tolower(interest.rates)

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

zero.rate <- function(data){
    data %>%
    mutate(
        Zero = {
            f <- function(r) Price * exp(r * TTM) - 100
            uniroot(f, c(-1, 1))$root
        }
    )
}

attm <- function(data) {
    data |>
    rowwise() |>
    mutate(
        ATTM = {
            reference <- c(1/4, 1/2, 3/4, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
            labels <- c('3m', '6m', '9m', '1y', '1.5y', '2y', '2.5y', '3y',
                        '3.5y', '4y', '4.5y', '5')
            nattm <- reference[which.min(abs(reference - TTM))]
            factor(nattm, levels = reference, labels = labels)
        }
    )
}

present.value <- function(data, zrates) {
   data %>% mutate(
        PV = {
            dates = sort(Maturity %m-%
                months(seq(0, by=6, length.out = floor(TTM*2))))
            ttm = time_length(interval(start.date, dates), "years")
            zs = unlist(sapply(ttm, function(t)
                zrates[which.min(abs(zrates$TTM - t)), "Zero"]))
            (Coupon/2) * sum(exp(-ttm*zs)) + 100 * exp(-last(ttm)*last(zs))
        }
   )
}

bonds <- suppressMessages(suppressWarnings(
    read_csv(
        bond.file,
        na=c('--')
    )
)) %>%
    select(-Action, -Quote, -`Estimated Total`, -YTW1) %>%
    filter(
        !is.na(Price),
        !is.na(YTM)
    ) %>%
    mutate(
        Maturity = mdy(Maturity),
        TTM = time_length(interval(start.date, Maturity), "years")
    ) %>%
    attm() %>%
    select(-Min, -Max, -`Accrued Interest`) %>%
    filter(
        Coupon > 0
    )

zeros <- suppressMessages(suppressWarnings(
    read_csv(
        zero.file,
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
        TTM = time_length(interval(start.date, Maturity), "years"),
    ) %>%
    attm() %>%
    select(-Min, -Max)
))

zrates <- zeros %>%
    select(-Description, -Qty) %>%
    zero.rate() %>%
    select(TTM, Zero) %>%
    group_by(TTM) %>%
    summarize(Zero = max(Zero))

#zrates <- tibble(
#    TTM = c(0.5, 1, 1.5, 2),
#    Zero = c(5, 5.8, 6.4, 6.8)/100
#)
#bonds <- tibble(
#    Maturity = as.Date("2025-08-26") + years(2),
#    TTM = 2.0,
#    Price = 98.39,
#    Coupon = 3
#)

tbonds <- bonds %>%
    filter(
        Coupon > 0,
        TTM >= .5
    ) %>%
    present.value(zrates)

#tbonds %>% mutate(Error = (PV - Price)/Price*100) %>% filter(abs(Error) > 1)

#x11()
#plot(x=tbonds$TTM, y=tbonds$YTM, type='l')
#Sys.sleep(30)

bond.1yr <- data.frame(bonds) %>%
    filter(
        !str_detect(Description, "TIP"),
    ) %>%
    filter(ATTM == '1y') %>%
    slice_max(YTM)

#bond1$Coupon/2 * exp(-z.6m$YTM/100 * bond1$TTM) + 100 * exp(-z.6m$YTM/100 * bond1$TTM)

z.6m <- zeros %>% filter(ATTM == '6m')

zrates <- zeros %>% rowwise() %>%
    zero.rate() %>%
    group_by(ATTM) %>%
    summarize(Zero = max(Zero))
#    mutate(
#        Zero = {
#            f <- function(r) Price * exp(r * TTM) - 100
#            uniroot(f, c(-1, 1))$root*100
#        }
#    ) %>%
#    select(Description, ATTM, YTM, Zero) %>%
#    group_by(ATTM) %>%
#    summarize(Zero = max(Zero))

#x11()
#plot(x=zrates$TTM, y=zrates$Zero, type='l')
#Sys.sleep(30)

#zbond <- bonds %>%
#    filter(ATTM == '1.5y', TTM > 1.5) %>%
#    select(Maturity, Price, YTM, Coupon, TTM, ATTM) %>%
#    mutate(
#        Zero = {
#            zs = (zrates %>% filter(ATTM %in% c('6m', '1y')))$Zero/100
#            dates = sort(Maturity %m-%
#                 months(seq(0, by=6, length.out=3)))
#            ttm = time_length(interval(start.date, dates), "years")
#            f <- function(r)
#                (Coupon/2) * sum(exp(-head(ttm,-1)*zs)) +
#                    100 * exp(-last(ttm)*r) - Price
#            uniroot(f, c(-1, 1))$root*100
#        } 
#    )

# 2027-03-15 101.   3.75  4.25   1.55 1.5y 
#zrates
#f <- function(r) exp(.0340*.25)*exp(r*.25) - exp(.0352*.5)
#uniroot(f, c(-1,1))$root
f <- function(r, t1, t2, z1, z2) exp(z1*t1)*exp(r*t1) - exp(z2*t2)
rF <- uniroot(f, interval=c(-1, 1), t1=.25, t2=.5, z1=.0340, z2=.0352)$root
rF
(.0352*.5 - .0340*.25)/(.5-.25)
