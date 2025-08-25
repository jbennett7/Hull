require(quantmod)

options_table <- function(sym) {
    opts <- getOptionChain(sym)
    lapply(opts, function(df) {
      #Fix a yahoo source type misspelling
      names(df)[names(df) == "ConractSize"] <- "ContractSize"
      #Fix date and time
      df$Expiration <- as.character(df$Expiration)
      df$LastTradeTime <- as.character(df$LastTradeTime)
      #Change field's sigdig is 2
      df$Chg <- round(df$Chg, 2)
      df
    })
}

tables <- options_table("T")
w <- 3
dtables <- lapply(tables, function(df) {
  idx <- which(diff(df$ITM) != 0)
  df[(idx-w):(idx+(w+1)), c("Bid", "Ask", "Last", "Chg", "Vol", "OI", "Strike")]
})
dtables
