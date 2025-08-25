require(shiny)
require(bslib)
require(ggplot2)
require(DT)

require(optionTables)

ui <- page_fluid(
  fluidRow(
    textInput("symbol", "Symbol", value = "T"),
    radioButtons("otype", "Call / Put",
                 choices = list("Call" = 1, "Put" = 2),
                 inline=T),
    sliderInput("orows", "Strikes", min=2, max=24, step=2, value=4),
    textInput("strike", "Strike Price"),
    textInput("premium", "Premium")
  ),
  fluidRow(
    column(6,
      dataTableOutput("puts")
    )
  )
)

server <- function(input, output){
  options_table <- reactive({
    req(input$symbol)
    #opts <- options_table(input$symbol)
    opts <- getOptionChain(input$symbol)
    ifelse(input$otype == "1", return(opts$calls), return(opts$puts))
  })

#      datatable(
#        df,
#        class = list(stripe = F),
#        options = list(
#          dom = 't',
#          ordering = F#,
#         columnDefs = list(
#           list(visible = F,
#             targets = which(names(df) %in% hidden_columns)),
#           list(visible = F, targets = 0)
#         )
#       )
#     )
#   })
#   ifelse(input$otype == "1", return(opts$calls), return(opts$puts))
# })
  
  output$puts <- renderDataTable({
    options_table()
  })

#         formatStyle(
#           'ITM',
#           target = 'row',
#           backgroundColor = styleEqual(T, 'lightgreen')
#         )
  
  #renderPlot({
  #  req(input$strike, input$premium)
  #  S <- seq(0, 80, by = 1)
  #  strike <- as.numeric(input$strike)
  #  premium <- as.numeric(input$premium)
  #  payoff <- ifelse(S > strike, - (S - strike) +
  #    premium, premium)
  #
  #  ggplot(data.frame(StockPrice = S, Profit = payoff),
  #         aes(x = StockPrice, y = Profit)) +
  #    geom_line(size = 1.2) +
  #    geom_hline(yintercept = 0, linetype = "dashed") +
  #    labs(
  #      title = "Payoff",
  #      x = "Stock Price at Expiration",
  #      y = "Profit"
  #    ) +
  #    theme_minimal()
  #})
}

shinyApp(ui, server, options=list(port = 8080))
