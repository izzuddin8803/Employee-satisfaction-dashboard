# server.R

library(shiny)

savings_rate <- function(annual_savings, annual_expenses) 100*(annual_savings/(annual_savings + annual_expenses))

# Annual ROI will be set to 5%
roi <<- .05

#FV <- function(annual_expenses, swr) annual_expenses/(swr/100)

networthify <- function(annual_expenses, swr, annual_savings, roi, current_portfolio_value) (log(annual_expenses/(swr/100) + annual_savings/roi) - log(current_portfolio_value + annual_savings/roi))/log(1 + roi)

shinyServer(function(input, output) {
  helpText = "This app calculates the Investment, in terms of dollar amounts, after a few years.  It will provide the final amount, and the a graph throughout the years as your investment grows.  To run this, please enter the rate of return, the years to invest, and the initial investment in terms of dollars.  Values are automatically calculated upon hitting enter.  Note that it is limited to 1-50 years with a maximumn of $1M"
  output$newInvestment <- renderPlot({ 
    lastYear <- 2016 + input$years - 1
    years = c(2016:lastYear)
    dollars<-c(1:input$years)
    
    for (i in dollars) {
      dollars[i] = input$startCash * (1+input$rate/100)^i
    }
    
    names(dollars) <- years
    barplot(dollars, xlab="Year", ylab="Dollar Amount", 
            col='lightblue', main="Dollar Amount per year")
    
  })

  output$SavingsRate <- renderText({
    paste("You have a savings rate of ", round(savings_rate(input$annualSavings, input$annualExpenses), 1), " percent. Keep it going!")
  })
  

  
  output$networthify <- renderText({
    paste("You have ", round(networthify(input$annualExpenses, input$SWR, input$annualSavings, roi, input$currentPortfolioValue), 1), " years until retirement!")
  })
  
  output$newFinal<-({
    renderText({paste("Final Amount $", input$startCash * round((1+input$rate/100)^input$years, 2))})
  })
  
  output$helpText <- renderText({
    if (input$help %% 2== 1) { isolate({HTML(helpText)})}
  })
  
})
