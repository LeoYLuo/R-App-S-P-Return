# Title: APP-1) S&P 500 Returns
# Description:  A shiny app to explore and visualize daily fluctuations as 
#               well as different types of returns from the Standard & Poor’s 
#               500 (S&P 500) market index.
# Author: Yihang Luo
# Date: 10/28/2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================

sp500_data = tq_get("^GSPC", from="1928-01-03", to="2023-12-29")
sp500_data = sp500_data |> select(date, close) |>
  mutate(year = year(date))

# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P 500 Returns Calculator"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # (adapt code with widgets of your choice!!!)
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(inputId = "timeperiod",
                  label = "Time Period",
                  min = 1928,
                  max = 2023,
                  value = c(1990, 2020),
                  step = 1),
      radioButtons(inputId = "scale",
                   label = "Y-axis scale",
                   choices = c("linear", "log-scale")),
      numericInput(inputId = "numberyear",
                   label = "Number-of-year",
                   value = 5),
      checkboxGroupInput(inputId = "stat",
                   label = "Show Statistics",
                   choices = c("Mean", "Median", "Std Dev")),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3(),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3(),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3(),
      tableOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Auxiliary table (note that this is a reactive conductor)
  # ------------------------------------------------------------
  timeline = reactive({
    sp_500 = sp500_data |> 
      filter(year >= input$timeperiod[1] & year <= input$timeperiod[2])
    sp_500
  })
  
  
  n_year_returns = reactive({
    start_year = input$timeperiod[1]
    end_year = input$timeperiod[2]
    stop_year = end_year-input$numberyear+1
    
    n_years_returns = rep(0, stop_year-start_year)
    years = rep(0, stop_year-start_year)
    
    for (m in start_year:stop_year) {
      n_year_return = sp500_data |>
        filter(year %in% m:(m+input$numberyear-1)) |>
        summarise(return = (last(close) - first(close)) / first(close) * 100) |>
        pull(return)
      
      n_years_returns[m+1-start_year] = n_year_return
      years[m+1-start_year] = paste(m, m+input$numberyear-1, sep='-')
    }
    
    # assemble results in a tibble
    sp500_n_year_returns = tibble(
      'years' = years,
      'return' = n_years_returns
    )
  
    sp500_n_year_returns
  })
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # line plot
    if (input$scale == "linear") {
      ggplot(data = timeline(), aes(x = date, y = close)) +
        geom_line(color="cornflowerblue") + 
        labs(
          title = paste0("Daily Closing Values (", input$timeperiod[1], "-",
                         input$timeperiod[2], ")"),
          x = "Date",
          y = "Closing Value ($)"
          ) + 
        theme_bw()
    } else if (input$scale == "log-scale") {
      ggplot(data = timeline(), aes(x = date, y = close)) + 
        geom_line(color="cornflowerblue") + 
        scale_y_log10() + 
        labs(
          title = paste0("Daily Closing Values (", input$timeperiod[1], "-",
                         input$timeperiod[2], ")"),
          x = "date",
          y = "Closing Value (Log Scalue)"
        ) + 
        theme_bw()
    }
  })


  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    
    dat = n_year_returns()
    
    # bar chart of n-year returns
    p = ggplot(data = dat, aes(x = years, y = return)) +
      geom_col(aes(fill=return > 0), show.legend=FALSE) + 
      labs(
        title = paste0(input$numberyear, "-Year S&P 500 Annual Returns (", input$timeperiod[1],
                       "-", input$timeperiod[2], ")"),
        x = paste0(input$numberyear, "-Year Period"),
        y = "Return (%)"
      ) + 
      theme_bw() + 
      guides(fill="none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    # show statistics
    if ("Mean" %in% input$stat) {
      avg_return = mean(dat$return)
      p = p + geom_hline(yintercept = avg_return, col = "blue", lwd = 0.7)
    } 
    
    if ("Median" %in% input$stat) {
      median_return = median(dat$return)
      p = p + geom_hline(yintercept = median_return, col = "red", lwd = 0.7)
    }
    
    if ("Std Dev" %in% input$stat) {
      sd_return = sd(dat$return)
      p = p + geom_hline(yintercept = sd_return, col = "yellow", lwd = 0.7)
    }
    
    p
  })
  
    
  # ------------------------------------------------------------
  # Table
  # ------------------------------------------------------------
  output$table <- renderTable({
    # the following code is for demo purposes only; adapt it!!!
    dat = n_year_returns()
    stats = tibble(
      statistic = c("perc10", "perc25", "mean", "median", "perc75", "perc90", "stdev", "IQR"),
      value = c(quantile(dat$return, 0.10),
                quantile(dat$return, 0.25),
                mean(dat$return),
                median(dat$return),
                quantile(dat$return, 0.75),
                quantile(dat$return, 0.90),
                sd(dat$return),
                quantile(dat$return, 0.75) - quantile(dat$return, 0.25))
    )
  })
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
