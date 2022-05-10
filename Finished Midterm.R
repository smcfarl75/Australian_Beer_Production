library(fpp3)
library(seasonal)
library(shinydashboard)
library(readr)
library(shinyWidgets)
library(shiny)
library(astsa)
library(DT)
library(dygraphs)



recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)



server <- shinyServer(function(input, output, session) {
  
  your_plot <- reactive({
    if(input$PlotChoice == "Decomposition") {
      plot(aus_production %>%
             model(
               classical_decomposition(Beer, type = "multiplicative")
             ) %>%
             components() %>%
             autoplot()+
             labs(
               title = "Multiplicative Decomposition plot of beer production",
               y = "Megalitres"
             ))
    }
    else if (input$PlotChoice == "Seasonal"){
      plot(aus_production %>%
             model(
               STL(Beer ~ trend(window = 7) +
                     season(window = "periodic"),
                   robust = TRUE)) %>%
             components() %>%
             autoplot()+
             labs(
               title = "Seasonal plot of beer production",
               y = "Megalitres"
             ))
    }
    else if (input$PlotChoice == "Autocorrelation"){
      plot(aus_production %>%
             ACF(Beer, lag_max = 48) %>%
             autoplot()+
             labs(
               title = "Autocorrelation of beer production",
               y = "Megalitres"
             ))
    }
    else if (input$PlotChoice == "Regular"){
      plot(aus_production %>%
             autoplot(Beer)+
             labs(
               title = "Beer production",
               y = "Megalitres"
             )
           )
    }
    else if (input$PlotChoice == "Forecast"){
      plot(fc_beer %>%
             autoplot(recent_production)+
             labs(
               title = "Forecasts of beer production",
               y = "Megalitres"
             )
             )
    }
    else if (input$PlotChoice == "Naive Model"){
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      Naive_Model <- recent_production %>%
        model(NAIVE(Beer))
      fc_N <- forecast(Naive_Model)
      fc_N %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Naive Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Seasonal Naive"){
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      SNaive_Model <- recent_production %>%
        model(SNAIVE(Beer))
      fc_SN <- forecast(SNaive_Model)
      fc_SN %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Seasonal Naive Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Mean Model"){
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      Mean_Model <- recent_production %>%
        model(MEAN(Beer))
      fc_M <- forecast(Mean_Model)
      fc_M %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Mean Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Drift Model"){
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      Drift <- recent_production %>%
        model(RW(Beer))
      fc_RW <- forecast(Drift)
      fc_RW %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Drift Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Holts Exponential Smoothing"){
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      Holts <- recent_production %>%
        model(ETS(Beer~ error("A") +
                    trend("A") + season("N")))
      fc_H <- forecast(Holts)
      fc_H %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Holts Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Holts/Winters Exponential Smoothing"){
      
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      HoltsWinters <- recent_production %>%
        model(ETS(Beer~ error("A") +
                    trend("A") + season("A")))
      fc_HW <- forecast(HoltsWinters)
      fc_HW %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Holts-Winters Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "2 1 0 ARIMA"){
      
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      MARIMA <- recent_production %>%
        model(ARIMA(Beer ~ pdq(2,1,0)))
      fc_MA <- forecast(MARIMA)
      fc_MA %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the 2 1 0 ARIMA Model",
          y = "Megalitres"
        )
    }
    else if (input$PlotChoice == "Automative ARIMA"){
      
      recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
      AARIMA <- recent_production %>%
        model(ARIMA(Beer))
      fc_AA <- forecast(AARIMA)
      fc_AA %>%
        autoplot(recent_production) +
        labs(
          title = "Forecasts of beer production using the Automatic ARIMA Model",
          y = "Megalitres"
        )
    }
  })
  
  output$SelectedPlot <- renderPlot({ 
    your_plot()
  })
  
  output$text <- renderText({
    paste("The plot above shows the total beer production of Australia from 1956 
    until 2010.  Australia's beer production steadily increased until 
    roughtly  1975.  From 1975 until 1992, the production of beer plateaus
    and neither increases nor decreases, on average.  Beer production
    later decreases from 1992 until the end of the dataset (2010).
    
You can view various timeseries features of the data by selecting a 
          feature from the options to the left of the graph.
          
Regular is the normal time series plot.

Autocorrelation shows the relationship between lagged variables 
  in a time series.  Because many of the bars in this graph are
  above the blue dashed line, these lag variables are 
  significantly present.

Decomposition used in this graph is multiplicative (compared
  to additive).Here, assuming the seasonal variable is 
  constant from year to year, we can see that the majority 
  of the variation in the decomposition comes from random variation

Forecast shows what the projected beer production in Australia will be
  in the future.  In this case, it appears to project a 
  continuation of the downward trend.  Reference the models below seasonal
  to see various other forecasting models.
  
Seasonal shows how beer production has changed over the course 
  of a year, organized by the season.  The big y-axis bar shows
  that Beer production is very affected by this
          
Below Seasonal, there are several models to forecastthe future beer production
with an 80% confidence and a 95% confidence.")
  })
  
})


ui <-  shinyUI(fluidPage(
  navbarPage(title="Final Project",
             tabPanel("Australian Beer Production",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("PlotChoice", "Displayed plot:", 
                                       choices = c("Regular", "Autocorrelation","Decomposition","Forecast","Seasonal", "Naive Model","Seasonal Naive",
                                                   "Mean Model","Drift Model","Holts Exponential Smoothing","Holts/Winters Exponential Smoothing",
                                                   "2 1 0 ARIMA","Automative ARIMA"))),
                        mainPanel(plotOutput("SelectedPlot"),
                                  verbatimTextOutput("text")))))
  ,  fluid=TRUE))

verbatimTextOutput("summary")

shinyApp(ui=ui, server=server)
