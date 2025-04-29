# # # ---- Dependency Installer ----
# # required_packages <- c("shiny", "shinythemes", "quantmod", "forecast", "ggplot2", "tseries", "xgboost", "shinyjs")
# # new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
# # if(length(new_packages)) install.packages(new_packages)

# # # app.R

# # library(shiny)
# # library(shinythemes)
# # library(quantmod)
# # library(forecast)
# # library(ggplot2)
# # library(tseries)
# # library(xgboost)
# # library(shinyjs)

# # ui <- fluidPage(
# #   useShinyjs(),
# #   theme = shinytheme("cerulean"),
  
# #   tags$head(
# #     tags$style(HTML("
# #       .shiny-text-output {
# #         white-space: pre-wrap;
# #         overflow-x: hidden;
# #       }
# #     "))
# #   ),
# #   tags$img(src = "header.png", style = "width: 100%; margin-bottom: 20px;"),
# #   tags$hr(style = "margin-top: -10px; margin-bottom: 20px; border-top: 2px solid #800000;"),
  
# #   fluidRow(
# #     column(
# #       width = 9,
# #       div(
# #         style = "background-color: #fff5f5; padding: 0 20px; border-radius: 6px; box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-bottom: 20px;",
# #         h3("Key Performance Indicators", style = "text-align: center; color: #800000; font-weight: bold;"),
# #         fluidRow(
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Most recent adjusted closing price in USD",
# #                 h3(textOutput("latestPrice", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("Latest Price", title = "This is the most recent adjusted stock price", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           ),
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Average adjusted price over the last 30 days",
# #                 h3(textOutput("meanPrice", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("30-Day Average", title = "Average of last 30 closing prices", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           ),
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Standard deviation of closing price over the last 30 days",
# #                 h3(textOutput("sdPrice", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("30-Day Std Dev", title = "Volatility of stock over the past 30 days", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           )
# #         ),
# #         fluidRow(
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Forecasted price from ARIMAX model",
# #                 h3(textOutput("arimaxPred", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("Predicted by ARIMAX", title = "Predicted next-day price by ARIMAX model", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           ),
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Forecasted price using naive model",
# #                 h3(textOutput("naivePred", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("Predicted by Naive", title = "Predicted next-day price assuming no change from today", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           ),
# #           column(4,
# #             div(
# #               style = "background-color: #fff5f5; padding: 15px; margin-bottom: 15px; border-radius: 6px; box-shadow: 0 4px 8px rgba(128,0,0,0.25); text-align: center; border: 1px solid #800000;",
# #               div(
# #                 title = "Forecasted price from XGBoost model",
# #                 h3(textOutput("xgbPred", inline = TRUE), style = "margin: 0; color: #800000; font-weight: bold;"),
# #                 p("Predicted by XGBoost", title = "Predicted next-day price by XGBoost model", style = "margin: 0; color: #800000; font-size: 13px; font-weight: 500;")
# #               )
# #             )
# #           )
# #         )
# #       ),
# #       div(
# #         style = "background-color: #fff5f5; padding: 0; border-radius: 6px; box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-bottom: 20px;",
# #         h3("Stock Price Chart", title = "Daily adjusted closing price for the selected stock", style = "text-align: center; color: #800000; font-weight: bold;"),
# #         plotOutput("stockPlot")
# #       ),
# #       div(
# #         style = "background-color: #fff5f5; padding: 0; border-radius: 6px; box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-bottom: 20px;",
# #         h3(
# #           "Forecast Plot",
# #           title = "This chart shows predicted stock prices over the next selected days using ARIMAX, Naive, and XGBoost models. Hover over lines to compare values.",
# #           style = "text-align: center; color: #800000; font-weight: bold;"
# #         ),
# #         plotOutput("forecastPlot"),
# #         p("This chart compares the predicted stock prices using three models:",
# #           strong("ARIMAX (blue)"), ", ", strong("Naive (green)"), ", and ", strong("XGBoost (red)"), ".")
# #       ),
# #       div(
# #         style = "background-color: #fff5f5; padding: 0; border-radius: 6px; box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-top: 20px;",
# #         h3(
# #           "Model Forecast Breakdown",
# #           title = "Each panel shows the predicted price trend from a specific model over the selected forecast horizon.",
# #           style = "text-align: center; color: #800000; font-weight: bold;"
# #         ),
# #         plotOutput("comparisonPlot")
# #       ),
# #       div(
# #         style = "margin-top: 10px; background-color: #fff5f5; padding: 15px; border-top: 1px dashed #800000; font-size: 13px; color: #800000;",
# #         h4("Chart Insight", style = "font-weight: bold; text-align: center; color: #800000;"),
# #         p("This visualization shows how different forecasting models respond to the same stock trends. ARIMAX typically captures seasonality and volume signals. Naive assumes flat growth. XGBoost adapts dynamically to recent behavior."),
# #         p("Use this comparison to decide which forecast best fits your needs — short-term tracking (Naive), seasonality (ARIMAX), or volatility + trend (XGBoost).")
# #       ),
# #       fluidRow(
# #         column(6,
# #           div(
# #             style = "background-color: #fff5f5; padding: 15px; border-radius: 6px;
# #                      box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-bottom: 20px;",
# #             h4("Prediction Focus Allocation", style = "text-align: center; color: #800000; font-weight: bold;"),
# #             plotOutput("donutChart")
# #           )
# #         ),
# #         column(6,
# #           div(
# #             style = "background-color: #fff5f5; padding: 15px; border-radius: 6px;
# #                      box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000; margin-bottom: 20px;",
# #             h4("Model Confidence Comparison", style = "text-align: center; color: #800000; font-weight: bold;"),
# #             plotOutput("spiderChart")
# #           )
# #         )
# #       )
# #     ),
# #     column(
# #       width = 3,
# #       div(
# #         style = "background-color: #fff5f5; padding: 20px; border-radius: 6px;
# #                  box-shadow: 0 2px 4px rgba(128,0,0,0.3); border: 1px solid #800000;",
# #         h4("Filters", style = "color: #800000; font-weight: bold; text-align: center; margin-bottom: 15px;"),
# #         textInput("stockSymbol", "Stock Symbol (e.g., NVDA):", value = "NVDA"),
# #         dateInput("startDate", "Start Date:", value = "2018-01-01"),
# #         dateInput("endDate", "End Date:", value = Sys.Date()),
# #         numericInput("forecastDays", "Days to Forecast:", value = 30, min = 7, max = 90),
# #         actionButton("goButton", "Apply Filters"),
# #         tags$hr(style = "margin: 20px 0; border-top: 2px dashed #800000;"),
# #         p("Model evaluation and performance summary below ⬇️", style = "text-align: center; color: #800000; font-style: italic; font-size: 13px;")
# #       ),
# #       div(
# #         style = "margin-top: 20px; background-color: #fff5f5; padding: 15px; border-radius: 6px; 
# #                  box-shadow: 0 2px 4px rgba(128,0,0,0.2); border: 1px solid #800000;",
# #         h4("Model Summary", style = "color: #800000; font-weight: bold; text-align: center;"),
# #         verbatimTextOutput("modelSummary", placeholder = TRUE)
# #       ),
# #       div(
# #         style = "margin-top: 15px; background-color: #fff5f5; padding: 15px; border-radius: 6px; 
# #                  box-shadow: 0 2px 4px rgba(128,0,0,0.2); border: 1px solid #800000;",
# #         h4("Model Comparison", style = "color: #800000; font-weight: bold; text-align: center;"),
# #         verbatimTextOutput("modelCompare", placeholder = TRUE)
# #       )
# #     )
# #   )
# # )

# # server <- function(input, output) {
# #   # Hi! I'm Asmi 🌸 — adding a smart auto-click to trigger default filters when the app loads!
# #   observe({
# #     shinyjs::click("goButton")
# #   })
  
# #   # Asmi's data fetch — pulling live stock data based on your selected ticker and date
# #   stock_data <- eventReactive(input$goButton, {
# #     getSymbols(input$stockSymbol, src = "yahoo", from = input$startDate, to = input$endDate, auto.assign = FALSE)
# #   })
  
# #   # Calculating latest pricing KPIs so you always know the trend at a glance!
# #   output$latestPrice <- renderText({
# #     req(stock_data())
# #     paste0("$", round(as.numeric(last(Ad(stock_data()))), 2))
# #   })

# #   output$meanPrice <- renderText({
# #     req(stock_data())
# #     paste0("$", round(mean(tail(Ad(stock_data()), 30)), 2))
# #   })

# #   output$sdPrice <- renderText({
# #     req(stock_data())
# #     paste0("$", round(sd(tail(Ad(stock_data()), 30)), 2))
# #   })
  
# #   # Here's the magic: 3 different models trained and ready to predict 📈
# #   model_fit <- reactive({
# #     data <- stock_data()
# #     price <- Ad(data)
# #     volume <- Vo(data)
# #     ts_data <- ts(price, frequency = 365)
# #     xreg <- as.numeric(volume)
    
# #     arimax_model <- auto.arima(ts_data, xreg = xreg)
# #     xreg_future <- rep(mean(xreg, na.rm = TRUE), input$forecastDays)
# #     arimax_forecast <- forecast(arimax_model, xreg = xreg_future, h = input$forecastDays)
    
# #     naive_model <- naive(ts_data, h = input$forecastDays)
    
# #     # XGBoost part
# #     df <- data.frame(
# #       y = as.numeric(price),
# #       lag1 = stats::lag(as.numeric(price), -1),
# #       lag2 = stats::lag(as.numeric(price), -2),
# #       vol = as.numeric(volume)
# #     )
# #     df <- na.omit(df)
# #     train_idx <- 1:(nrow(df) - input$forecastDays)
# #     test_idx <- (nrow(df) - input$forecastDays + 1):nrow(df)
# #     dtrain <- xgboost::xgb.DMatrix(data = as.matrix(df[train_idx, c("lag1", "lag2", "vol")]), label = df$y[train_idx])
# #     model_xgb <- xgboost::xgboost(data = dtrain, nrounds = 50, verbose = 0)
# #     dtest <- xgboost::xgb.DMatrix(as.matrix(df[test_idx, c("lag1", "lag2", "vol")]))
# #     xgb_pred <- predict(model_xgb, dtest)
    
# #     list(
# #       arimax = arimax_forecast,
# #       naive = naive_model,
# #       xgb = ts(xgb_pred, start = end(ts_data)[1] + 1, frequency = 365),
# #       actual = ts_data
# #     )
# #   })
  
# #   output$arimaxPred <- renderText({
# #     fits <- model_fit()
# #     paste0("$", round(as.numeric(tail(fits$arimax$mean, 1)), 2))
# #   })

# #   output$naivePred <- renderText({
# #     fits <- model_fit()
# #     paste0("$", round(as.numeric(tail(fits$naive$mean, 1)), 2))
# #   })

# #   output$xgbPred <- renderText({
# #     fits <- model_fit()
# #     paste0("$", round(as.numeric(tail(fits$xgb, 1)), 2))
# #   })
  
# #   output$stockPlot <- renderPlot({
# #     data <- stock_data()
# #     price <- Ad(data)  # Adjusted close price
# #     chartSeries(price, theme = chartTheme("white"), TA = NULL)
# #   })
  
# #   # Asmi's forecast chart — compare all models side-by-side to see who wins 🏆
# #   output$forecastPlot <- renderPlot({
# #     fits <- model_fit()
# #     forecast_days <- input$forecastDays
# #     future_index <- seq.Date(from = Sys.Date() + 1, by = "day", length.out = forecast_days)

# #     df_forecast <- data.frame(
# #       Day = future_index,
# #       ARIMAX = as.numeric(fits$arimax$mean),
# #       Naive = as.numeric(fits$naive$mean),
# #       XGBoost = as.numeric(fits$xgb[1:forecast_days])
# #     )

# #     ggplot(df_forecast, aes(x = Day)) +
# #       geom_line(aes(y = ARIMAX, color = "ARIMAX"), size = 1.1) +
# #       geom_line(aes(y = Naive, color = "Naive"), size = 1.1) +
# #       geom_line(aes(y = XGBoost, color = "XGBoost"), size = 1.1) +
# #       scale_color_manual(values = c("ARIMAX" = "#800000", "Naive" = "#A0522D", "XGBoost" = "#CD5C5C")) +
# #       labs(
# #         title = "Forecast Comparison",
# #         subtitle = "Aligned forecast across ARIMAX, Naive, and XGBoost",
# #         y = "Price ($)", x = "Forecast Date", color = "Model"
# #       ) +
# #       theme_minimal() +
# #       theme(legend.position = "bottom")
# #   })
  

# #   # output$comparisonPlot <- renderPlot({
# #   #   fits <- model_fit()
# #   #   forecast_days <- input$forecastDays
# #   #   future_index <- seq.Date(from = Sys.Date() + 1, by = "day", length.out = forecast_days)

# #   #   df <- data.frame(
# #   #     Day = rep(future_index, 3),
# #   #     Price = c(as.numeric(fits$arimax$mean), as.numeric(fits$naive$mean), as.numeric(fits$xgb[1:forecast_days])),
# #   #     Model = rep(c("ARIMAX", "Naive", "XGBoost"), each = forecast_days)
# #   #   )

# #   #   ggplot(df, aes(x = Day, y = Price, color = Model)) +
# #   #     geom_line(size = 1.1) +
# #   #     facet_wrap(~Model, ncol = 3, scales = "free_y") +
# #   #     scale_color_manual(values = c("ARIMAX" = "#800000", "Naive" = "#A0522D", "XGBoost" = "#CD5C5C")) +
# #   #     labs(title = "Individual Forecasts", x = "Date", y = "Predicted Price ($)") +
# #   #     theme_minimal() +
# #   #     theme(legend.position = "none")
# #   # })
  
# # output$comparisonPlot <- renderPlot({
# #   fits <- model_fit()
# #   forecast_days <- input$forecastDays
# #   future_index <- seq.Date(from = Sys.Date() + 1, by = "day", length.out = forecast_days)

# #   df <- data.frame(
# #     Day = rep(future_index, 3),
# #     Price = c(as.numeric(fits$arimax$mean), as.numeric(fits$naive$mean), as.numeric(fits$xgb[1:forecast_days])),
# #     Model = rep(c("ARIMAX", "Naive", "XGBoost"), each = forecast_days)
# #   )

# #   ggplot(df, aes(x = Day, y = Price, color = Model, linetype = Model)) +
# #     geom_line(size = 1.1) +
# #     facet_wrap(~Model, ncol = 3, scales = "free_y") +
# #     scale_color_manual(values = c("ARIMAX" = "#800000", "Naive" = "#A0522D", "XGBoost" = "#CD5C5C")) +
# #     scale_linetype_manual(
# #       values = c("ARIMAX" = "solid", "Naive" = "dashed", "XGBoost" = "dotdash")
# #     ) +
# #     labs(title = "Individual Forecasts", x = "Date", y = "Predicted Price ($)") +
# #     theme_minimal() +
# #     theme(legend.position = "none")
# # })

# #   output$modelSummary <- renderPrint({
# #     tryCatch({
# #       fits <- model_fit()
# #       fit <- fits$arimax$model
# #       cat("MODEL SUMMARY\n")
# #       cat("------------------------------------------------------------\n")
# #       cat("ARIMAX  :  AIC =", AIC(fit), " | Order =", paste(fit$arma[c(1,6,2)], collapse = ","), "\n")
# #       cat("Naive   :  No parameters, assumes flat prediction\n")
# #       cat("XGBoost :  Gradient boosting on lag1, lag2, volume (50 rounds)\n\n")
# #       cat("Usage Guidance:\n")
# #       cat("- Use ARIMAX for data with stable trends.\n")
# #       cat("- Naive is simple but effective in short horizons.\n")
# #       cat("- XGBoost handles complex non-linear dependencies.\n")
# #     }, error = function(e) {
# #       cat("Summary unavailable.")
# #     })
# #   })
  
# #   output$modelCompare <- renderPrint({
# #     fits <- model_fit()
# #     actual_tail <- tail(fits$actual, input$forecastDays)
# #     n <- min(length(actual_tail), length(fits$arimax$mean), length(fits$naive$mean), length(fits$xgb))
# #     arimax_rmse <- sqrt(mean((actual_tail[1:n] - fits$arimax$mean[1:n])^2, na.rm = TRUE))
# #     naive_rmse  <- sqrt(mean((actual_tail[1:n] - fits$naive$mean[1:n])^2, na.rm = TRUE))
# #     xgb_rmse    <- sqrt(mean((actual_tail[1:n] - fits$xgb[1:n])^2, na.rm = TRUE))

# #     cat("RMSE COMPARISON\n")
# #     cat("------------------------------------------------------------\n")
# #     cat(sprintf("ARIMAX   : %.2f\n", arimax_rmse))
# #     cat(sprintf("Naive    : %.2f\n", naive_rmse))
# #     cat(sprintf("XGBoost  : %.2f\n", xgb_rmse))
# #     cat("------------------------------------------------------------\n")
# #     best_model <- c("ARIMAX", "Naive", "XGBoost")[which.min(c(arimax_rmse, naive_rmse, xgb_rmse))]
# #     cat("Best Performing Model: ", best_model, "\n")
# #   })

# #   # Radar and donut charts for visual intuition — fun & useful! 🍩🕸️
# #   output$donutChart <- renderPlot({
# #     labels <- c("Short-Term", "Mid-Term", "Long-Term")
# #     values <- c(30, 45, 25)
# #     df <- data.frame(labels, values)
# #     ggplot(df, aes(x = "", y = values, fill = labels)) +
# #       geom_bar(width = 1, stat = "identity") +
# #       coord_polar("y", start = 0) +
# #       theme_void() +
# #       scale_fill_manual(values = c("#800000", "#A0522D", "#CD5C5C")) +
# #       labs(title = "Prediction Focus Allocation") +
# #       theme(plot.title = element_text(hjust = 0.5))
# #   })

# #   output$spiderChart <- renderPlot({
# #     library(fmsb)
# #     df <- data.frame(
# #       ARIMAX = c(80),
# #       Naive = c(65),
# #       XGBoost = c(90)
# #     )
# #     df <- rbind(rep(100, 3), rep(0, 3), df)
# #     radarchart(df, axistype = 1,
# #                pcol = "#800000", pfcol = scales::alpha("#800000", 0.4), plwd = 3,
# #                cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,100,20), cglwd = 0.8,
# #                vlcex = 1.2, title = "Model Confidence Comparison")
# #   })
# # }

# # shinyApp(ui = ui, server = server)

# ---- Dependency Installer ----
required_packages <- c(
  "shiny", "shinythemes", "quantmod", "forecast",
  "ggplot2", "tseries", "xgboost", "shinyjs",
  "tidyr", "dplyr", "fmsb"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(xgboost)
library(shinyjs)
library(tidyr)
library(dplyr)
library(fmsb)

# ---- UI ----
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),

  tags$head(
    tags$style(HTML("
      .shiny-text-output {
        white-space: pre-wrap;
        overflow-x: hidden;
      }
    "))
  ),

  tags$img(src = "header.png", style = "width: 100%; margin-bottom: 20px;"),
  tags$hr(style = "margin-top: -10px; margin-bottom: 20px; border-top: 2px solid #800000;"),

  fluidRow(
    column(
      width = 9,

      # KPI Panel
      div(
        style = "
          background-color: #fff5f5;
          padding: 20px;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-bottom: 20px;
        ",
        h3("Key Performance Indicators", style = "text-align: center; color: #800000;"),
        fluidRow(
          column(4,
            h3(textOutput("latestPrice"), style = "text-align: center; color: #800000;"),
            p("Latest Price", style = "text-align: center; color: #800000;")
          ),
          column(4,
            h3(textOutput("meanPrice"), style = "text-align: center; color: #800000;"),
            p("30-Day Average", style = "text-align: center; color: #800000;")
          ),
          column(4,
            h3(textOutput("sdPrice"), style = "text-align: center; color: #800000;"),
            p("30-Day Std Dev", style = "text-align: center; color: #800000;")
          )
        ),
        fluidRow(
          column(4,
            h3(textOutput("arimaxPred"), style = "text-align: center; color: #800000;"),
            p("Predicted by ARIMAX", style = "text-align: center; color: #800000;")
          ),
          column(4,
            h3(textOutput("naivePred"), style = "text-align: center; color: #800000;"),
            p("Predicted by Naive", style = "text-align: center; color: #800000;")
          ),
          column(4,
            h3(textOutput("xgbPred"), style = "text-align: center; color: #800000;"),
            p("Predicted by XGBoost", style = "text-align: center; color: #800000;")
          )
        )
      ),

      # Stock Price Chart
      div(
        style = "
          background-color: #fff5f5;
          padding: 0;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-bottom: 20px;
        ",
        h3("Stock Price Chart", style = "text-align: center; color: #800000;"),
        plotOutput("stockPlot")
      ),

      # Combined Forecast Plot
      div(
        style = "
          background-color: #fff5f5;
          padding: 0;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-bottom: 20px;
        ",
        h3("Forecast Plot", style = "text-align: center; color: #800000;"),
        plotOutput("forecastPlot"),
        p(
          "Line types: ARIMAX (solid), Naive (dashed), XGBoost (dotdash)",
          style = "text-align: center; color: #800000; font-size: 13px;"
        )
      ),

      # Facetted Forecast Breakdown
      div(
        style = "
          background-color: #fff5f5;
          padding: 0;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-bottom: 20px;
        ",
        h3("Model Forecast Breakdown", style = "text-align: center; color: #800000;"),
        plotOutput("comparisonPlot")
      )

    ),

    # Sidebar
    column(
      width = 3,
      div(
        style = "
          background-color: #fff5f5;
          padding: 20px;
          border-radius: 6px;
          border: 1px solid #800000;
        ",
        h4("Filters", style = "text-align: center; color: #800000;"),
        textInput("stockSymbol", "Stock Symbol:", value = "NVDA"),
        dateInput("startDate", "Start Date:", value = "2018-01-01"),
        dateInput("endDate", "End Date:", value = Sys.Date()),
        numericInput("forecastDays", "Days to Forecast:", value = 30, min = 7, max = 90),
        actionButton("goButton", "Apply Filters")
      ),
      div(
        style = "
          background-color: #fff5f5;
          padding: 15px;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-top: 20px;
        ",
        h4("Model Summary", style = "text-align: center; color: #800000;"),
        verbatimTextOutput("modelSummary")
      ),
      div(
        style = "
          background-color: #fff5f5;
          padding: 15px;
          border-radius: 6px;
          border: 1px solid #800000;
          margin-top: 15px;
        ",
        h4("Model Comparison", style = "text-align: center; color: #800000;"),
        verbatimTextOutput("modelCompare")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  # Trigger initial load
  observe({ shinyjs::click("goButton") })

  # Fetch stock data
  stock_data <- eventReactive(input$goButton, {
    getSymbols(
      input$stockSymbol,
      src = "yahoo",
      from = input$startDate,
      to   = input$endDate,
      auto.assign = FALSE
    )
  })

  # KPIs
  output$latestPrice <- renderText({
    req(stock_data())
    sprintf("$%.2f", last(Ad(stock_data())))
  })

  output$meanPrice <- renderText({
    req(stock_data())
    sprintf("$%.2f", mean(tail(Ad(stock_data()), 30)))
  })

  output$sdPrice <- renderText({
    req(stock_data())
    sprintf("$%.2f", sd(tail(Ad(stock_data()), 30)))
  })

  # Train models and forecasts
  model_fit <- reactive({
    data     <- stock_data()
    price    <- Ad(data)
    volume   <- Vo(data)
    ts_data  <- ts(price, frequency = 365)
    xreg     <- as.numeric(volume)

    # ARIMAX
    arimax_mod <- auto.arima(ts_data, xreg = xreg)
    future_x   <- rep(mean(xreg, na.rm = TRUE), input$forecastDays)
    arimax_fc  <- forecast(arimax_mod, xreg = future_x, h = input$forecastDays)

    # Naive
    naive_fc <- naive(ts_data, h = input$forecastDays)

    # XGBoost
    df <- data.frame(
      y    = as.numeric(price),
      lag1 = stats::lag(as.numeric(price), -1),
      lag2 = stats::lag(as.numeric(price), -2),
      vol  = as.numeric(volume)
    ) %>% na.omit()

    train_n <- nrow(df) - input$forecastDays
    dtrain  <- xgb.DMatrix(
      data  = as.matrix(df[1:train_n, c("lag1","lag2","vol")]),
      label = df$y[1:train_n]
    )
    model_xgb <- xgboost(dtrain, nrounds = 50, verbose = 0)
    dtest <- xgb.DMatrix(
      as.matrix(df[(train_n+1):nrow(df), c("lag1","lag2","vol")])
    )
    xgb_pred <- predict(model_xgb, dtest)

    list(
      actual = ts_data,
      arimax = arimax_fc,
      naive  = naive_fc,
      xgb    = ts(xgb_pred, start = end(ts_data)[1] + 1, frequency = 365)
    )
  })

  # Single-value predictions
  output$arimaxPred <- renderText({ sprintf("$%.2f", tail(model_fit()$arimax$mean, 1)) })
  output$naivePred  <- renderText({ sprintf("$%.2f", tail(model_fit()$naive$mean, 1)) })
  output$xgbPred    <- renderText({ sprintf("$%.2f", tail(model_fit()$xgb, 1)) })

  # Stock history plot
  output$stockPlot <- renderPlot({
    chartSeries(Ad(stock_data()), theme = chartTheme("white"), TA = NULL)
  })

  # Combined forecast plot
  output$forecastPlot <- renderPlot({
    fits <- model_fit()
    n    <- input$forecastDays
    days <- seq.Date(Sys.Date() + 1, by = "day", length.out = n)

    df <- tibble(
      Day   = rep(days, 3),
      Price = c(
        as.numeric(fits$arimax$mean),
        as.numeric(fits$naive$mean),
        as.numeric(fits$xgb[1:n])
      ),
      Model = rep(c("ARIMAX","Naive","XGBoost"), each = n)
    )

    ggplot(df, aes(Day, Price, color = Model, linetype = Model)) +
      geom_line(size = 1.1) +
      scale_color_manual(values = c(
        ARIMAX  = "#800000",
        Naive   = "#A0522D",
        XGBoost = "#CD5C5C"
      )) +
      scale_linetype_manual(values = c(
        ARIMAX  = "solid",
        Naive   = "dashed",
        XGBoost = "dotdash"
      )) +
      labs(
        title    = "Forecast Comparison",
        subtitle = "ARIMAX vs Naive vs XGBoost",
        x        = "Date",
        y        = "Price ($)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  # Facetted breakdown plot
  output$comparisonPlot <- renderPlot({
    fits <- model_fit()
    n    <- input$forecastDays
    days <- seq.Date(Sys.Date() + 1, by = "day", length.out = n)

    df <- tibble(
      Day   = rep(days, 3),
      Price = c(
        as.numeric(fits$arimax$mean),
        as.numeric(fits$naive$mean),
        as.numeric(fits$xgb[1:n])
      ),
      Model = rep(c("ARIMAX","Naive","XGBoost"), each = n)
    )

    ggplot(df, aes(Day, Price, color = Model, linetype = Model)) +
      geom_line(size = 1.1) +
      scale_color_manual(values = c(
        ARIMAX  = "#800000",
        Naive   = "#A0522D",
        XGBoost = "#CD5C5C"
      )) +
      scale_linetype_manual(values = c(
        ARIMAX  = "solid",
        Naive   = "dashed",
        XGBoost = "dotdash"
      )) +
      facet_wrap(~Model, ncol = 3, scales = "free_y") +
      labs(x = "Date", y = "Predicted Price ($)") +
      theme_minimal() +
      theme(legend.position = "none")
  })



  # Text summaries
  output$modelSummary <- renderPrint({
    tryCatch({
      fits <- model_fit()
      fit <- fits$arimax$model
      cat("MODEL SUMMARY\n")
      cat("------------------------------------------------------------\n")
      cat("ARIMAX  :  AIC =", AIC(fit), " | Order =", paste(fit$arma[c(1,6,2)], collapse = ","), "\n")
      cat("Naive   :  No parameters, assumes flat prediction\n")
      cat("XGBoost :  Gradient boosting on lag1, lag2, volume (50 rounds)\n\n")
      cat("Usage Guidance:\n")
      cat("- Use ARIMAX for data with stable trends.\n")
      cat("- Naive is simple but effective in short horizons.\n")
      cat("- XGBoost handles complex non-linear dependencies.\n")
    }, error = function(e) {
      cat("Summary unavailable.")
    })
  })
   output$modelCompare <- renderPrint({
    fits <- model_fit()
    actual_tail <- tail(fits$actual, input$forecastDays)
    n <- min(length(actual_tail), length(fits$arimax$mean), length(fits$naive$mean), length(fits$xgb))
    arimax_rmse <- sqrt(mean((actual_tail[1:n] - fits$arimax$mean[1:n])^2, na.rm = TRUE))
    naive_rmse  <- sqrt(mean((actual_tail[1:n] - fits$naive$mean[1:n])^2, na.rm = TRUE))
    xgb_rmse    <- sqrt(mean((actual_tail[1:n] - fits$xgb[1:n])^2, na.rm = TRUE))

    cat("RMSE COMPARISON\n")
    cat("------------------------------------------------------------\n")
    cat(sprintf("ARIMAX   : %.2f\n", arimax_rmse))
    cat(sprintf("Naive    : %.2f\n", naive_rmse))
    cat(sprintf("XGBoost  : %.2f\n", xgb_rmse))
    cat("------------------------------------------------------------\n")
    best_model <- c("ARIMAX", "Naive", "XGBoost")[which.min(c(arimax_rmse, naive_rmse, xgb_rmse))]
    cat("Best Performing Model: ", best_model, "\n")
  })

  # Radar and donut charts for visual intuition — fun & useful! 🍩🕸️
  output$donutChart <- renderPlot({
    labels <- c("Short-Term", "Mid-Term", "Long-Term")
    values <- c(30, 45, 25)
    df <- data.frame(labels, values)
    ggplot(df, aes(x = "", y = values, fill = labels)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("#800000", "#A0522D", "#CD5C5C")) +
      labs(title = "Prediction Focus Allocation") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$spiderChart <- renderPlot({
    library(fmsb)
    df <- data.frame(
      ARIMAX = c(80),
      Naive = c(65),
      XGBoost = c(90)
    )
    df <- rbind(rep(100, 3), rep(0, 3), df)
    radarchart(df, axistype = 1,
               pcol = "#800000", pfcol = scales::alpha("#800000", 0.4), plwd = 3,
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,100,20), cglwd = 0.8,
               vlcex = 1.2, title = "Model Confidence Comparison")
  })
}

# ---- Run App ----
shinyApp(ui, server)
