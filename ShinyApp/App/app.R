packages = c("ggcorrplot","shiny","corrplot","argonR","argonDash","magrittr","forecast","gplots","dygraphs","h2o","tibble","ggplot2","dplyr","vip","Metrics","ggplot2","utf8","rpivotTable","plotly","tidymodels","modeltime","timetk","lubridate","tidyverse","glmnet")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if(!require(x,character.only = TRUE)){
      install.packages(x,dependencies = TRUE)
      library(x,character.only = TRUE) 
    }
  }
)


source("sidebar.R")
source("header.R")

source("cards/cards_tab.R")
source("tabs/tabsets_tab.R")
source("Tabs-7/Tabs_7.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "Dashboard",
    description = " Dash Test",
    sidebar = argonSidebar,
    br(),
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        
        summary_tab,
        visualizations_tab,
        modeltime_tab
        
      )
    )
  
  ),
  server = function(input, output,session) {
    
    data <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      df <- as.tibble(read.csv(inFile$datapath))
      df$Time <- as.Date(df$Time)
      df
      
    })
    
    dt <-reactive({
      data() %>%  mutate(Day =day(Time),Month = month(Time),Year = year(Time),Time = as.Date(Time, format = "%Y-%m-%d"))
    })
    
    DayStats <-reactive({
      dt() %>% group_by(Time,Year,Month,Day) %>% summarize(DayAvgLoad = mean(Load),DayAvgFullTemp = mean(full_temp),DayAvgFullHumid = mean(full_humid))
    }) 
    
    DayData <- reactive({
      DayStats() %>% ungroup(Time,Year,Month,Day)
    })
    
    first_year <- reactive({
      d <- as.POSIXct(data()[[input$col]],format="%Y-%m-%d")
      f_viti <- as.integer(format(d[1],format = "%Y"))
      
      f_viti
    })
    last_year <- reactive({
      d_1 <- as.POSIXct(data()[[input$col]],format="%Y-%m-%d")
      l_viti <- as.integer(format(d_1[as.integer(nrow(data()))],format = "%Y"))
      l_viti
    })
    #
    train <- reactive({
      head(DayData(),input$train_1)
    })
    #
    #test <- reactive({
    #  tail(data(),input$test_1)
    #})
    
    observeEvent(data(), {
      updateSelectInput(session, "col", choices = names(DayData()[]))
    })

    observeEvent(data(), {
      updateSelectInput(session, "target", choices = names(DayData()[]))
    })

    observeEvent(data(), {
      updateSelectInput(session, "exploratory", choices = names(DayData()[]))
    })

    observeEvent(data(), {
      updateSelectInput(session, "fill", choices = names(DayData()[]))
    })
    
   
    
    observeEvent(data(), {
      updateSliderInput(session,"train_1",min=0,max=nrow(DayData()),value=0,step=1)
    })
    
    observeEvent(data(), {
      updateSliderInput(session, "test_1",min=0,max=round(as.integer(nrow(DayData())/30)*0.3),value=0,step=1)
    })
    # updateSliderInput(session, "test_1",min=(as.integer(nrow(data())))*0.2,max=(as.integer(nrow(data())))*0.3,value=(as.integer(nrow(data())))*0.2,step=(as.integer(nrow(data())))*0.05)
    
    output$summary <- renderPrint({
      dataset <- data()
      summary(dataset)
    })
    
    output$Seasonplot <- renderPlot({
      
      data_1 <- ts(DayData()[[input$target]],start = c(first_year(),1),frequency = 365)
      
      
      seasonplot(data_1,ylab =names(DayData()[input$target]),xlab = "Day",main = names(DayData()[input$target]),year.labels = TRUE, year.labels.left = TRUE, col= 2:6, pch=19)
      
      
      
    })
    output$Monthplot <- renderPlot({
      data_2 <- ts(DayData()[[input$target]],start = c(first_year(),1),frequency = 12)
      
      
      monthplot(data_2,ylab =names(DayData()[input$target]), xlab = "Month",main = names(DayData()[input$target]), col = 2:6, pch=19)
      
    })
    
    output$cor <- renderPlot({
      #-which(names(data()[]) == input$col)
     # te_dhenat <- data()
     # cor.mat=cor(te_dhenat[,c(1,2,3,4,5,6,8,9)]) # matrica korrelacionit
     # corrplot(cor.mat)
      #chart.Correlation(data()[], histogram = TRUE, method = "pearson")
      model.matrix(~0+., data=data()[,c(1,2,3,4,5,6,8,9)]) %>% 
        cor(use="pairwise.complete.obs") %>% 
        ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
     
      
    })
    
    output$scatterPlot <- renderPlotly({
      scatter_plot <- plot_ly(data = DayData(), x = DayData()[[input$exploratory]], y = DayData()[[input$target]], color = DayData()[[input$fill]])%>% layout(plot_bgcolor = "#e5ecf6",xaxis=list(title=names(DayData()[input$exploratory])),yaxis=list(title=names(DayData()[input$target])))
      scatter_plot
   })
    
    output$VI <- renderPlot({
      h2o.init(nthreads = -1, max_mem_size = '16G', ip = "127.0.0.1", port = 50001)
      #h2o.init(max_mem_size = "16G")
      h2o.no_progress()
      
      E<-as.tibble(DayData())
      
      
      y <- names(DayData()[input$target])
      x <- setdiff(names(train() %>% as.h2o()),c(y,"Time"))
      # Modeli random forest
      rft_model <- 
        h2o.randomForest(
          x = x, 
          y = y, 
          training_frame = train() %>% as.h2o(),
          nfolds = 10,
          ntrees = 500,
          stopping_metric = "RMSE",
          stopping_rounds = 10,
          stopping_tolerance = 0.005,
          seed = 1975
        )
      
      # Modeli gradient boosting machine
      gbm_model <-  
        h2o.gbm(
          x = x, 
          y = y, 
          training_frame = as.h2o(train()),
          nfolds = 10,
          ntrees = 500,
          stopping_metric = "RMSE",
          stopping_rounds = 10,         
          stopping_tolerance = 0.005,
          seed = 1975
        )
      
      # Modeli linear i pergjithesuar (glm)
      glm_model <- 
        h2o.glm(
          x = x, 
          y = y, 
          training_frame = as.h2o(train()),
          nfolds = 10,
          family = "gaussian",
          seed = 1975
        )
      
      
      
      p_glm <- vip(glm_model) + ggtitle("GLM")
      p_rft <- vip(rft_model) + ggtitle("RF")
      p_gbm <- vip(gbm_model) + ggtitle("GBM")
      
      grid.arrange(p_glm, p_rft, p_gbm,nrow = 2)
    })
   
    
   Energjia_tbl <- reactive({
      
      energjia_tbl <- DayData() %>%
        select(input$col,input$target) %>%set_names(c("date", "value"))   
      energjia_tbl
    })
    
    splits <- reactive({
      split <- Energjia_tbl() %>%time_series_split(assess = paste(input$test_1,"months",sep = " "), cumulative = TRUE)
      split
    })
    
    
    output$crossValidation <- renderPlotly({
      splits() %>%tk_time_series_cv_plan() %>%plot_time_series_cv_plan(date,value, .interactive = TRUE)
      
    })
    
    
    model_fit_arima <- reactive({
      arima_reg() %>% set_engine("auto_arima") %>% fit(value ~ date, training(splits()))
    })
    
    model_fit_prophet <- reactive({
      prophet_reg(seasonality_daily = TRUE) %>%set_engine("prophet") %>%fit(value ~ date, training(splits()))
    })
    
    recipe_spec <-reactive({
      recipe(value ~ date, training(splits())) %>%
        step_timeseries_signature(date) %>%
        step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                contains("second"), contains("xts")) %>%
        step_fourier(date, period = 365, K = 5) %>%
        step_dummy(all_nominal())
    }) 
    
    model_spec_glmnet <-reactive({
      linear_reg(penalty = 0.01, mixture = 0.5) %>%set_engine("glmnet")
    }) 
    
    workflow_fit_glmnet <-reactive({
      workflow() %>% add_model(model_spec_glmnet()) %>%add_recipe(recipe_spec() %>% step_rm(date)) %>%fit(training(splits()))
    }) 
    
    model_spec_prophet_boost <- reactive({
      prophet_boost(seasonality_daily = TRUE) %>%
        set_engine("prophet_xgboost")
    }) 
    
    workflow_fit_prophet_boost <- reactive({
      workflow() %>%add_model(model_spec_prophet_boost()) %>%add_recipe(recipe_spec()) %>%fit(training(splits()))
    })
    
    model_table <-reactive({
      modeltime_table(
        model_fit_arima(), 
        model_fit_prophet(),
        workflow_fit_glmnet(),
        workflow_fit_prophet_boost()
      ) 
    }) 
    
    calibration_table <- reactive({
      model_table() %>% modeltime_calibrate(testing(splits()))
    })
    
    # training errors 
    # calibration_table1 <- reactive({
    #   model_table() %>% modeltime_calibrate(training(splits()))
    # })
    
    output$forecastPlotTest <- renderPlotly({
      calibration_table() %>%
        modeltime_forecast(actual_data = Energjia_tbl()) %>%
        plot_modeltime_forecast(.interactive = TRUE)
    })
    
    output$performance <- renderTable({
      calibration_table() %>%
        modeltime_accuracy() %>%
        table_modeltime_accuracy(.interactive = FALSE)
      
    })
    
    output$forecastPlot <- renderPlotly({
      calibration_table() %>%
        modeltime_refit(Energjia_tbl()) %>%
        modeltime_forecast(h = paste(input$ahead,"months",sep = " "), actual_data = Energjia_tbl()) %>%
        plot_modeltime_forecast(.interactive = TRUE)
    })
    #---------------------------------------------------------------------------------------
  
  }
)