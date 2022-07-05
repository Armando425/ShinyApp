modeltime_tab <- argonTabItem(
  tabName = "models",
  
  argonRow(
    
    # Horizontal Tabset
    argonColumn(
      width = 12,
      argonH1("Models", display = 4),
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:4, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Time Series Cross Validation",
          active = TRUE,
          plotlyOutput("crossValidation")
        ),
        argonTab(
          tabName ="Forecast Plot Test",
          active = FALSE,
          plotlyOutput("forecastPlotTest")
        ),
        argonTab(
          tabName ="Models Performance",
          active = FALSE,
          tableOutput("performance")
        ),
        argonTab(
          tabName ="Forecast Plot",
          active = FALSE,
          plotlyOutput("forecastPlot")
        )
        
      )
    )
  )
)
