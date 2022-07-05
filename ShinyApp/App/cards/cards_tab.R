visualizations_tab <- argonTabItem(
  tabName = "visualization",
  
  argonRow(
    
    # Horizontal Tabset
    argonColumn(
      width = 12,
      argonH1("Variable Importance", display = 4),
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Correlation plot",
          active = TRUE,
          plotOutput("cor")
        ),
        argonTab(
          tabName ="Variable Importance",
          active = FALSE,
          plotOutput("VI")
        ),
        argonTab(
          tabName ="Scatter Plot",
          active = FALSE,
          plotlyOutput("scatterPlot")
        )
        
      )
    )
  )
)
