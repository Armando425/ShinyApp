summary_tab <- argonTabItem(
  tabName = "summary",
  
  argonRow(
    
    # Horizontal Tabset
    argonColumn(
      width = 12,
      argonH1("Summary", display = 4),
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Summary",
          active = TRUE,
          verbatimTextOutput("summary")
        ),
        argonTab(
          tabName ="Vizualizations",
          active = FALSE,
          plotOutput("Seasonplot"),
          plotOutput("Monthplot")
          
        )
        
      )
    )
  )
)