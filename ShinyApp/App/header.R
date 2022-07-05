
argonHeader <- argonDashHeader(
  gradient = TRUE,
  color = "primary",
  separator = TRUE,
  separator_color = "secondary",
  argonCard(
    title = "Dashboard",
    hover_lift = TRUE,
    width = 12,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonIcon("atom"),
    status = "primary",
    background_color =NULL,
    gradient = FALSE, 
    floating = FALSE,
    #---------------------------------------------------------------------------------------
    argonRow(
      argonColumn(
        width=3,fileInput("file1","Choose CSV File")  
        ),
      argonColumn(
        width=3,
        selectInput("col", "Time: Date format yyyy-mm-dd",character(0))),
      argonColumn(
        width=3,
        selectInput("target", "Target variable:numeric",character(0))),
      argonColumn(
        width=3,
        selectInput("exploratory", "Exploratory variable:numeric",character(0)))
     
      
      
      
    ),
    argonRow(
      
      argonColumn(
        width=3,
        selectInput("fill", "Groups",character(0))),
      argonColumn(
        width=3,
        sliderInput("train_1", "train data for h2o:", min=0,max=0,value=0)),
      argonColumn(
        width=3,
        sliderInput("test_1", "test data for modeltime models:", min=0,max=0,value=0)),
      argonColumn(
        width=3,
        numericInput("ahead", "Months to forecast:", 12))
      
    ),
    #----------------------------------------------------------------------------------------
    
    
    
  )
)