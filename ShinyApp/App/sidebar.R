argonSidebar <- argonDashSidebar(
  vertical =TRUE,
  skin = "light",
  background = "white",
  size = "s",
  side = "left",
  id = "my_sidebar",
  brand_url = "http://www.google.com",
  brand_logo = "https://demos.creative-tim.com/argon-design-system/assets/img/brand/blue.png",
  
  br(),
  argonSidebarMenu(
    
    argonSidebarItem(
      tabName = "summary",
      icon = argonIcon(name = "planet", color = "warning"),
      "Summary"
    ),
    argonSidebarItem(
      tabName = "visualization",
      icon = argonIcon(name = "tv-2", color = "info"),
      "Var Importance"
    ),
    argonSidebarItem(
      tabName = "models",
      icon = argonIcon(name = "ungroup", color = "info"),
      "Modeltime"
    ),
    br()
  )
)