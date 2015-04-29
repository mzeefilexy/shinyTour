# shinyTour

Create tour of inputs and groups.

# Installation

```{r}
library(devtools)
install_github('fxi/shinyTour')
```

# Usage

Init tour init (server.r)
```{r}
library(shinyTour)
# tour settings
tConf<-tourConfig$new("~/Desktop/tour.sqlite")
# tour manage all action.
tourMembersManager(input,session,tConf)
  ```


Tour UI (ui.r)
```{r}
# tour panel : to place in the beginning of a page
tourPanel(title="shinyTour")
# overlay
div(class="tour_overlay",style="display: none;")
# Group to describe
tourGroup(id="demo_main",title="Main page",...)
```

# Example

```{r}
library(shinyTour)
  server <- function(input, output,session) {
# tour settings
    tConf<-tourConfig$new("~/Desktop/tour.sqlite")
# tour manage all action.
      tourMembersManager(input,session,tConf)

      output$distPlot <- renderPlot({
          hist(rnorm(input$obs), col = 'darkgray', border = 'white')
          })
  }

ui <- fluidPage(
    tourPanel(title="shinyTour"),
    div(class="tour_overlay",style="display: none;"),
    tourGroup(id="demo_app","Your application",
      sidebarLayout(
        tourGroup(id="demo_side_bar",title="Your side bar",
          sidebarPanel(
            tourBtnTogglePanel('Start tour'),
            checkboxInput('tour_panel_edit_mode','Edit mode'),
            sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
            )
          ),
        mainPanel(plotOutput("distPlot"))
        ))
    )

shinyApp(ui = ui, server = server)
```

