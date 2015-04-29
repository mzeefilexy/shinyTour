# shinyTour

Create guided tour of inputs and groups of a Shiny app. 

Inside a defined group in `ui.r`, every input label will be registered in a database. These entries could be described in edition mode. You can write knitr chunk in edit mode, so, there is no limit on what can be displayed in the draggable help boxes.

*This is an experimental implementation, you can expect a lot of changes.*

# Installation
```{r}
library(devtools)
install_github('fxi/shinyTour')
```

# Usage in a Shiny app

### Init tour  (server.r)
In the server part, the only thing to do is 
```{r}
library(shinyTour)
# tour settings the unique parameter to set is the path to write the sqlite file (self contained database).
tConf<-tourConfig$new("~/Desktop/tour.sqlite")
# tour manage all action.
tourMembersManager(input,session,tConf)
  ```


### UI elements (ui.r)
```{r}
# tour panel : to place in the beginning of a page
tourPanel(title="shinyTour")
# overlay
div(class="tour_overlay",style="display: none;")
# Group to describe
tourGroup(id="demo_main",title="Main page",...)
```

# Example

## Edit mode
In edit mode, you can change the text of the group or input to describe. 
You can write knitr chunck and it will be rendered in display mode. 
So, it could be a good idea to hide the edit checkbox if your application is published.
![shinyTour edit mode](https://raw.githubusercontent.com/fxi/shinyTour/master/inst/example/img/shinyTourEdit.png)


## Display mode
In display mode, let your users learn about visible elements from your applications.
![shinyTour display mode](https://raw.githubusercontent.com/fxi/shinyTour/master/inst/example/img/shinyTourDisplay.png)






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

