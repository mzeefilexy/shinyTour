library(shiny)
library(shinyTour)


shinyUI(
  fluidPage(
    tourPanel(title="shinyTour"),
    div(class="tour_overlay",style="display: none;"),
    tourGroup("demo_main","Main page for demo page", titlePanel("shinyTour!"),  
      sidebarLayout(
        sidebarPanel(
          
          tourBtnTogglePanel('Start tour'),
          checkboxInput('tour_panel_edit_mode','Edit mode'),

          tourGroup('demo_side_bar',"Sidebar panel",
            tourGroup('demo_third_level_first',"Third level 1",
              numericInput("demo_numeric_1","Type a first number",value=0),
              tourGroup('demo_fourth_level_first',"Fourth level 1",
                numericInput("demo_numeric_4","Type another number",value=0)
                )
              ),
            tourGroup('demo_third_level_second',"Third level 2",
              numericInput("demo_numeric_2","Type second number",value=0)
              ),
            textInput('demo_test_input',label = 'Enter some text',value = "empty"),
            sliderInput("demo_observation",
              "Number of observations:",
              min = 0,
              max = 1000,
              value = 500),
            selectInput('demo_letters','Select a letter',choices=letters[1:26]),
     tourGroup(id='demo_inner_group','Inner Group',  
            checkboxInput('demo_hidden',"Show more"),
            conditionalPanel(condition="input.demo_hidden==true",                                     
                radioButtons('demo_radio',"choose radio",c(a=2,b=3)),
                textInput("demo_text","Super test text")
                )
              ),
            actionButton('start_tour','Start tour'),
          actionButton('showBrowser','show browser')
            )
          ),
        mainPanel(
          tourGroup("demo_plot_output","Demo of plot inside group",
          plotOutput('sample')
          ),
          textOutput('tour_step_knited')
          )
        )
      )
    )
  )





