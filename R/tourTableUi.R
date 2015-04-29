.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyTour',
      directoryPath = system.file('www', package='shinyTour'))
    .global$loaded <- TRUE
  }
  HTML("")
}




#'tourGroup
#'
#' define new group for shiny tour
#'
#' @param idGroup group id. Ex. subsection-intro
#' @param title title of the group
#' @param ... children
#' @export
tourGroup<-function(idGroup,title,...){
  tagList(singleton(tags$head(
        initResourcePaths(),
        #tags$script(type="text/javascript",src='shinyTour/intro/intro.min.js'),
        #tags$link(rel="stylesheet",type="text/css",href="shinyTour/intro/introjs.min.css"))),
        #tags$script(type="text/javascript",src='shinyTour/html2canvas/html2canvas.js'),
        #tags$script(type="text/javascript",src='shinyTour/shepherd/shepherd.min.js'),
        #tags$link(rel="stylesheet",href="shinyTour/shepherd/shepherd-theme-arrows.css"),
        tags$script(type="text/javascript",src='shinyTour/shinyTour.js'),
        #tags$script(type="text/javascript",src="shinyTour/draggable/draggable.min.js"),
        tags$script(type="text/javascript",src="shinyTour/draggable/draggable.js"),
        tags$link(rel="stylesheet",href='shinyTour/shinyTour.css'))
      ),        
    div(class='tour_group',tour_title=title,id=idGroup,...)
    )
}



#' tourBtnToggleEditor
#' 
#' toggle help panel
#' 
#' @param text text of the button
#' @export
tourBtnTogglePanel<-function(text){
  actionButton('tour_btn_panel',label=text,class="btn btn-default btn-xs tour_btn_toggle_panel")
}

##' tourBtnToggleDisplay
##' 
##' toggle help panel
##' 
##' @param text text of the button
#tourBtnToggleDisplay<-function(text){
#  actionButton('tour_btn_display',label=text,class="btn btn-default tour_btn_toggle_display")
#}



#'tourPanel
#'
#'Editor for selected step
#'
#'@param idEditor outputId of ace editor
#'@param idSelectGroup id of group selectize input
#'@param idSelectID id of id selectize input
#'
#'@export
tourPanel<-function(title="Tour step content edit",idEditor="tour_ace_editor",idSelectGroup="tour_select_group",idSelectSelector="tour_select_selector",width=12){
  tagList(
    div(id="tour_panel",style="display: none;",class="panel panel-default",
      div(class="panel-heading", 
        span(title),
        div(class="tour_panel_head_btn",
          tourBtnTogglePanel(icon('times'))
          )),
      div(class="panel-body",
 div(class='btn-group',
                actionButton('tour_previous','',class='btn-inline btn-primary btn-xs',icon=icon('backward')),
                actionButton('tour_next','',class='btn-inline btn-primary btn-xs',icon=icon("forward"))
                ),
        conditionalPanel(condition="input.tour_panel_edit_mode===false",
                           hr(),
              div(id="tour_step_html",class="tour_knited")
          ),
        conditionalPanel(condition="input.tour_panel_edit_mode===true",
          div(class="form-inline",
            selectInput(idSelectGroup,"Select group",choices=""),
            selectInput(idSelectSelector, "Select step id (type,selector,level)",choices="")
            ),
          tabsetPanel(type='pills',
            tabPanel("Edit",
              hr(),
              aceEditor(outputId = idEditor, "", mode="markdown",theme="github"),
              tags$script("editor.setOption('wrap',true)"),
              tags$script("editor.setKeyboardHandler('ace/keyboard/vim');")
              ),
            tabPanel("Render",  
              hr(),
              actionButton("tour_update_render_knit","update"),
              div(id="tour_edit_html",class="tour_knited")
              )
            )
          )
        ),
      singleton( tags$script(paste(
            "elementToDrag=$('#tour_panel');",
            "handle=elementToDrag.children('.panel-heading')[0];",
            "draggable(elementToDrag[0],handle);"
            ))) 
      )
    )
}

##'tourDisplay
##'
##'Display panel for selected step
##'
##'
##'@export
#tourDisplay<-function(title="Tour step content edit",width=12){
#  tagList(
#    div(id="tour_display_panel",style="display: none;",class="panel panel default",
#      div(class="panel-heading tour_edit_panel_head",
#        title,
#        tourBtnToggleDisplay(icon('times'))
#        ),
#      div(class="panel-body",
#        tabPanel("Render",
#          uiOutput('tour_display_output_knited')
#          )
#        )), 
#    #div(class="tour_overlay",style="display: none;"),
#    singleton(tags$script(paste("draggable($('#tour_display_panel')[0]);")))
#    )
#}
#

