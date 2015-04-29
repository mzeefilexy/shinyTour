library(shiny)
library(shinyTour)
library(markdown)
library(RJSONIO)
library(knitr)
library(R6)
library(htmltools)

shinyServer( function(input, output,session) {  
  # demo output
  output$sample<-renderPlot({plot(rnorm(input$demo_observation))})
  # tour settings
  tConf<-tourConfig$new("~/Desktop/tour.sqlite")
  # tour manage all action.
  tourMembersManager(input,session,tConf) 
  # interrupt session, show inspection browser
  observeEvent(input$showBrowser,{
    browser()
  })

  # tests
#  dbCon<-dbConnect(SQLite(),tConf$dbPath) 
#  sql<-paste0("SELECT * FROM ",tConf$dbTableName," WHERE [group] != 0 ORDER BY position")
#    
#  tbl<-dbGetQuery(dbCon,sql)
#  testGroup<-tbl[tbl$type=='group',c('type','level','position','group','parentGroup')]
#  testItems<-tbl[tbl$type=='item',c('type','level','position','group','parentGroup')]
#  tbl$group
#  tbl$parentGroup
##

 })  
