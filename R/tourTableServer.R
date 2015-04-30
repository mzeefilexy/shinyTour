
#' Tour configuration 
#'
#' create a R6 object to handle tour configuration and store temporary data. 
#'
#' @param dbPath path to sqlite members self contained database
#' @param dbTableName name of table containing members info
#' @param idEditor id of ShinyAce editor used internaly
#' @param idSelectSelector id of select input for member's id
#' @param idSelectGroup id of select input for member's group

#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
tourConfig<-R6Class("tourConfig",
  public=list(
    # sqlite database path
    dbPath = NULL,
    # sqlite database table name
    dbTableName = "tourMembers",
    # html id for the editor
    idEditor = "tour_ace_editor",
    # html id for selectors select input
    idSelectSelector="tour_select_selector",
    # html id for group select input
    idSelectGroup="tour_select_group",
    # disable collecting members
    disableUpdate=F,
    # disable kniting (recommanded after edits are done)
    disableKnit=F,
    # member visible data
    memberTable = data.frame(NULL),
    # member position to querry for documentation column
    memberPos = 0,
    initialize = function(dbPath){
      stopifnot(!missing(dbPath))
      self$dbPath=dbPath
    },
    # replace member table, return first selector
    setTable = function(table){
      if(isTRUE(nrow(table)>0)){
        self$memberTable = table
        self$memberPos=1
        res<-list(
          selector = paste(table[1,'selector']),
          group = paste(table[1,'group'])
          )
        return(res)
      }
    },
    # query next member, return next selector
    nextMember = function(){
      pos = self$memberPos +1
      if(isTRUE(pos>nrow(self$memberTable)))pos=1
      self$memberPos=pos
      res<-list(
        selector = paste(self$memberTable[pos,]$selector),
        group = paste(self$memberTable[pos,]$group)
        )
      return(res)
    },
    # query previous member, return next selector
    previousMember = function(){
      pos = self$memberPos -1
      if(isTRUE(pos<1))pos=nrow(self$memberTable)
      self$memberPos=pos
      res<-list(
        selector = paste(self$memberTable[pos,]$selector),
        group = paste(self$memberTable[pos,]$group)
        )
      return(res)
    }
    )
  )


#
#tourConfig<-function(
#  dbPath='tourMembersTable.sqlite',
#  dbTableName='tourMembers',
#  idEditor='tour_ace_editor',
#  idSelectSelector="tour_select_selector",
#  idSelectGroup="tour_select_group",
#  disableUpdate=F,
#  disableKnit=F
#  ){
#  tConf<-list(
#    dbPath,dbTableName,idEditor,idSelectSelector,idSelectGroup,
#    disableUpdate,disableKnit
#    )
#  names(tConf)<-names(formals(tourConfig))
#  class(tConf)<-c(class(tConf),'tourConfig')
#  return(tConf)
#}


#' tourUpdateHtml
#' 
#'
#'
#'
#'
#'@export
tourUpdateHtml<-function(session,id=NULL,b64=NULL){
  jq<-paste0("$('#",id,"').html(atob('",b64,"'))")
  session$sendCustomMessage(type="tourJs",list(code=jq))
}



#' tourManager
#' 
#' Manage shinyTour
#' 
#' @param config configuration list of class "tourConfig"
#'
#' @export
tourMembersManager<-function(input=NULL,session=shiny::getDefaultReactiveDomain(),config=NULL){
  stopifnot("tourConfig" %in% class(config),!missing(input),!missing(session))
  config$dbPath<-file.path(dirname(config$dbPath),basename(config$dbPath))
  observe({
    tourInit(
      session,
      members = input$tour_members_list,
      dbPath=config$dbPath,
      dbTableName=config$dbTableName,
      idSelectGroup=config$idSelectGroup,
      disableUpdate=config$disableUpdate
      )
  },label="tourMembersManager: init or update based on collected members in ui")
  observeEvent(input[[config$idSelectGroup]],{
    tourFilterId(
      session,
      selectedGroup = input[[config$idSelectGroup]],
      dbPath=config$dbPath,
      dbTableName = config$dbTableName,
      idSelectSelector = config$idSelectSelector
      )
  },label="tourMembersManager: subset id by group")
  observeEvent(input[[config$idSelectSelector]],{
    tourFilterDoc(
      session,
      selectedId = input[[config$idSelectSelector]],
      dbPath = config$dbPath,
      dbTableName = config$dbTableName,
      editorId = config$idEditor)
  },label="tourMembersManager: query text according to select id")

  observe({
    panelEditEnabled<-isTRUE(input$tour_panel_enabled) && isTRUE(isolate(input$tour_panel_edit_mode))
    selectedSelector<-input[[config$idSelectSelector]]    
    tourHighlightMember(
      session,
      memberSelector=selectedSelector,
      enabled=panelEditEnabled 
      )
  },label="tourMembersManager: highlight selected member in edit mode")

  observeEvent(input[[config$idEditor]],{
    tourUpdateDoc(
      selectedId = input[[config$idSelectSelector]],
      text=input[[config$idEditor]],
      dbPath = config$dbPath,
      dbTableName = config$dbTableName
      )
  },label="tourMembersManager: update text")
  observeEvent(input$tour_update_render_knit,{
    res<-tourPreviewRender64(
      txt=input[[config$idEditor]],
      disableKnit=config$disableKnit
      )
    tourUpdateHtml(session,'tour_edit_html',res)
    #output$tour_edit_knited<-renderUI(res)
  },label="Update knit preview")

  # TODO: simplify these redundant observer and/or put them in R6 object ?
  observeEvent(input$tour_members_list_visible,{
    tbl <- tourTable(input$tour_members_list_visible)
    #tbl <- tbl[with(tbl,order(type,groupParentPos,position)),]
    #tbl$position <- 1:length(tbl)
    sel <- config$setTable(tbl)
    res<-tourDbDocTob64(
      selector=sel$selector,
      dbPath=config$dbPath,
      dbTableName=config$dbTableName,
      disableKnit=config$disableKnit
      ) 
    tourUpdateHtml(session,id='tour_step_html',b64=res)
    tourHighlightMember(session,sel$selector,TRUE)
    updateSelectInput(session,config$idSelectGroup,selected=sel$group)
    updateSelectInput(session,config$idSelectSelector,choices=sel$selector)

  },label="Observer: set first step")

  #
  observeEvent(input$tour_next,{
    # get next selector based on stored visible table
    sel<-config$nextMember()
    # convert db markdown doc to base64 encoded html
    res<-tourDbDocTob64(
      selector=sel$selector,
      dbPath=config$dbPath,
      dbTableName=config$dbTableName,
      disableKnit=config$disableKnit
      )  
    tourUpdateHtml(session,id='tour_step_html',b64=res)
    tourHighlightMember(session,sel$selector,TRUE)
    updateSelectInput(session,config$idSelectGroup,selected=sel$group)
    updateSelectInput(session,config$idSelectSelector,selected=sel$selector)

  })

  observeEvent(input$tour_previous,{
    # get previous selector based on stored visible table
    sel<-config$previousMember()
    # convert db markdown doc to base64 encoded html
    res<-tourDbDocTob64(
      selector=sel$selector,
      dbPath=config$dbPath,
      dbTableName=config$dbTableName,
      disableKnit=config$disableKnit
      )  
    tourUpdateHtml(session,id='tour_step_html',b64=res)
    tourHighlightMember(session,sel$selector,TRUE)
    updateSelectInput(session,config$idSelectGroup,selected=sel$group)
    updateSelectInput(session,config$idSelectSelector,selected=sel$selector)   
  })



}



#' tourCheckMembers
#' 
#' tourCheckMembers check html for tourGroup and input label
#' 
#' @param session The session object passed to function in shinyServer
#' @param visible Select only visible elements.
#' 
#' @return input$tour_members_list values
#' 
#' @export 
tourCheckMembers<-function(session,visible=FALSE){
  stopifnot(exists('session'))
  v='false'
  if(visible==TRUE)v='true'
  session$sendCustomMessage(type="tourJs",list(code=paste0("getTourMembers(",v,")"))) 
}


#' tourHighlightMember
#' 
#' Toggle red box around member selected
#'
#'@param session The session object passed to function in shinyServer
#'@param memberSelector Member to highlight
#'
#'
tourHighlightMember<-function(session,memberSelector,enabled){
  if(enabled){
    if(isTRUE(!is.null(memberSelector)) && isTRUE(length(memberSelector)>0)){
      js<-"$('.tour_member_active').removeClass('tour_member_active')"
      #if(enable){
      js<-paste0(js,";$('#",memberSelector,"').toggleClass('tour_member_active');")
      #}
      session$sendCustomMessage(type="tourJs",list(code=js)) 
    }
  }
}


#' tourTable
#' 
#' list of members found (input$tourMembers) to data.frame.
#' 
#' @param tourMembers input$tour_members_list
#' @return table of tour groups and input
#' 
#' @export
tourTable<-function(tourMembers){ 
  members<-tourMembers
  
  if(!is.null(members) && length(members)>0 && is.list(members)){
    membersL<-length(members)
    membersHeader<-names(members)
    table<-data.frame(matrix(unlist(members),ncol=membersL)) 
    #TODO:check why table contain number in factors instead of integer
    names(table)<-membersHeader
    table$groupParentPos<-as.integer(as.character(table$groupParentPos))
    table$position<-as.integer(as.character(table$position))
    table<-table[!table$group==0,] # items not in any group : discard
    # table order logic : present all group (type) first, then for each element, 
    # by position of parent group  
    table<-table[with(table,order(type,groupParentPos,position)),]
    table
  }
}

#' tourMembersToDb
#'
#' Write news members to existing DB table, create new if needed. 
#'
#' @param dbCon database connection
#' @param membersTable table of members and group generated by tourTable
#' @param dbTableName name of table in sqlite db
#'
#' @export
tourMembersToDb<-function(dbPath,membersTable,dbTableName='tourMembers'){
  require(RSQLite)  
  dbCon=dbConnect(SQLite(),dbPath)
  if(isTRUE(class(dbCon) == "SQLiteConnection")){
    if(isTRUE(!is.null(membersTable)) && isTRUE(nrow(membersTable)>0)){
      membersTable$doc=""
      tableExists <- dbTableName %in% dbListTables(dbCon)
      if(tableExists){
        # Write new members
        sql<-paste("SELECT id from",dbTableName)
        dbTableId <- dbGetQuery(dbCon,sql)
        # test for new and removed / hidden
        idNew<-!membersTable$id %in% dbTableId$id
        idHidden<-paste0("'",dbTableId$id[!dbTableId$id %in% membersTable$id],"'",collapse=',')
        membersTableSubset<-membersTable[idNew,]
        if(nrow(membersTableSubset)>0){
          dbWriteTable(dbCon,dbTableName,membersTableSubset,append=T)
        }
        # if id is not present in member list, set position to 0
        if(isTRUE(nchar(idHidden)>2)){
          sql<-paste0(
            "UPDATE ",dbTableName,
            " SET",
            " position=0,",
            " [group]=0,",
            " groupParentPos=0,",
            " level=0",
            " WHERE id IN (",idHidden,")"
            )
          dbGetQuery(dbCon,sql)
        }

        # udate position and title for existing members
        for(n in 1:nrow(membersTable)){
          nVal = membersTable[n,c('id','position','title','group','groupParent','groupParentPos','level')]
          sql<-paste0("UPDATE ",dbTableName,
            " SET ",
            " position=",nVal$position,
            ", title=",shQuote(nVal$title),
            ", [group]=",shQuote(nVal$group),
            ", groupParent=",shQuote(nVal$groupParent),
            ", groupParentPos=",shQuote(nVal$groupParentPos),
            ", level=",shQuote(nVal$level)," ",
            "WHERE id=",shQuote(nVal$id),";")
          dbGetQuery(dbCon,sql)
        }

      }else{
        fields <- list(
          type="TEXT",
          id="INTEGER",
          selector="TEXT",
          title="TEXT",
          group="TEXT",
          level="INTEGER",
          groupParent="TEXT",
          groupParentPos="INTEGER",
          position="INTEGER",
          time="date",
          doc="TEXT"
          )
        dbWriteTable(dbCon,dbTableName,membersTable,field.types=fields,row.names=FALSE)
      }

    }
  }
  dbDisconnect(dbCon)
}


#' tourInit
#'
# tourInit reacts only on members list sent to it. 
#' If nothing found, it will ask tourCheckMembers to look again at html for new members.
#' 
#' @param session shiny session
#' @param members list of members from input$tour_members_list (generated by shinyTour.js)
#' @param dbPath path to SQLite db
#' @param dbTableName name of SQLite db table that contains members info
#' @param idSelectGroup id of group select input defined in tourEditor()
#' @param disableUpdate avoid update
#' 
#' @export
tourInit<-function(session=NULL,members=NULL,dbPath="tour.sqlite",dbTableName="tourMembers",idSelectGroup="tourSelectGroup",disableUpdate=F){

  require(RSQLite)
  if(!disableUpdate){
    if(is.null(members)){
      # empty member list. Go back when you found something.
      tourCheckMembers(session,visible = F)
      return()
    }else{
      # tour init received members. Convert list to table,
      # send them to db and populate db table if new members id are found. 
      members<-tourTable(members)
      tourMembersToDb(dbPath,membersTable=members,dbTableName=dbTableName)
    }
  }
  dbCon<-dbConnect(SQLite(),dbPath)
  sql<-paste0('SELECT DISTINCT [group] FROM ',dbTableName,' ORDER BY groupParentPos, position')
  groups<-dbGetQuery(dbCon,sql)
  updateSelectInput(session,idSelectGroup,choices=groups)
  dbDisconnect(dbCon) 
}



#'tourFilterId
#'
#'Given a group of members, update id selectInput
#'
#'@param session current shiny session
#'@param selectedGroup group selected
#'@param dbPath path to sqlite db
#'@param dbTableName table containing members data
#'@param idSelectSelector id of selector's select input. in tourEditor()
#'@export
tourFilterId<-function(session,selectedGroup,dbPath='tour.sqlite',dbTableName="tourMembers",idSelectSelector="tourSelectId"){
  require(RSQLite)
  if(!is.null(selectedGroup) && isTRUE(nchar(selectedGroup)>0)){
    dbCon<-dbConnect(SQLite(),dbPath)
    val<-dbGetQuery(dbCon,paste0("SELECT position,selector,level,type FROM ",dbTableName," WHERE [group]='",selectedGroup,"' ORDER BY type,position"))
    valSelect=val$selector
    names(valSelect)=as.list(apply(val,1,function(x){paste0(x['type']," '",x['selector'],"' (",x['level'],")")}))
    updateSelectInput(session,idSelectSelector,choices=valSelect)
    dbDisconnect(dbCon)
  }  
}

#'tourFilterDoc
#'
#'Given an member id, update editor with "doc" field value
#'
#'@param session current shiny session
#'@param selectedId id selected
#'@param dbPath path to sqlite db
#'@param dbTableName name of table containing members data 
#'@param editorID outputID of the editor as defined in tourEditor()
#'
#'@export
tourFilterDoc<-function(session, selectedId,dbPath='tour.sqlite',dbTableName='tourMembers',editorId="tour_ace_editor"){
  require(RSQLite)
  if(!is.null(selectedId) && isTRUE(nchar(selectedId)>0)){
    dbCon<-dbConnect(SQLite(),dbPath)
    sql<-paste0("SELECT title,doc FROM ",dbTableName," WHERE selector='",selectedId,"'")
    val<-dbGetQuery(dbCon,sql)
    val$doc<-rawToChar(base64decode(val$doc))
    if(nchar(val$doc)==0)val$doc=val$title
    updateAceEditor(session,editorId,value=val$doc)
    dbDisconnect(dbCon)
  }  
}

#'tourUpdateDoc
#'
#'As soon as the doc in editor is modified, update SQLite table
#'
#'@param selectedId member id selected
#'@param text text value from editor
#'@param dbPath path to sqliteDb
#'@param dbTableName name of table containing members data
#'
#'@export
tourUpdateDoc<-function(selectedId,text,dbPath='tour.sqlite',dbTableName='tourMembers'){
  if(!is.null(selectedId) && isTRUE(nchar(selectedId)>0)){
    dbCon<-dbConnect(SQLite(),dbPath)
    text<-base64encode(charToRaw(text))
    sql<-paste0("UPDATE ",dbTableName," SET doc='",text,"' WHERE selector='",selectedId,"';")
    dbGetQuery(dbCon,sql)
    dbDisconnect(dbCon)
  }  
}

#'tourPreviewRender64
#'
#'
#'@param txt text/markdown/Rmarkdown to render in html
#'@param disableKnit option to disable knit. An option to use standard markdown could be implemented.
#'
#'
#'@export
tourPreviewRender64<-function(txt,disableKnit=T){
  if(!disableKnit){
    require(knitr)
    if(!is.null(txt) && nchar(txt)>1){
      rendered<-HTML(
        knit2html(text = txt, quiet = TRUE, 
          #NOTE: options from markdownHTMLoptions()
          options=c("toc","base64_images","highlight_code","fragment_only"))
        )
      rendered<-base64encode(charToRaw(rendered))
      return(rendered)
    }
  }
}
#' tourDbDocTo64
#'
#' extract doc field (stored in ascii base64) from db for given selector, knit R code , produce html, convert back to b64
#'
#'@param dbPath path to sqlite databse
#'@param dbTableName table containing tour information 
#'@param disableKnit disable knit (see tourPreviewRender64)
#'@param selector selector id for which to extract doc column content
#'
#'@export
tourDbDocTob64<-function(dbPath,dbTableName,disableKnit,selector=NULL){
  dbCon <- dbConnect(SQLite(),dbPath)
  sql <- paste0("SELECT doc FROM ",dbTableName," WHERE selector='",selector,"'")
  text <- dbGetQuery(dbCon,sql)$doc
  text <- rawToChar(base64decode(text))
  res <- tourPreviewRender64(txt=text,disableKnit)

}



