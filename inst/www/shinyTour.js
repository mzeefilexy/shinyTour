
/* register custom message handler.*/
Shiny.addCustomMessageHandler('tourJs',function(js){eval(js.code)})


$( document ).ready(function() {
  /* display edit panel of button of class tour_btn_toggle_editor is pressed*/
  $('.tour_btn_toggle_panel').click(function() {
    $('.tour_member_active').removeClass('tour_member_active');
    $('#tour_panel').toggle();
    $('.tour_overlay').toggle();
    editorCheck=$("#tour_panel").is(":visible");
    /* say to shiny that tour panel is visible/enabled: tourHighlightMember() will know that's time for highlight selected element */
    Shiny.onInputChange("tour_panel_enabled",editorCheck);
    if(editorCheck){
      getTourMembers(visible=true);
    }
  });
});

getTourMembers= function(visible,choice){
  var start= Date.now();
  var titles = [];
  var ids = [];
  var idsGroup = [];
  var idsParentGroup = [];
  var idsLevel = [];
  var selectors = [];
  var selector="";
  var types = [];
  var v = "";
  var i = 0;
  var pos = [];
  var posGroupParent = [];
  var time = [];


  if(visible==true)v=":visible"

    /* push group members data*/
    $('.tour_group'+v).each(function(){
      i=i+1;
      pos.push(i);
      time.push(Date.now());
      id=$(this).attr('id');
      selector=id;
      types.push('group');
      titles.push($(this).attr('tour_title')),
      ids.push(id);
      selectors.push(selector);
      idsGroup.push(id);
      idsLevel.push($(this).parents('.tour_group').length+1);
      idsParentGroup.push($(this).parents('.tour_group').attr('id'));  
    })



  /* for each '.form-group' with '.label' as children, populate members list*/
  $(".form-group"+v).children("label").each(function(){
    i=i+1;
    pos.push(i);
    time.push(Date.now());
    types.push('item');
    id=$(this).attr('for');
    selector = "tour_" + id;
    $(this).parent('.form-group').attr('id', selector); /*set new ID for form-group */
    ids.push(id); /* destination id */
    selectors.push(selector); /* new id for tour ref */
    titles.push($(this).text()); /* text of label as title*/
    idsGroup.push( $(this).closest('.tour_group').attr('id')); /* parent tour group*/
    idsLevel.push( $(this).parents('.tour_group').parents('.tour_group').length +1); /* recursion level  */
    idsParentGroup.push( $(this).parents('.tour_group').parents('.tour_group').attr('id')); /* container group */
  }
  );




  /* remove undefined*/ 
  for( var i = 0; i < ids.length; i++ ) {
    if( typeof(ids[i])==="undefined" ) {
      ids[i] = NA;
    };
    if( typeof(titles[i])==="undefined" ) {
      titles[i] =0;
    };
    if( typeof(idsLevel[i])==="undefined" ) {
      idsLevel[i] = 0;
    };
    if( typeof(idsLevel[i])==="undefined" ) {
      idsLevel[i] = 0;
    };

    if( typeof(idsGroup[i])==="undefined" ) {
      idsGroup[i] = 0;
      idsParentGroup[i] = 0;
      posGroupParent.push(0);
    }else{ 
      if( typeof(idsParentGroup[i])==="undefined" ) {
        idsParentGroup[i] = 0;
        posGroupParent.push(0);
      }else{
        posGroupParent.push(pos[idsGroup.indexOf(idsParentGroup[i])]);
      }
    }

    if( typeof(posGroupParent[i])==="undefined"){
      posGroupParent[i]=0;
    }

//console.log(i+")"+"group:"+idsGroup[i]+"; posGroupParent:"+posGroupParent[i]+";idsParentGroup: "+idsParentGroup[i])

  }
  if(visible==false){
    tourMembers = {type:types,id:ids,selector:selectors,title:titles,group:idsGroup,level:idsLevel,groupParent:idsParentGroup,groupParentPos:posGroupParent,position:pos,time:time}
    Shiny.onInputChange("tour_members_list", tourMembers);
  }else{
    tourMembers = {type:types,id:ids,selector:selectors,title:titles,group:idsGroup,level:idsLevel,groupParent:idsParentGroup,groupParentPos:posGroupParent,position:pos,time:time}
    Shiny.onInputChange("tour_members_list_visible", tourMembers);

  }
  var end = Date.now();
  var time = end - start;
  var membersLength = ids.length
  console.log('shinyTour generate member list (n='+membersLength +';v='+visible +'): ' + time," [ms]");

}


