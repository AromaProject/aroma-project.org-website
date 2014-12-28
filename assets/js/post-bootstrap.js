/* Auto-adjust height of navigation bar in case it
   word wraps into two or more rows or some devices */
$(document).ready(function(){
  $(document.body).css('padding-top', $('#topnavbar').height() + 10);
  $(window).resize(function(){
    $(document.body).css('padding-top', $('#topnavbar').height() + 10);
  });
});
