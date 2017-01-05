/* Center images */
$(document).ready(function(){
  var imgs = $('img'), i, img, parent;
  for (i=0; i < imgs.length; i++) {
    img = imgs[i];
    // center an image if it is the only element of its parent
    parent = img.parentElement;
    if (parent.childElementCount === 1)
      parent.style.textAlign = 'center';
  }
});



/* Collapse Appendix */
$(document).ready(function(){
  var section = $(":header:contains('Appendix')").last();
  if (section.length > 0) {
    var content = section.nextAll();
    var button = $('<button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#appendix" aria-controls="#appendix">Show content</button>');
    section.after(button);
    var div = $('<div id="appendix" class="collapse" aria-expanded="false"></div>');
    div.on('hidden.bs.collapse', function() {
      button.text('Show content');
    });
    div.on('shown.bs.collapse', function() {
      button.text('Hide content');
    });
    div.append(content);
    button.after(div);
  }
});




/*
 Make code snippets etc. collapsable, iff marked, e.g.

 <a data-toggle="collapse" data-target="#sessionInfo">expand/collapse</a>     
 ```{r collapse}
   ...
 ```
*/

$(document).ready(function(){
  var items = $("a[data-toggle='collapse'"), item, id, id2, root, target;
  var i, j;
  for (i = 0; i < items.length; i++) {
    item = items[i];
    id = $(item).attr('data-target');
    target = $(id);
    if (target.length == 0) {
      id2 = id.substring(1);
      root = $(item);
      for (j = 0; j < 3; j++) {
        target = root.nextAll(':has(.collapse):first');
        if (target.length > 0) {
          /* Drop collapse class from all it's children, e.g.
             <pre><code class="r collapse">                   */
          target.children().removeClass("collapse");
          target.addClass('collapse');
          target.attr('id', id2);
          j = 3;
        } else {
          root = root.parent();
        }
      }
    }
  }
});
