var gridstackBinding = new Shiny.InputBinding();

$.extend(gridstackBinding, {
  find: function(scope) {
    return $(scope).find('.grid-stack');
  },
  initialize: function(el) {
    let options = {
      float: false,
      draggable: {
        handle: '.card-header'
      }
    };

    let grid = GridStack.init(options);

    grid.on('dragstop', function(event, ui) {
      let element = event.target;
      let id = $(element).find('.plotly').prop('id');

      if (id == null)
        return;

      //Plotly.Plots.resize(id);
    })

    grid.on('gsresizestop', function(event, element) {
      let id = $(element).find('.plotly').prop('id');

      if (id == null)
        return;

      //Plotly.Plots.resize(id);
    })
    //$(el).gridstack({ float: false });
    //$(el).summernote('pasteHTML', el.dataset.value);
  },
  getValue: function(el) {
    return null;
  }
});

Shiny.inputBindings.register(gridstackBinding);

Shiny.addCustomMessageHandler('addWidgetGrid', function(message) {
  console.log(message.value);
  let grid = document.getElementById(message.inputId).gridstack;
  grid.addWidget(`<div>
  <div class="card card-outline grid-stack-item-content">
    <div class="card-header"></div>
    <div id="${message.value}" class="card-body" style="height: inherit;"></div>
  </div>
  </div>`, {width: 5, height: 5});
});
