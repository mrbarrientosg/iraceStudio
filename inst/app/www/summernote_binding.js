var summernoteBinding = new Shiny.InputBinding();
$.extend(summernoteBinding, {
  find: function(scope) {
    return $(scope).find('.summernoteInput');
  },
  initialize: function(el) {
    $(el).summernote(JSON.parse(el.dataset.options)).on('summernote.change', function(e) {
      console.log(e);
      $(el).trigger('change');
    });
    //$(el).summernote('pasteHTML', el.dataset.value);
  },
  getValue: function(el) {
    return $(el).summernote('code');
  },
  setValue: function(el, value) {
    $(el).summernote('code', value);
  },
  subscribe: function(el, callback) {
    $(el).on('change.summernoteBinding', function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.summernoteBinding');
  }
});

Shiny.inputBindings.register(summernoteBinding);

Shiny.addCustomMessageHandler('updateSummernoteInput', function(message) {
  let el = document.getElementById(message.inputId);
  $(el).summernote('pasteHTML', message.value); // Summernote-Input aktualisieren
});
