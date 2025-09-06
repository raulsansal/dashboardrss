//ui/www/custom.js

// Manejo del opción "Extraordinaria"
Shiny.addCustomMessageHandler('disableExtraordinaria', function(disable) {
  $('#tipo_eleccion[value=EXTRAORDINARIA]').prop('disabled', disable);
  $('label[for="tipo_eleccionEXTRAORDINARIA"]').toggleClass('disabled-option', disable);
});
