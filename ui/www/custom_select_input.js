// ui/www/custom_select_input.js

$(document).ready(function() {
  // Función para inicializar los selectize con funcionalidad de eliminación
  function initializeSelectizeRemoval() {
    $('.selectize-control').each(function() {
      var $control = $(this);
      var selectizeId = $control.prev('select').attr('id');
      
      if (selectizeId) {
        // Escuchar eventos de cambio en los elementos de selectize
        $control.on('click', '.item', function(e) {
          var $item = $(this);
          
          // Solo adjuntar la X si aún no existe
          if ($item.find('.remove-option').length === 0) {
            $item.append('<span class="remove-option" style="margin-left: 5px; cursor: pointer; color: #999;">×</span>');
          }
        });

        // Manejar clic en la X para eliminar
        $control.on('click', '.remove-option', function(e) {
          e.stopPropagation();
          e.preventDefault();
          
          var $item = $(this).closest('.item');
          var value = $item.attr('data-value');
          
          // Obtener el objeto selectize
          var selectize = $control.prev('select')[0].selectize;
          
          if (selectize) {
            // Eliminar el valor seleccionado
            selectize.removeItem(value);
            
            // Enviar el valor a Shiny para procesar
            Shiny.setInputValue(selectizeId + '_removed', {
              value: value,
              timestamp: new Date().getTime()
            });
          }
        });
      }
    });
  }

  // Inicializar al cargar la página
  setTimeout(initializeSelectizeRemoval, 1000); // Aumentado de 500 a 1000 ms
  
  // Reinicializar cuando Shiny actualice los inputs
  $(document).on('shiny:inputchanged', function(event) {
    setTimeout(initializeSelectizeRemoval, 200);
  });

  // También reinicializar cuando se muestre una nueva pestaña
  $(document).on('shown.bs.tab', function(e) {
    setTimeout(initializeSelectizeRemoval, 200);
  });
});

// Mantener el handler existente para actualizaciones
$(document).ready(function() {
  Shiny.addCustomMessageHandler("updateSelectInput", function(message) {
    var $select = $("#" + message.id);
    $select.empty();
    $.each(message.choices, function(value, label) {
      $select.append($("<option>").attr("value", value).text(label));
    });
    $select.val(message.selected);
    $select.trigger("change");
    
    // Reinicializar la funcionalidad de eliminación
    setTimeout(initializeSelectizeRemoval, 200);
  });
});