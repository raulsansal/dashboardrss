// ui/www/custom.js

// Manejo del opción "Extraordinaria"
Shiny.addCustomMessageHandler('disableExtraordinaria', function(disable) {
  $('#tipo_eleccion[value=EXTRAORDINARIA]').prop('disabled', disable);
  $('label[for="tipo_eleccionEXTRAORDINARIA"]').toggleClass('disabled-option', disable);
});

// Forzar resize de gráficos de Plotly para mantener responsividad
$(document).on('shiny:connected', function() {
  // Esperar a que los gráficos se rendericen inicialmente
  setTimeout(function() {
    // Forzar resize en todos los gráficos de Plotly
    $('.plotly').each(function() {
      var plotlyDiv = this;
      if (plotlyDiv && plotlyDiv.layout) {
        try {
          Plotly.Plots.resize(plotlyDiv);
        } catch(e) {
          console.log('Error al redimensionar gráfico:', e);
        }
      }
    });
  }, 1000);
  
  // Detectar cambios de tab y forzar resize
  $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
    setTimeout(function() {
      $('.tab-pane.active .plotly').each(function() {
        var plotlyDiv = this;
        if (plotlyDiv && plotlyDiv.layout) {
          try {
            Plotly.Plots.resize(plotlyDiv);
          } catch(e) {
            console.log('Error al redimensionar gráfico en tab:', e);
          }
        }
      });
    }, 500);
  });
});

// Resize cuando cambia el tamaño de la ventana
$(window).on('resize', function() {
  clearTimeout(window.resizeTimer);
  window.resizeTimer = setTimeout(function() {
    $('.tab-pane.active .plotly').each(function() {
      var plotlyDiv = this;
      if (plotlyDiv && plotlyDiv.layout) {
        try {
          Plotly.Plots.resize(plotlyDiv);
        } catch(e) {
          console.log('Error al redimensionar gráfico en resize:', e);
        }
      }
    });
  }, 250);
});

// Forzar resize específico cuando se renderiza un nuevo gráfico en Lista Nominal
$(document).on('shiny:value', function(event) {
  if (event.name && (event.name.includes('grafico') || event.name.includes('plot'))) {
    setTimeout(function() {
      var targetId = '#' + event.name.replace(/:/g, '-');
      var plotlyDiv = $(targetId)[0];
      if (plotlyDiv && plotlyDiv.layout) {
        try {
          Plotly.Plots.resize(plotlyDiv);
        } catch(e) {
          console.log('Error al redimensionar gráfico específico:', e);
        }
      }
    }, 500);
  }
});