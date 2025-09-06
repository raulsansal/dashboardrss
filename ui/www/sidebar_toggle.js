// ui/www/sidebar_toggle.js

$(document).ready(function() {
  // Inicialización: añadir atributos data para recordar el estado
  $(".sidebar-right").each(function() {
    const sidebarId = $(this).attr("id");
    // Encontrar el contenedor del toggle asociado a este sidebar
    const toggleContainer = $(`[data-sidebar-id="${sidebarId}"]`).closest(".toggle-container");
    
    // Configurar texto inicial
    const toggleBtn = $(`[data-sidebar-id="${sidebarId}"]`);
    toggleBtn.text(">>");
    
    // Asociar el contenedor del toggle con el sidebar
    toggleContainer.attr("data-for-sidebar", sidebarId);
  });
  
  // Manejador del evento click para los botones de toggle
  $(document).on("click", ".toggle-sidebar-btn", function() {
    const sidebarId = $(this).attr("data-sidebar-id");
    const sidebar = $(`#${sidebarId}`);
    const toggleContainer = $(this).closest(".toggle-container");
    
    // Cambiar el estado del sidebar
    if (sidebar.hasClass("hidden")) {
      // Mostrar el sidebar
      sidebar.removeClass("hidden");
      toggleContainer.removeClass("sidebar-hidden");
      $(this).text(">>"); // Cambiar a "<<" cuando el sidebar está visible
    } else {
      // Ocultar el sidebar
      sidebar.addClass("hidden");
      toggleContainer.addClass("sidebar-hidden");
      $(this).text("<<"); // Cambiar a ">>" cuando el sidebar está oculto
    }
  });
});