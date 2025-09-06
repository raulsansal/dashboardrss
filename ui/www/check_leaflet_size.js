// ui/www/check_leafet_size.js

echo 'Shiny.addCustomMessageHandler("checkLeafletSize", function(message) {
  var element = document.getElementById(message.id);
  if (element) {
    var rect = element.getBoundingClientRect();
    console.log("Leaflet container size for " + message.id + ": width=" + rect.width + ", height=" + rect.height);
    if (rect.width === 0 || rect.height === 0) {
      console.warn("Warning: Leaflet container " + message.id + " has zero size");
    }
  } else {
    console.error("Error: Element " + message.id + " not found");
  }
});' > /Volumes/Salamandra/Sefix/sefixModular/www/check_leaflet_size.js