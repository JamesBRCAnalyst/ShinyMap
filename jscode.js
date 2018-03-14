$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var Latitude = $el.data("Latitude");
  var Longitude = $el.data("Longitude");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    Latitude: Latitude,
    Longitude: Longitude,
    nonce: Math.random()
  });
});