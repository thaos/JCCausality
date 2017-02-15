//
// code included inside $(document).ready() will only run once the page is ready for JavaScript code to execute
$(document).ready(function() {
  // initialize a counter
  var nright = 0;
  var nleft = 0;
  // create a click handler which listens for a click on the element with id equal to RStudio
  $(document).on("keydown", function(e){
    // increment the counter each time we click on the Rstudio logo
    if (e.which == 39) {
      nright++;
    // send message to Shiny
    Shiny.onInputChange("nright", nright);
    }
  });
  $(document).on("keydown", function(e){
    // increment the counter each time we click on the Rstudio logo
    if (e.which == 37) {
      nleft++;
    // send message to Shiny
    Shiny.onInputChange("nleft", nleft);
    }
  });
});

