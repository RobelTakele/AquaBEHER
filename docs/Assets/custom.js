// Custom JavaScript to add a search bar to the navbar
document.addEventListener("DOMContentLoaded", function() {
  var navbarRight = document.querySelector(".navbar-nav.ml-auto");
  
  if (navbarRight) {
    // Create the search form
    var searchForm = document.createElement("form");
    searchForm.className = "form-inline my-2 my-lg-0";
    searchForm.setAttribute("action", "#");  // Replace with actual search logic if needed
    searchForm.setAttribute("method", "get");

    // Create the search input
    var searchInput = document.createElement("input");
    searchInput.className = "form-control mr-sm-2";
    searchInput.setAttribute("type", "search");
    searchInput.setAttribute("placeholder", "Search...");
    searchInput.setAttribute("aria-label", "Search");

    // Create the search button
    var searchButton = document.createElement("button");
    searchButton.className = "btn btn-outline-primary my-2 my-sm-0";
    searchButton.setAttribute("type", "submit");
    searchButton.innerHTML = "Search";

    // Append input and button to the form
    searchForm.appendChild(searchInput);
    searchForm.appendChild(searchButton);

    // Insert the form into the navbar
    navbarRight.insertBefore(searchForm, navbarRight.firstChild);
  }
});


