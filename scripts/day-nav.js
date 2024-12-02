document.addEventListener("DOMContentLoaded", function () {
  // Get the current page's path
  const currentPage = window.location.pathname;
  console.log("Current page path:", currentPage); // Debugging the current path

  // Select all links in the day navigation
  const links = document.querySelectorAll(".day-nav a");
  console.log("Found links:", links); // Debugging the selected links

  links.forEach(link => {
    console.log("Checking link:", link.pathname); // Debugging each link's pathname
    // Compare the href of each link with the current page's path
    if (link.pathname === currentPage) {
      link.classList.add("current-day"); // Add the class to the matching link
      console.log("Matched link:", link); // Confirm match
    }
  });
});
