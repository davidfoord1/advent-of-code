document.addEventListener("DOMContentLoaded", function () {
  // Get the current page's path
  const currentPage = window.location.pathname;

  // Select all links in the day navigation
  const links = document.querySelectorAll(".day-nav a");
  console.log("Found links:", links); // Debugging the selected links

  links.forEach(link => {
    // Compare the href of each link with the current page's path
    if (link.pathname === currentPage) {
      link.classList.add("current-day"); // Add the class to the matching link
    }
  });
});
