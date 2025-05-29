$( document ).ready(function() {
  // Custom function to toggle sidebar
function toggleSidebar() {
  var sidebarElement = $('.main-sidebar');
  var bodyElement = $('.content-wrapper');

  if (sidebarElement.css('display') === 'none') {
    sidebarElement.show();
    bodyElement.css('margin-left', '230px');
  } else {
    sidebarElement.hide();
    bodyElement.css('margin-left', '0px');
  }
}

// Override the default sidebar toggle behavior
$(document).ready(function() {
  $('.sidebar-toggle').on('click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    toggleSidebar();
  });

  // Fix for submenu toggle functionality
  $('.treeview > a').on('click', function(e) {
    e.preventDefault();
    $(this).parent().toggleClass('active');
    $(this).parent().children('.treeview-menu').slideToggle('fast');
  });
});
});
