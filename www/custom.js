// Dashboard Custom JavaScript

// Initialize dashboard when DOM is ready
$(document).ready(function() {
  console.log('Dashboard initialized');
  
  // Custom Shiny input bindings
  initializeCustomBindings();
  
  // Setup dashboard enhancements
  setupDashboardEnhancements();
});

// Custom input bindings for Shiny
function initializeCustomBindings() {
  // Add custom interactions here
  
  // Example: Enhanced plot interactions
  $('.plotly').on('plotly_hover', function(eventdata) {
    console.log('Plot hover:', eventdata);
  });
}

// Dashboard UI enhancements
function setupDashboardEnhancements() {
  // Add loading spinners
  addLoadingSpinners();
  
  // Setup responsive behavior
  setupResponsiveBehavior();
}

// Add loading spinners to output elements
function addLoadingSpinners() {
  $('.shiny-output-container').each(function() {
    if (!$(this).find('.loading-spinner').length) {
      $(this).append('<div class="loading-spinner" style="display:none;">Loading...</div>');
    }
  });
}

// Setup responsive behavior
function setupResponsiveBehavior() {
  // Handle window resize
  $(window).on('resize', function() {
    // Trigger Plotly resize if needed
    if (window.Plotly) {
      $('.plotly').each(function() {
        window.Plotly.Plots.resize(this);
      });
    }
  });
}
