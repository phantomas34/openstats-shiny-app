# R/utils.R

# A helper function to add a clickable info icon with a popover
with_info_popover <- function(ui_element, title, content) {
  
  # Arrange the main UI element and the icon side-by-side
  tagList(
    div(
      style = "display: flex; align-items: center; justify-content: space-between;",
      ui_element, # The main UI (e.g., the h4 title)
      
      # The popover icon
      popover(
        # We are now using bs_icon() instead of icon()
        trigger = bs_icon("question-circle-fill", class = "text-primary"), 
        
        title = title,
        content
      )
    )
  )
}