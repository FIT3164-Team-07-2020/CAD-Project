library(shiny)

fluidPage(
  # The title of the UI part
  titlePanel("A Simple Prototype"),
  # Ask users for inputs in the side bar.
  sidebarPanel(
    # Slider input, usually used for select ranged values.
    sliderInput('slider0', 'Slider Input Example', min=1, max=100,
                value=50, step=1, round=0),
    # Select input, usually used for multi-categorical values.
    selectInput('select0', 'Select Input Example', 
                c('None', 3164, '07', 'Data Science Project'),
                selected = 3164),
    # Radio button input, usually used for binary values.
    radioButtons('radio_button0', 'Radio Button Input Example',
                       choices = c('Yes', 'No'), selected = 'Yes'),
    # Numerical input, usually used for multi-valued numerical values.
    numericInput('numeric0', 'Numeric Input Example (ranged 0 to 100)',
                 0, min = 0, max = 100)
  ),
  # Print the text output (in a gray box) to users in the main area.
  mainPanel(
    verbatimTextOutput('out')
  )
)