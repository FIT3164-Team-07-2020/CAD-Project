library(shiny)

fluidPage(
  # The title of the UI part
  titlePanel("A Simple Prototype"),
  # Ask users for inputs in the side bar.
  sidebarPanel(
    
    # Numerical input for age with the min limitation of 0-year-old, initial 0.
    numericInput('age',
                 'Please enter your age in integer years: ',
                 value = 0,
                 min = 0),
    
    # Numerical input for weight with the min limitation of 1 kilogram, initial 1.
    numericInput('weight',
                 'Please enter your weight in integer kilograms: ',
                 value = 1,
                 min = 1),
    
    # Numerical input for height with the min limitation of 1 centimeter, initial 1.
    numericInput('height',
                 'Please enter your height in integer centimeters: ',
                 value = 1,
                 min = 1),
    
    # Numerical input for BMI with the min limitation of 0.001, initial 0.0001.
    numericInput('bmi',
                 'Please enter your BMI (Note: BMI can be calculated using the weight and height obtained): ',
                 value = 0.0001,
                 min = 0.0001),
    
    # Radio button input for typical chest pain with default value of No.
    radioButtons('typicalChestPain',
                 'Do you have a typical chest pain?',
                 choices = c('Yes', 'No'), selected = 'No'),
    
    # Select input, usually used for multi-categorical values, default 0.
    selectInput('RegionRWMA',
                'Please enter your Region.RWMA value (some descriptive information should appear here)', 
                c(0, 1, 2, 3, 4),
                selected = 0),

  ),
  # Print the text output (in a gray box) to users in the main area.
  mainPanel(
    verbatimTextOutput('out')
  )
)