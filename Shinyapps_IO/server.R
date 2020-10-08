library(shiny)

function(input, output) {
  # Text output.
  output$out <- renderText(paste0(
    "The output can change corresponding to the input:\n",
    "The input of slider input: ", input$slider0, ";\n",
    "The input of select input: ", input$select0, ";\n",
    "The input of radio button input: ", input$radio_button0, ";\n",
    "The input of numeric input: ", input$numeric0, ".\n\n",
    "Can also implement a button so that the model will run only when users click."))
}