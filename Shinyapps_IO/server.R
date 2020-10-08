library(shiny)

function(input, output) {
  # Text output.
  output$out <- renderText(paste0(
    "The output can change corresponding to the input:\n",
    "The age input: ", input$age, " years;\n",
    "The weight input: ", input$weight, "kg;\n",
    "The height input: ", input$height, "cm;\n",
    "The BMI input: ", input$bmi, ";\n",
    "The Chest Pain input: ", input$typicalChestPain, ";\n",
    "The Region RWMA input: ", input$RegionRWMA, ".\n\n",
    "Maybe implement a button so that the model will run only when users click?"))
}