library(shiny)

# # Create data frame used to store necessary features.
# create_df = function(){
#   features = c("Age", "Weight", "Length", "BMI", "HTN", "BP", "PR",
#                "Typical.Chest.Pain", "Atypical", "Tinversion", "FBS", "CR", "TG", 
#                "LDL", "BUN", "ESR", "HB", "K", "Na", "WBC", "Lymph", "PLT", 
#                "EF.TTE", "Region.RWMA_0", "Region.RWMA_1", "Region.RWMA_2",
#                "Region.RWMA_3", "Region.RWMA_4")
#   features_with_nas = as.data.frame( matrix(ncol = length(features), nrow=0) )
#   features_with_nas = rbind(features_with_nas,
#                             data.frame(t(rep(c(NA), times = 28))))
#   colnames(features_with_nas) = features
#   features_with_values = features_with_nas
#   return(features_with_values)
# }
# 
# # Define the function used to process and add categorical data into 
# # features_with_values dataframe.
# cat_data_process = function(df, HTN, ChestPain, Tinversion, Region.RWMA){
#   # Convert HTN Y/N values to 0/1 and add into features_with_values dataframe.
#   if (HTN == 'Yes') {
#     df$HTN = 1
#   }
#   else {
#     df$HTN = 0
#   }
#   # According to the user's chest pain input, set values for Typical.Chest.Pain
#   # and Atypical features.
#   df$Typical.Chest.Pain = 0
#   df$Atypical = 0
#   if (ChestPain == 'Typical') {
#     df$Typical.Chest.Pain = 1
#   }
#   else if (ChestPain == 'Atypical'){
#     df$Atypical = 1
#   }
#   # Convert Tinversion Y/N values to 0/1 and add into features_with_values
#   # dataframe.
#   if (Tinversion == 'Yes') {
#     df$Tinversion = 1
#   }
#   else {
#     df$Tinversion = 0
#   }
#   # According to the user's Region RWMA input, set values for Region.RWMA_X
#   # features.
#   df$Region.RWMA_0 = 0
#   df$Region.RWMA_1 = 0
#   df$Region.RWMA_2 = 0
#   df$Region.RWMA_3 = 0
#   df$Region.RWMA_4 = 0
#   selectedRR = paste0('Region.RWMA_', toString(Region.RWMA))
#   df[, (colnames(ZAS) %in% selectedRR)] = 1
#   return(object)
# }
# 
# 
# 
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }

function(input, output) {
  # FeatureInfo session will check user's inputs' validation as well as output
  # a feature summary for user to double check.
  output$valid <- renderText({
    # Check validation.
    validate(
     need(input$Age > 0, 'Age should be greater then 0 year.'),
     need(input$Weight > 0, 'Weight should be greater than 0 kg.'),
     need(input$Length > 0, 'Height should be greater than 0 cm.'),
     need(input$BP >= 0, 'Blood pressure should be no smaller than 0 mmHg.'),
     need(input$PR >= 0, 'Pulse rate should be no smaller than 0 ppm.'),
     need(input$FBS >= 0, 'Fasting blood sugar should be no smaller than 0 mg/dl.'),
     need(input$CR >= 0, 'Creatine should be no smaller than 0 mg/dl.'),
     need(input$TG >= 0, 'Triglyceride should be no smaller than 0 mg/dl.'),
     need(input$LDL >= 0, 'Low density lipoprotein should be no smaller than 0 mg/dl.'),
     need(input$BUN >= 0, 'Blood urea nitrogen should be no smaller than 0 mg/dl.'),
     need(input$ESR >= 0, 'Erythrocyte sedimentation rate should be no smaller than 0 mm/h.'),
     need(input$HB >= 0, 'Hemoglobin should be no smaller than 0 g/dl.'),
     need(input$K >= 0, 'Potassium should be no smaller than 0 mEq/lit.'),
     need(input$Na >= 0, 'Sodium should be no smaller than 0 mEq/lit.'),
     need(input$WBC >= 0, 'White blood cell should be no smaller than 0 cells/ml.'),
     need(input$Lymph >= 0 & input$Lymph <= 100, 'Lymphocyte should be between 0 ~ 100% (both inclusive).'),
     need(input$PLT >= 0, 'Platelet should be no smaller than 0/ml.'),
     need(input$EF.TTE >= 0 & input$EF.TTE <= 100, 'Ejection fraction should be between 0 ~ 100% (both inclusive).')
    )
    # Summary.
    paste0(
    "Summary of inputs, please double check:\n",
    "Age: ", input$Age, " years;\n",
    "Wight: ", input$Weight, " kg;\n",
    "Length: ", input$Length, " cm;\n",
    "BMI: ", (input$Weight / (input$Length/100)^2), " kg/m^2;\n",
    "HTN: ", input$HTN, ";\n",
    "BP: ", input$BP, " mmHg;\n",
    "PR: ", input$PR, " ppm;\n",
    "ChestPain: ", input$ChestPain, ";\n",
    "Tinversion: ", input$Tinversion, ";\n",
    "FBS: ", input$FBS, " mg/dl;\n",
    "CR: ", input$CR, " mg/dl;\n",
    "TG: ", input$TG, " mg/dl;\n",
    "LDL: ", input$LDL, " mg/dl;\n",
    "BUN: ", input$BUN, " mg/dl;\n",
    "ESR: ", input$ESR, " mm/h;\n",
    "HB: ", input$HB, " g/dl;\n",
    "K: ", input$K, " mEq/lit;\n",
    "Na: ", input$Na, " mEq/lit;\n",
    "WBC: ", input$WBC, " cells/ml;\n",
    "Lymph: ", input$Lymph, "%;\n",
    "PLT: ", (input$PLT * 1000), "/ml;\n",
    "EF.TTE: ", input$EF.TTE, "%;\n",
    "The Region RWMA input: ", input$Region.RWMA, ".\n\n"
    )})
  
  # Since the user inputs all values, the confirm button will appear on the  UI.
  outputOptions(output, "valid", suspendWhenHidden = FALSE)
  
  # When the user click the action button, analysis process will begin.
  observeEvent(input$BeginAnalysis, {
    output$AnalysisResult <- renderText(
      "Here will be the analysis coding and results."
    )
  })
}