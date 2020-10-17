library(shiny)
# Boost model needs this library to perform prediction.
library(adabag)

# Load the boosting machine learning model.
load('boost_model.rdata')

function(input, output) {
  # FeatureInfo session will check user's inputs' validation as well as output
  # a feature summary for user to double check.
  output$InputValidation <- renderText({
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
    "Weight: ", input$Weight, " kg;\n",
    "Height: ", input$Length, " cm;\n",
    "BMI: ", (input$Weight / (input$Length/100)^2), " kg/(m^2) (This value is auto-calculated according to inputs Weight and Height);\n",
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
  outputOptions(output, "InputValidation", suspendWhenHidden = FALSE)
  
  # When the user click the action button, analysis process will begin.
  observeEvent(input$BeginAnalysis, {
    
    # Create the dataframe which will be feed into the ML model.
    
    # Note that for all non-numerical features, corresponding reactive factor
    # values are generated and used to replace the original input values. This 
    # type convention step is necessary for the ML model used later.
    
    features = data.frame(
      Age = input$Age,
      Weight = input$Weight,
      Length = input$Length,
      BMI = ((input$Weight / (input$Length/100)^2)),
      
      # According to HTN Y/N inputs, generate corresponding factor values.
      HTN = reactive(factor(if (input$HTN == 'Yes') 1 else 0))(),
      
      BP = input$BP,
      PR = input$PR,
      
      # According to ChestPain None / Typical / Atypical inputs, generate
      # corresponding factor values.
      Typical.Chest.Pain = reactive(factor(if (input$ChestPain == 'Typical') 1
                                           else 0))(),
      Atypical = reactive(factor(if (input$ChestPain == 'Atypical') 1
                                 else 0))(),
      
      # According to Tinversion Y/N inputs, generate corresponding factor
      # values.
      Tinversion = reactive(factor(if (input$Tinversion == 'Yes') 1 else 0))(),
      
      FBS = input$FBS,
      CR = input$CR,
      TG = input$TG,
      LDL = input$LDL,
      BUN = input$BUN,
      ESR = input$ESR,
      HB = input$HB,
      K = input$K,
      Na = input$Na,
      WBC = input$WBC,
      Lymph = input$Lymph,
      PLT = (input$PLT * 1000),
      EF.TTE = input$EF.TTE,
      
      # According to Region.RWMA 0/1/2/3/4 inputs, generate corresponding factor
      # values.
      Region.RWMA_0 = reactive(factor(if (input$Region.RWMA == 0) 1 else 0))(),
      Region.RWMA_1 = reactive(factor(if (input$Region.RWMA == 1) 1 else 0))(),
      Region.RWMA_2 = reactive(factor(if (input$Region.RWMA == 2) 1 else 0))(),
      Region.RWMA_3 = reactive(factor(if (input$Region.RWMA == 3) 1 else 0))(),
      Region.RWMA_4 = reactive(factor(if (input$Region.RWMA == 4) 1 else 0))()
    )
    
    # # Output the features as well as the types summary
    # # (for testing and debugging only).
    # output$FeaturesDF = renderTable(features)
    # output$DFTypeSummary = renderText(
    #   sapply(features, class)
    # )
    
    # Use the pre-trained ML model to predict on the input data and output the preducted result.
    predicted = predict.boosting(boost, features)
    output$AnalysisResult = renderText({
      paste0(
        'By using pre-trained boosting model to analyse the input data: \n',
        '- The predicted probablity of the class result No is: ', predicted$prob[1], "\n",
        '- The predicted probablity of the class result Yes is: ', predicted$prob[2], "\n", 
        '- The predicted class result is: ', reactive(if (predicted$class == '1') 'Yes' else 'No')(), "\n",
        '(Yes means likely to have coronary artery disease, No means not likely to have coronary artery disease)', "\n",
        'Note that this is just the analysis result of a machine learning model, which is just for reference and not guaranteed to have 100% accuracy.')
    })
  })
}