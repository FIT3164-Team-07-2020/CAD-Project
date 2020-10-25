library(shiny)

fluidPage(
  # The title of the UI part.
  titlePanel("Coronary Artery Disease Prediction Model"),
  # Ask users for inputs in the side bar.
  sidebarPanel(
    
    # Introductory text.
    h5("Insert individual patient data in the fields below."),
    h5("Please strictly follow the medical test results. Note that all values
       below are required to enter, otherwise the analysis is unable to be 
       performed."),
    
    # Read inputs from users.
    # Note that all features' initial values are set as invalid.
    
    #Sub-Title text
    h3("Basic Demographic Information"),
    
    # Input for age.
    numericInput('Age',
                 'Age (years): ',
                 value = 0,
                 min = 0),
    
    # Input for weight.
    numericInput('Weight',
                 'Weight (kg): ',
                 value = 0,
                 min = 0),
    
    # Input for Height.
    numericInput('Length',
                 'Height (cm): ',
                 value = 0,
                 min = 0),
    
    #Sub-Title text
    h3("Symptom Examination"),
    
    # Input for BP (blood pressure).
    numericInput('BP',
                 'Blood Pressure (mmHg) ',
                 value = -1,
                 min = 0),
    
    # Input for PR (pulse rate).
    numericInput('PR',
                 'Pulse Rate (ppm) ',
                 value = -1,
                 min = 0),
    
    #Insert text
    h5("Select the appropriate option below if the patient has the following:"),
    
    # Input for HTN (Hypertension).
    radioButtons('HTN',
                 'Hypertension',
                 choices = c('Yes', 'No'), selected = 'No'),
    
    # Input for chest pain. Corresponds to Atypical and Typical.Chest.Pain variables
    # Atypical = atypical chest pain, Typical.Chest.Pain = typical chest pain.
    # If None, then return 0 for both variables.
    radioButtons('ChestPain',
                 'Chest pain',
                 choices = c('None','Typical', 'Atypical'), selected = 'None'),
    
    #Sub-Title text
    h3("EKG and Echocardiogram Results"),
    
    # Input for Tinversion.
    radioButtons('Tinversion',
                 'T Inversion',
                 choices = c('Yes', 'No'), selected = 'No'),

    # Input for Region.RWMA.
    selectInput('Region.RWMA',
                'Region with RWMA (Region Wall Motion Abnormality)', 
                c(0, 1, 2, 3, 4),
                selected = 0),
    
    #Sub-Title text
    h3("Blood Analysis"),
    
    # Input for FBS (fasting blood sugar).
    numericInput('FBS',
                 'Fasting Blood Sugar (mg/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for Cr (creatine).
    numericInput('CR',
                 'Creatine (mg/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for TG (triglyceride).
    numericInput('TG',
                 'Triglyceride (mg/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for LDL (low density lipoprotein).
    numericInput('LDL',
                 'Low Density Lipoprotein (mg/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for BUN (blood urea nitrogen).
    numericInput('BUN',
                 'Blood Urea Nitrogen (mg/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for ESR (erythrocyte sedimentation rate).
    numericInput('ESR',
                 'Erythrocyte Sedimentation Rate (mm/h) ',
                 value = -1,
                 min = 0),
    
    # Input for HB (hemoglobin).
    numericInput('HB',
                 'Hemoglobin (g/dl) ',
                 value = -1,
                 min = 0),
    
    # Input for K (potassium).
    numericInput('K',
                 'Potassium (mEq/lit) ',
                 value = -1,
                 min = 0),
    
    # Input for Na (sodium).
    numericInput('Na',
                 'Sodium (mEq/lit) ',
                 value = -1,
                 min = 0),
    
    # Input for WBC (white blood cell).
    numericInput('WBC',
                 'White Blood Cell (cells/ml) ',
                 value = -1,
                 min = 0),
    
    # Input for Lymph (lymphocyte).
    numericInput('Lymph',
                 'Lymphocyte (%) ',
                 value = -1,
                 min = 0, 
                 max = 100),
    
    # Input for PLT (platelet).
    numericInput('PLT',
                 'Platelet (1000/ml) ',
                 value = -1,
                 min = 0),
    
    # Input for EF.TTE (ejection fraction).
    numericInput('EF.TTE',
                 'Ejection Fraction (%) ',
                 value = -1,
                 min = 0, 
                 max = 100),
    
    # This button appears only when user inputs all valid values.
    # Clicking it will begin the ML model analysis process.
    conditionalPanel(condition = "output.InputValidation",
                     actionButton("BeginAnalysis", "Confirm and start analysis")
    )
  ),
  
  # Print the text output (in a gray box) to users in the main area.
  mainPanel(
    
    # Output the input summary and any possible input invalid message.
    verbatimTextOutput('InputValidation'),
    
    # # Output the features as well as the types summary
    # # (for testing and debugging only).
    # tableOutput("FeaturesDF"),
    # verbatimTextOutput('DFTypeSummary'),
    
    # Output the analysis result.
    htmlOutput('AnalysisResult')
  )
)