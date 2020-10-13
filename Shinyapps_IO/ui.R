library(shiny)

fluidPage(
  # The title of the UI part
  titlePanel("Coronary Artery Disease Prediction Model"),
  # Ask users for inputs in the side bar.
  sidebarPanel(
    
    #Introductory text
    h5("*Insert individual patient data in the fields below."),
    
    h3("Basic Demographic Information"),
    
    # Input for sex, Male or Female
    radioButtons('sex',
                 'Sex',
                 choices = c('Male', 'Female')),
    
    # Numerical input for age with the min limitation of 0-year-old, initial 0.
    numericInput('age',
                 'Age (years): ',
                 value = 0,
                 min = 0),
    
    # Numerical input for weight with the min limitation of 1 kilogram, initial 1.
    numericInput('weight',
                 'Weight (kg): ',
                 value = 0,
                 min = 0),
    
    # Numerical input for length with the min limitation of 1 cm, initial 1.
    numericInput('length',
                 'Height (cm): ',
                 value = 0,
                 min = 0),
    
    # Numerical input for BMI with the min limitation of 0.001, initial 0.0001.
    numericInput('bmi',
                 'BMI (Note: BMI can be calculated using the weight and height obtained): ',
                 value = 0.0001,
                 min = 0.0001),
    
    h3("Symptom Examination"),
    
    # Input for BP (blood pressure)
    numericInput('BP',
                 'Blood Pressure (mmHg) ',
                 value = 0,
                 min = 0),
    
    # Input for PR (pulse rate)
    numericInput('PR',
                 'Pulse Rate (ppm) ',
                 value = 0,
                 min = 0),
    
    h5("Select the appropriate option below if the patient has the following:"),
    
    # Input for HTN (Hypertension
    radioButtons('HTN',
                 'Hypertension',
                 choices = c('Yes', 'No'), selected = 'No'),
    
    # Input for chest pain. Corresponds to Atypical and Typical.Chest.Pain variables
    # Atypical = atypical chest pain, Typical.Chest.Pain = typical chest pain
    # If None, then return 0 for both variables.
    radioButtons('ChestPain',
                 'Chest pain',
                 choices = c('None','Typical', 'Atypical'), selected = 'None'),
    
    h3("EKG and Echocardiogram Results"),
    
    # Input for Tinversion
    radioButtons('Tinversion',
                 'T Inversion',
                 choices = c('Yes', 'No'), selected = 'No'),

    # Input for Region.RWMA
    selectInput('Region.RWMA',
                'Region with RWMA (Region Wall Motion Abnormality)', 
                c(0, 1, 2, 3, 4),
                selected = 0),
    
    h3("Blood Analysis"),
    
    # Input for FBS (fasting blood sugar)
    numericInput('FBS',
                 'Fasting Blood Sugar (mg/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for Cr (creatine)
    numericInput('Cr',
                 'Creatine (mg/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for TG (triglyceride)
    numericInput('TG',
                 'Triglyceride (mg/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for LDL (low density lipoprotein)
    numericInput('LDL',
                 'Low Density Lipoprotein (mg/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for BUN (blood urea nitrogen)
    numericInput('BUN',
                 'Blood Urea Nitrogen (mg/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for ESR (erythrocyte sedimentation rate)
    numericInput('ESR',
                 'Erythrocyte Sedimentation Rate (mm/h) ',
                 value = 0,
                 min = 0),
    
    # Input for HB (hemoglobin)
    numericInput('HB',
                 'Hemoglobin (g/dl) ',
                 value = 0,
                 min = 0),
    
    # Input for K (potassium)
    numericInput('K',
                 'Potassium (mEq/lit) ',
                 value = 0,
                 min = 0),
    
    # Input for NA (sodium)
    numericInput('NA',
                 'Sodium (mEq/lit) ',
                 value = 0,
                 min = 0),
    
    # Input for WBC (white blood cell)
    numericInput('WBC',
                 'White Blood Cell (cells/ml) ',
                 value = 0,
                 min = 0),
    
    # Input for Lymph (lymphocyte)
    numericInput('Lymph',
                 'Lymphocyte (%) ',
                 value = 0,
                 min = 0),
    
    # Input for PLT (platelet)
    numericInput('PLT',
                 'Platelet (1000/ml) ',
                 value = 0,
                 min = 0),
    
    # Input for EF.TTE (ejection fraction)
    numericInput('EF.TTE',
                 'Ejection Fraction (%) ',
                 value = 0,
                 min = 0),

  ),
  # Print the text output (in a gray box) to users in the main area.
  mainPanel(
    verbatimTextOutput('out')
  )
)