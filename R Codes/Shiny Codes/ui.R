#-------------------------------------------------------------------------
#  This application is governed by the CeCILL-B license. 
#  You can  use, modify and/ or redistribute this code under the terms
#  of the CeCILL license:  http://www.cecill.info/index.en.html
#
#  Marc Lavielle, Inria Saclay
#  May 11th, 2015
#-------------------------------------------------------------------------

library(shinydashboard)
library(bcimodel)
load("ex1.rda")
load("incidence_mel.rda")
load("othercausemort.rda")
source("compile_naturalhistmelanoma.R")
source("simpolicies_melanoma.R")
source("databases_melanoma.R")
source("initialize_melanoma.R")
source("outcomes_melanoma.R")
source("cantrance_melanoma.R")
source("earlydetect_melanoma.R")
source("general_melanoma.R")
source("initialize_melanoma.R")
source("outcomes_melanoma.R")
source("parallelized_melanoma.R")
source("screentreatlibrary_melanoma.R")
source("systime_melanoma.R")
source("treatment_melanoma.R")




sidebar <- dashboardSidebar(
  hr(),
  width=450,
  sidebarMenu(id="tabs",
              menuItem("Introduction", tabName = "introduction"),
              menuItem("Inputs to the Microsimulation Model", tabName ="inputs_model"),
              menuItem("Disease Features", tabName = "disease", 
                       menuSubItem("Age, Incidence Rates and Gender ", tabName = "age", icon = icon("angle-right")),
                       menuSubItem("Stage Distributions and Early Detection", tabName = "stage", icon = icon("angle-right"))),
              menuItem("Treatment", tabName = "treatment",
                       menuSubItem("Distribution under Standard of Care", tabName = "stdcare", icon = icon("angle-right")),
                       menuSubItem("Distribution under ITreatment Advances", tabName = "intervene", icon = icon("angle-right")),
                       menuSubItem("Efficacy of Each Treatment", tabName = "efficacy", icon = icon("angle-right"))
                       
              ),
              menuItem("Mortality", tabName = "mortality",
                       menuSubItem("Melanoma Cancer Survival", tabName = "survival", icon = icon("angle-right")),
                       menuSubItem("Other Cause Mortality", tabName = "other", icon = icon("angle-right"))
              ),
              menuItem("Additional Settings About Simulation", tabName = "setting",
                       menuSubItem("Cohort Size and Simulations", tabName = "size", icon = icon("angle-right"))
                       ),
              menuItem("Results", tabName = "result",
                       menuSubItem("Review inputs", tabName = "review", icon = icon("angle-right")),
                       menuSubItem("Point estimates", tabName = "point", icon = icon("angle-right"))
                       
                       )
  )
)

body <- dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Overview:",
              h5("This interface allows you to model the survival benefit of a melanoma 
              screening and/or treatment intervention in a virtual population of your choosing.
              The model posits a simple stage-shift mechanism of screening benefit. 
              For more information about stage-shift models, visit:"),
              tags$a(href="https://cancerpolicy.shinyapps.io/breastcancer/", "https://cancerpolicy.shinyapps.io/breastcancer/"))),
      
      tabItem(tabName = "inputs_model",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="1.Incidence:",
              h5(strong("Clinical Melanoma Incidence by Age, for Year Of Diagnosis 2005-2009")),
              h5(strong("Source:"),"Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) 
                 SEER*Stat Database: Incidence - SEER 9 Regs Research Data, 
                 Nov 2018 Sub (1975-2016), 
                 Linked To County Attributes - Total U.S., 1969-2017 Counties, National Cancer Institute, 
                 DCCPS, Surveillance Research Program, released April 2019, based on the November 2018 submission.")),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="2.Default Advanced Stage Proportions:",
                h5(strong("Proportion of cases that are advanced stage (stage III and IV) at clinical diagnosis by gender:")),
                h5("Male:17.51%, Female:14.09%"),
                h5(strong("Source:"),"Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) 
                 SEER*Stat Database: Incidence - SEER 9 Regs Research Data, 
                 Nov 2018 Sub (1975-2016), 
                 Linked To County Attributes - Total U.S., 1969-2017 Counties, National Cancer Institute, 
                 DCCPS, Surveillance Research Program, released April 2019, based on the November 2018 submission.")),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="3.Default Treatment Proportions of Population:",
                  h5("Model assumes that all patients recieve surgical therapy."),
                  h5("For other treatments and proportions see Treatment Section.")),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="4.Treatment Efficacies:",
                  h5("See Treatment Section.")),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="5.Default Disease Specific Survival Rates by Stage and Gender at diagnosis:",
                  h5(strong("5 year survival rates:")),
                  h5("For males: Early:95.3%, Advanced:49.9%"),
                  h5("For females: Early:97.5%, Advanced:59.4%"),
                  h5(strong("10 year survival rates:")),
                  h5("For males: Early:92.4%, Advanced:41.8%"),
                  h5("For females: Early:96.1%, Advanced:53.5%"),
                  h5(strong("Source:"),"Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) 
                 SEER*Stat Database: Incidence - SEER 9 Regs Research Data, 
                 Nov 2018 Sub (1975-2016), 
                 Linked To County Attributes - Total U.S., 1969-2017 Counties, National Cancer Institute, 
                 DCCPS, Surveillance Research Program, released April 2019, based on the November 2018 submission.")),
                          
              box(width = NULL, status = "primary", solidHeader = TRUE, title="6.Other cause mortality rates by age:",
                  h5(strong("Source:"),"Human Mortality Database, for Year 2009")),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="7.Default Stage Shift From Advanced to Early Stage:",
                  h5("Screening reduces the incidence of presentation of advanced stage disease by 7%")),
                  
              box(width = NULL, status = "primary", solidHeader = TRUE, title="For Model Customization:",
              h5("Specify inputs using the left panel to navigate."))
              ),
    
             
      
      tabItem(tabName = "age", box(width = NULL, status = "primary", solidHeader = TRUE, title="Define the age range and gender of interest",
                              h5("The model will track outcomes for a cohort of of these ages and gender")),
                        box(width = NULL, status = "primary", solidHeader = TRUE, title="Select Gender:",          
                            radioButtons(inputId="gender", label = "Gender",
                                         c("Male" = "male","Female" = "female"), selected="male")
                        ),
                        box(width = NULL, status = "primary", solidHeader = TRUE, title="Define The Age Range:",         
                            sliderInput("tfd","", value=c(50,70), min=0, max = 100, step=1)
                            )
                        )
      ,
      
      tabItem(tabName="stage",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Enter the percentage of advanced stage cases under two scenarios:",
                      h5("For each of the standard-of-care and intervention scenarios, this is the percent of cases who are advanced-stage at the time of clinical diagnosis. If the intervention is expected to detect cases early, the percent of cases diagnosed in advanced stage should be lower.")
                  ),
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Percent advanced, standard of care:",         
                  sliderInput("advst","", value=17.5, min=0, max = 100, step=0.1)
              ),
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Percent advanced, intervention:",         
                  sliderInput("advin","", value=10.5, min=0, max = 100, step=0.1)
              ),
              
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Summary of specified stage distributions:", 
            
                tableOutput("values")),
                
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Summary of specified stage distributions according to male and female:", 
                  
                  h5("According to SEER database, 60 percent of early stage diseased patients
                  and 66 percent of advanced stage diseased patients are male.")
  
              )
      ),
     
    tabItem(tabName = "efficacy",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Efficacy of treatment is obtained by literature review",
            h5("Here hazard ratios of treatments of melanoma are listed according to stages.")
            ),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Hazard Ratios", 
                tableOutput("efficacy")
            ),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Papers presenting efficacy values of treatments:",
                tableOutput("papers")
            )
             
    ),
            
      
    
    tabItem(tabName="survival",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Specify the percent of cases surviving at k years after diagnosis, or baseline survival",
            selectInput("survival", label = h3("Year of survival statistic"), 
                        choices = c(5, 10), 
                        selected = 5)),
            
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Advanced cases:baseline survival at k years",         
                sliderInput("advsur","", value=49.9, min=0, max = 100, step=0.1)),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Early cases:baseline survival at k years",         
                sliderInput("earlysur","", value=95.3, min=0, max = 100, step=0.1))
    ),
    
    tabItem(tabName="other",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Other cause mortality",
                h5(strong("Source:"),"Human Mortality Database, for Year 2009"))
    ),
    
    tabItem(tabName="stdcare",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Select the treatment distributions under standard of care for early and advanced stages of melanoma",
            h5("The proportion for each melanoma treatment should be entered as summing up to 100.")),
            
           
            box(width = 6, status = "primary", solidHeader = TRUE, title="ADVANCED STAGE",         
                sliderInput("advintst", "Interferon:", value=30, min=0, max = 100, step=1),
                sliderInput("advchmst", "Chemotherapy:", value=40, min=0, max = 100, step=1),
                sliderInput("advicbst", "ICB:", value=0, min=0, max = 100, step=1),
                sliderInput("advttst", "Targeted Therapy:", value=0, min=0, max = 100, step=1),
                sliderInput("advnonest", "None:", value=30, min=0, max = 100, step=1)
            ),
          
            box(width = 6, status = "primary",solidHeader = TRUE, title="EARLY STAGE",        
                sliderInput("earintst", "Interferon:", value=0, min=0, max = 100, step=1),
                sliderInput("earchmst", "Chemotherapy:", value=0, min=0, max = 100, step=1),
                sliderInput("earicbst", "ICB:", value=0, min=0, max = 100, step=1),
                sliderInput("earttst", "Targeted Therapy:", value=0, min=0, max = 100, step=1),
                sliderInput("earnonest", "None:", value=100, min=0, max = 100, step=1)
            )
    ),
    
    tabItem(tabName="intervene",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Select the treatment distributions under advances in care for early and advanced stages of melanoma",
                h5("The proportion for each melanoma treatment should be entered as summing up to 100.")),
            
            box(width = 6, status = "primary", solidHeader = TRUE, title="ADVANCED STAGE",         
                sliderInput("advintint", "Interferon:", value=20, min=0, max = 100, step=1),
                sliderInput("advchmint", "Chemotherapy:", value=20, min=0, max = 100, step=1),
                sliderInput("advicbint", "ICB:", value=37, min=0, max = 100, step=1),
                sliderInput("advttint", "Targeted Therapy:", value=13, min=0, max = 100, step=1),
                sliderInput("advnoneint", "None:", value=10, min=0, max = 100, step=1)
            ),
            
            box(width = 6, status = "primary",solidHeader = TRUE, title="EARLY STAGE",        
                sliderInput("earintint", "Interferon:", value=0, min=0, max = 100, step=1),
                sliderInput("earchmint", "Chemotherapy:", value=0, min=0, max = 100, step=1),
                sliderInput("earicbint", "ICB:", value=0, min=0, max = 100, step=1),
                sliderInput("earttint", "Targeted Therapy:", value=0, min=0, max = 100, step=1),
                sliderInput("earnoneint", "None:", value=100, min=0, max = 100, step=1)
            )
    ),
    
    tabItem(tabName="size",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Select the size of population and number of simulations"),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Size of population",         
                sliderInput("sizepop","", value=100000, min=100000, max = 1000000, step=100000)),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Number of simulations",         
                sliderInput("nsim", "", value=40, min=0, max = 100, step=5))
            ),
    
    tabItem(tabName="review",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Confirmation page of selected parameters for simulation",
            h5("If you want to make changes, please revisit the previous pages for model reparametrization")),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Disease Features:", 
                tableOutput("features")),
            
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Treatment distributions, Advanced stage:", 
                tableOutput("treatmentadv")),
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Treatment distributions, Early stage:", 
                tableOutput("treatmentear"))
            
    ),
    
    tabItem(tabName="point",
            box(width = NULL, status = "primary", solidHeader = TRUE, title="Simulation Results",

            
            tableOutput("policy"))
    )
    
    
    
    
    
    
   
    
    
    
    
            
     
)
)




dashboardPage(
  dashboardHeader(title = "Melanoma Microsimulation Model of Early Detection and Treatment", titleWidth = 750),
  sidebar,
  body
)