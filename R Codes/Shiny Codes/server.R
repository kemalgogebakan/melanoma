

library(shinydashboard)
library(reshape)  
library(ggplot2)
library(knitr)
library(plyr)
load("ex1.rda")
load("incidence_mel.rda")
load("othercausemort.rda")
load("agestructure.rda")
library(bcimodel)
source("compile_naturalhistmelanoma.R",local = TRUE)
source("simpolicies_melanoma.R", local = TRUE)
source("databases_melanoma.R", local = TRUE)
source("initialize_melanoma.R", local=TRUE)
source("outcomes_melanoma.R", local=TRUE)
source("cantrance_melanoma.R", local=TRUE)
source("earlydetect_melanoma.R", local = TRUE)
source("general_melanoma.R", local = TRUE)
source("initialize_melanoma.R", local = TRUE)
source("outcomes_melanoma.R", local=TRUE)
source("parallelized_melanoma.R", local=TRUE)
source("screentreatlibrary_melanoma.R", local=TRUE)
source("systime_melanoma.R", local=TRUE)
source("treatment_melanoma.R", local=TRUE)



shinyServer <- function(input, output, session){
  
  observe({gen<-input$gender
               if(gen=="male")
                  {updateSliderInput(session, "advst", value=17.5)}
                 else if(gen=="female")
                  {updateSliderInput(session, "advst", value=14.09)}
               })
  
  observe({gen<-input$gender
  if(gen=="male")
  {updateSliderInput(session, "advin", value=10.5)}
  else if(gen=="female")
  {updateSliderInput(session, "advin", value=7.09)}
  })
  
  observe({gen<-input$gender
  surv<-input$survival
  if(gen=="male")
  {
    if(surv==5)
    {updateSliderInput(session, "advsur", value=49.9)
      updateSliderInput(session, "earlysur", value=95.3)}
    else if(surv==10)
    {updateSliderInput(session, "advsur", value=41.8)
      updateSliderInput(session, "earlysur", value=92.4)}
  }
    
  else if(gen=="female")
    if(surv==5)
    {updateSliderInput(session, "advsur", value=59.4)
      updateSliderInput(session, "earlysur", value=97.5)}
  else if(surv==10)
  {updateSliderInput(session, "advsur", value=53.5)
    updateSliderInput(session, "earlysur", value=96.1)}
  }
  
  )
  

  
  
  sliderValues <- reactive({
    
    data.frame(
      Parameter = c("Percent advanced stage, standard of care",
               "Percent advanced stage, intervention",
               "Percent reduction in advanced stage due to intervention"),
      Value = as.character(c(input$advst,
                             input$advin,
                             round(((input$advst-input$advin)/input$advst)*100, digits = 0))),
      stringsAsFactors = FALSE)
    
  })
  
  
  
  sliderFeatures <- reactive({
    
    data.frame(
      Parameter = c("Gender","Year of survival statistic (k)",
                    "Percent surviving k years at advanced stage",
                    "Percent surviving k years at early stage",
                    "Percent presenting in advanced stage, standard of care",
                    "Percent reduction in advanced stage due to intervention"),
      Value = as.character(c(input$gender,
                             input$survival, input$advsur,
                             input$earlysur,
                             input$advst, input$advin)),
      stringsAsFactors = FALSE)
    
  })
  
  sliderTreatmentsAdv <- reactive({
    
    data.frame(
      "Treatment" = c("None",
                    "Interferon",
                    "Chemotherapy",
                    "Immunotherapy Checkpoint Inhibitor",
                    "Targeted Therapy"),
     "Standard Care" = as.character(c(input$advnonest, input$advintst,
                             input$advchmst,
                             input$advicbst, input$advttst)),
     "Intervention" = as.character(c(input$advnoneint, input$advintint,
                                      input$advchmint,
                                      input$advicbint, input$advttint)),
      stringsAsFactors = FALSE)
    
  })
  
  sliderTreatmentsEar <- reactive({
    
    data.frame(
      "Treatment" = c("None",
                      "Interferon",
                      "Chemotherapy",
                      "Immunotherapy Checkpoint Inhibitor",
                      "Targeted Therapy"),
      "Standard Care" = as.character(c(input$earnonest, input$earintst,
                                       input$earchmst,
                                       input$earicbst, input$earttst)),
      "Intervention" = as.character(c(input$earnoneint, input$earintint,
                                      input$earchmint,
                                      input$earicbint, input$earttint)),
      stringsAsFactors = FALSE)
    
  })
  
  sliderEfficacy <- reactive({
  
  data.frame(
    Stage = c("Advanced",
              "",
              "",
              ""),
    Treatment=c("Interferon","Chemotherapy","Immune Checkpoint Inhibitor","Targeted Therapy"),
    "Hazard Ratio" = as.character(c(0.90,
                                    0.93, 
                                    0.57,
                                    0.72)),
    "Implied Improvement in Survival (%)"=as.character(c(10,
                                                     7, 
                                                     43,
                                                     28)),
    stringsAsFactors = FALSE
  )
  })
  
  sliderPapers <- reactive({
    
    data.frame(
      Treatment = c("Interferon",
                "Chemotherapy",
                "Immune Checkpoint Inhibitor",
                "Targeted Therapy"),
      "Paper"=c("Ives, N. J., Suciu, S., Eggermont, A. M., Kirkwood, J., Lorigan, P., Markovic, S. N., ... & Cascinelli, N. (2017). Adjuvant interferon for the treatment of high-risk melanoma: an individual patient data meta-analysis. European Journal of Cancer, 82, 171-183.",
                  "Educated Guess",
                  "Dobry, A. S., Zogg, C. K., Hodi, F. S., Smith, T. R., Ott, P. A., & Iorgulescu, J. B. (2018). Management of metastatic melanoma: improved survival in a national cohort following the approvals of checkpoint blockade immunotherapies and targeted therapies. Cancer Immunology, Immunotherapy, 67(12), 1833-1844.",
                  "Dobry, A. S., Zogg, C. K., Hodi, F. S., Smith, T. R., Ott, P. A., & Iorgulescu, J. B. (2018). Management of metastatic melanoma: improved survival in a national cohort following the approvals of checkpoint blockade immunotherapies and targeted therapies. Cancer Immunology, Immunotherapy, 67(12), 1833-1844.")
      ,
                  stringsAsFactors = FALSE
    )
  })
  
  slidersimulation <- reactive({
    
    surr<-input$survival
    if(surr==5){survv<-5} else if(surr==10){survv<-10}
    
    melanomUS <- vector('list', length=length(ex1))
    names(melanomUS) <- names(ex1)
    propERpos<-1
    pop_chars <- list(male = data.frame(male = c(0), prop = c(1)))
    melanomUS$pol  <- data.frame(num=c(1:4),
                                    id=c('adv.base', 
                                         'advbase.treatment','adv.shift', 'adv.shift.treatment'),
                                    name=c('M0: Surgery, Interferon, Chemotherapy',
                                           'M1: M0 plus Usage of ICB and Targeted Therapy for Late Stage',
                                           'M2: M0 plus Stage Shift',
                                           'M2: M1 with Stage Shift'),
                                           pairnum=c(NA, NA, c(1,2)),
                                           earlydetHR=c(rep(1, 2), rep(input$advin/input$advst, 2)),
                                           stringsAsFactors=FALSE)
    
    #Calculating baseline survival for early stages
    f = function(x, p=propERpos, ipos=input$earintst/100, chemopos=input$earchmst/100, icbpos=input$earicbst/100, tapos=input$earttst/100, hi=0.9, hchemo=1, hicb=1, hta=1, So=input$earlysur/100) { 
      p*(1-ipos-chemopos-icbpos-tapos)*x + p*ipos*x^hi + p*chemopos*x^hchemo + p*icbpos*x^hicb + p*tapos*x^hta - So
    }
    Sb.early <- uniroot(f, lower=0.5, upper=input$earlysur/100, tol = 0.0001)
    early.mrate.co = cumsurv_to_exprate(Sb.early$root, year=survv)
    round(100*Sb.early$root)
    
    #Calculating baseline survival for advanced stages
      f = function(x, p=propERpos, ipos=input$advintst/100, chemopos=input$advchmst/100, icbpos=input$advicbst/100, tapos=input$advttst/100, hi=0.9, hchemo=0.93, hicb=0.57, hta=0.73, So=input$advsur/100) { 
      p*(1-ipos-chemopos-icbpos-tapos)*x + p*ipos*x^hi + p*chemopos*x^hchemo + p*icbpos*x^hicb + p*tapos*x^hta - So
    }
    Sb.late <- uniroot(f, lower=0.2, upper=input$advsur/100, tol = 0.0001)
    late.mrate.co = cumsurv_to_exprate(Sb.late$root, year=survv)
    round(100*Sb.late$root)
    
    melanomUS$nh<-compile_naturalhist_melanoma(prop_adv = input$advst/100, mortrates=c(Early=early.mrate.co,Advanced=late.mrate.co))
    melanomUS$map <- create_stageshift_map(melanomUS$nh)
    
    melanomUS$tx <- data.frame(expand.grid(txSSid=c('None', 'Interferon', 'Chemotherapy',
                                                       'Checkpoint inhibitor','Targeted Therapy'),
                                              SSno=1:nrow(melanomUS$nh)),
                                  stringsAsFactors=FALSE)
    ntreat <- nrow(melanomUS$tx)
    ntx <- length(unique(melanomUS$tx$txSSid))
    # Proportions sum to 1 within stages
    melanomUS$tx <- transform(melanomUS$tx, 
                                 SSid=c(rep('Early', ntx), rep('Late', ntx)), 
                                 txSSno=1:ntreat)
    melanomUS$tx <- transform(melanomUS$tx,
                                 txHR=c(1, 0.9, 1, 1, 1, 
                                        1, 0.9, 0.93, 0.57, 0.73))
    advA <- vector('list', length=length(unique(melanomUS$tx$SSid)))
    # Order of SSid: Early Advanced
    # Order of treatments: None, Interferon, Chemo, Checkpoint, Targeted
    advA[[1]] <- c(input$earnonest, input$earintst, input$earchmst, input$earicbst, input$earttst)
    advA[[2]] <- c(input$advnonest, input$advintst, input$advchmst, input$advicbst,input$advttst)
    # After treatment advances:
    advB<-advA
    advB[[1]] <- c(input$earnoneint, input$earintint, input$earchmint, input$earicbint, input$earttint)
    advB[[2]] <- c(input$advnoneint, input$advintint, input$advchmint, input$advicbint, input$advttint)
    # Put together complete vectors
    advA.v <- do.call('c', advA)
    advB.v <- do.call('c', advB)
    props <- data.frame(advA.v, advB.v, advA.v, advB.v)
    
    # colSums(props)  
    colnames(props) <- melanomUS$pol$id
    props2 <- props
    colnames(props2) <- melanomUS$pol$name
    toprint <- data.frame(melanomUS$tx, props2, stringsAsFactors=FALSE, check.names=FALSE)
    melanomUS$tx <- data.frame(melanomUS$tx, props, stringsAsFactors=FALSE)
    melanomUS$tx <- data.frame(melanomUS$tx, stringsAsFactors=FALSE)
    startclock <- proc.time()
    
   if (input$gender=="male")
   {
      withProgress(message = 'Simulation in Progress', {
      manuscript_melanoma_male <- simpolicies_mel(melanomUS$pol, melanomUS$nh, melanomUS$tx, 
                                                 incsource='Male',
                                                 mortsource='Male', 
                                                 returnstats=c('mean', 'lower', 
                                                               'upper'), 
                                                 futimes=c(5,10),
                                                 minage=input$tfd[1], maxage=input$tfd[2], sims=input$nsim)
      # Runtime
     
      })
     finaltab2 <- format_bounds_list(manuscript_melanoma_male, 
                                     paren=TRUE, includemean=TRUE, 
                                     digits=c(0,0,1,2,0,0),
                                     compileall=TRUE)
   }
    
  else if (input$gender=="female")
  {
    withProgress(message = 'Simulation in Progress', {
      manuscript_melanoma_male <- simpolicies_mel(melanomUS$pol, melanomUS$nh, melanomUS$tx, 
                                                  incsource='Female',
                                                  mortsource='Female', 
                                                  returnstats=c('mean', 'lower', 
                                                                'upper'), 
                                                  futimes=c(5,10),
                                                  minage=input$tfd[1], maxage=input$tfd[2], sims=input$nsim)
      # Runtime
      
    })
    # Format and save results
    finaltab2 <- format_bounds_list(manuscript_melanoma_female, 
                                    paren=TRUE, includemean=TRUE, 
                                    digits=c(0,0,1,2,0,0),
                                    compileall=TRUE)
      
  }
    
    
    

      
    
   
 
    

    })
  
    

 
    
  
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  output$features<-renderTable({
    sliderFeatures()
  })
  
  output$treatmentadv<-renderTable({
    sliderTreatmentsAdv()
  })
  
  output$treatmentear<-renderTable({
    sliderTreatmentsEar()
  })
  
  output$efficacy<- renderTable({
    sliderEfficacy()
  })
  
  output$papers<- renderTable({
    sliderPapers()
  })
  
  output$policy<-renderTable({
    slidersimulation()
  })
}
