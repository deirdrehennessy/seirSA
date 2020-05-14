rm(list = ls())
#################################################################################
#
# Modified PHAC S(E)IR model script with n age classes to estimate the number of
# COVID-19 cases (infected, hospitalized, etc).
#
#==================================================================================
#
#
# Author of the modifications: Maikol Diasparra, Claude Nadeau and Joel Barnes
#
# Date: 25 MAR 2020
# Last update: 8 APR 2020 
# Version: 1.0.1
#
# Modification objective: to extend the model to get the estimates by age-groups
# Original script provided by PHAC Analysts: Antoinette Ludwig & Erin Rees.
#
# But the script can be used for n age classes (e.g.age groups: 1=under 20, 2=20-59, 3=60-69, 4=70-79, 5=80+)
# Also, the differential equations are no longer hardcoded but rather built from the excel representation of the flow chart.
#
#     
# 
#################################################################################


#==========================================================================
#  packages
#==========================================================================

package_names <- c("janitor","readxl","dplyr","deSolve","tidyr","ggplot2", "ggpubr", "tidyverse", "lhs") # , "viridis") 
package_names <- c("readxl","dplyr","tidyr","janitor"  ,  "deSolve","triangle","lhs") # 

# Install packages if they haven't been installed previously
install_packages <- lapply(package_names, FUN = function(x) if(! require(x, character.only = TRUE)) install.packages(x))


load_packages <- lapply(package_names, require, character.only = TRUE)


### User input parameters
WDir <- "C:/Users/colli/Desktop/v10_snapshot1"             # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/colli/Desktop/v10_snapshot1" # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/colli/Desktop/v10_snapshot1" # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/colli/Desktop/v10_snapshot1" # working directory **** NO TRAILING /   akin to choose.dir()  ****

#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)# make it your working directory
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes and friends.R")


# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , sigma  (but not beta)
# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , sigma  (but not beta)
 
# BEGIN User inputs
# BEGIN User inputs

# BEGIN    covid.target = list(donnees= data.frame , time.ranges = data.frame )
   target_file= paste0(WDir,"/RE__Update_on_SEIR_model_sensitivity_analysis/CDN covid epi data (cleaner file from file called Comparison with observed data).xls")
   target.df =  as.data.frame.from.tbl( readxl::read_excel(target_file, sheet ="Canadian data" ) )
   target.df$cumI = target.df$`Cumulative cases`
   target.df$D    = target.df$Deaths
   
   covid.targets=list(donnees=target.df[,c("time","cumI","D")] )
   covid.targets$time.ranges = data.frame(lower.bound=20 +10*seq(5))
   covid.targets$time.ranges$upper.bound = pmin( covid.targets$time.ranges$lower.bound + 9 , 75)
   
  #covid.targets=list(donnees=target.df[,c("time","cumI")] )             # only interested in cumI
  #covid.targets$time.ranges = data.frame(lower.bound=75,upper.bound=75) # only interested in day 75
# END    covid.target = list(donnees= data.frame , time.ranges = data.frame )

 # BEGIN MODEL WORKBOOK  -->   file_name and  sheet_names

 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1.xls"           # File misnamed.  There is only 1 age group in here. Date is prior to April 20.
 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1 - April20.xls" # File misnamed.  There is only 1 age group in here
 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1 - April23.xls" # dummy 5 age group counterpart of "...(1agegrp)_SC1 - April23.xls" 
   file_name = "BluePrint4 Conceptual model_V4_20200415V2(1agegrp)_SC1 - April23.xls" # From PHAC"s misnamed "...(1agegrp)_SC1.xls" shared on April 23
   file_name = "BluePrint4 Conceptual model_V4_20200415V2(1agegrp)_SC1 - April23 - MaxTime222.xls"
   
  sheet_names = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",
                      model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations",post.processing="Post Processing")
  # END MODEL WORKBOOK  -->   file_name and  sheet_names 
  
  
  # BEGIN Sampling specs --> hypercube.upper.bounds , hypercube.lower.bounds, n.repeat.within.hypercube, racine, backend.transformation, reference.alteration, tmin.alter.scope
  
  #  4 different examples provided below

  # Example 1) Silly : 4 hypercubes with 100 attemps in each cube
  if(FALSE)   
  {
    # NOTE: this is the only place where you name the parameters
    # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
    
    LB   = list( lambda=c(0.3 ,0.7 ) , delta  = c(0.4 ,0.7 ) )  
    UB   = list( lambda=c(0.4 ,0.8 ) , delta  = c(0.5 ,0.8 ) )
    apex = list( lambda=c(0.36,0.72) , delta  = c(0.48,0.77) )  
    
    
    parm.cloud.grid.specs = list(
      hypercube.lower.bounds = LB ,
      hypercube.upper.bounds = UB ,
      hypercube.apex.mode    = apex , 
      n.repeat.within.hypercube = 100 , 
      LatinHypercubeSampling = c(FALSE,TRUE)[2] ,
      racine = 42  ,
      
      backend.transformation = function(x) {x} , # need to provide a function like exp here
      reference.alteration = c("overwrite","add","multiply")[1] ,
     # tmin.alter.scope = 81    # to alter only values of the parameters at tmin = 81
      tmin.alter.scope = 0:81   # to alter all values of the parameters
    )
  }
  
  # Example 2) Code replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx
  if(FALSE)  # 12 hypercubes with 1 attempt in each hypercubes (and each hypercubes has volume 0)
  {
     # NOTE: this is the only place where you name the parameters
     # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
     
     # Make sure file_name is " ... (1agegrp)_SC1 - April23.xls"
     # Make sure to use v1 for S --> L transitions in sheet "Model Specs (not lazy)" in workbook  "... (1agegrp)_SC1 - April23.xls"
    
     hypercube.lower.bounds = list( lambda=seq(0.3,0.7,0.2) , delta  = seq(0.4,0.7,0.1) )  # lambda first ==> inner loop will be lambda just like PHAC
     hypercube.upper.bounds = hypercube.lower.bounds 
     
     parm.cloud.grid.specs = list(
        hypercube.lower.bounds = hypercube.lower.bounds ,
        hypercube.upper.bounds = hypercube.upper.bounds ,
        n.repeat.within.hypercube = 1 , 
        LatinHypercubeSampling = c(FALSE,TRUE)[2] ,
        racine = 42  ,
        
        backend.transformation = function(x) {x} , # need to provide a function like exp here
        reference.alteration = c("overwrite","add","multiply")[1] ,
        tmin.alter.scope = 81
     )
  }
  
  
  
  # Example 3) Random attempts univariate (one parameter)
  if(FALSE)  # 1 hypercube with several attempts within it
  {
    # NOTE: this is the only place where you name the parameters
    # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
    
    center = c(sigma=0) 
    half.range = 0.1 + 0*center # small  hypercube
    half.range = 0.2 + 0*center # larger hypercube
    
    n.repeat.within.hypercube = 5
    
    parm.cloud.grid.specs = list(
      hypercube.lower.bounds = as.list( center -     half.range ) ,
      hypercube.upper.bounds = as.list( center +     half.range ) ,
      hypercube.apex.mode    = as.list( center +0.2* half.range ) ,
      n.repeat.within.hypercube = n.repeat.within.hypercube, 
      LatinHypercubeSampling = c(FALSE,TRUE)[1] ,
      racine = 42  ,  # random seed
      
      backend.transformation = exp ,# need to provide a function like exp here
      reference.alteration = c("overwrite","add","multiply")[3] ,
      tmin.alter.scope = 40:55
    )
    
  }
      
  # Example 4) Random attempts multivariate (several parameters)
   if(TRUE)  # 1 hypercube with several attempts within it
   {
      # NOTE: this is the only place where you name the parameters
      # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
      
      center = c(Cgg=0.25,Cgq=0.0,lambda=-0.75,sigma=-0.75) # Joel solution 5-8 with Cgq set to 0  (exp(Cgq) = 1).
      center = c(Cgg=0   ,Cgq=0  ,lambda=0    ,sigma=0) 
      half.range = 0.1 + 0*center # small  hypercube
      half.range = 0.2 + 0*center # larger hypercube
      
      n.repeat.within.hypercube = 5
      n.repeat.within.hypercube = 4
      
     #half.range["beta"] = 0
      parm.cloud.grid.specs = list(
       # hypercube.lower.bounds = c(Cgg=0.05,Cgq=-0.4,lambda=-0.75,sigma=-0.75) , # can also spell out bounds
       # hypercube.upper.bounds = c(Cgg=0.15,Cgq=-0.1,lambda=-0.25,sigma=-0.45) , # can also spell out bounds
         hypercube.lower.bounds = as.list( center -      half.range ) ,
         hypercube.upper.bounds = as.list( center +      half.range ) ,
          hypercube.apex.mode   = as.list( center + 0.2* half.range ) ,
         n.repeat.within.hypercube = n.repeat.within.hypercube, 
         LatinHypercubeSampling = c(FALSE,TRUE)[2] ,
         racine = 42  ,  # random seed
         
        #backend.transformation = function(x) {round(exp(x),4)} ,# need to provide a function like exp here
         backend.transformation = exp ,# need to provide a function like exp here
         reference.alteration = c("overwrite","add","multiply")[3] ,
         tmin.alter.scope = 40:55
      )
     
   }
  
  # END Sampling specs --> hypercube.upper.bounds , hypercube.lower.bounds, n.repeat.within.hypercube, racine, backend.transformation, reference.alteration, tmin.alter.scope
  
  
# END User inputs
# END User inputs
   
 
# BEGIN run stuff according to parm.cloud.grid.specs

# results.baseline = SEIR.n.Age.Classes(file_name,sheet_names)[c("input.info","input.info.verbatim")] # all you really need
results.baseline = SEIR.n.Age.Classes(file_name,sheet_names,post.processing.companion.kit=list(targets=covid.targets))

str(parm.cloud.grid.specs) # quick look at the specs
take.a.quick.look    = try.various.parms.values(results.baseline,parm.cloud.grid.specs,only.show.parms.to.try=TRUE)
dim(take.a.quick.look$parms.to.try)  # check size of sweep you are about to do  (number of scenarios , number of parameters)
#plot(take.a.quick.look$parms.to.try$lambda , take.a.quick.look$parms.to.try$delta )

various.parms.result = try.various.parms.values(results.baseline,parm.cloud.grid.specs)


# END run stuff according to parm.cloud.grid.specs


#Unpack
parms.tried.df      = various.parms.result$parms.to.try
list.sweep          = various.parms.result$list.sweep
  df.sweep          = various.parms.result$df.sweep
outcomes.summary.df = various.parms.result$outcomes.summary.df

# Backup
verbose.save("list.sweep"           ) # creates file "This file contains an R object called list.sweep.SavedFromR". Use load() to read back 
verbose.save("df.sweep"             ) # creates file "This file contains an R object called   df.sweep.SavedFromR". Use load() to read back 
verbose.save("outcomes.summary.df"  ) 
verbose.save("parms.tried.df"       )  
###verbose.save("various.parms.result")  


 names(outcomes.summary.df)
 
 # Wu et al Scatter plots
 plot(outcomes.summary.df$Cgg.multiplier,outcomes.summary.df$daymaxInc)
 
 # Wu et al Partial Correlations
 what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method="spearman-partial-correlation-slow") ; plot(what.matters)
#what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method= "kendall-partial-correlation-slow") ; plot(what.matters)
#what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method= "pearson-partial-correlation-slow") ; plot(what.matters) # "pearson-...-slow" and "pearson-...-fast" are equivalent
#what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method= "pearson-partial-correlation-fast") ; plot(what.matters) # "pearson-...-slow" and "pearson-...-fast" are equivalent
#what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method="t-test")              ; plot(what.matters)
 what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df),"maxI",method="negative-log-p-value"); plot(what.matters); abline(3,0)
 
 # BEGIN Miscellaneous explorations
 
 names(list.sweep)[1:2]
 names(list.sweep[[2]])
 names(df.sweep)
 table(df.sweep$etiquette)[1:2]
 names(outcomes.summary.df)
 
 
 # quick crack at plotting. Can plot much better ... but good enough for proof of concept
 plot(parms.tried.df$Cgg.multiplier,parms.tried.df$Cgq.multiplier)  
 #plot(df.sweep$time , df.sweep$S,type="l") 
 plot(outcomes.summary.df$daymaxInc, outcomes.summary.df$maxInc)
 plot(outcomes.summary.df$daymaxInc, outcomes.summary.df$cumI.75days)
 plot(outcomes.summary.df$`GOF cumI days 50-59`,outcomes.summary.df$`GOF cumI days 60-69`)
 interesting = identify(outcomes.summary.df$`GOF cumI days 50-59`,outcomes.summary.df$`GOF cumI days 60-69`)
 interesting
 interesting.label = names(list.sweep)[interesting]
 df.sweep.interesting = subset(df.sweep, etiquette %in% interesting.label)
 plot(df.sweep.interesting$time , df.sweep.interesting$S,type="l")  # Ugly plot ... just proof of concept
 # plot(subset(df.sweep,time<50)$time , subset(df.sweep,time<50)$S2,type="l") 
 
 # Save interesting stuff in excel workbook
  interesting = c(2,8)
 for(k in interesting)
    SaveModelInExcel (list.sweep[[k]]$input.info,paste0("Interesting ",k,".xlsx"),unlist(sheet_names))
 
 confirm2 =  SEIR.n.Age.Classes("Interesting 2.xlsx",sheet_names,post.processing.companion.kit=list(targets=covid.targets))
 confirm8 =  SEIR.n.Age.Classes("Interesting 8.xlsx",sheet_names,post.processing.companion.kit=list(targets=covid.targets))
 
 range(list.sweep[[2]]$solution - confirm2$solution) # compare interesting2 to interesting2 (reran)
 range(list.sweep[[8]]$solution - confirm8$solution) # compare interesting8 to interesting8 (reran)
 range(list.sweep[[8]]$solution - confirm2$solution) # compare interesting8 to interesting2 (reran)
 # END Miscellaneous explorations
 
 
 
 
 # BEGIN Example 2 continued ... Check if successful in replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx
 if(FALSE)
 {
    # BEGIN Read 12 different sheets in PHAC's SEIR_results_1agegrpSC1_12 Figformated.xlsx workbook
    SC1_12_file= paste0(WDir,"/RE__Update_on_SEIR_model_sensitivity_analysis/SEIR_results_1agegrpSC1_12 Figformated.xlsx")
    PHAC=list()
    for(k in 1:12)
       PHAC[[k]] = as.data.frame.from.tbl( readxl::read_excel(SC1_12_file, sheet =paste0("SEIR_results_1agegrpSC",k) ) ) 
    # END Read 12 different sheets in PHAC's SEIR_results_1agegrpSC1_12 Figformated.xlsx workbook
    
    # Compare with results of "Code replicating PHAC results ..."
    common.col = intersect(names(list.sweep[[1]]),names(PHAC[[1]]))
    for(k in 1:12)
         print( range(list.sweep[[k]][,common.col] - PHAC[[k]][,common.col] ) )
    
#    [1] -5.587935e-08  6.332994e-08
#    [1] -5.587935e-08  5.215406e-08
#    [1] -7.078052e-08  5.587935e-08
#    [1] -5.215406e-08  8.940697e-08
#    [1] -7.078052e-08  5.215406e-08
#    [1] -5.215406e-08  7.450581e-08
#    [1] -390.3934  390.4064             # weird difference
#    [1] -0.7411783  0.7418581           # weird difference but very small
#    [1] -1.415610e-07  2.440065e-07
#    [1] -5.215406e-08  6.519258e-08
#    [1] -5.215406e-08  5.587935e-08
#    [1] -5.215406e-08  5.215406e-08    
    
 }
 # END Example 2 continued ... Check if successful in replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx
 