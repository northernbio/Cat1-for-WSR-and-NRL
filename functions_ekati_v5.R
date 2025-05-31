# required functions for Ekati RSF models
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# functions for rescaling RSF predictions from 0 to 1
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_norm2 <- function(x) {
  (x) / (max(x))
}

predictSSFx <- function (mod, newdata) {
  # This functions expects a model without an intercept in coefficients
  #mod = SSF.model
  #newdata = traindata
  co.data <- coef(mod)
  nameList <- names(co.data)
  # dat <- head(newdata,10) # small data for testing
  datSel<- subset(newdata, select=nameList)
  mat <- data.matrix(datSel) 
  M <- unname(mat)
  xdat <- M %*% co.data  # version for no intercept
  dfXdat <- as.data.frame(xdat)
  dfXdat$pred <- logit2prob(dfXdat$V1)
  #newdata <- cbind(newdata,dfXdat$pred)
  return(dfXdat$pred)
}


# Definition of the predictSSF function
predictSSF <- function (mod, newdata) {
  
  # Expectation of the function: a model without an intercept in coefficients
  
  # Prepare the coefficient data from the passed model
  co.data <- coef(mod)
  
  # Get the names from the coefficient data
  nameList <- names(co.data)
  
  # Subset the data from "newdata" using only the variables in "nameList"
  datSel<- subset(newdata, select=nameList)
  
  # Convert the data to matrix format
  mat <- data.matrix(datSel)
  
  # Remove the row and column names from the matrix to simplify calculations
  M <- unname(mat)
  
  # Multiply the matrix by the coefficients to obtain predicted outcomes
  xdat <- M %*% co.data  # version for no intercept
  
  # Convert the predicted values to a data frame for easy manipulation
  dfXdat <- as.data.frame(xdat)
  
  # Convert the predicted values to probabilities using the logit2prob function
  dfXdat$pred <- logit2prob(dfXdat$V1)
  
  # Add the predicted probabilities to the original data frame to facilitate analysis
  return(dfXdat$pred)
}




predictclogit <- function (mod, newdata) {
  # This functions expects a model without an intercept in coefficients
  
  #Extract coefficient data from the model
  co.data <- coef(mod) 
  #Create a list of the variable names in the model
  nameList <- names(co.data)
  
  #Subset the new data to only include columns with matching names as the variable names in the model
  datSel<- subset(newdata, select=nameList)
  
  #Convert the subsetted data to a matrix where all variables are numeric
  mat <- data.matrix(datSel)
  
  #Remove the variable names from the matrix
  M <- unname(mat)
  
  #Calculate predicted log-odds using the model coefficients and the new data, and convert to probabilities
  xdat <- exp(M %*% co.data)  # version for no intercept
  dfXdat <- as.data.frame(xdat)
  dfXdat$pred <- logit2prob(dfXdat$V1)
  #pred <- sapply(xdat, function(x) logit2prob(x)) #Alternative code for above 2 lines
  
  #Return the predicted probabilities
  return(dfXdat$pred)
}



predictRSF <- function (mod, newdata) {
  # GLM stepmodel version  This gives same answer as using predict
  # This functions expects a model with an intercept in coefficients, and
  # uses the intercept in the calculation
  # e.g. step.model
  #mod = step.model # for testing
  #newdata = traindata # for testing
  co.data <- coef(mod) #Intercept present
  #co.data <- co.data[-1]
  nameList <- names(co.data) #intercept in names list
  nameList <-nameList[-1]  #remove intercept because there is no intercept in data columns
  #traindataSmall <- head(traindata,10)
  traindataSel<- subset(newdata, select=nameList) #No intercept
  mat <- data.matrix(traindataSel)
  M <- unname(mat)
  x <- cbind(1, M) # Put intercept (1) back (1 * intercept = intercept)
  xdat <- x %*% co.data
  dfXdat <- as.data.frame(xdat)
  dfXdat$pred <- logit2prob(dfXdat$V1)
  return(dfXdat$pred)
}  

perfClogitStats <- function (mod,dat) { #pass a model
  # summod2 <- gofm(mod)
  # summod2$AIC
  print(summary(mod)$coefficients)
  print(gofm(mod))
  print(plot_model(mod, ci_method="wald"))
  dat$pred_mod <- predictclogit(mod, newdata = dat)
  dfROC<- ROC(dat$pred_mod, dat$case_, plot ="ROC")
  #title (main = paste(s))
  
}

perfTestStats <- function (mod,dat) { #pass a model
  # summod2 <- gofm(mod)
  # summod2$AIC
#  print(summary(mod)$coefficients)
#  print(gofm(mod))
#  print(plot_model(mod, ci_method="wald"))
  dat$pred_mod <- predictclogit(mod, newdata = dat)
  dfROC<- ROC(dat$pred_mod, dat$case_, plot ="ROC")
  #title (main = paste(s))
  
}


## JR edited March 27, 2023 lines 161-163 re: GKMineBuffer file location and line 193, changed line 148 to writeFiles=TRUE
  filterDistToDevelopment <- function (dat, season, writeFiles=TRUE, pathBase) {
    Outside30k <- dplyr::filter(dat, case_ == "TRUE" & DF_MINES > 30 & DF_PROADS >30) #output correct 42,328 records
    List<-Outside30k$stepnum  # either one of the criteria DF_MINES and DF_PROADS <30000 puts the data in the Ekati/Diavik 30 km halo
    dfInside30km<-subset(dat,!(stepnum %in% List)) # data for modelling movement

    # Need to screen out FALSE case records with no matching TRUE case records (function of deleting is.na(ELEVATION) records)
    stepnumTrue <- dplyr::filter(dfInside30km, case_ == "TRUE") #screen for true records
    TRUEStepList<-stepnumTrue$stepnum  # list of true stepnums
    #nomatch<-subset(dfInside30km,!(stepnum %in% TRUEStepList))
    dfInside30km<-subset(dfInside30km, stepnum %in% TRUEStepList)
    rm(stepnumTRUE, TRUEStepList) # clean up un-needed df and list

    dfForiSSF<-subset(dat, stepnum %in% List) #253,501 records
##    path2 <- paste(pathBase,"QGIS/GKMine_30km_buffer.shp", sep="") 
    GKMineBuff <- read_sf("C:/Users/Jim/Dropbox/Ekati/QGIS/GKMine_30km_buffer.shp")
##    GKMineBuff <- read_sf(path2)
    
    sf_obj <- st_as_sf(dfForiSSF, coords = c("x2_", "y2_"), crs = 3978)
    sfGKMinePoints <- st_filter(sf_obj,GKMineBuff)
    Inside30kmGK <- as.data.frame(st_drop_geometry(sfGKMinePoints))  ## JR - I get 501 observations
    ListInsideGK <- dplyr::filter(Inside30kmGK, case_ == "TRUE") %>% .$stepnum  ## JR - I get 90 TRUE observations
    dfInsideMINE3GK <- subset(dfForiSSF,(stepnum %in% ListInsideGK)) # JR I get 540 total
    dfForiSSF<-subset(dfForiSSF,!(stepnum %in% ListInsideGK)) ## Records = 252,961 (253,501 - 540) as expected
    OutsideMINE3_5km<-dplyr::filter(dfForiSSF, case_ == "TRUE" & DF_MINES3 > 5) # 41,801 records
    OutMINE3List<-OutsideMINE3_5km$stepnum 
    dfInsideMINE3<-subset(dfForiSSF,!(stepnum %in% OutMINE3List)) # 396 records
    dfForiSSF<-subset(dfForiSSF,(stepnum %in% OutMINE3List)) ## Records = 252,565 (252,961 - 396) as expected
      if (season == "Winter") {
        OutsideLROAD_5km<-dplyr::filter(dfForiSSF, case_ == "TRUE" & DF_LROADS > 5) # should be <= 7,263 records
        OutsideLROADList<-OutsideLROAD_5km$stepnum
        dfInLROAD5k<-subset(dfForiSSF,!(stepnum %in% OutsideLROADList)) # locates winter road data
        dfForiSSF<-subset(dfForiSSF,(stepnum %in% OutsideLROADList))  # removes winter road data from WinteriSSF analysis data
        dfInsideMINE3<-rbind(dfInsideMINE3,dfInLROAD5k)  # adds the winter road buffer data to the other excluded data
      }
    dfInsideMINE3<-rbind(dfInsideMINE3,dfInsideMINE3GK)  # 936 records
    
#Following code added March 27, 2023
    write.csv(dfInsideMINE3, file = paste("SSF_Models/excluded_data/V2_dfInsideMINE3_",season, ".csv", sep=""))
    save(dfInside30km, file= paste("SSF_Models/dataGeoFence/V2_dfInside30km_",season, ".rda", sep=""))
    save(dfForiSSF, file = paste("SSF_Models/data/V2_dfForiSSF_",season, ".rda", sep=""))
    
#    if (writeFiles==TRUE){
#      fNamedfInside30km <- paste("V2_dfInside30km",season, ".rda", sep="_")
#      fNamedfForiSSF <- paste("V2_dfForiSSF",season, ".rda", sep="_")
#      fNamedfInsideMINE3 <- paste("V2_dfInsideMINE3",season, ".csv", sep="_")
#      path3 <- paste(pathBase,"SSF_Models/dataGeoFence", sep="")
#      #setwd("C:/Users/north/Dropbox/Etaki/RSF_Output/Data_SSF")
#      setwd(path3)
#      save(dfInside30km, file= fNamedfInside30km)
#      save(dfForiSSF, file = fNamedfForiSSF)
#      write.csv(dfInsideMINE3, file = fNamedfInsideMINE3)  # write a copy to keep track of records
#    }
    return(dfForiSSF)
  }


## JR edited March 27, 2023
transformDataSSF <- function (x) {
  #x<- Ekati_8hour_calving_intersection # for testing only
  #remove missing values for elevation right at the start
  x <- x[!(is.na(x$ELEVATION)), ]
  #***********************************************************
  # Subset, transform, and then scale all proportion variables
  x1 <-  subset(x, select=BEDBOULD:FOREST_S4)
  # Warning during logit transform: (Note: largest value of p > 1 so values of p interpreted as percents)
  x1[x1 > 1] <- 1  # in some cases prop was 1.1, 
  x2 <-  subset(x, select=P_ESKER:WBAREA_S4) 
  x2[x2 > 1] <- 1 #truncate proportions as 1
  matrixRSF1 <- scale(car::logit(x1)) # transform proportions, then scale as z-deviates
  matrixRSF2 <- scale(car::logit(x2)) # transform proportions, then scale
  # *****************************************************
  # Scale elevation, slope, and edge density
  x3 <- scale(subset(x, select=ELEVATION:SLOPE)) # elevation and slope.  No trans, Only scaled
  x2b <-  scale(subset(x, select=WAT_EDGE:WATEDGE_S3)) #Water edge, value 0 ~ 300. Scaled, not transformed
  matrixRSF0 <- data.frame(x2b) 
  matrixRSF3 <- data.frame(x3) #matrix to dataframe
  #***********************************************************************
  # Subset and scale from m to kms all the distance to feature variables. 
  distToFeature <- (subset(x, select=ROAD_WINTR:DF_MINES3))/1000
  #******************************
  # Create factors from all categorical variables
  factorData<-  subset(x, select=Sex) #start the df with Sex
  factorData$SexF <- factor(factorData$Sex)
  factorData$VegZoneF <- factor(x$VEGZONENUM)
  factorData$YearF <- factor(x$AnalysisYr)
  factorData$HerdF <- factor(x$Herd)
  factorData$EskerF <- factor(x$ESKER)
  factorData <- factorData %>% dplyr::select(-Sex)
  #**************************
  #*Put variables back together
  collarData <-  subset(x, select=Season:SETID)    # this line revised in functions_ekati_V5.R. It previously said =Season:dt_
  ## the edit captures the Oestrid and Mosquito indices in the summer months, We then remove the unneeded SETID field
  collarData<-within(collarData, rm(SETID))  #of all input files, the LateSumm file did not have a SETID field (I added one and assigned it values of 1 -JR).
  dfRSFx <- cbind(collarData, factorData, distToFeature, matrixRSF0,matrixRSF1,matrixRSF2, matrixRSF3)
  dfRSFx$IDYr <- paste(dfRSFx$ID,dfRSFx$AnalysisYr) #This needed to randomly select ID_Year combo
  return(dfRSFx)
}

transformNewData <- function (x) {
  #x<- Ekati_8hour_calving_intersection # for testing only
  #remove missing values for elevation right at the start
  x <- x[!(is.na(x$ELEVATION)), ]
  #***********************************************************
  # Subset, transform, and then scale all proportion variables
  x1 <-  subset(x, select=BEDBOULD:FOREST_S4)
  # Warning during logit transform: (Note: largest value of p > 1 so values of p interpreted as percents)
  x1[x1 > 1] <- 1  # in some cases prop was 1.1, 
  x2 <-  subset(x, select=P_ESKER:WBAREA_S4) 
  x2[x2 > 1] <- 1 #truncate proportions as 1
  matrixRSF1 <- scale(car::logit(x1)) # transform proportions, then scale as z-deviates
  matrixRSF2 <- scale(car::logit(x2)) # transform proportions, then scale
  # *****************************************************
  # Scale elevation, slope, and edge density
  x3 <- scale(subset(x, select=ELEVATION:SLOPE)) # elevation and slope.  No trans, Only scaled
  x2b <-  scale(subset(x, select=WAT_EDGE:WATEDGE_S3)) #Water edge, value 0 ~ 300. Scaled, not transformed
  matrixRSF0 <- data.frame(x2b) 
  matrixRSF3 <- data.frame(x3) #matrix to dataframe
  #***********************************************************************
  # Subset and scale from m to kms all the distance to feature variables. 
  #distToFeature <- (subset(x, select=ROAD_WINTR:DF_MINES3))/1000
  #******************************
  # Create factors from all categorical variables
  factorData<-  subset(x, select=VEGZONENUM) #start the df with VEGZONENUM
  factorData$VegZoneF <- factor(x$VEGZONENUM)
  factorData$YearF <- factor(x$AnalysisYr)
  factorData$HerdF <- factor(x$Herd)
  factorData$EskerF <- factor(x$ESKER)
  factorData <- factorData %>% dplyr::select(-VEGZONENUM)
  #**************************
  #*Put variables back together
 # collarData <-  subset(x, select=Season:dt_) 
  dfRSFx <- cbind(factorData, matrixRSF0,matrixRSF1,matrixRSF2, matrixRSF3)
  #dfRSFx$IDYr <- paste(dfRSFx$ID,dfRSFx$AnalysisYr) #This needed to randomly select ID_Year combo
  return(dfRSFx)
}


## JR edited March 27, 2023
squareVariables<- function (dat){
  dat$TUNDRA_sq <- dat$TUNDRA^2
  dat$TUSSK_sq <- dat$TUSSK^2
  #dat$FOREST_S3_sq <- dat$FOREST_S3^2
  dat$ELEVATION_sq <- dat$ELEVATION^2
  dat$WBAREA_sq <- dat$WBAREA^2
  dat$SEDGEWET_sq <- dat$SEDGEWET^2
  dat$LHSHRUB_sq <- dat$LHSHRUB^2
  dat$SLOPE_sq <- dat$SLOPE^2
  dat$P_ESKER_sq <- dat$P_ESKER^2
  dat$WAT_EDGE_sq <- dat$WAT_EDGE^2
  dat$BEDBOULD_sq <- dat$BEDBOULD^2
  dat$TUNDR_S3_sq <- dat$TUNDR_S3^2      # added in functions_ekati_V5
  dat$TUSSK_S3_sq <- dat$TUSSK_S3^2      # added in functions_ekati_V5
  dat$SEDWET_S3_sq <- dat$SEDWET_S3^2    # added in functions_ekati_V5
  dat$LHSHRUB_S3_sq <- dat$LHSHRUB_S3^2  # added in functions_ekati_V5
  dat$BEDBLD_S3_sq <- dat$BEDBLD_S3^2    # added in functions_ekati_V5
  dat$WBAREA_S3_sq <- dat$WBAREA_S3^2    # added in functions_ekati_V5
  dat$P_ESKER_S3_sq <- dat$P_ESKER_S3^2  # added in functions_ekati_V5
  dat$TUNDR_S4_sq <- dat$TUNDR_S4^2
  dat$TUSSK_S4_sq <- dat$TUSSK_S4^2
  #dat$FOREST_S4_sq <- dat$FOREST_S4^2
  dat$ELEVATION_sq <- dat$ELEVATION^2
  dat$WBAREA_S4_sq <- dat$WBAREA_S4^2
  dat$SEDWET_S4_sq <- dat$SEDWET_S4^2
  dat$LHSHRUB_S4_sq <- dat$LHSHRUB_S4^2
  dat$SLOPE_sq <- dat$SLOPE^2
  dat$P_ESKER_S4_sq <- dat$P_ESKER_S4^2
  dat$WATEDGE_S3_sq <- dat$WATEDGE_S3^2 #Note, Wateedge_S4 not available
  dat$BEDBLD_S4_sq <- dat$BEDBLD_S4^2
  return(dat)
}

make_Train = function(grouped_df, prop = 0.7, sex = "Both", repl = FALSE, wgt=NULL ) {
  # grouped_df <- dfRSFx %>% group_by(IDYr) # For testing
  #  sex <- "Female" # For testing
  # https://dplyr.tidyverse.org/reference/slice.html 
  grp_var <- grouped_df %>% 
    groups %>%
    unlist %>% #convert to vector
    as.character
  #for testing only
  #grouped_df2 <- grouped_df # make a copy so we don't remove a layer of grouping when calling summarise
  if (sex == "Female" | sex == "Male") {
    grouped_df<- grouped_df %>% dplyr::filter (Sex ==sex) #Optionally get traindata for one sex
  }
  
  nx <- nrow (unique(grouped_df[grp_var]))
  size <- as.integer(nx * prop) 
  random_grp <- grouped_df %>% 
    dplyr::summarise() %>% 
    slice_sample(n = size, replace = repl, weight_by = wgt) %>% 
    mutate(unique_id = 1:NROW(.))
  grouped_df %>% 
    right_join(random_grp, by=grp_var) %>% 
    group_by_(grp_var) 
  
}

createDir1  <- function (main_dir, sub_dir) {
  #main_dir <- "D:\\FERIT_1\\Webequie Access\\R\\Nov2022_Response"
  #  sub_dir <- "GBMOutTest5"
  
  sub_dirUL1 <- "GIS_Joins"
  sub_dirUL2 <- "saveMods"
  sub_dirUL3 <- "savePlots"
  sub_dirUL4 <- "saveVarList"
  sub_dirUL5 <- "summaries"
  sub_dirUL3a <- "CrossVal"
  sub_dirUL3b <- "Influence"
  sub_dirUL3c <- "Response"
  sub_dirUL3d <- "ROC"
  
  if (file.exists (sub_dir)) {
    setwd(file.path(main_dir, sub_dir))
  } else {
    dir.create (file.path(main_dir, sub_dir,  sub_dirUL2), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir, sub_dir,  sub_dirUL3), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir, sub_dir,  sub_dirUL4), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir,  sub_dir, sub_dirUL5), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir,  sub_dir, sub_dirUL3, sub_dirUL3a), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir,  sub_dir, sub_dirUL3, sub_dirUL3b), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir,  sub_dir, sub_dirUL3, sub_dirUL3c), recursive = TRUE, mode = "0777")
    dir.create (file.path(main_dir,  sub_dir, sub_dirUL3, sub_dirUL3d), recursive = TRUE, mode = "0777")
    setwd(file.path(main_dir))
  }
}
