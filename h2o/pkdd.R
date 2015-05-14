sink("pkdd.log", split = T)

## This code block is to install a particular version of H2O
# START
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1597/R", getOption("repos")))) #choose a build here
# END

# Fetch the latest nightly build using Jo-fai Chow's package
#devtools::install_github("woobe/deepr")
#deepr::install_h2o()

library(h2o)
library(stringr)

## Connect to H2O server (On server(s), run 'java -Xmx64G -jar h2o.jar -port 53322 -name CTR -data_max_factor_levels 100000000' first)
## Go to http://server:53322/ to check Jobs/Data/Models etc.
h2oServer <- h2o.init(ip="localhost", port = 54321)
#h2oServer <- h2o.init(nthreads=-1, max_mem_size='64g')

## Helper function for feature engineering
h2o.addNewFeatures <- function(frame, timecol, intcols, factorcols, key) {
  cat("\nFeature engineering for time column.")
  hour <- frame[,timecol] %% 100
  colnames(hour) <- "hour"
  day <- ((frame[,timecol] - hour) %% 10000)/100
  colnames(day) <- "day"
  dow <- day %% 7
  colnames(dow) <- "dayofweek"
  frame <- cbind(frame[,-match(timecol,colnames(frame))], day, dow, hour, as.factor(day), as.factor(dow), as.factor(hour))
  frame <- h2o.assign(frame, key)
  h2o.rm(h2oServer, grep(pattern = "Last.value", x = h2o.ls(h2oServer)$Key, value = TRUE))

  cat("\nFeature engineering for integer columns.")
  newfactors <- c()
  for (int in intcols) {
    # turn integers into factors, keep top 100 levels
    trim_integer_levels <- h2o.interaction(as.factor(frame[,int]), factors = 1, pairwise = FALSE, max_factors = 100, min_occurrence = 1)
    newfactors <- c(newfactors, colnames(trim_integer_levels))
    frame <- cbind(frame, trim_integer_levels)
    frame <- h2o.assign(frame, key)
    h2o.rm(h2oServer, grep(pattern = "Last.value", x = h2o.ls(h2oServer)$Key, value = TRUE))
  }

  cat("\nFeature engineering for factor columns.")
  # create pair-wise interaction between factors, keep top 100 levels
  factor_interactions <- h2o.interaction(frame, factors = c(newfactors,factorcols), pairwise = TRUE, max_factors = 100, min_occurrence = 2)
  frame <- cbind(frame, factor_interactions)

  # Store frame under designated key
  frame <- h2o.assign(frame, key)

  h2o.rm(h2oServer, grep(pattern = "Last.value", x = h2o.ls(h2oServer)$Key, value = TRUE))
  frame
}

h2o.logLoss <- function(preds, resp) {
  tpc <- preds
  tpc <- h2o.exec(h2oServer,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
  tpc <- h2o.exec(h2oServer,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
  LL <- h2o.exec(h2oServer,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
  h2o.rm(h2oServer, grep(pattern = "Last.value", x = h2o.ls(h2oServer)$Key, value = TRUE))
  LL
}

h2o.HaversineDistance=function(lat1,lon1,lat2,lon2)
{
  #returns the distance in km
  REarth<-6371
  lat<-abs(lat1-lat2)*pi/180
  lon<-abs(lon1-lon2)*pi/180
  lat1<-lat1*pi/180
  lat2<-lat2*pi/180
  a<-sin(lat/2)*sin(lat/2)+cos(lat1)*cos(lat2)*sin(lon/2)*sin(lon/2)
  d<-2*atan2(sqrt(a),sqrt(1-a))
  d<-REarth*d
  return(d)
}

h2o.meanHaversineDistance<-function(lat1,lon1,lat2,lon2)
{
  return(mean(HaversineDistance(lat1,lon1,lat2,lon2)))
}

h2o.RMSE<-function(pre,real)
{
  return(sqrt(mean((pre-real)*(pre-real))))
}


### START
myseed=5816985749037550201
path <- "/Users/Abdelhaq/dev/DataScience/Kaggle/pkdd-15-predict-taxi-service-trajectory-i/"

path_submission <- paste0(path,"./data/sampleSubmission.csv")
path_train <- paste0(path,"./data/train2.csv")
path_test <- paste0(path,"./data/test2.csv")

cat("\nReading data.")
train_hex <- h2o.importFile(h2oServer, path = path_train)
test_hex <- h2o.importFile(h2oServer, path = path_test)




## Split into train/validation based on training days (first 9 days: train, last day: test)
cat("\nSplitting into train/validation")
reproducible_mode = T # Set to TRUE if you want reproducible results, e.g. for final Kaggle submission if you think you'll win :)  Note: will be slower for DL
splits <- h2o.splitFrame(train_hex, ratios = 0.95, shuffle=!reproducible_mode)
train <- splits[[1]]
valid <- splits[[2]]
h2o.rm(h2oServer, grep(pattern = "Last.value", x = h2o.ls(h2oServer)$Key, value = TRUE))

# target are 17 and 18 column

cat("\nTraining H2O model on training/validation ")
## Note: This could be grid search models, after which you would obtain the best model with model <- cvmodel@model[[1]]
cvmodelLatitude <- h2o.randomForest(data=train, validation=valid, x=c(2:16), y=18,classification=F,
                           type="BigData", ntree=200, depth=20, seed=myseed)
cvmodelLangitude <- h2o.randomForest(data=train, validation=valid, x=c(2:16), y=17,classification=F,
                                    type="BigData", ntree=200, depth=20, seed=myseed)

#GBM
cvmodelLatitude <- h2o.gbm(data=train, validation=valid, x=c(2:16), y=18,distribution="gaussian"
                   , n.tree=50, interaction.depth=10)

cvmodelLangitude <- h2o.gbm(data=train, validation=valid, x=c(2:16), y=17,distribution="gaussian"
                   , n.tree=50, interaction.depth=10)


#cvmodel <- h2o.deeplearning(data=train, validation=valid, x=c(3:ncol(train)), y=2,
#                            hidden=c(50,50), max_categorical_features=100000, train_samples_per_iteration=10000, score_validation_samples=10000)

# Latitude Part
train_resp_lat <- train[,18] #actual label
train_preds_lat <- h2o.predict(cvmodelLatitude, train)[,1] #[,3] is probability for class 1
valid_resp_lat <- valid[,18]
valid_preds_lat <- h2o.predict(cvmodelLatitude, valid)[,1]

# Longitude Part
train_resp_long <- train[,17] #actual label
train_preds_long <- h2o.predict(cvmodelLangitude, train)[,1] #[,3] is probability for class 1
valid_resp_long <- valid[,17]
valid_preds_long <- h2o.predict(cvmodelLangitude, valid)[,1]


cat("\nHaversineDistance on Training data:",  h2o.meanHaversineDistance(data.matrix(as.data.frame(train_preds_lat)),
                                                                          data.matrix(as.data.frame(train_preds_long)),
                                                                          data.matrix(as.data.frame(train_resp_lat)),
                                                                          data.matrix(as.data.frame(train_resp_long))))

cat("\nHaversineDistance on validation data:",  h2o.meanHaversineDistance(data.matrix(as.data.frame(valid_preds_lat)),
                          data.matrix(as.data.frame(valid_preds_long)),
                          data.matrix(as.data.frame(valid_resp_lat)),
                          data.matrix(as.data.frame(valid_resp_long))))

# Submit File and Building From Full Model 

fullModel = T

if(fullModel){
  cvmodelLatitudeFull <- h2o.randomForest(data=train_hex, x=c(2:16), y=18,classification=F,
                                      type="BigData", ntree=200, depth=20, seed=myseed)
  cvmodelLangitudeFull <- h2o.randomForest(data=train_hex, x=c(2:16), y=17,classification=F,
                                       type="BigData", ntree=200, depth=20, seed=myseed)
  
  #GBM
  cvmodelLatitudeFull <- h2o.gbm(data=train_hex, validation=valid, x=c(2:16), y=18,distribution="gaussian"
                             , n.tree=100, interaction.depth=10)
  
  cvmodelLangitudeFull <- h2o.gbm(data=train_hex, validation=valid, x=c(2:16), y=17,distribution="gaussian"
                              , n.tree=100, interaction.depth=10)
  
  pred_lat <- h2o.predict(cvmodelLatitudeFull, test_hex)[,1]
  pred_long <- h2o.predict(cvmodelLangitudeFull, test_hex)[,1]
  submission <- read.csv(path_submission, colClasses = c("character"))
  submission[2] <- as.data.frame(pred_lat)
  submission[3] <- as.data.frame(pred_long)
  colnames(submission) <- c("TRIP_ID","LATITUDE","LONGITUDE")
  cat("\nWriting predictions on test data.")
  write.csv(as.data.frame(submission), file = paste(path,"./submission1.3.csv", sep = ''), quote = F, row.names = F)
  sink()
  
}
  

