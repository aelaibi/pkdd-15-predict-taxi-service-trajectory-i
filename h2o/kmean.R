library(h2o)
library(stringr)

h2oServer <- h2o.init(ip="localhost", port = 54321)

myseed=5816985749037550201

path <- "/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/"
path_points_train <- paste0(path,"./data/train_points.csv")
path_points_train_hex <- h2o.importFile(h2oServer, path = path_points_train)
kmeanModel <- h2o.kmeans(centers=100,data=path_points_train_hex, seed=myseed)
kmeanModel@model$centers