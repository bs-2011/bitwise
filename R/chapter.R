#' A Predict_Cluster Function
#'
#' This function allows you to predict the cluster group in which point lies
#' @param input input data in csv or dataframe format
#' @keywords input file
#' @export
#' @examples
#' predict_cl()


# predicting the cluster
predict_cl <- function(input){
  #input can either be csv file or data
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }
  stopifnot("Correct.Step.Duration..sec." %in% names(newdata))
  stopifnot("Error.Step.Duration..sec."  %in% names(newdata))
  stopifnot("Incorrects" %in% names(newdata))
  stopifnot("Hints" %in% names(newdata))
  stopifnot("Corrects" %in% names(newdata))
  stopifnot("Opportunity.SubSkills." %in% names(newdata))
  stopifnot("Correct.First.Attempt" %in% names(newdata))

 mean= c(3.0677659,5.8675305, 0.7447891, 0.2317014,1.0252060, 48.2542414 ,11.4030538,24.5055744, 0.7750848 ,1.1456617 ,0.5647116 ,0.9466796,8.9352964)
 sd = c(  10.5229614    ,        23.6043839 ,
 2.8453583    ,        0.8347736 ,
 1.4038370    ,     117.3577202 ,
 35.1267237  ,     45.4824252 ,
 0.4175771    ,     1.2263231 ,
 0.7206431     ,    0.2246989 ,
 25.1374324 )
 variables = c(  "Correct.Step.Duration..sec." ,"Error.Step.Duration..sec.",
                 "Incorrects"                  ,"Hints" ,
                 "Corrects"                    ,"Opportunity.SubSkills." ,
                 "Opportunity.KTracedSkills."  ,"Opportunity.Rules." ,
                 "Correct.First.Attempt"       ,"count_subskill" ,
                 "count_tracedskill"           ,"count_rules"  ,
                 "Step.Duration..sec."        )
  newdata = newdata[,variables]


  nedata = (newdata-mean)/sd
  newdata_pca = predict(pca, newdata)
 # Min Distance
  PC1 =c(0.5703497 ,-1.1556615, -0.6365549,0.8070864,6.118324)
  PC2 = c(1.0005320,0.4970475,-0.2136389,-2.3633537,1.528501)
  centroid_pca_1 = data.frame(cbind(PC1,PC2))

  points = rbind(centroid_pca_1,newdata_pca[,c("PC1","PC2")])
  dist_matrix = dist(points)
  dist_matrix = as.matrix(dist_matrix)
  dist_matrix = as.array(dist_matrix)
  dist_matrix = dist_matrix[1:5,]
  c_index =  which.min(dist_matrix[,6])

  #prediction by predict_clust
  newdata$cluster_number <- c_index
  return(newdata$cluster_number)
}

