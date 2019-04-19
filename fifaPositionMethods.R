rm(list = ls(all = T))
source('./libraries.R')

return_list_for_GK <- function() {
  foo <- vector(mode="list", length=6)
  names(foo) <- c("GKDiving", "GKHandling", "GKPositioning"
                  , "GKReflexes", "Reactions", "GKKicking")
  foo[[1]] <- 0.24; foo[[2]] <- 0.22; foo[[3]] <- 0.22
  foo[[4]] <- 0.22; foo[[5]] <- 0.06; foo[[6]] <- 0.04
  return (foo)
}

return_list_for_CB_or_RCB_or_LCB <- function() {
  foo <- vector(mode="list", length=11)
  names(foo) <- c("Marking", "StandingTackle", "SlidingTackle"
                  , "HeadingAccuracy", "Strength", "Aggression"
                  , "Interceptions", "ShortPassing", "BallControl"
                  , "Reactions", "Jumping")
  foo[[1]] <- 0.15; foo[[2]] <- 0.15; foo[[3]] <- 0.15
  foo[[4]] <- 0.10; foo[[5]] <- 0.10; foo[[6]] <- 0.08
  foo[[7]] <- 0.08; foo[[8]] <- 0.05; foo[[9]] <- 0.05
  foo[[10]] <- 0.05; foo[[11]] <- 0.04;
  return (foo)
}

return_list_for_RB_or_LB <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("SlidingTackle", "StandingTackle", "Interceptions"
                  , "Marking", "Stamina", "Reactions"
                  , "Crossing", "HeadingAccuracy", "BallControl"
                  , "ShortPassing", "SprintSpeed", "Aggression")
  foo[[1]] <- 0.13; foo[[2]] <- 0.12; foo[[3]] <- 0.12
  foo[[4]] <- 0.10; foo[[5]] <- 0.08; foo[[6]] <- 0.08
  foo[[7]] <- 0.07; foo[[8]] <- 0.07; foo[[9]] <- 0.07
  foo[[10]] <- 0.06; foo[[11]] <- 0.05; foo[[12]] <- 0.05;
  return (foo)
}

return_list_for_RWB_or_LWB <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("StandingTackle", "SlidingTackle", "Crossing"
                  , "ShortPassing", "BallControl", "Interceptions"
                  , "Marking", "Stamina", "Reactions"
                  , "Dribbling", "SprintSpeed", "Agility")
  foo[[1]] <- 0.11; foo[[2]] <- 0.10; foo[[3]] <- 0.10
  foo[[4]] <- 0.10; foo[[5]] <- 0.10; foo[[6]] <- 0.10
  foo[[7]] <- 0.09; foo[[8]] <- 0.08; foo[[9]] <- 0.08
  foo[[10]] <- 0.07; foo[[11]] <- 0.04; foo[[12]] <- 0.03;
  return (foo)
}

return_list_for_CDM_or_LDM_or_RDM <- function() {
  foo <- vector(mode="list", length=11)
  names(foo) <- c("ShortPassing", "Interceptions", "LongPassing"
                  , "Marking", "StandingTackle", "BallControl"
                  , "Reactions", "Vision", "Stamina"
                  , "Strength", "Aggression")
  foo[[1]] <- 0.13; foo[[2]] <- 0.12; foo[[3]] <- 0.11
  foo[[4]] <- 0.10; foo[[5]] <- 0.10; foo[[6]] <- 0.09
  foo[[7]] <- 0.09; foo[[8]] <- 0.08; foo[[9]] <- 0.06
  foo[[10]] <- 0.06; foo[[11]] <- 0.05;
  return (foo)
}

return_list_for_CAM_or_LAM_or_RAM <- function() {
  foo <- vector(mode="list", length=11)
  names(foo) <- c("ShortPassing", "Vision", "BallControl"
                  , "Positioning", "Dribbling", "Reactions"
                  , "LongShots", "Finishing", "ShotPower"
                  , "Acceleration", "Agility")
  foo[[1]] <- 0.16; foo[[2]] <- 0.16; foo[[3]] <- 0.13
  foo[[4]] <- 0.12; foo[[5]] <- 0.11; foo[[6]] <- 0.08
  foo[[7]] <- 0.06; foo[[8]] <- 0.05; foo[[9]] <- 0.05
  foo[[10]] <- 0.04; foo[[11]] <- 0.04;
  return (foo)
}

return_list_for_RM_or_LM_or_LCM_or_RCM_or_CM <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("Crossing", "Dribbling", "ShortPassing"
                  , "BallControl", "LongPassing", "Vision"
                  , "Reactions", "Positioning", "Stamina"
                  , "Acceleration", "SprintSpeed", "Agility")
  foo[[1]] <- 0.14; foo[[2]] <- 0.14; foo[[3]] <- 0.12
  foo[[4]] <- 0.12; foo[[5]] <- 0.08; foo[[6]] <- 0.08
  foo[[7]] <- 0.07; foo[[8]] <- 0.07; foo[[9]] <- 0.05
  foo[[10]] <- 0.05; foo[[11]] <- 0.05; foo[[12]] <- 0.03;
  return (foo)
}

return_list_for_RW_or_LW <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("Crossing", "Positioning", "Dribbling"
                  , "BallControl", "ShotPower", "LongShots"
                  , "Reactions", "ShortPassing", "HeadingAccuracy"
                  , "Vision", "Acceleration", "SprintSpeed")
  foo[[1]] <- 0.16; foo[[2]] <- 0.12; foo[[3]] <- 0.11
  foo[[4]] <- 0.11; foo[[5]] <- 0.10; foo[[6]] <- 0.10
  foo[[7]] <- 0.10; foo[[8]] <- 0.06; foo[[9]] <- 0.05
  foo[[10]] <- 0.05; foo[[11]] <- 0.04; foo[[12]] <- 0.04;
  return (foo)
}

return_list_for_RF_or_CF_or_LF <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("Finishing", "Positioning", "Dribbling"
                  , "BallControl", "ShotPower", "LongShots"
                  , "Reactions", "ShortPassing", "HeadingAccuracy"
                  , "Vision", "Acceleration", "SprintSpeed")
  foo[[1]] <- 0.12; foo[[2]] <- 0.12; foo[[3]] <- 0.11
  foo[[4]] <- 0.11; foo[[5]] <- 0.10; foo[[6]] <- 0.10
  foo[[7]] <- 0.10; foo[[8]] <- 0.06; foo[[9]] <- 0.05
  foo[[10]] <- 0.05; foo[[11]] <- 0.04; foo[[12]] <- 0.04;
  return (foo)
}

return_list_for_ST_or_RS_or_LS <- function() {
  foo <- vector(mode="list", length=12)
  names(foo) <- c("Finishing", "Positioning", "HeadingAccuracy"
                  , "ShotPower", "Reactions", "Dribbling"
                  , "BallControl", "Volleys", "LongShots"
                  , "Acceleration", "SprintSpeed", "Strength")
  foo[[1]] <- 0.20; foo[[2]] <- 0.12; foo[[3]] <- 0.10
  foo[[4]] <- 0.10; foo[[5]] <- 0.10; foo[[6]] <- 0.08
  foo[[7]] <- 0.08; foo[[8]] <- 0.05; foo[[9]] <- 0.05
  foo[[10]] <- 0.05; foo[[11]] <- 0.04; foo[[12]] <- 0.03;
  return (foo)
}

getPositions<-function(positions){
  if(position=="GK"){
    return (c("GK"))
  }
  else if(position=="CB"){
    return (c("CB","RCB","LCB"))
  }
  else if(position=="B"){
    return (c("LB","RB"))
  }
  else if(position=="WB"){
    return (c("LWB","RWB"))
  }
  else if(position=="DM"){
    return (c("CDM","RDM","LDM"))
  }
  else if(position=="AM"){
    return (c("CAM","RAM","LAM"))
  }
  else if(position=="M"){
    return (c("LM","RM","RCM","RCM","LCM","CM"))
  }
  else if(position=="W"){
    return (c("RW","LW"))
  }
  else if(position=="F"){
    return (c("LF","RF","CF"))
  }
  else if(position=="S"){
    return (c("ST","RS","LS"))
  }
}

getPositionClass<-function(position){
  if(position=="GK"){
    return (c("GK"))
  }
  else if(position=="CB" || position=="RCB" || position=="LCB"){
    return ("CB")
  }
  else if(position=="LB" || position=="RB"){
    return ("B")
  }
  else if(position=="LWB" || position=="RWB"){
    return ("WB")
  }
  else if(position=="CDM" ||position=="RDM" || position=="LDM"){
    return ("DM")
  }
  else if(position=="CAM" || position=="RAM"|| position=="LAM" ){
    return ("AM")
  }
  else if(position=="RM" || position=="LM"  || position=="RCM"  || position=="LCM" || position=="CM" ){
    return ("M")
  }
  else if(position=="RW" || position=="LW" ){
    return ("W")
  }
  else if(position=="LF" || position=="RF" || position=="CF"){
    return ("F")
  }
  else if(position=="ST" || position=="RS" || position=="LS"){
    return ("S")
  }
}

return_list_for_position <- function(position) {
  if(position=="GK"){
    return (return_list_for_GK())
  }
  else if(position=="CB"|| position=="RCB" || position=="LCB"){
    return (return_list_for_CB_or_RCB_or_LCB())
  }
  else if(position=="LB" || position=="RB" ){
    return (return_list_for_RB_or_LB())
  }
  else if(position=="LWB" || position=="RWB" ){
    return (return_list_for_RWB_or_LWB())
  }
  else if(position=="CDM"|| position=="LDM" || position=="RDM"){
    return (return_list_for_CDM_or_LDM_or_RDM())
  }
  else if(position=="CAM"|| position=="LAM" || position=="RAM"){
    return (return_list_for_CAM_or_LAM_or_RAM())
  }
  else if(position=="LM" || position=="RM" || position=="RCM" || position=="LCM"|| position=="CM"){
    return (return_list_for_RM_or_LM_or_LCM_or_RCM_or_CM())
  }
  else if(position=="LW" || position=="RW"){
    return (return_list_for_RW_or_LW())
  }
  else if(position=="LF" || position=="RF" || position=="CF"){
    return (return_list_for_RF_or_CF_or_LF())
  }
  else if(position=="ST" || position=="RS" || position=="LS"){
    return (return_list_for_ST_or_RS_or_LS())
  }
}

calculate_overall <- function() {
  FullData <- as.data.frame(read.csv("./data/data_new.csv",header=TRUE,encoding = "UTF-8"))
  overall_vector<-vector()
  for(i in seq(1, nrow(FullData))){
    overall=0
    vector<-return_list_for_position(FullData[i,'Position'])
    for (j in 1:length(vector)) {
      overall=overall+(vector[[j]] * as.numeric(FullData[i,names(vector[j])]))
    }
    overall_vector[i]<-overall
  }
  FullData <- cbind(FullData, overall_vector) 
  colnames(FullData)[ncol(FullData)] <- "CalculatedOverall"
  write.csv(FullData, file="calc_overall.csv", row.names = FALSE)
  
}
calculate_overall()

assign_position_class <- function() {
  FullData <- as.data.frame(read.csv("./data/data_normalize.csv",header=TRUE,encoding = "UTF-8"))
  class_vector<-vector()
  for(i in seq(1, nrow(FullData))){
    if(FullData[i,'Position'] %in% c("CB","LCB","RCB","LB","RB","LWB","RWB")){
      class_vector<-c(class_vector,"D")
    }
    else if(FullData[i,'Position'] %in% c("CDM","LDM","RDM","CAM","RAM","LAM","RM","LM","RCM","LCM","CM")){
      class_vector<-c(class_vector,"M")
    }
    else if(FullData[i,'Position'] %in% c("RW","LW","LF","RF","CF","ST","RS","LS")){
      class_vector<-c(class_vector,"A")
    }
    else if(FullData[i,'Position'] %in% c("GK")){
      class_vector<-c(class_vector,"GK")
    }
  }
  FullData <- cbind(FullData, class_vector) 
  colnames(FullData)[ncol(FullData)] <- "SuperPosition"
  write.csv(FullData, file="superPosition.csv", row.names = FALSE)
  
}
assign_position_class()

