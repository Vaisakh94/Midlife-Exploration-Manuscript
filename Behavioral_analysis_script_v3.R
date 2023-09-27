#load in packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(networkD3)
library(CINNA)
library(umap)
library(plotly)
library(factoextra)
library(lsr)
library(car)
library(ggpubr)
library(entropy)
library(ds4psy)
library(pROC)
library(devtools)
library(RColorBrewer)
library(ggpubr)
library(readxl)
library(writexl)


#Import in preprocessed data 

midlife_p_master<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Midlife/Output_Iteration2/MLINDIV_participant_master.csv")
midlife_t_master<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Midlife/Output_Iteration2/MLINDIV_trial_master.csv")
young_p_master<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Young/Output_Iteration2/MLINDIV_participant_master.csv")
young_t_master<-read.csv("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Young/Output_Iteration2/MLINDIV_trial_master.csv")
shuying_IDs<-read_xlsx ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/HAS_SNAG_data (Shuying).xlsx", "tidy_format")

#Data Cleanup#

#First, for midlife, only include subject IDs that were marked as having "Useable" Maze Data
shuying_IDs<- filter(shuying_IDs, maze_useable == "Yes" & age_group == "Midlife") #filtering dataframe to show midlife subject IDs whose data can be analyzed 
shuying_IDs<- as.integer(shuying_IDs$subject_id) #grabbing subject IDs 
midlife_p_master<-filter(midlife_p_master, Subject %in% shuying_IDs)#keeping only subjects with useable maze data 
midlife_t_master<-filter(midlife_t_master, Subject %in% shuying_IDs)#keeping only subjects with useable maze data 

#We now remove participants that do not have all 24 trials, firstly in midlife, then in young 
midlife_row_index<- which(midlife_p_master$n_trials!=24) #finding which rows indicate subjects with less than 24 trials 
midlife_IDs_remove<- midlife_p_master[midlife_row_index,1] #finding which IDs correspond to these rows 
midlife_p_master<-subset(midlife_p_master, !(Subject %in% midlife_IDs_remove))#removing from participant master file 
midlife_t_master<-subset(midlife_t_master, !(Subject %in% midlife_IDs_remove))#removing from trial master file 

young_row_index<- which(young_p_master$n_trials!=24) #finding which rows indicate subjects with less than 24 trials 
young_IDs_remove<- young_p_master[young_row_index,1] #finding which IDs correspond to these rows 
young_p_master<-subset(young_p_master, !(Subject %in% young_IDs_remove))#removing from participant master file 
young_t_master<-subset(young_t_master, !(Subject %in% young_IDs_remove))#removing from trial master file 

#Cleaning up exploration trials
midlife_exploration <- midlife_t_master %>% filter(is.na(accuracy) == TRUE) %>% filter(Task_type == 1 | Task_type == 2) # grabbing midlife exploration data 
young_exploration <- young_t_master %>% filter(is.na(accuracy) == TRUE) %>% filter(Task_type == 1 | Task_type == 2) # grabbing young exploration data 

##define objects - this will be useful for us later 
#creating list of all possible locations, including orientations 
loc_list<-rep(LETTERS) #creating location list
#seq_list<-rep(seq(1:4),each=26)#creating orientation list 
#locations<-paste(loc_list,seq_list,sep = "")#combining both together 

#which we then divide into objects or hallways, including orientations.  This will help us later when we want to figure out how much time/# of visits each participant made to each location
objects_orientations<- c('A4', 'I2', 'K4', 'L2', 'N3', 'O2', 'P2', 'Y2', 'W4')
objects<-c('A', 'I', 'K', 'L', 'N', 'O', 'P', 'Y', 'W')
hallways <- c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'M', 'Q', 'R', 'S', 'T', 'U', "V", 'X', "Z")

#A=guitar
#I=snowman
#L=spaceship
#K=lamp post
#N=chicken
#O=trophy
#P=chair
#Y=umbrella
#W=cuckoo clock

#ttest output colnames for data later 
#colttest <-  c("estimate","statistic", "p.value", "parameter"," conf.low"," conf.high"," method","alternative")

#In exploration trajectories, replacing all F3 visits as an 'N' visit so that these are identified as target object visits in calculations conducted later below; 
#same with replacing all V2 visits as an 'Y' visit 

midlife_explore_data<- data.frame(matrix(nrow = nrow(midlife_exploration)))#creating empty dataframe to store contents of for loop into 

for (i in 1:nrow(midlife_exploration)){
  e_string_orient<-as.list(strsplit(midlife_exploration$e_paths[i],split=" ")) #splitting exploration path trajectory (with orientations) of subject into individual strings and storing as list
  e_string<-as.list(strsplit(midlife_exploration$paths[i],split=" ")) #splitting exploration path trajectories (w/o orientations) of subject into individual strings 
  F3_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "F3" %in% X))+1 #finding which element in string list is F3, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding a 1)
  e_string[[1]][F3_index]<-"N" #replacing with N
  V2_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "V2" %in% X))+1#finding which element in string list is V2, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding 1)  
  e_string[[1]][V2_index]<-"Y" #replacing with Y
  e_string<-paste(e_string[[1]], collapse = " ") #collapsing exploration path trajectory into single string list 
  midlife_explore_data[i,1]<-e_string #storing transposed version into dataframe
}

#Repeating process above for young 
young_explore_data<- data.frame(matrix(nrow = nrow(young_exploration)))#creating empty dataframe to store contents of for loop into 

for (i in 1:nrow(young_exploration)){
  e_string_orient<-as.list(strsplit(young_exploration$e_paths[i],split=" ")) #splitting exploration path trajectory (with orientations) of subject into individual strings and storing as list
  e_string<-as.list(strsplit(young_exploration$paths[i],split=" ")) #splitting exploration path trajectories (w/o orientations) of subject into individual strings 
  F3_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "F3" %in% X))+1 #finding which element in string list is F3, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding a 1)
  e_string[[1]][F3_index]<-"N" #replacing with N
  V2_index<-which(sapply(e_string_orient[[1]], FUN=function(X) "V2" %in% X))+1#finding which element in string list is V2, and we need to add a constant of 1 to 
  #match this up to where the same element would be in the "paths" variable. If element doesn't exist, R takes this into account and doesn't add a 1 (i.e., it keeps the
  #index value as 'non existent' even after adding 1)  
  e_string[[1]][V2_index]<-"Y" #replacing with Y
  e_string<-paste(e_string[[1]], collapse = " ") #collapsing exploration path trajectory into single string list 
  young_explore_data[i,1]<-e_string #storing transposed version into dataframe
}

midlife_explore_data$Subject<-midlife_exploration$Subject #adding subject ID to dataframe 
midlife_explore_data <- midlife_explore_data %>% rename(paths = matrix.nrow...nrow.midlife_exploration..) #renaming column containing exploration trajectories 
young_explore_data$Subject<-young_exploration$Subject #adding subject ID to dataframe 
young_explore_data <- young_explore_data %>% rename(paths = matrix.nrow...nrow.young_exploration..) #renaming column containing exploration trajectories 

#Analysis 
midlife_exploration_analysis<-as.data.frame(midlife_p_master$Subject) #creating master analysis data-frame to store all final exploration variable outputs into 
young_exploration_analysis<-as.data.frame(young_p_master$Subject) #same thing for young 

#Distance Traveled 
midlife_distances<- midlife_exploration %>% transmute(Subject, path_dist_trav)#grabbing distance vals for each subject 
midlife_distances_sum<-aggregate(midlife_distances$path_dist_trav, list(midlife_distances$Subject), FUN=sum) #calculating sum of distances across both exploration sessions
midlife_exploration_analysis$Dist<- midlife_distances_sum$x #adding vals to master analysis dataframe 
young_distances<- young_exploration %>% transmute(Subject, path_dist_trav)#same process for young  
young_distances_sum<-aggregate(young_distances$path_dist_trav, list(young_distances$Subject), FUN=sum) #calculating sum of distances across both exploration sessions
young_exploration_analysis$Dist<- young_distances_sum$x #adding vals to master analysis dataframe #same process for young 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$Dist)
shapiro.test(young_exploration_analysis$Dist)
#Wilcoxon Test 
dist_groupdiff<-wilcox.test(young_exploration_analysis$Dist,midlife_exploration_analysis$Dist)
dist_groupdiff_effsize<-cohensD(young_exploration_analysis$Dist,midlife_exploration_analysis$Dist)

#Exploration Speed 
midlife_speed_calc<-midlife_p_master$em_path_dist_trav/480 #in pixels per second 
midlife_exploration_analysis$Speed<-midlife_speed_calc #adding vals to master analysis dataframe 
young_speed_calc<-young_p_master$em_path_dist_trav/480 #in pixels per second 
young_exploration_analysis$Speed<-young_speed_calc #adding vals to master analysis dataframe 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$Speed)
shapiro.test(young_exploration_analysis$Speed)
#Wilcoxon Test 
speed_groupdiff<-wilcox.test(young_exploration_analysis$Speed,midlife_exploration_analysis$Speed)
speed_groupdiff_effsize<-cohensD(young_exploration_analysis$Speed,midlife_exploration_analysis$Speed)

#Number of Location Visits 
midlife_big_explore<-as.data.frame(str_split_fixed(midlife_explore_data$paths," ",max(midlife_exploration$nodesturns_count)))# splits the path strings into separate letters  
midlife_big_explore <- midlife_big_explore %>% mutate(subject = midlife_explore_data$Subject) #adding subject ID as final column 
midlife_big_explore <-midlife_big_explore[,c(ncol(midlife_big_explore), 1:(ncol(midlife_big_explore)-1))] # moving subject ID to first column 

#We now need to remove consecutive repeats of locations (eg. AA or GG), which indicates turning/rotating about a location. Here, only one instance would reflect a 'true' visit 
#to that location, whereas the remaining consecutive repeats represent movements around that location (eg. A represents visit, second A represents turning away from A)
row_labels<-c(1:nrow(midlife_big_explore))#creating row labels, which will be handy for splitting up the dataframe into chunks below 
midlife_big_explore<- split( midlife_big_explore , f = row_labels)#splitting dataframe up into chunks, with each row being a chunk

#for loop to find consecutive location repeats, and remove them 
for (i in 1:length(midlife_big_explore)){
  midlife_big_explore[[i]]<-t(midlife_big_explore[[i]])#transposing chunks
  midlife_big_explore[[i]][midlife_big_explore[[i]] == ""] <- NA #replacing empty cells with NA 
  repeat_logical<-t(c(NA, midlife_big_explore[[i]]) == c(midlife_big_explore[[i]], NA))[1:length(midlife_big_explore[[i]])]
  repeats<- grep("TRUE",repeat_logical) #find positions of items in list that are TRUE, which correspond to row numbers of chunk  
  midlife_big_explore[[i]][repeats,1]<-"repeats"  #relabel these to "repeats"
  midlife_big_explore[[i]]<-t(midlife_big_explore[[i]])#transposing to original form 
  midlife_big_explore[[i]]<-as.data.frame(midlife_big_explore[[i]]) #re-converting to dataframe 
}

midlife_big_explore<-unsplit(midlife_big_explore,row_labels)#merging all chunks back into single dataframe 
midlife_big_explore[is.na(midlife_big_explore)]<-"none" #replacing all NAs with string, otherwise for loop below won't work 

#for loop to determine location counts 
midlife_big_explore_counts <- data.frame(matrix(ncol = 0, nrow = nrow(midlife_big_explore)))#creating dataframe to store counts into  

for (i in loc_list){
  midlife_big_explore_counts$subject <- midlife_big_explore$subject
  d <- as.data.frame(apply(midlife_big_explore,1,function(x) sum(x==i))) # not sure what this function is 
  names(d) <- i
  midlife_big_explore_counts <- cbind(d, midlife_big_explore_counts)
}

#Object Visits
midlife_object_visits <- midlife_big_explore_counts %>% transmute(subject, A, I, L, K, N, O, P, Y, W)#counts for each specific object locations 
cols_sum<-c(2:length(midlife_object_visits))#finding columns to include in summing function below 
midlife_object_visits$total<-rowSums(midlife_object_visits[,cols_sum])#finding total object counts for each subject for each session 
midlife_object_visits_sum<-aggregate(midlife_object_visits$total, list(midlife_object_visits$subject), FUN=sum) #calculating total objects visits across both exploration sessions
midlife_exploration_analysis$TotalObjVisits<-midlife_object_visits_sum$x #storing into master analysis dataframe 

#Hallway Visits 
midlife_hallway_visits <- midlife_big_explore_counts %>% transmute(subject, B, C, D, E, `F`, G, H, J, M, Q, R, S, `T`, U, V, X, Z)#counts for each specific hallway locations 
cols_sum<-c(2:length(midlife_hallway_visits))#finding columns to include in summing function below 
midlife_hallway_visits$total<-rowSums(midlife_hallway_visits[,cols_sum])#finding total hallway counts for each subject for each session 
midlife_hallway_visits_sum<-aggregate(midlife_hallway_visits$total, list(midlife_hallway_visits$subject), FUN=sum) #calculating total hallway visits across both exploration sessions
midlife_exploration_analysis$TotalHallwayVisits<-midlife_hallway_visits_sum$x #storing into master analysis dataframe 

#repeating same process above for young participants 
young_explore_data <- young_exploration %>% transmute(Subject, paths)#condensing exploration dataframe, and then storing as new variable
young_big_explore<-as.data.frame(str_split_fixed(young_explore_data$paths," ",max(young_exploration$nodesturns_count)))# splits the path strings into separate letters  
#young_big_explore$V100<-as.data.frame(str_split_fixed(young_big_explore$V100," ",nrow(young_big_explore)))#ensuring string sequence chunk in last column of df above is split 
young_big_explore <- young_big_explore %>% mutate(subject = young_explore_data$Subject) #adding subject ID as final column 
young_big_explore <-young_big_explore[,c(ncol(young_big_explore), 1:(ncol(young_big_explore)-1))] # moving subject ID to first column 

#We now need to remove consecutive repeats of locations (eg. AA or GG), which indicates turning/rotating about a location. Here, only one instance would reflect a 'true' visit 
#to that location, whereas the remaining consecutive repeats represent movements around that location (eg. A represents visit, second A represents turning away from A)
row_labels<-c(1:nrow(young_big_explore))#creating row labels, which will be handy for splitting up the dataframe into chunks below 
young_big_explore<- split(young_big_explore , f = row_labels)#splitting dataframe up into chunks, with each row being a chunk

#for loop to find consecutive location repeats, and remove them 
for (i in 1:length(young_big_explore)){
  young_big_explore[[i]]<-t(young_big_explore[[i]])#transposing chunks
  young_big_explore[[i]][young_big_explore[[i]] == ""] <- NA #replacing empty cells with NA 
  repeat_logical<-t(c(NA, young_big_explore[[i]]) == c(young_big_explore[[i]], NA))[1:length(young_big_explore[[i]])]#logical indicating whether each character is a repeat of what came before it 
  repeats<- grep("TRUE",repeat_logical) #find positions of items in list that are TRUE, which correspond to row numbers of chunk  
  young_big_explore[[i]][repeats,1]<-"repeats"  #relabel these to "repeats"
  young_big_explore[[i]]<-t(young_big_explore[[i]])#transposing to original form 
  young_big_explore[[i]]<-as.data.frame(young_big_explore[[i]]) #re-converting to dataframe 
}

young_big_explore<-unsplit(young_big_explore,row_labels)#merging all chunks back into single dataframe 
young_big_explore[is.na(young_big_explore)]<-"none" #replacing all NAs with string, otherwise for loop below won't work 

#for loop to determine location counts 
young_big_explore_counts <- data.frame(matrix(ncol = 0, nrow = nrow(young_big_explore)))#creating dataframe to store counts into  
for (i in loc_list){
  young_big_explore_counts$subject <- young_big_explore$subject
  d <- as.data.frame(apply(young_big_explore,1,function(x) sum(x==i))) # not sure what this function is 
  names(d) <- i
  young_big_explore_counts <- cbind(d, young_big_explore_counts)
}

#Object Visits
young_object_visits <- young_big_explore_counts %>% transmute(subject, A, I, L, K, N, O, P, Y, W)#counts for each specific object locations 
cols_sum<-c(2:length(young_object_visits))#finding columns to include in summing function below 
young_object_visits$total<-rowSums(young_object_visits[,cols_sum])#finding total object counts for each subject for each session 
young_object_visits_sum<-aggregate(young_object_visits$total, list(young_object_visits$subject), FUN=sum) #calculating total objects visits across both exploration sessions
young_exploration_analysis$TotalObjVisits<-young_object_visits_sum$x #storing into master analysis dataframe 

#Hallway Visits 
young_hallway_visits <- young_big_explore_counts %>% transmute(subject, B, C, D, E, `F`, G, H, J, M, Q, R, S, `T`, U, V, X, Z)#counts for each specific hallway locations 
cols_sum<-c(2:length(young_hallway_visits))#finding columns to include in summing function below 
young_hallway_visits$total<-rowSums(young_hallway_visits[,cols_sum])#finding total hallway counts for each subject for each session 
young_hallway_visits_sum<-aggregate(young_hallway_visits$total, list(young_hallway_visits$subject), FUN=sum) #calculating total hallway visits across both exploration sessions
young_exploration_analysis$TotalHallwayVisits<-young_hallway_visits_sum$x #storing into master analysis dataframe 

#Object Visits- Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$TotalObjVisits)
shapiro.test(young_exploration_analysis$TotalObjVisits)
#Object Visits - T-Test 
objvisits_groupdiff<-t.test(young_exploration_analysis$TotalObjVisits,midlife_exploration_analysis$TotalObjVisits)
objvisits_groupdiff_effsize<-cohensD(young_exploration_analysis$TotalObjVisits,midlife_exploration_analysis$TotalObjVisits)
#Hallway Visits - Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$TotalHallwayVisits)
shapiro.test(young_exploration_analysis$TotalHallwayVisits)
#Hallway Visits - Wilcoxon Rank Sum Test
hallwayvisits_groupdiff<-wilcox.test(young_exploration_analysis$TotalHallwayVisits,midlife_exploration_analysis$TotalHallwayVisits)
hallwayvisits_groupdiff_effsize<-cohensD(young_exploration_analysis$TotalHallwayVisits,midlife_exploration_analysis$TotalHallwayVisits)

#Turns Made 
midlife_turns<- midlife_exploration %>% transmute(Subject, turns_count)#grabbing turns vals  
midlife_turns_sum<-aggregate(midlife_turns$turns_count, list(midlife_turns$Subject), FUN=sum) #calculating sum 
midlife_exploration_analysis$Turns<-midlife_turns_sum$x #adding to master analysis df 
young_turns<-young_exploration %>% transmute(Subject, turns_count)#repeating same process for young   
young_turns_sum<-aggregate(young_turns$turns_count, list(young_turns$Subject), FUN=sum)  
young_exploration_analysis$Turns<-young_turns_sum$x #adding to master analysis df  
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$Turns)
shapiro.test(young_exploration_analysis$Turns)
#TTest 
turns_groupdiff<-t.test(young_exploration_analysis$Turns,midlife_exploration_analysis$Turns)
turns_groupdiff_effsize<-cohensD(young_exploration_analysis$Turns,midlife_exploration_analysis$Turns)

#Pause Duration 
midlife_b_master<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Midlife/Output_Iteration2/MLINDIV_behavioral_master.csv") #importing behavioral master file 
midlife_b_master<-filter(midlife_b_master, Subject %in% shuying_IDs)#keeping only subjects with useable maze data 
midlife_b_master<-subset(midlife_b_master, !(Subject %in% midlife_IDs_remove))#removing subjects with less than 24 trials  
midlife_b_master<- midlife_b_master %>% filter(Procedure == "ExploreProc")# grabbing midlife exploration data 
midlife_pauses<-midlife_b_master %>% transmute (Subject, Choose.RT) #condensing df to just pause variable 
midlife_pauses_sum<-aggregate(midlife_pauses$Choose.RT, list(midlife_pauses$Subject), FUN=sum) #calculating sum 
midlife_exploration_analysis$Pauses<-midlife_pauses_sum$x #adding to main analysis df 

young_b_master<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Young/Output_Iteration2/MLINDIV_behavioral_master.csv") #importing behavioral master file 
young_b_master<-subset(young_b_master, !(Subject %in% young_IDs_remove))#removing subjects with less than 24 trials  
young_b_master<- young_b_master %>% filter(Procedure == "ExploreProc")# grabbing exploration data 
young_pauses<-young_b_master %>% transmute (Subject, Choose.RT) #condensing df to just pause variable 
young_pauses_sum<-aggregate(young_pauses$Choose.RT, list(young_pauses$Subject), FUN=sum) #calculating sum 
young_exploration_analysis$Pauses<-young_pauses_sum$x #adding to main analysis df 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$Pauses)
shapiro.test(young_exploration_analysis$Pauses)
#Wilcoxon Test 
pauses_groupdiff<-wilcox.test(young_exploration_analysis$Pauses,midlife_exploration_analysis$Pauses)
pauses_groupdiff_effsize<-cohensD(young_exploration_analysis$Pauses,midlife_exploration_analysis$Pauses)

#Evenness of Exploration (Target Objects)
midlife_spread_obj_visits <- midlife_object_visits %>% gather("location", "count", 2:10) %>% pivot_wider(id_cols = subject, names_from = location, values_from = count, values_fn = sum) #adding object visits across both exploration sessions, for each subject
midlife_spread_obj_visits <- midlife_spread_obj_visits %>% mutate(std = apply(midlife_spread_obj_visits[2:10], 1, sd)) #finding standard deviations 
midlife_exploration_analysis$ExplorEvenObj<-midlife_spread_obj_visits$std #adding to master analysis dataframe 
#repeating process above for young 
young_spread_obj_visits <- young_object_visits %>% gather("location", "count", 2:10) %>% pivot_wider(id_cols = subject, names_from = location, values_from = count, values_fn = sum) #adding object visits across both exploration sessions, for each subject
young_spread_obj_visits <- young_spread_obj_visits %>% mutate(std = apply(young_spread_obj_visits[2:10], 1, sd)) #finding standard deviations 
young_exploration_analysis$ExplorEvenObj<-young_spread_obj_visits$std #adding to master analysis dataframe 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$ExplorEvenObj)
shapiro.test(young_exploration_analysis$ExplorEvenObj)
#Wilcoxon Test 
obj_evenness_groupdiff<-wilcox.test(young_exploration_analysis$ExplorEvenObj,midlife_exploration_analysis$ExplorEvenObj)
obj_eveness_groupdiff_effsize<-cohensD(young_exploration_analysis$ExplorEvenObj,midlife_exploration_analysis$ExplorEvenObj)

#Eveness of Exploration (Hallways)
midlife_spread_hallway_visits <- midlife_hallway_visits %>% gather("location", "count", 2:18) %>% pivot_wider(id_cols = subject, names_from = location, values_from = count, values_fn = sum) #adding object visits across both exploration sessions, for each subject
midlife_spread_hallway_visits <- midlife_spread_hallway_visits %>% mutate(std = apply(midlife_spread_hallway_visits[2:18], 1, sd)) #finding standard deviations 
midlife_exploration_analysis$ExplorEvenHallway<-midlife_spread_hallway_visits$std #adding to master analysis dataframe 
#repeating process above for young 
young_spread_hallway_visits <- young_hallway_visits %>% gather("location", "count", 2:18) %>% pivot_wider(id_cols = subject, names_from = location, values_from = count, values_fn = sum) #adding object visits across both exploration sessions, for each subject
young_spread_hallway_visits <- young_spread_hallway_visits %>% mutate(std = apply(young_spread_hallway_visits[2:18], 1, sd)) #finding standard deviations 
young_exploration_analysis$ExplorEvenHallway<-young_spread_hallway_visits$std #adding to master analysis dataframe 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$ExplorEvenHallway)
shapiro.test(young_exploration_analysis$ExplorEvenHallway)
#Wilcoxon Test 
hallway_evenness_groupdiff<-wilcox.test(young_exploration_analysis$ExplorEvenHallway,midlife_exploration_analysis$ExplorEvenHallway)
hallway_eveness_groupdiff_effsize<-cohensD(young_exploration_analysis$ExplorEvenHallway,midlife_exploration_analysis$ExplorEvenHallway)

#Button Press Moves 
midlife_button_press<- midlife_t_master %>% filter(Procedure == "ExploreProc") #filtering just explore rows from df 
midlife_button_press<-midlife_button_press %>% transmute(Subject, nodesturns_count) #condensing df to just vars we care about 
midlife_button_press_sum<-aggregate(midlife_button_press$nodesturns_count, list(midlife_button_press$Subject), FUN=sum) #calculating sum
midlife_exploration_analysis$ButtonPress<-midlife_button_press_sum$x #adding to master analysis dataframe 

young_button_press<- young_t_master %>% filter(Procedure == "ExploreProc") #filtering just explore rows from df 
young_button_press<-young_button_press %>% transmute(Subject, nodesturns_count) #condensing df to just vars we care about 
young_button_press_sum<-aggregate(young_button_press$nodesturns_count, list(young_button_press$Subject), FUN=sum) #calculating sum 
young_exploration_analysis$ButtonPress<-young_button_press_sum$x #adding to master analysis dataframe 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$ButtonPress)
shapiro.test(young_exploration_analysis$ButtonPress)
#Wilcoxon Test 
buttonpress_groupdiff<-wilcox.test(young_exploration_analysis$ButtonPress,midlife_exploration_analysis$ButtonPress)
buttonpress_groupdiff_effsize<-cohensD(young_exploration_analysis$ButtonPress,midlife_exploration_analysis$ButtonPress)

#Path Roaming Entropy 
midlife_exp_paths<-as.data.frame(midlife_big_explore$subject) #building empty dataframe which we will populate with output from for loop below (part 1)
midlife_exp_paths$paths<-rep("TBC",nrow(midlife_exp_paths)) #adding TBCs for now, this will be replaced with output from for loop below 

#For loop to condense path trajectories from midlife_big_explore into single sequence, and remove "repeats" and "none"
for (i in 1:nrow(midlife_big_explore)){
  cols<-2:length(midlife_big_explore) #indexing columns to condense into single sequence 
  midlife_exp_paths[i,2]<-paste(midlife_big_explore[i,cols], collapse = '') #combining all characters into single sequence 
  midlife_exp_paths[i,2]<-gsub('[repeatsnone]', '', midlife_exp_paths[i,2])#removing strings "repeats" and "none" from sequence
}

#Combining each subject's exploration trajectories across both sessions into one exploration trajectory 
midlife_exp_paths<- midlife_exp_paths %>% group_by(`midlife_big_explore$subject`) %>% mutate(newcol=paste(paths, collapse = '')) #combining trajectories for each subject
midlife_exp_paths<- midlife_exp_paths %>% filter (row_number() %% 2 !=0) #deletes even rows, which are duplicates with regards to combined trajectories 
names(midlife_exp_paths)[names(midlife_exp_paths) == 'newcol'] <- 'CombinedPaths' #renaming column 
midlife_exp_paths<-as.data.frame(midlife_exp_paths) #converting to df so that for loop below works 

#For loop to calculate path roaming entropies 
for (i in 1:nrow(midlife_exp_paths)){
  y<-strsplit(midlife_exp_paths[i,3],split = "")
  y_counts_table<-table(y)
  y_counts_vals<-as.numeric(y_counts_table)
  y_entropy<-entropy(y_counts_vals,method = c("ML")) #calculate Shannon's Entropy 
  midlife_exp_paths$entropy[i]<-y_entropy
} 

midlife_exploration_analysis$pathentropy<-midlife_exp_paths$entropy #adding to main analysis dataframe 

#repeating same process above for young group 
young_exp_paths<-as.data.frame(young_big_explore$subject) #building empty dataframe which we will populate with output from for loop below (part 1)
young_exp_paths$paths<-rep("TBC",nrow(young_exp_paths)) #adding TBCs for now, this will be replaced with output from for loop below 

#For loop to condense path trajectories from young_big_explore into single sequence, and remove "repeats" and "none"
for (i in 1:nrow(young_big_explore)){
  cols<-2:length(young_big_explore) #indexing columns to condense into single sequence 
  young_exp_paths[i,2]<-paste(young_big_explore[i,cols], collapse = '') #combining all characters into single sequence 
  young_exp_paths[i,2]<-gsub('[repeatsnone]', '', young_exp_paths[i,2])#removing strings "repeats" and "none" from sequence
}

#Combining each subject's exploration trajectories across both sessions into one exploration trajectory 
young_exp_paths<- young_exp_paths %>% group_by(`young_big_explore$subject`) %>% mutate(newcol=paste(paths, collapse = '')) #combining trajectories for each subject
young_exp_paths<- young_exp_paths %>% filter (row_number() %% 2 !=0) #deletes even rows, which are duplicates with regards to combined trajectories 
names(young_exp_paths)[names(young_exp_paths) == 'newcol'] <- 'CombinedPaths' #renaming column 
young_exp_paths<-as.data.frame(young_exp_paths) #converting to df so that for loop below works 

#For loop to calculate path roaming entropies 
for (i in 1:nrow(young_exp_paths)){
  y<-strsplit(young_exp_paths[i,3],split = "")
  y_counts_table<-table(y)
  y_counts_vals<-as.numeric(y_counts_table)
  y_entropy<-entropy(y_counts_vals,method = c("ML")) #calculate Shannon's Entropy 
  young_exp_paths$entropy[i]<-y_entropy
} 

young_exploration_analysis$pathentropy<-young_exp_paths$entropy #adding to main analysis dataframe 

#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$pathentropy)
shapiro.test(young_exploration_analysis$pathentropy)
#Wilcoxon Test 
entropy_groupdiff<-wilcox.test(young_exploration_analysis$pathentropy,midlife_exploration_analysis$pathentropy)
entropy_groupdiff_effsize<-cohensD(young_exploration_analysis$pathentropy,midlife_exploration_analysis$pathentropy)

#Explorative Analysis Midlife- Re-calculating path roaming entropies based on original exploration trajectories without consecutive repeats removed 
#midlife_exp_paths_v2<-as.data.frame(midlife_big_explore$subject) #building empty dataframe which we will populate with output from for loop below (part 1)
#midlife_exp_paths_v2$paths<-rep("TBC",nrow(midlife_exp_paths)) #adding TBCs for now, this will be replaced with output from for loop below 

#For loop to calculate path roaming entropies 
#for (i in 1:nrow(midlife_exp_paths_v2)){
  #y<-strsplit(midlife_explore_data[i,1],split = " ")
  #y_counts_table<-table(y)
  #y_counts_vals<-as.numeric(y_counts_table)
  #y_entropy<-entropy(y_counts_vals,method = c("ML")) #calculate Shannon's Entropy 
  #midlife_exp_paths_v2$entropy[i]<-y_entropy
#} 

#midlife_entropy_mean_v2<-aggregate(midlife_exp_paths_v2$entropy, list(midlife_exp_paths_v2$`midlife_big_explore$subject`), FUN=mean) #calculating mean entropy for each subject 
#midlife_entropy_v2<-as.data.frame(midlife_entropy_mean_v2$x) #creating df for assessing group differences

#Explorative Analysis Young - Re-calculating path roaming entropies based on original exploration trajectories without consecutive repeats removed 
#young_exp_paths_v2<-as.data.frame(young_big_explore$subject) #building empty dataframe which we will populate with output from for loop below (part 1)
#young_exp_paths_v2$paths<-rep("TBC",nrow(young_exp_paths)) #adding TBCs for now, this will be replaced with output from for loop below 

#For loop to calculate path roaming entropies 
#for (i in 1:nrow(young_exp_paths_v2)){
  #y<-strsplit(young_explore_data[i,2],split = " ")
  #y_counts_table<-table(y)
  #y_counts_vals<-as.numeric(y_counts_table)
  #y_entropy<-entropy(y_counts_vals,method = c("ML")) #calculate Shannon's Entropy 
  #young_exp_paths_v2$entropy[i]<-y_entropy
#} 

#young_entropy_mean_v2<-aggregate(young_exp_paths_v2$entropy, list(young_exp_paths_v2$`young_big_explore$subject`), FUN=mean) #calculating mean entropy for each subject 
#young_entropy_v2<-as.data.frame(young_entropy_mean_v2$x) #creating df for assessing group differences

#Explorative Analysis Group Differences 
#Normality test to assess whether samples are normally distributed 
#shapiro.test(midlife_entropy_v2$`midlife_entropy_mean_v2$x`)
#shapiro.test(young_entropy_v2$`young_entropy_mean_v2$x`)
#Wilcoxon Test 
#entropy2_groupdiff<-wilcox.test(young_entropy_v2$`young_entropy_mean_v2$x`,midlife_entropy_v2$`midlife_entropy_mean_v2$x`)
#entropy2_groupdiff_effsize<-cohensD(young_entropy_v2$`young_entropy_mean_v2$x`,midlife_entropy_v2$`midlife_entropy_mean_v2$x`)

#Longest Hallway Sequence 
#For loop to identify longest hallway sequence 
for (i in 1:nrow(midlife_exp_paths)){
  z<-strsplit(midlife_exp_paths[i,3],split = "")
  logical<-as.vector(sapply(z, `%in%`, objects)) #finding if each visited location is an object or not in binary form (true=object, false=hallway)
  midlife_exp_paths$hallwayseq[i]<-with(rle(logical),max(lengths))#finding longest sequence of hallway visits that is not interrupted by an object visit; note that max seq is bound to be hallway as you can't have long sequences of obj visits ('Trues')  
}

midlife_exploration_analysis$hallwayseq<-midlife_exp_paths$hallwayseq #adding to main analysis dataframe 

#Repeating same process above for young group  
for (i in 1:nrow(young_exp_paths)){
  z<-strsplit(young_exp_paths[i,3],split = "")
  logical<-as.vector(sapply(z, `%in%`, objects)) #finding if each visited location is an object or not in binary form (true=object, false=hallway)
  young_exp_paths$hallwayseq[i]<-with(rle(logical),max(lengths))#finding longest sequence of hallway visits that is not interrupted by an object visit; note that max seq is bound to be hallway as you can't have long sequences of obj visits ('Trues')  
}

young_exploration_analysis$hallwayseq<-young_exp_paths$hallwayseq #adding to main analysis dataframe 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$hallwayseq)
shapiro.test(young_exploration_analysis$hallwayseq)
#Wilcoxon Test 
hallwayseq_groupdiff<-wilcox.test(young_exploration_analysis$hallwayseq,midlife_exploration_analysis$hallwayseq)
hallwayseq_groupdiff_effsize<-cohensD(young_exploration_analysis$hallwayseq,midlife_exploration_analysis$hallwayseq)


#Proportion Correct (Spatial Memory)
midlife_exploration_analysis$PropCorr<-midlife_p_master$tm_accuracy #adding to master analysis df
young_exploration_analysis$PropCorr<-young_p_master$tm_accuracy #adding to master analysis df
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$PropCorr)
shapiro.test(young_exploration_analysis$PropCorr)
#Wilcoxon Test 
propcorr_groupdiff<-wilcox.test(young_exploration_analysis$PropCorr,midlife_exploration_analysis$PropCorr)
propcorr_groupdiff_effsize<-cohensD(young_exploration_analysis$PropCorr,midlife_exploration_analysis$PropCorr)
#Checking if participant performances are above chance level 
midlife_chance_dummy<-c(rep(0.111,87)) #creating dummy dataset showing chance performance 
young_chance_dummy<-c(rep(0.111,50)) #same for young 
midlife_chance_test<-wilcox.test(midlife_exploration_analysis$PropCorr,midlife_chance_dummy,"greater") #one-sided t-test for midlife 
young_chance_test<-wilcox.test(young_exploration_analysis$PropCorr, young_chance_dummy,"greater")#same for young 

#Demographics Analysis - Initial Sample 
Demographics_midlife_initial_df<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Midlife/Output_Iteration2/MLINDIV_participant_master.csv")
Demographics_midlife_initial_ids<-Demographics_midlife_initial_df$Subject #grabbing IDs
Demographics_df<-read_xlsx ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/HAS_SNAG_data (Shuying).xlsx", "tidy_format")#reading in demographics file 
midlife_demographics_initial<-filter(Demographics_df, subject_id %in% Demographics_midlife_initial_ids) #creating initial demographics table for midlife 
Demographics_young_initial_df<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Young/Output_Iteration2/MLINDIV_participant_master.csv")
Demographics_young_initial_ids<-Demographics_young_initial_df$Subject #grabbing IDs
young_demographics_initial<-filter(Demographics_df, subject_id %in% Demographics_young_initial_ids) #creating initial demographics table for young 

#Demographics Analysis - Final Sample 
Demographics_df<-read_xlsx ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/HAS_SNAG_data (Shuying).xlsx", "tidy_format")#reading in demographics file 
Demographics_df<- filter(Demographics_df, maze_useable == "Yes") #filtering dataframe to show just IDs with useable maze data 
midlife_demographics<-filter(Demographics_df, age_group == "Midlife") #creating demographics table for midlife 
young_demographics<-filter(Demographics_df, age_group == "Young")#same for young 

#Group Differences in Age - Final Sample 
shapiro.test(midlife_demographics$age_spatial_years)
shapiro.test(young_demographics$age_spatial_years)
age_diff<-wilcox.test(midlife_demographics$age_spatial_years,young_demographics$age_spatial_years)
age_diff_effsize<-cohensD(midlife_demographics$age_spatial_years,young_demographics$age_spatial_years)

#Group Differences in Sex - Final Sample 
table(Demographics_df$age_group, Demographics_df$sex)
chisq.test(Demographics_df$age_group, Demographics_df$sex)

#Sex Differences in Exploration Variables 
subject_sexes<- Demographics_df %>% transmute(subject_id, sex)#condensing dataframe, and then storing as new variable
subject_sexes <- subject_sexes %>% rename(Subject = subject_id) #renaming first column so that step below can be performed  

#Exporting Demographics Table & Data for Upload in OSF 
midlife_demographics_export<-midlife_demographics %>% transmute(subject_id, sex, age_group, age_spatial_years) #condensing dataframe to just variables we care about
young_demographics_export<-young_demographics %>% transmute(subject_id, sex, age_group, age_spatial_years) #same for young
demographics_export<-bind_rows(midlife_demographics_export,young_demographics_export) #combining df's 
write_xlsx(demographics_export,"/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Manuscript/For_Submission/Neurobiology of Aging/OSF_Data/Puthusseryppady_et_al_Demographics")#exporting as excel file to file path
write.csv(,"/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Manuscript/For_Submission/Neurobiology of Aging/OSF_Data/Puthusseryppady_et_al_Demographics")#exporting as excel file to file path

#ANCOVA Analysis 
midlife_identifiers<-rep("Midlife",87) #creating string identifiers for midlife 
midlife_exploration_analysis$group<-as.factor(midlife_identifiers)#adding to analysis df, and converting to factor
young_identifiers<-rep("Young",50) #doing same process above for young 
young_exploration_analysis$group<-as.factor(young_identifiers)#adding to analysis df
colnames(midlife_exploration_analysis)[1]<- "Subject" #making sure names of first column matches in both midlife & young analysis dataframes 
colnames(young_exploration_analysis)[1]<- "Subject" #making sure names of first column matches in both midlife & young analysis dataframes 
ancova_df<-rbind(young_exploration_analysis,midlife_exploration_analysis)#creating combined df for ancova analysis 
ancova_df<-merge(ancova_df, subject_sexes, by = "Subject") #adding in subject sex information
ancova_df$sex<-as.factor(ancova_df$sex) #converting sex to factor 

library(moments)
#Distance Travelled 
skewness(midlife_exploration_analysis$Dist)#checking skewness 
skewness(young_exploration_analysis$Dist)#checking skewness 
ancova_df$Dist<-sqrt(max(ancova_df$Dist+1)-ancova_df$Dist) #sqrt transformation for negative skew 
dist_ancova <- aov(Dist ~ group + sex,data = ancova_df)
Anova(dist_ancova, type = "III")

#Pause Duration 
skewness(midlife_exploration_analysis$Pauses)#checking skewness 
skewness(young_exploration_analysis$Pauses)#checking skewness 
ancova_df$Pauses<-(1/(ancova_df$Pauses))*10 #inverse transformation
pauses_ancova <- aov(Pauses ~ group + sex,data = ancova_df)
Anova(pauses_ancova, type = "III")

#Button Presses 
skewness(midlife_exploration_analysis$ButtonPress)#checking skewness 
skewness(young_exploration_analysis$ButtonPress)#checking skewness 
ancova_df$ButtonPress<-sqrt(max(ancova_df$ButtonPress+1)-ancova_df$ButtonPress) #sqrt transformation for negative skew 
buttonpress_ancova <- aov(ButtonPress ~ group + sex,data = ancova_df)
Anova(buttonpress_ancova, type = "III")

#Turns Made
turns_ancova <- aov(Turns ~ group + sex,data = ancova_df)
Anova(turns_ancova, type = "III")

#Target Object Visits 
objvisits_ancova <- aov(TotalObjVisits ~ group + sex,data = ancova_df)
Anova(objvisits_ancova, type = "III")

#Hallway Visits 
skewness(midlife_exploration_analysis$TotalHallwayVisits)#checking skewness 
skewness(young_exploration_analysis$TotalHallwayVisits)#checking skewness 
ancova_df$TotalHallwayVisits<-sqrt(max(ancova_df$TotalHallwayVisits+1)-ancova_df$TotalHallwayVisits) #sqrt transformation for negative skew 
hallwayvisits_ancova <- aov(TotalHallwayVisits ~ group + sex,data = ancova_df)
Anova(hallwayvisits_ancova, type = "III")

#Evenness of Exploration of Objects
skewness(midlife_exploration_analysis$ExplorEvenObj)#checking skewness 
skewness(young_exploration_analysis$ExplorEvenObj)#checking skewness 
ancova_df$ExplorEvenObj<- log10(ancova_df$ExplorEvenObj) #log10 transformation  
explorevenobj_ancova <- aov(ExplorEvenObj ~ group + sex,data = ancova_df)
Anova(explorevenobj_ancova, type = "III")

#Evenness of Exploration of Hallways 
skewness(midlife_exploration_analysis$ExplorEvenHallway)#checking skewness 
skewness(young_exploration_analysis$ExplorEvenHallway)#checking skewness 
ancova_df$ExplorEvenHallway<- log10(ancova_df$ExplorEvenHallway) #log10 transformation  
explorevenhallway_ancova <- aov(ExplorEvenHallway ~ group + sex,data = ancova_df)
Anova(explorevenhallway_ancova, type = "III")

#Longest Hallway Sequence 
skewness(midlife_exploration_analysis$hallwayseq)#checking skewness 
skewness(young_exploration_analysis$hallwayseq)#checking skewness 
ancova_df$hallwayseq<-1/(ancova_df$hallwayseq) #inverse transformation
hallwayseq_ancova <- aov(hallwayseq ~ group + sex,data = ancova_df)
Anova(hallwayseq_ancova, type = "III")

#Path Roaming Entropy
skewness(midlife_exploration_analysis$pathentropy)#checking skewness 
skewness(young_exploration_analysis$pathentropy)#checking skewness 
ancova_df$pathentropy<-1/(max(ancova_df$pathentropy+1) - ancova_df$pathentropy) #inverse transformation for negative skew 
pathentropy_ancova <- aov(pathentropy ~ group + sex,data = ancova_df)
Anova(pathentropy_ancova, type = "III")

#Wayfinding Success 
skewness(midlife_exploration_analysis$PropCorr)#checking skewness 
skewness(young_exploration_analysis$PropCorr)#checking skewness 
ancova_df$PropCorr<-1/(0.01 + ancova_df$PropCorr) #inverse transformation; adding small constant to prevent taking inverse of zero values
propcorr_ancova <- aov(PropCorr ~ group + sex,data = ancova_df)
Anova(propcorr_ancova, type = "III")

#Mediation Analysis 
library(mediation) #loading in required package 

Mediation_Df<-rbind(young_exploration_analysis,midlife_exploration_analysis)#creating combined df for mediation analysis 

#Step 1- Finding Total Effect (relationship between independent variable & dependent variable)
fit.totaleffect<-lm(PropCorr ~ group, Mediation_Df)
summary(fit.totaleffect)

#Pause Duration As Mediator Analysis (Wayfinding Success)
pause_fit.mediator<-lm(Pauses ~ group, Mediation_Df)
summary(pause_fit.mediator)
pause_fit.dv<-lm(PropCorr ~ Pauses + group, Mediation_Df)
summary(pause_fit.dv)
pause_mediator_wayfinding<-mediate(pause_fit.mediator, pause_fit.dv, treat = "group", mediator= "Pauses", boot = T)
summary(pause_mediator_wayfinding)


#Object Visits As Mediator Analysis 
obj_fit.mediator<-lm(TotalObjVisits ~ group, Mediation_Df)
summary(obj_fit.mediator)
obj_fit.dv<-lm(PropCorr ~ TotalObjVisits + group, Mediation_Df)
summary(obj_fit.dv)
obj_mediator_wayfinding<-mediate(obj_fit.mediator, obj_fit.dv, treat = "group", mediator= "TotalObjVisits", boot = T)
summary(obj_mediator_wayfinding)


#Hallway Visits As Mediator Analysis 
hallway_fit.mediator<-lm(TotalHallwayVisits ~ group, Mediation_Df)
summary(hallway_fit.mediator)
hallway_fit.dv<-lm(PropCorr ~ TotalHallwayVisits + group, Mediation_Df)
summary(hallway_fit.dv)
hallway_results<-mediate(hallway_fit.mediator, hallway_fit.dv, treat = "group", mediator= "TotalHallwayVisits", boot = T)
summary(hallway_results)

#Path Entropy As Mediator Analysis 
pathentropy_fit.mediator<-lm(pathentropy ~ group, Mediation_Df)
summary(pathentropy_fit.mediator)
pathentropy.dv<-lm(PropCorr ~ pathentropy + group, Mediation_Df)
summary(pathentropy.dv)
pathentropy_results<-mediate(pathentropy_fit.mediator, pathentropy.dv, treat = "group", mediator= "pathentropy", boot = T)
summary(pathentropy_results)

#Turns Made As Mediator Analysis 
turns_fit.mediator<-lm(Turns ~ group, Mediation_Df)
summary(turns_fit.mediator)
turns.dv<-lm(PropCorr ~ Turns + group, Mediation_Df)
summary(turns.dv)
turns_results<-mediate(turns_fit.mediator, turns.dv, treat = "group", mediator= "Turns", boot = T)
summary(turns_results)

#Distance Traveled As Mediator Analysis 
dist_fit.mediator<-lm(Dist ~ group, Mediation_Df)
summary(dist_fit.mediator)
dist_fit.dv<-lm(PropCorr ~ Dist + group, Mediation_Df)
summary(dist_fit.dv)
dist_results<-mediate(dist_fit.mediator, dist_fit.dv, treat = "group", mediator= "Dist", boot = T)
summary(dist_results)

#Evenness of Exploration As Mediator Analysis 
evenness_fit.mediator<-lm(ExplorEven ~ group, Mediation_Df)
summary(evenness_fit.mediator)
evenness_fit.dv<-lm(PropCorr ~ ExplorEven + group, Mediation_Df)
summary(evenness_fit.dv)
evenness_results<-mediate(evenness_fit.mediator, evenness_fit.dv, treat = "group", mediator= "ExplorEven", boot = T)
summary(evenness_results)

#Button Press Moves As Mediator Analysis 
buttonpress_fit.mediator<-lm(ButtonPress ~ group, Mediation_Df)
summary(buttonpress_fit.mediator)
buttonpress_fit.dv<-lm(PropCorr ~ ButtonPress + group, Mediation_Df)
summary(buttonpress_fit.dv)
buttonpress_results<-mediate(buttonpress_fit.mediator, buttonpress_fit.dv, treat = "group", mediator= "ButtonPress", boot = T)
summary(buttonpress_results)

#Longest Hallway Sequence As Mediator Analysis 
hallwayseq_fit.mediator<-lm(hallwayseq ~ group, Mediation_Df)
summary(hallwayseq_fit.mediator)
hallwayseq_fit.dv<-lm(PropCorr ~ hallwayseq + group, Mediation_Df)
summary(hallwayseq_fit.dv)
hallwayseq_results<-mediate(hallwayseq_fit.mediator, hallwayseq_fit.dv, treat = "group", mediator= "hallwayseq", boot = T)
summary(hallwayseq_results)

#Exploration Measures-Spatial Memory Linear Models in Midlife: The PCA Approach 
midlife_exploration_PCA<-prcomp(midlife_exploration_analysis[,c(2,4:12)], center = TRUE, scale. = TRUE) #running PCA analysis on dataframe with midlife 
str(midlife_exploration_PCA)

#Plotting PCA 
midlife_pca_df<-midlife_exploration_analysis[,c(2,4:12)] #creating df for PCA analysis 
midlife_pca<-prcomp(midlife_pca_df, scale. = TRUE, center = TRUE) #running the PCA 
plot(midlife_pca$x[,1], midlife_pca$x[,2]) #plotting first 2 principal components 
midlife_pca.var <- midlife_pca$sdev^2 #finding out how much of variation in dataset is explained by each principal components
midlife_pca.var<-round(midlife_pca.var/sum(midlife_pca.var)*100,1) #converting these values to percentages

#Scree Plot
qplot(c(1:10), midlife_pca.var)+
  geom_line()+
  xlab("Principal Component")+
  ylab ("Variance Explained")+
  ggtitle("Scree Plot")+
  ylim(0,100)

#Finding variables that significantly load onto each principal component 
install_github("arleyc/PCAtest")
library(PCAtest)
midlife_pca_sig<-PCAtest(midlife_pca_df,100,100, 0.05, varcorr = FALSE, counter = FALSE, plot = FALSE) #running significance tests

#Finding loading scores 
midlife_loading_scores_PC1<-midlife_pca$rotation[,1]# finding actual loading scores for PC1 (higher value indicates pushing data point to right of graph)
#(negative values indicates pushing data points to left side of graph)
abs_midlife_loading_scores_PC1<-abs(midlife_loading_scores_PC1) #finding absolute magnitude of loading scores 
abs_midlife_loading_scores_PC1<-sort(abs_midlife_loading_scores_PC1, decreasing = TRUE) #sorting loading scores from high to low 
midlife_loading_scores_PC2<-midlife_pca$rotation[,2] # finding loading scores for PC2
abs_midlife_loading_scores_PC2<-abs(midlife_loading_scores_PC2) #finding absolute magnitude of loading scores  
abs_midlife_loading_scores_PC2<-sort(abs_midlife_loading_scores_PC2, decreasing = TRUE)#sorting loading scores from high to low 

#Running Regression Model 
midlife_pc1_reversed<- -1*midlife_pca$x[,1]
midlife_pcs_propcorr<-lm(midlife_exploration_analysis$PropCorr ~ midlife_pc1_reversed + midlife_pca$x[,2]) #multiple regression model using PC1 + PC2 as predictors
#note that axes for PC1 has been reversed for easier visual interpretation 
summary (midlife_pcs_propcorr)

setwd("~/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/Plots/Iteration_3") #changing working dir to save plots into 

#Creating added variable plot
install.packages("remotes")
remotes::install_github("dereksonderegger/ggAVplots")
library(ggAVplots)

tiff(filename = "Midlife_PC1_PropCorr.tiff", units="in", width=5, height=5, res=500)
midlife_pc1_propcorr_plot<-ggAVplot(midlife_pcs_propcorr,'midlife_pc1_reversed')+
  scale_y_continuous(limits = c(-0.4,1.0))+
  theme_classic()+
  theme(text = element_text(size = 14))

midlife_pc1_propcorr_plot

dev.off()

tiff(filename = "Midlife_PC2_PropCorr.tiff", units="in", width=5, height=5, res=500)
midlife_pc2_propcorr_plot<-ggAVplot(midlife_pcs_propcorr,'midlife_pca$x[,2]')+
  scale_y_continuous(limits = c(-0.5,1.0))+
  theme_classic()+
  theme(text = element_text(size = 14))

midlife_pc2_propcorr_plot

dev.off()

#Creating individual raw plots (PC1-Wayfinding; PC2-Wayfinding)
midlife_df_plot<-data.frame(midlife_pc1_reversed,midlife_pca$x[,2],midlife_exploration_analysis$PropCorr) #creating df for plotting 

tiff(filename = "Midlife_PC1_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
midlife_pc1_propcorr_raw_plot<-ggplot(midlife_df_plot, aes(midlife_pc1_reversed,midlife_exploration_analysis.PropCorr)) +
                                        geom_point() +
                                        geom_smooth(method='lm')+
                                        theme_classic()+
                                        theme(text = element_text(size = 14))
midlife_pc1_propcorr_raw_plot

dev.off()

tiff(filename = "Midlife_PC2_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
midlife_pc2_propcorr_raw_plot<-ggplot(midlife_df_plot, aes(midlife_pca.x...2.,midlife_exploration_analysis.PropCorr)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_continuous(breaks = c(seq(-7.0,3.0, by=1)))+
  theme_classic()+
  theme(text = element_text(size = 14))

midlife_pc2_propcorr_raw_plot

dev.off()


#Exploration Measures-Spatial Memory Linear Models in Young: The PCA Approach 
young_exploration_PCA<-prcomp(young_exploration_analysis[,c(2,4:12)], center = TRUE, scale. = TRUE) #running PCA analysis on dataframe with young
str(young_exploration_PCA)

#Plotting PCA 
young_pca_df<-young_exploration_analysis[,c(2,4:12)] #creating df for PCA analysis 
young_pca<-prcomp(young_pca_df, scale. = TRUE, center = TRUE) #running the PCA 
plot(young_pca$x[,1], young_pca$x[,2]) #plotting first 2 principal components 
young_pca.var <- young_pca$sdev^2 #finding out how much of variation in dataset is explained by each principal components
young_pca.var<-round(young_pca.var/sum(young_pca.var)*100,1) #converting these values to percentages

#finding significance values for components & loadings 
young_pca_sig<-PCAtest(young_pca_df,100,100, 0.05, varcorr = FALSE, counter = FALSE, plot = FALSE) #running significance tests

#Finding loading scores 
young_loading_scores_PC1<-young_pca$rotation[,1]# finding raw loading scores for PC1
abs_young_loading_scores_PC1<-abs(young_loading_scores_PC1) #finding absolute loading scores 
abs_young_loading_scores_PC1<-sort(abs_young_loading_scores_PC1, decreasing = TRUE) #sorting loading scores from high to low 
young_loading_scores_PC2<-young_pca$rotation[,2] # finding loading scores for PC1
abs_young_loading_scores_PC2<-abs(young_loading_scores_PC2) #finding which variables correspond to which loading scores 
abs_young_loading_scores_PC2<-sort(abs_young_loading_scores_PC2, decreasing = TRUE)#sorting loading scores from high to low 
#midlife_loading_scores_PC3<-midlife_pca$rotation[,3] # finding loading scores for PC1
#midlife_loading_scores_PC3<-abs(midlife_loading_scores_PC3) #finding which variables correspond to which loading scores 
#midlife_loading_scores_PC3<-sort(midlife_loading_scores_PC3, decreasing = TRUE)#sorting loading scores from high to low 

#Running Regression Model 
young_pc2_reversed <- -1*young_pca$x[,2] #reversing scoring for PC2 for visual interpretation 
young_pcs_propcorr<-lm(young_exploration_analysis$PropCorr ~ young_pca$x[,1] + young_pc2_reversed) #multiple regression model using PC1 + PC2 as predictors 
summary (young_pcs_propcorr)

#Plotting Added Variable Plot
setwd("~/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/Plots/Iteration_3") #changing working dir to save plots into 

tiff(filename = "Young_PC2_PropCorr.tiff", units="in", width=5, height=5, res=500)
young_pc2_propcorr_plot<-ggAVplot(young_pcs_propcorr,'young_pc2_reversed')+
  #scale_y_continuous(limits = c(-0.4,1.0))+
  scale_x_continuous(limits = c(-4,4))+
  theme_classic()+
  theme(text = element_text(size = 14))

young_pc2_propcorr_plot

dev.off()

#Creating individual raw plots (PC2-Wayfinding)
young_df_plot<-data.frame(young_pc2_reversed,young_exploration_analysis$PropCorr) #creating df for plotting 

tiff(filename = "Young_PC2_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
young_pc2_propcorr_raw_plot<-ggplot(young_df_plot, aes(young_pc2_reversed,young_exploration_analysis.PropCorr)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_continuous(breaks = c(seq(-4.5,4.5, by=1)))+
  theme_classic()+
  theme(text = element_text(size = 14))

young_pc2_propcorr_raw_plot

dev.off()


#Logistic Regression Analysis 
exploration_PCA<-prcomp(Mediation_Df[,c(2,4:12)], center = TRUE, scale. = TRUE) #running PCA analysis on dataframe with midlife & young data combined (same as used in mediation analysis)
summary(exploration_PCA)
str(exploration_PCA)

#Step One: PCA To Find Predictors 
PCA_df<-Mediation_Df[,c(2,4:12)] #creating df for PCA analysis 
pca<-prcomp(PCA_df, scale. = TRUE, center = TRUE) #running the PCA 
plot(pca$x[,1], pca$x[,2]) #plotting first 2 principal components 
pca.var <- pca$sdev^2 #finding out how much of variation in dataset is explained by each principal components
pca.var<-round(pca.var/sum(pca.var)*100,1) #converting these values to percentages

#finding significance values for components & loadings 
pca_sig<-PCAtest(PCA_df,100,100, 0.05, varcorr = FALSE, counter = FALSE, plot = FALSE) #running significance tests

#Finding loading scores 
loading_scores_PC1<-pca$rotation[,1]# finding loading scores for PC1
abs_loading_scores_PC1<-abs(loading_scores_PC1) #finding absolute magnitude of loading scores 
abs_loading_scores_PC1<-sort(abs_loading_scores_PC1, decreasing = TRUE) #sorting loading scores from high to low 
loading_scores_PC2<-pca$rotation[,2] # finding loading scores for PC1
abs_loading_scores_PC2<-abs(loading_scores_PC2) #finding which variables correspond to which loading scores 
abs_loading_scores_PC2<-sort(abs_loading_scores_PC2, decreasing = TRUE)#sorting loading scores from high to low 
loading_scores_PC3<-pca$rotation[,3] # finding loading scores for PC1
abs_loading_scores_PC3<-abs(loading_scores_PC3) #finding which variables correspond to which loading scores 
abs_loading_scores_PC3<-sort(abs_loading_scores_PC3, decreasing = TRUE)#sorting loading scores from high to low 

#Step Two: Logistic Regression Model; Wayfinding Success 
library(pROC)
logit_wayfinding<- glm(group ~ PropCorr, data = Mediation_Df, family = "binomial") #model for wayfinding success as predictor 
summary(logit_wayfinding)
exp(coef(logit_wayfinding)) #getting odds ratios
logit_wayfinding_roc<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot = TRUE) #plotting ROC curve 
par(pty = "s") #modifying plot 
logit_wayfinding_roc<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot = TRUE, legacy.axes=TRUE, percent=TRUE, xlab = "False Positive Percentage",
ylab= "True Positive Percentage") #plotting updated curve
logit_wayfinding_roc$auc #display AUC value 

#Logistic Regression Model; PCs  
pc3_reversed<- -1*pca$x[,3] #reverse coding for ease of interpretation 
logit_pcs<- glm(Mediation_Df$group ~ pca$x[,1] + pca$x[,2] + pc3_reversed, family = "binomial") #model for PCs as predictor 
summary(logit_pcs)
exp(coef(logit_pcs)) #getting odds ratios
logit_pc1_pc3<- glm(Mediation_Df$group ~ pca$x[,1] + pc3_reversed, family = "binomial") #model using just PC1 and PC3 as predictor 
summary(logit_pc1_pc3)
exp(coef(logit_pc1_pc3)) #getting odds ratios
logit_pc1_pc3_roc<-roc(Mediation_Df$group, logit_pc1_pc3$fitted.values, plot = TRUE) #plotting ROC curve 
par(pty = "s") #modifying plot 
logit_pc1_pc3_roc<-roc(Mediation_Df$group, logit_pc1_pc3$fitted.values, plot = TRUE, legacy.axes=TRUE, percent=TRUE, xlab = "False Positive Percentage",
                          ylab= "True Positive Percentage") #plotting updated curve
logit_pc1_pc3_roc$auc
roc.test(logit_wayfinding_roc, logit_pc1_pc3_roc, method="delong") #assessing statistical significance of difference in AUC vals 

#ROC Curve Plots of Exploration Variables 
tiff(filename = "ROC_Curve_Exploration.tiff", units="in", width=5, height=5, res=500)
plot1<-roc(Mediation_Df$group, logit_pc1_pc3$fitted.values, plot=TRUE)
par(pty = "s")
plot1
plot1<-roc(Mediation_Df$group, logit_pc1_pc3$fitted.values, plot=TRUE, legacy.axes=TRUE, xlab = "False Positive Rate", ylab= "True Positive Rate", print.auc=TRUE)

dev.off()

#ROC Curve Plots of Wayfinding Variables 
tiff(filename = "ROC_Curve_Wayfinding.tiff", units="in", width=5, height=5, res=500)
plot2<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot=TRUE)
par(pty = "s")
plot2
plot2<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot=TRUE, legacy.axes=TRUE, xlab = "False Positive Rate", ylab= "True Positive Rate", print.auc=TRUE)

dev.off()

#ROC Curve Plots of Wayfinding Success & Exploration Variables Combined
tiff(filename = "ROC_Curves.tiff", units="in", width=5, height=5, res=500)

#Predictor Variable - Wayfinding Success
plot3<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot=TRUE)
par(pty = "s")
plot3
plot3<-roc(Mediation_Df$group, logit_wayfinding$fitted.values, plot=TRUE, legacy.axes=TRUE, xlab = "False Positive Rate", ylab= "True Positive Rate", col = "darkorchid", print.auc=TRUE)

#Adding Subsequent Predictor Variable ROC Curves In (i.e., PCs)
plot.roc(Mediation_Df$group, logit_pc1_pc3$fitted.values, add=TRUE, legacy.axes=TRUE, xlab = "False Positive Rate", ylab= "True Positive Rate", col = "darkorange", print.auc=TRUE, print.auc.y=0.4)
legend("bottomright",
       legend=c("Wayfinding Success", "PC1,PC3 (Exploration Variables)"),
       col=c("darkorchid", "darkorange"),
       lwd=4, cex =0.7, xpd = TRUE, horiz = FALSE)


dev.off()

#Exploration vs. Wayfinding Success Regression Iteration 2 (Pooled Dataset) - Midlife 
midlife_pooled_pca_df<-data.frame(matrix(NA,nrow = 87,ncol = 4)) #creating empty df
midlife_pooled_pca_df$X1<-pca$x[51:137,1] #inputting PC1 values of midlife 
midlife_pooled_pca_df$X2<-pca$x[51:137,2] #inputting PC2 values of midlife 
midlife_pooled_pca_df$X3<-pca$x[51:137,3] #inputting PC3 values of midlife 
midlife_pooled_pca_df$X3<-midlife_pooled_pca_df$X3 * -1 #reversing PC3 scores 
midlife_pooled_pca_df$X4<-Mediation_Df$PropCorr[51:137] #inputting wayfinding success values
midlife_pcs_propcorr_v2<-lm(midlife_pooled_pca_df$X4 ~ midlife_pooled_pca_df$X1 + midlife_pooled_pca_df$X2 + midlife_pooled_pca_df$X3) #running regression 
summary(midlife_pcs_propcorr_v2) #model output


#Creating individual raw plots (PC1-Wayfinding; PC2-Wayfinding) - Midlife 

setwd("~/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/Plots/Iteration_3") #changing working dir to save plots into 

tiff(filename = "Midlife_Pooled_PC1_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
midlife_pooled_pc1_propcorr_raw_plot<-ggplot(midlife_pooled_pca_df, aes(X1,X4)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_continuous(limits = c(-5,5))+
  theme_classic()+
  theme(text = element_text(size = 14))
midlife_pooled_pc1_propcorr_raw_plot

dev.off()

tiff(filename = "Midlife_Pooled_PC2_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
midlife_pooled_pc2_propcorr_raw_plot<-ggplot(midlife_pooled_pca_df, aes(X2,X4)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_continuous(breaks = c(seq(-7.0,4.0, by=1)))+
  theme_classic()+
  theme(text = element_text(size = 14))

midlife_pooled_pc2_propcorr_raw_plot

dev.off()

#Exploration vs. Wayfinding Success Regression Iteration 2 (Pooled Dataset) - Young 
young_pooled_pca_df<-data.frame(matrix(NA,nrow = 50,ncol = 4)) #creating empty df
young_pooled_pca_df$X1<-pca$x[1:50,1] #inputting PC1 values of midlife 
young_pooled_pca_df$X2<-pca$x[1:50,2] #inputting PC2 values of midlife 
young_pooled_pca_df$X3<-pca$x[1:50,3] #inputting PC3 values of midlife 
young_pooled_pca_df$X3<-young_pooled_pca_df$X3 * -1 #reversing PC3 scores 
young_pooled_pca_df$X4<-Mediation_Df$PropCorr[1:50] #inputting wayfinding success values
young_pcs_propcorr_v2<-lm(young_pooled_pca_df$X4 ~ young_pooled_pca_df$X1 + young_pooled_pca_df$X2 + young_pooled_pca_df$X3) #running regression 
summary(young_pcs_propcorr_v2) #model output

#Creating individual raw plots (PC2-Wayfinding) - Young 
setwd("~/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/Plots/Iteration_3") #changing working dir to save plots into 

tiff(filename = "Young_Pooled_PC2_PropCorr_Raw.tiff", units="in", width=5, height=5, res=500)
young_pooled_pc2_propcorr_raw_plot<-ggplot(young_pooled_pca_df, aes(X2,X4)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_x_continuous(limits = c(-4,4))+
  theme_classic()+
  theme(text = element_text(size = 14))
young_pooled_pc2_propcorr_raw_plot

dev.off()

#Create Violin Plots Showing Group Differences in Exploration & Memory Variables 
#first we need to structure the dataframe that we will use to make these plots accordingly so that the ggplot script works 
exploration_analysis<-rbind(midlife_exploration_analysis[,1:14],young_exploration_analysis[,1:14]) #combining the midlife and young dataframes into one single dataframe 

setwd("~/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Behavioral Analysis/SNAG_Midlife/ITERATION_4") #changing working dir to save plots into 

exploration_analysis <- exploration_analysis %>%
  mutate(group=factor(group,levels=c("Young", "Midlife")))

#Plot - Distance Travelled

#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "DistTravlled_Violin.tiff", units="in", width=5, height=5, res=500)

p_DistTravelled<-ggplot(exploration_analysis, aes(x = group, y = Dist, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,600))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_DistTravelled

dev.off()

#Plot - Object Visits
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "ObjVisits_Violin.tiff", units="in", width=5, height=5, res=500)

p_ObjVisits<-ggplot(exploration_analysis, aes(x = group, y = TotalObjVisits, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,40))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_ObjVisits

dev.off()

#Plot - Turns Made
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "TurnsMade_Violin.tiff", units="in", width=5, height=5, res=500)

p_TurnsMade<-ggplot(exploration_analysis, aes(x = group, y = Turns, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,200))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_TurnsMade

dev.off()

#Plot - Pause Duration 
exploration_analysis$Pauses<-exploration_analysis$Pauses/1000 #converting to seconds 

#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "PauseDuration_Violin.tiff", units="in", width=5, height=5, res=500)

p_Pauses<-ggplot(exploration_analysis, aes(x = group, y = Pauses, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,500))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Pauses

dev.off()

#Plot - Evenness of Exploration Objects
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "Evenness_Object_Violin.tiff", units="in", width=5, height=5, res=500)

p_Eveness<-ggplot(exploration_analysis, aes(x = group, y = ExplorEvenObj, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,4))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Eveness

dev.off()

#Plot - Eveness of Exploration Hallways
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "Evenness_Hallways_Violin.tiff", units="in", width=5, height=5, res=500)

p_Eveness_Hallways<-ggplot(exploration_analysis, aes(x = group, y = ExplorEvenHallway, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,8))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Eveness_Hallways

dev.off()

#Plot - Path Roaming Entropy
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "Roaming_Entropy_Violin.tiff", units="in", width=5, height=5, res=500)

p_Roaming_Entropy<-ggplot(exploration_analysis, aes(x = group, y = pathentropy, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(2.75,3.25))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Roaming_Entropy

dev.off()


#Plot - Hallway Visits 
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "Hallways_Visits_Violin.tiff", units="in", width=5, height=5, res=500)

p_Hallways<-ggplot(exploration_analysis, aes(x = group, y = TotalHallwayVisits, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(40,160))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Hallways

dev.off()


#Plot - Longest Hallway Sequence
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "Hallways_Sequence_Violin.tiff", units="in", width=5, height=5, res=500)

p_Hallway_Sequence<-ggplot(exploration_analysis, aes(x = group, y = hallwayseq, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,30))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_Hallway_Sequence

dev.off()



#Plot - Button Press Moves 
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "ButtonPressMoves_Violin.tiff", units="in", width=5, height=5, res=500)

p_ButtonPressMoves<-ggplot(exploration_analysis, aes(x = group, y = ButtonPress, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun.y = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(100,400))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

p_ButtonPressMoves

dev.off()

#Plot - Prop Correct 
#levels(exploration_analysis$group) <- gsub(" ", "\n", levels(exploration_analysis$group)) 

tiff(filename = "PropCorr_Violin.tiff", units="in", width=5, height=5, res=500)

p_PropCorr<-ggplot(exploration_analysis, aes(x = group, y = PropCorr, fill= group))+
  geom_violin()+
  scale_fill_brewer(palette="PuRd")+
  stat_summary(fun.data = "mean_sdl",fun.args= list(mult=1), geom="pointrange",color="red")+
  stat_summary(fun = "median", geom = "point", color="blue")+
  theme_classic()+
  theme(legend.position = "none")+
  #scale_y_continuous(limits = c(-0.25,1.00))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
  theme(text = element_text(size = 20))

print(p_PropCorr)

dev.off()




#Path Efficiency All Trials - Group Differences
midlife_patheff_df<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Midlife/Output_Iteration2/PathEffiency_Calculation/MLINDIV_participant_master_patheff.csv") #importing file with updated path eff scores
midlife_patheff_df<-filter(midlife_patheff_df, Subject %in% shuying_IDs) #keeping only IDs with useable maze data 
midlife_exploration_analysis$PathEff<-midlife_patheff_df$tm_path_efficiencies #adding to master analysis df

young_patheff_df<-read.csv ("/Users/vaisakhputhusseryppady/Desktop/UCI_Postdoc/Work/My Projects/SNAG_Exploration/Analysis/Preprocessing/SNAG_Young/Output_Iteration2/Path_Effiency_Calculation/MLINDIV_participant_master_patheff.csv") #importing file with updated path eff scores
young_patheff_df<-filter(young_patheff_df, Subject %in% young_exploration_analysis$Subject) #keeping only IDs with useable maze data 
young_exploration_analysis$PathEff<-young_patheff_df$tm_path_efficiencies #adding to master analysis df

#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$PathEff)
shapiro.test(young_exploration_analysis$PathEff)
#Wilcoxon Test 
patheff_groupdiff<-wilcox.test(young_exploration_analysis$PathEff,midlife_exploration_analysis$PathEff)
patheff_groupdiff_effsize<-cohensD(young_exploration_analysis$PathEff,midlife_exploration_analysis$PathEff)

#PCA: Exploration Variables - Path Efficiency All Trials Linear Models in Midlife 
midlife_pcs_patheff<-lm(midlife_exploration_analysis$PathEff ~ midlife_pc1_reversed + midlife_pca$x[,2])#running regression model 
summary(midlife_pcs_patheff)

#PCA: Exploration Variables - Path Efficiency All Trials Linear Models in Young 
young_pcs_patheff<-lm(young_exploration_analysis$PathEff ~ young_pca$x[,1] + young_pc2_reversed) #running regression model  
summary (young_pcs_patheff)

#Path Efficiency Correct Trials - Group Differences 
midlife_exploration_analysis$PathEffAccOnly<-midlife_patheff_df$tm_path_efficiencies_acc_only
young_exploration_analysis$PathEffAccOnly<-young_patheff_df$tm_path_efficiencies_acc_only 
#Normality test to assess whether samples are normally distributed 
shapiro.test(midlife_exploration_analysis$PathEffAccOnly)
shapiro.test(young_exploration_analysis$PathEffAccOnly)
#Wilcoxon Test
patheff_acc_only_groupdiff<-wilcox.test(na.omit(midlife_exploration_analysis$PathEffAccOnly),na.omit(young_exploration_analysis$PathEffAccOnly))
patheff_acc_only_groupdiff_effsize<-cohensD(na.omit(midlife_exploration_analysis$PathEffAccOnly), na.omit(young_exploration_analysis$PathEffAccOnly))

#PCA: Exploration Variables - Path Efficiency Correct Trials Linear Models in Midlife 
midlife_pcs_patheff_acc<-lm(midlife_exploration_analysis$PathEffAccOnly ~ midlife_pc1_reversed + midlife_pca$x[,2])#running regression model 
summary(midlife_pcs_patheff_acc)

#PCA: Exploration Variables - Path Efficiency Correct Trials Linear Models in Young 
young_pcs_patheff_acc<-lm(young_exploration_analysis$PathEffAccOnly ~ young_pca$x[,1] + young_pc2_reversed) #running regression model  
summary (young_pcs_patheff_acc)

#Mediation Analysis - Path Efficiency Correct Trials 
Mediation_Df_2<-rbind(young_exploration_analysis,midlife_exploration_analysis)#creating new combined df for mediation analysis 
Mediation_Df_2<-na.omit(Mediation_Df_2) #removing all participants with NA scores 

#Step 1- Finding Total Effect (relationship between independent variable & dependent variable)
fit.totaleffect_2<-lm(PathEffAccOnly ~ group, Mediation_Df_2)
summary(fit.totaleffect_2)

#Distance Travelled As Mediator Analysis 
distancetravelled_2.mediator<-lm(Dist ~ group, Mediation_Df_2)
summary(distancetravelled_2.mediator)
distancetravelled_2.dv<-lm(PathEffAccOnly ~ Dist + group, Mediation_Df_2)
summary(distancetravelled_2.dv)
distancetravelled_mediator_patheff<-mediate(distancetravelled_2.mediator, distancetravelled_2.dv, treat = "group", mediator= "Dist", boot = T)
summary(distancetravelled_mediator_patheff)

#Pause Duration As Mediator Analysis 
pd_2.mediator<-lm(Pauses ~ group, Mediation_Df_2)
summary(pd_2.mediator)
pd_2.dv<-lm(PathEffAccOnly ~ Pauses + group, Mediation_Df_2)
summary(pd_2.dv)
pd_mediator_patheff<-mediate(pd_2.mediator, pd_2.dv, treat = "group", mediator= "Pauses", boot = T)
summary(pd_mediator_patheff)

#Button Presses As Mediator Analysis 
buttonpress_2.mediator<-lm(ButtonPress ~ group, Mediation_Df_2)
summary(buttonpress_2.mediator)
buttonpress_2.dv<-lm(PathEffAccOnly ~ ButtonPress + group, Mediation_Df_2)
summary(buttonpress_2.dv)
buttonpress_mediator_patheff<-mediate(buttonpress_2.mediator, buttonpress_2.dv, treat = "group", mediator= "ButtonPress", boot = T)
summary(buttonpress_mediator_patheff)

#Object Visits As Mediator Analysis 
objvisits_2.mediator<-lm(TotalObjVisits ~ group, Mediation_Df_2)
summary(objvisits_2.mediator)
objvisits_2.dv<-lm(PathEffAccOnly ~ TotalObjVisits + group, Mediation_Df_2)
summary(objvisits_2.dv)
objvisits_mediator_patheff<-mediate(objvisits_2.mediator, objvisits_2.dv, treat = "group", mediator= "TotalObjVisits", boot = T)
summary(objvisits_mediator_patheff)

#Hallway Visits As Mediator Analysis 
hallwayvisits_2.mediator<-lm(TotalHallwayVisits ~ group, Mediation_Df_2)
summary(hallwayvisits_2.mediator)
hallwayvisits_2.dv<-lm(PathEffAccOnly ~ TotalHallwayVisits + group, Mediation_Df_2)
summary(hallwayvisits_2.dv)
hallwayvisits_mediator_patheff<-mediate(hallwayvisits_2.mediator, hallwayvisits_2.dv, treat = "group", mediator= "TotalHallwayVisits", boot = T)
summary(hallwayvisits_mediator_patheff)

#Longest Hallway Sequence As Mediator Analysis 
hallwayseq_2.mediator<-lm(hallwayseq ~ group, Mediation_Df_2)
summary(hallwayseq_2.mediator)
hallwayseq_2.dv<-lm(PathEffAccOnly ~ hallwayseq + group, Mediation_Df_2)
summary(hallwayseq_2.dv)
hallwayseq_mediator_patheff<-mediate(hallwayseq_2.mediator, hallwayseq_2.dv, treat = "group", mediator= "hallwayseq", boot = T)
summary(hallwayseq_mediator_patheff)

#Logistic Regression - Path Efficiency Correct Trials 
logit_patheffacc<- glm(group ~ PathEffAccOnly, data = Mediation_Df_2, family = "binomial") #model for path efficiency scores as predictor 
summary(logit_patheffacc)
exp(coef(logit_patheffacc)) #getting odds ratios
logit_patheffacc_roc<-roc(Mediation_Df_2$group, logit_patheffacc$fitted.values, plot = TRUE) #plotting ROC curve 
par(pty = "s") #modifying plot 
logit_patheffacc_roc<-roc(Mediation_Df_2$group, logit_patheffacc$fitted.values, plot = TRUE, legacy.axes=TRUE, percent=TRUE, xlab = "False Positive Percentage",
                          ylab= "True Positive Percentage") #plotting updated curve
logit_patheffacc_roc$auc #display AUC value 

#Comparing path efficiency AUC with wayfinding & exploration curve AUCs
roc.test(logit_wayfinding_roc, logit_pc1_pc3_roc, method="delong")#comparison 1
roc.test(logit_pc1_pc3_roc, logit_patheffacc_roc, method="delong")#comparison 2
roc.test(logit_wayfinding_roc, logit_patheffacc_roc, method="delong")#comparison 3


