#Final Iteration of PRM Flight Clustering < R-Script

#Approximate Code RunTime: 21.23 minutes

############################################################################################################################################################################################


#Step 1: Package Downloads - Installations for packages that will be used throughout the script.


###########################################################################################################################################################################################

# install.packages('factoextra')
library('factoextra')
# install.packages("gridExtra")
library('gridExtra')
# install.packages('purrr')
library('purrr')
# install.packages('dplyr')
library(dplyr)
# install.packages('outliers')
library(outliers)
library(glue)
# install.packages('plotly')
library(plotly)
# install.packages('RODBC')
library(RODBC)
# install.packages('xlsx')
library(xlsx)

###########################################################################################################################################################################################


#Step 2: Functions - Defining key formulas to be used throughout the script


############################################################################################################################################################################################


#conversion# is a function dedicated towards standardization on a 0 - 100 scale and allows you to put arg1 in terms of this scale. Refer to pgs. 5-7 in project documentation for elaboration on standardization method
conversion <- function(arg1, maxx, minn){
  term1 = arg1 - minn
  term2 = maxx - minn
  round(term1 * (100/term2),2)
}


#optimal_k# is a function dedicated towards selecting and assigning the optimal number of k-clusters for a given route.  
optimal_k <- function(flight_df, cluster_df){
  if (nrow(flight_df) < 50){
    k_range <- subset(cluster_df, Optimal_k_test > 23)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if(nrow(flight_df) < 250){
    k_range <- subset(cluster_df, Optimal_k_test >= 20.5)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if(nrow(flight_df) < 1000){
    k_range <- subset(cluster_df, Optimal_k_test >= 20)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if(nrow(flight_df) < 1250){
    k_range <- subset(cluster_df, Optimal_k_test >= 18)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if (nrow(flight_df) < 1750){
    k_range <- subset(cluster_df, Optimal_k_test >= 16.5)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if (nrow(flight_df) < 2250){
    k_range <- subset(cluster_df, Optimal_k_test >= 15.5)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if (nrow(flight_df) < 3600){
    k_range <- subset(cluster_df, Optimal_k_test >= 15)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if (nrow(flight_df) < 4750){
    k_range <- subset(cluster_df, Optimal_k_test >= 14.75)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else if (nrow(flight_df) < 5250){
    k_range <- subset(cluster_df, Optimal_k_test >= 14.5)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  } else{
    k_range <- subset(cluster_df, Optimal_k_test >= 14.5)
    opt_k <- tail(k_range,1)[,"k"]
    opt_k
  }
}


#elbowGenerator# is a function dedicated to generating the first elbow table which allows us to see the change in tots.withins for a given route.Refer to pgs. 17-19 in project documentation for elaboration on elbow methodology
elbowGenerator <- function(eval_df){
  elbow <- map_dbl(1:15, function(k){
    model <- kmeans(eval_df, k, nstart = 10)
    model$tot.withinss
  })
  elbow
}


#k_test# is a function that derives the variable (column) in the elbow DF that is used by 'optimal_k' to set the appropriate k value for a given route.
k_test <- function(df){
  raw_ans <- (df$tot_withins/max(df$tot_withins))*100
  ans <- round(raw_ans,2)
  ans
}

#remove_outliers# is a function that removes outliers for the Average Revenue variable utilizing the MAD outlier identification methodology. Refer to pgs. 11-13 in project documentation for elaboration on Median Absolute deviation outlier removal method
remove_outliers <- function(df, mult){
  nu_df <- df
  med <- median(df$AvgRevenue)
  absdev <- mad(df$AvgRevenue, constant = 1)
  
  if (nrow(df)>35){
    nu_df <- filter(nu_df,AvgRevenue < med + mult*absdev)
    nu_df <- filter(nu_df,AvgRevenue > med - mult*absdev)
  }
  
  df <- nu_df
}


#remove_outliersLF# is a function that removes outliers for the Load Factor variable utilizing a set of fixed Outlier definition rules that establish constant upper and lower thresholds. Refer to pg. 9 in project documentation for elaboration on load factor outlier removal
remove_outliersLF <- function(df){
  nu_df <- df
  if (nrow(df)>35){
    if (mean(df$LFpctage)<50){
      nu_df <- filter(nu_df, LFpctage > 5)
      nu_df <- filter(nu_df, LFpctage <= 100)
    } else {
      nu_df <- filter(nu_df, LFpctage > 10)
      nu_df <- filter(nu_df, LFpctage <= 100)
    }
  } else{nu_df <- df}
}


#dataselector# is a function that implements the filter function from the dplyr package to create a data frame for a given route based on the DirMkt variable
dataselector <- function(df,route){
  route_data <- subset(df, DirMkt == route)
}


#Two_D_plot# is a function that implements the plotly package to visualize the cluster outputs on a Two_dimensional plot
Two_D_plot <- function(df){
  name <- deparse(substitute(df))
  x <- max(df$ClusterGroup)
  for (k in df$ClusterGroup){
    ID_name <- paste(c('Cluster', k),collapse = " ")
    df$ClusterGroup[which(df$ClusterGroup == k)] <- ID_name
  }
  df$ClusterGroup <- as.factor(df$ClusterGroup)
  ggplot(df, aes(nu_LF, nu_Rev, color = df$ClusterGroup )) + 
    geom_point() + 
    ggtitle(glue(name))
}


############################################################################################################################################################################################


#Step 3: Defining Key Variables that will be used throughout the script


###########################################################################################################################################################################################

#SQLHandle# - establishes a connection to the Revenue Management SQL server
SQLhandle <- odbcDriverConnect('driver={SQL Server};
                               server=SQLPDMREVENUEMGMT,1441;
                               database=DM_RevenueMgmt;
                               trusted_connection=true')

#This is done to run a query in SQL that clears all the data in the Sandbox table that we dedicate to storing the clustered flight observations we receive as an output from this script
sqlQuery(SQLhandle,"truncate table Sandbox.ClusterGroupDataFrame")


#read_tbl# - represents a dataframe that takes all the flight observations that we stored in the ClusteringFinalDataTable in SQL
read_tbl <- sqlFetch(SQLhandle,sqtable = "Sandbox.ClusteringFinalDataTable")


#DirMkts# - represents a vector containing the names of every directional market in our data set in the character type
DirMkts <- read_tbl$DirMkt %>%
  as.vector()%>%
  unique() %>% 
  sort() 


#datalist# - is an empty list that will be populated with all flight observations with clusters at the end of the script
datalist <- list()


#Assigned_Ks# - represents an empty vector which we will use to store the number Ks we assign to each directional market in our pre outlier removal clustering process
Assigned_Ks <- c()


#FlightFreq# - represents an empty vector which we will populate with the flight frequencies of each of the markets listed in #DirMkts#
FlightFreq <- c()


#FlightFreqMinusLF# - represents an empty vector which we will populate with the post load factor outlier removal flight frequencies of each of the markets listed in #DirMkts#
FlightFreqMinusLF <- c()


#nu_FlightFreq# - represents an empty vector which we will populate with the post outlier removal flight frequencies of each of the markets listed in #DirMkts#
nu_FlightFreq <- c()


#nu_Assigned_Ks# - represents an empty vector which we will use to store the number of Ks we assign to each directional market in our post outlier removal clustering process 
nu_Assigned_Ks <- c()


#Max_LF# - represents an empty vector which will be used to store the maximum observation of load factor for each of the directional markets in #DirMkts#
Max_LF <- c()


#Min_LF# - represents an empty vector which will be used to store the minimum observation of load factor for each of the directional markets in #DirMkts#
Min_LF <- c()


#Mean_LF# - represents an empty vector which will be used to store the mean load factor for each of the directional markets in #Dirmkts#
Mean_LF <- c()


#Max_Rev# - represents an empty vector which will be used to store the maximum observation of Average Revenue for each of the directional markets in #DirMkts#
Max_Rev <- c()


#Min_Rev# - represents an empty vector which will be used to store the minimum observation of Average Revenue for each of the directional markets in #DirMkts#
Min_Rev <- c()


#Mean_Rev# - represents an empty vector which will be used to store the mean Average Revenue for each of the directional markets in #DirMkts#
Mean_Rev <- c()


#Mean_nuRev# - represents an empty vector which will be used to store the mean standardized Average Revenue for each of the directional markets in #DirMkts#
Mean_nuRev <- c()


#Multiplier# - represents an empty vector which will be used to store the multiplier assigned to each directional market to be used in the final iteration of the Average Revenue outlier removal 
Multiplier <- c()


###########################################################################################################################################################################################


#Step 4: Data Partitioning - This section is dedicated to assigning data corresponding to each directional market to a data frame of its own and storing the Max, Min and Mean Average Revenue and Load factor


###########################################################################################################################################################################################


#For Loop 1# - this is dedicated mainly to breaking down all the observations in #read_tbl# to mini-data frames for each of the directional markets in #DirMkts#
for (dir in DirMkts){
  
  #getting the index of the directional market relative to its position in #DirMkts#
  index <- match(dir,DirMkts) #KEY NOTE# - This piece of code might not be necssary until final clustering for loop in the section dedicated to step 7
  
  #assigning the data observations corresponding to the directional market using our custom made function #dataselector#  
  assign(dir,dataselector(read_tbl,dir))
  df_name <- get(dir)
  
  #recording the frequency of the directional market using a count of the number of rows in the mini-data frame associated with the directional market. 
  FlightFreq <- c(FlightFreq,toString(nrow(df_name)))
  
  #storing maximum, minimum and mean Average Revenues and Load Factors for the directional market to the approrpriate empty vectors initialized in Step 3
  Max_LF <- round(append(Max_LF, max(get(dir)$LFpctage)),2)
  Min_LF <- round(append(Min_LF, min(get(dir)$LFpctage)),2)
  Mean_LF <- round(append(Mean_LF, mean(get(dir)$LFpctage)),2)
  Max_Rev <- round(append(Max_Rev, max(get(dir)$AvgRevenue)),2)
  Min_Rev <- round(append(Min_Rev, min(get(dir)$AvgRevenue)),2)
  Mean_Rev <- round(append(Mean_Rev, mean(get(dir)$AvgRevenue)),2)
  
  #For Loop to ensure that if the maximum load factor observation in the market is greater than 100, it is set to 100.  
  for (lf in df_name$LFpctage){
    index <- match(lf,df_name$LFpctage)
    if (lf > 100){
      df_name$LFpctage[index]<- 100
    }
  } #KEY NOTE: This step may be a bit redundant and make sure to investigate and possibly remove#
  
  #if statement to carry out our custom made function #conversion# to standardize our clustering variables if the directional market has a frequency greater than 1 flight
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }
  
  #overwriting the mini-data frame created for the directional market with a new version containing 'nu_Rev' and 'nu_LF'
  assign(dir,df_name)
  
}


###########################################################################################################################################################################################


#Step 5: Outlier Removal & Initial Clustering - This section of the code is dedicated to removing all observations deemed as outliers and conducting a first round of clustering 


###########################################################################################################################################################################################


#For Loop 2# - This for loop is dedicated to removing outliers for load factor and conducting initial clustering
for(dir in DirMkts){
  
  assign(dir,remove_outliersLF(get(dir))) #removing load factor outliers 
  df_name <- get(dir) #assigning the remaining observations to the mini-dataframes
  df_len <- nrow(df_name) #recording the new flight frequency post outlier removal 
  FlightFreqMinusLF <- c(FlightFreqMinusLF,toString(nrow(df_name)))  #adding the new flight frequency after removing outliers to the appropriate empty variable 
  
  #if statement to carry out our custom made function #conversion# to standardize our clustering variables if the directional market has a frequency greater than 1 flight
  if(df_len >1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }  
  

  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev) #represents a temporary dataframe that has two columns to house all the observations from our 2 clustering variables for the directional market
  k <- 2 #setting an arbitrary K to start off the k-means clustering process (refer to Project Documentation page 14 to see step by step break down of k-means algorithm)
  
  
  #if statement that conducts clustering and elbow test to identify optimal K for all markets with frequency greater than or = to  25 flights and sets custom rules for markets with observations less than 25.  
  if(df_len <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100) #Assigns k of 1 to all markets with less than or equal to 16 and conducts kmeans cluster
    df_name$ClusterGroup <- Cluster$cluster
    Assigned_Ks <- c(Assigned_Ks,toString(1)) #updates the empty vector dedicated to storing the number of Ks for every given route with k = 1 for markets with less than 16 flights
  } else if(df_len < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100) #Assigns our arbitrary K to initialize clustering for markets w/ frequency between 15 - 25 and conducts kmeans cluster
    elbowEval <- elbowGenerator(ClusterVars) #finding the total within sum of squares for Ks in the range 1-15 using our custom function #elbowGenerator 
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval) # creating a data frame with two columns, one of them being the results of our application of the #elbowGenerator# function 
    elbowDF$Optimal_k_test <- k_test(elbowDF) #adjusting the elbow data frame with a column housing results from our custom function #k_test#
    Opt_k <- optimal_k(df_name, elbowDF) #identifying an optimal K using our custom function #Opt_k#
    Assigned_Ks <- c(Assigned_Ks,toString(Opt_k)) #updates the empty vector dedicated to storing the number of Ks prior to complete outlier removal for every given route with the Opt_K
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100) #re-running the kmeans cluster process with the Optimal - K to get the appropriate cluster output
    df_name$ClusterGroup <- Cluster$cluster #assigning the appropriate cluster group from the output of our run of the kmeans cluster in line 340 to a column the mini-data frame created for the directional market
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100) #Assigns our arbitrary K to initialize clustering for markets w/ frequency greater than 25 and conducts kmeans cluster
    elbowEval <- elbowGenerator(ClusterVars) #refer to line 334 comments
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval) #similar to line 335 
    elbowDF$Optimal_k_test <- k_test(elbowDF) #similar to line 336
    Opt_k <- optimal_k(df_name, elbowDF) #similar to line 337
    Assigned_Ks <- c(Assigned_Ks,toString(Opt_k)) #similar to line 338
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100) #similar to line 340
    df_name$ClusterGroup <- Cluster$cluster #similar to line 341
  }
  
  assign(dir,df_name) #overwriting the newly edited mini-dataframe w/ the cluster group of each flight observation to 
  
  rm(dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF) #iteratively clearing the variables assigned for each directional market in #DirMkts#
}




#Converting all the observations in #Assigned_Ks#, #FlightFreq# and, #FlightFreqMinusLF to the numeric type to allow for easier manipulation.
Assigned_Ks <- as.numeric(Assigned_Ks)
FlightFreq <- as.numeric(FlightFreq)
FlightFreqMinusLF <- as.numeric(FlightFreqMinusLF)

#Determining the count of outliers removed due to the normal range rule we set with load factor in our formula #remove_outliersLF# by subtracting the change in frequencies 
LFOutlier_count <- FlightFreq - FlightFreqMinusLF




#For Loop 3# - This for loop is dedicated to removing outliers for Average Revenue and conducting second round clustering
for (dir in DirMkts){
  #similar to line 252
  index <- match(dir,DirMkts) #KEY NOTE# - This piece of code might not be necssary until final clustering for loop in the section dedicated to step 7
  
  route <- paste(dir,"_2",sep = "") #creates a new name for a mini- data frame that contains all observations from the directional market post outlier removal. Ex: "YVRYYC_2".
  assign(route,remove_outliers(get(dir),2)) #removing Average Revenue outliers with an initial multiplier of 2. 
  df_name <- get(route) #assigning the remaining observations to the mini-dataframes dedicated towards storing all observations for the directional market post outlier removal.
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name))) #adding the new flight frequency post removal of average revenue outliers to the appropriate empty vector. 
  
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  } #Similar to the if statement written in line 312
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2) #dedicated to calculating the mean standardized revenue and storing it in the appropriate empty vector defined in step 3
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev) #similar to line 324
  k <- 2 #similar to line 325
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } #similar step by step to the if statement from lines 329-353
  
  assign(route,df_name) #similar to line 355
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF) #similar to line 357
}

#Converting all the observations in #nu_Assigned_Ks# and #nu_FlightFreq#  to the numeric type to allow for easier manipulation.
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks)
nu_FlightFreq <- as.numeric(nu_FlightFreq)

#Determining the count of Average Revenue outliers removed with the MAD methodology we set in our formula #remove_outliers# by subtracting the change in frequencies 
AvgRevOutliers_count <- FlightFreqMinusLF - nu_FlightFreq

#OGstats# - represents a data frame that we use to store the now populated vectors that we initially defined in step 3. This is done to create a summary of key facts for our iterative evaluation of the effectiveness of the clustering script 
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#creating a new column in #OGstats# that represents the % of observations removed as Average revenue outliers from each directional market
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)   


###########################################################################################################################################################################################


#Step 6: MAD Multiplier Assignment - This section of the code is dedicated to iteratively assigning every directional market to an appropriate multiplier group to be used in the final clustering
#KEY NOTE# - this represents the most inefficient section of the code and could use some re-working to reduce the number of clustering iterations.

###########################################################################################################################################################################################


#routes_w_mult2# - represents a vector that contains all the directional markets from #DirMkts# that will use 2 as a multiplier in the #remove_outliers# function
routes_w_mult2 <- OGstats %>%
  select(DirMkts,FreqDiff) %>% #takes the Directional markets and FreqDiff columns from the #OGstats# data frame
  filter(abs(FreqDiff)<5) %>% #filters all the Directional markets that lose less than 5% of observations when #remove_outliers# takes 2 as an input for multiplier
  select(DirMkts) %>%
  unlist() %>%
  as.character() #converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7

#remaining_routes# - represents a vector that contains all the directional markets from #DirMkts# that don't pass the condition to use 2,3,3.5,4 & 5 as a multiplier and therefore will use 6 as a multiplier
remaining_routes <- OGstats %>%
  select(DirMkts)%>%
  filter(!(DirMkts %in% routes_w_mult2)) #filtering out markets that are in #routes_w_mult2#

#clearing the appropriate vectors to prepare for re-clustering of the directional markets stored in #remaining_routes#
Mean_nuRev <- c()
rm(OGstats)
datalist <- list()
nu_FlightFreq <- c() 
nu_Assigned_Ks <- c()


for (dir in DirMkts){
  index <- match(dir,DirMkts)
  
  route <- paste(dir,"_2",sep = "")
  assign(route,remove_outliers(get(dir),3))
  df_name <- get(route)
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name)))
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2)
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev)
  k <- 2
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  }
  
  assign(route,df_name)
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF)
} #identical to #For Loop 3# from step 5 except it uses 3 as a multiplier in this iteration of our #remove_outliers# function

#refer to comments in line 431
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks) 
nu_FlightFreq <- as.numeric(nu_FlightFreq)


#refer to comments in line 438
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#refer to comments in line 445
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)


#routes_w_mult3# - represents a vector that contains all the directional markets from #DirMkts# that will use 3 as a multiplier in the #remove_outliers# function
routes_w_mult3 <- OGstats %>%
  select(DirMkts,FreqDiff) %>%
  filter(abs(FreqDiff)< 5 & !(DirMkts %in% routes_w_mult2)) %>% #filters all the Directional markets that lose less than 5% of observations when #remove_outliers# takes 3 as an input for multiplier AND hasn't been assigned to #routes_w_mult2#
  select(DirMkts) %>% 
  unlist() %>%
  as.character() #converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7

remaining_routes <- OGstats %>%
  select(DirMkts)%>%
  filter(!(DirMkts %in% routes_w_mult2)) %>%
  filter(!(DirMkts %in% routes_w_mult3)) #filtering out markets that are in #routes_w_mult2# and #routes_w_mult3# 

#clearing the appropriate vectors to prepare for re-clustering of the directional markets stored in #remaining_routes#
Mean_nuRev <- c()
rm(OGstats)
datalist <- list()
nu_FlightFreq <- c()
nu_Assigned_Ks <- c()


for (dir in DirMkts){
  index <- match(dir,DirMkts) 
  
  route <- paste(dir,"_2",sep = "")
  assign(route,remove_outliers(get(dir),3.5))
  df_name <- get(route)
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name)))
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2)
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev)
  k <- 2
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  }
  
  assign(route,df_name)
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF)
} #identical to #For Loop 3# from step 5 except it uses 3.5 as a multiplier in this iteration of our #remove_outliers# function

#refer to comments in line 431
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks)
nu_FlightFreq <- as.numeric(nu_FlightFreq)

#refer to comments in line 438
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#refer to comments in line 445
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)


#routes_w_mult3.5# - represents a vector that contains all the directional markets from #DirMkts# that will use 3.5 as a multiplier in the #remove_outliers# function
routes_w_mult3.5 <- OGstats %>%
  select(DirMkts,FreqDiff) %>%
  filter(abs(FreqDiff)< 5 & !(DirMkts %in% c(routes_w_mult2,routes_w_mult3))) %>% #filters all the Directional markets that lose less than 5% of observations when #remove_outliers# takes 3.5 as an input for multiplier AND hasn't been assigned to #routes_w_mult2#, #routes_w_mult3#
  select(DirMkts) %>% 
  unlist() %>%
  as.character() #converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7

remaining_routes <- OGstats %>%
  select(DirMkts)%>%
  filter(!(DirMkts %in% routes_w_mult2)) %>%
  filter(!(DirMkts %in% routes_w_mult3)) %>%
  filter(!(DirMkts %in% routes_w_mult3.5)) #filtering out markets that are in #routes_w_mult2#, #routes_w_mult3# and #routes_w_mult3.5# 

#clearing the appropriate vectors to prepare for re-clustering of the directional markets stored in #remaining_routes#
Mean_nuRev <- c()
rm(OGstats)
datalist <- list()
nu_FlightFreq <- c()
nu_Assigned_Ks <- c()


for (dir in DirMkts){
  index <- match(dir,DirMkts)
  
  route <- paste(dir,"_2",sep = "")
  assign(route,remove_outliers(get(dir),4))
  df_name <- get(route)
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name)))
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2)
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev)
  k <- 2
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  }
  
  assign(route,df_name)
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF)
} #identical to #For Loop 3# from step 5 except it uses 4 as a multiplier in this iteration of our #remove_outliers# function

#refer to comments in line 431
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks)
nu_FlightFreq <- as.numeric(nu_FlightFreq)

#refer to comments in line 438
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#refer to comments in line 445
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)


#routes_w_mult4# - represents a vector that contains all the directional markets from #DirMkts# that will use 4 as a multiplier in the #remove_outliers# function
routes_w_mult4 <- OGstats %>%
  select(DirMkts,FreqDiff) %>%
  filter(abs(FreqDiff)< 5 & !(DirMkts %in% c(routes_w_mult2,routes_w_mult3,routes_w_mult3.5))) %>% #filters all the Directional markets that lose less than 5% of observations when #remove_outliers# takes 4 as an input for multiplier AND hasn't been assigned to #routes_w_mult2#, #routes_w_mult3#, #routes_w_mult3.5#
  select(DirMkts) %>% 
  unlist() %>%
  as.character() #converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7

remaining_routes <- OGstats %>%
  select(DirMkts)%>%
  filter(!(DirMkts %in% routes_w_mult2)) %>%
  filter(!(DirMkts %in% routes_w_mult3)) %>%
  filter(!(DirMkts %in% routes_w_mult3.5)) %>%
  filter(!(DirMkts %in% routes_w_mult4)) #filtering out markets that are in #routes_w_mult2#, #routes_w_mult3#, #routes_w_mult3.5# and #routes_w_mult4# 


#clearing the appropriate vectors to prepare for re-clustering of the directional markets stored in #remaining_routes#
Mean_nuRev <- c()
rm(OGstats)
datalist <- list()
nu_FlightFreq <- c()
nu_Assigned_Ks <- c()

for (dir in DirMkts){
  index <- match(dir,DirMkts)
  
  route <- paste(dir,"_2",sep = "")
  assign(route,remove_outliers(get(dir),5))
  df_name <- get(route)
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name)))
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  }
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2)
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev)
  k <- 2
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  }
  
  assign(route,df_name)
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF)
} #identical to #For Loop 3# from step 5 except it uses 5 as a multiplier in this iteration of our #remove_outliers# function

#refer to comments in line 431
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks)
nu_FlightFreq <- as.numeric(nu_FlightFreq)

#refer to comments in line 438
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#refer to comments in line 445
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)


#routes_w_mult5# - represents a vector that contains all the directional markets from #DirMkts# that will use 5 as a multiplier in the #remove_outliers# function
routes_w_mult5 <- OGstats %>%
  select(DirMkts,FreqDiff) %>%
  filter(abs(FreqDiff)< 5 & !(DirMkts %in% c(routes_w_mult2,routes_w_mult3,routes_w_mult3.5,routes_w_mult4))) %>% #filters all the Directional markets that lose less than 5% of observations when #remove_outliers# takes 5 as an input for multiplier AND hasn't been assigned to #routes_w_mult2#, #routes_w_mult3#, #routes_w_mult3.5#, or #routes_w_mult4#
  select(DirMkts) %>% 
  unlist() %>%
  as.character() #converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7

remaining_routes <- OGstats %>%
  select(DirMkts)%>%
  filter(!(DirMkts %in% routes_w_mult2)) %>%
  filter(!(DirMkts %in% routes_w_mult3)) %>%
  filter(!(DirMkts %in% routes_w_mult3.5)) %>%
  filter(!(DirMkts %in% routes_w_mult4)) %>%
  filter(!(DirMkts %in% routes_w_mult5)) %>% #filtering out markets that are in #routes_w_mult2#, #routes_w_mult3#, #routes_w_mult3.5#, #routes_w_mult4# and #routes_w_mult5#
  unlist() %>%
  as.character()#converting each of the directional markets to the type character so it can be taken as an input in the final outlier removal process in the for loop in step 7


#clearing the appropriate vectors to prepare for re-clustering of the directional markets stored in #remaining_routes#
Mean_nuRev <- c()
rm(OGstats)
datalist <- list()
nu_FlightFreq <- c()
nu_Assigned_Ks <- c()


###########################################################################################################################################################################################


#Step 7: Final Round of Outlier Removal & Clustering  - This section of the code is dedicated to conducting a final round of outlier removal using the appropriate multipliers and kmeans clustering with the new directional market data frames that have been filtered for outliers


###########################################################################################################################################################################################


#For Loop 4# - This loop is dedicated to the final Average outlier removal process and final clustering 
for (dir in DirMkts){
  #similar to line 252
  index <- match(dir,DirMkts)
  
  #similar to line 379
  route <- paste(dir,"_2",sep = "")
  
  #if statement dedicated towards carrying out our #remove_outliers# function on each directional market with the appropriate multiplier as determined in step 6
  if (dir %in% routes_w_mult2){
    assign(route,remove_outliers(get(dir),2)) #conducts our function #remove_outliers# on all directional markets in the vector #routes_w_mult2#
    Multiplier <- append(Multiplier, 2) #updates the empty vector multiplier that we defined in step 3 with a value of 2
  } else if (dir %in% routes_w_mult3){
    assign(route,remove_outliers(get(dir),3)) #conducts our function #remove_outliers# on all directional markets in the vector #routes_w_mult3#
    Multiplier <- append(Multiplier, 3) #updates the empty vector multiplier that we defined in step 3 with a value of 3
  } else if (dir %in% routes_w_mult3.5){
    assign(route,remove_outliers(get(dir),3.5)) #conducts our function #remove_outliers# on all directional markets in the vector #routes_w_mult3.5#
    Multiplier <- append(Multiplier, 3.5) #updates the empty vector multiplier that we defined in step 3 with a value of 3.5
  } else if (dir %in% routes_w_mult4){
    assign(route,remove_outliers(get(dir),4)) #conducts our function #remove_outliers# on all directional markets in the vector #routes_w_mult4#
    Multiplier <- append(Multiplier, 4) #updates the empty vector multiplier that we defined in step 3 with a value of 4
  } else if (dir %in% routes_w_mult5){
    assign(route,remove_outliers(get(dir),5)) #conducts our function #remove_outliers# on all directional markets in the vector #routes_w_mult5#
    Multiplier <- append(Multiplier, 5) #updates the empty vector multiplier that we defined in step 3 with a value of 5
  } else{
    assign(route,remove_outliers(get(dir),6)) #conducts our function #remove_outliers# on all directional markets in the vector #remaining_routes#
    Multiplier <- append(Multiplier, 6) #updates the empty vector multiplier that we defined in step 3 with a value of 6
  }
  
  df_name <- get(route) #refer to comments on line 384
  nu_FlightFreq <- c(nu_FlightFreq,toString(nrow(df_name))) #refer to comments on line 385
  
  if(nrow(df_name)>1){
    df_name$nu_LF <- conversion(df_name$LFpctage,
                                max(df_name$LFpctage),min(df_name$LFpctage))
    df_name$nu_Rev <- conversion(df_name$AvgRevenue,
                                 max(df_name$AvgRevenue),min(df_name$AvgRevenue))
  } else{
    df_name$nu_LF <- df_name$LFpctage
    df_name$nu_Rev <- df_name$AvgRevenue
  } #similar to the if statement written in line 312
  
  Mean_nuRev <- round(append(Mean_nuRev, mean(df_name$nu_Rev)),2) #refer to commments on line 398
  
  ClusterVars <- data.frame(df_name$nu_LF,df_name$nu_Rev) #refer to comments on line 324
  k <- 2 #refer to comments on line 325
  
  if(nrow(df_name) <= 15){
    Cluster <- kmeans(ClusterVars, 1, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(1))
  } else if(nrow(df_name) < 25){
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } else{
    Cluster <- kmeans(ClusterVars, k, nstart = 100)
    elbowEval <- elbowGenerator(ClusterVars)
    elbowDF <- data.frame(k = 1:15, tot_withins = elbowEval)
    elbowDF$Optimal_k_test <- k_test(elbowDF)
    Opt_k <- optimal_k(df_name, elbowDF)
    nu_Assigned_Ks <- c(nu_Assigned_Ks,toString(Opt_k))
    
    Cluster <- kmeans(ClusterVars, Opt_k, nstart = 100)
    df_name$ClusterGroup <- Cluster$cluster
  } #similar step by step to the if statement from lines 329-353
  
  assign(route,df_name)#refer to comments on line 355
  datalist[[index]] <- df_name #updates #datalist# with all the observations from the new mini-data frame made throughout the for loop
  
  rm(index,dir,df_name,ClusterVars,k,Opt_k,Cluster,elbowEval,elbowDF,route) #refer to comments on line 357
}

#Refer to comments on line 434
nu_Assigned_Ks <- as.numeric(nu_Assigned_Ks)
nu_FlightFreq <- as.numeric(nu_FlightFreq)
AvgRevOutliers_count <- FlightFreqMinusLF - nu_FlightFreq

#refer to comments on line 441
OGstats <- data.frame(DirMkts,FlightFreq,nu_FlightFreq,
                      LFOutlier_count,AvgRevOutliers_count, Multiplier, 
                      Mean_nuRev,Mean_Rev,Max_Rev,Min_Rev,
                      Mean_LF,Max_LF,Min_LF,
                      Assigned_Ks, nu_Assigned_Ks)

#refer to comments on line 448
OGstats$FreqDiff <- round((nu_FlightFreq - FlightFreqMinusLF)/ FlightFreq *100)



#############################################################################################################################################################################################


#Step 8: Data Cosnolidation and Exportation - This section of the code is dedicated to combining all the clustered data and the unclustered outliers exporting them back to our sandbox table in SQL


#############################################################################################################################################################################################


#FinalDataFrame# - variable that serves to combine all the observations that have been clustered and stored in #datalist# up to this point in a single data frame so that they are all in 1 easily accessible area.
FinalDataFrame = do.call(rbind, datalist)



# this serves to conduct a join between our #FinalDataFrame# and #read_tbl# so that we can combine both our clustered and non-clustered observations.   
FinalDataFrame <-left_join(x =read_tbl , y =FinalDataFrame , 
                  by = c("CaptureDate","FlightDate","FlightDate","FlightNumber","DOW",
                         "MonthName","DeptTime","ArriveTime","DirMkt","EquipmentCode",
                         "Capacity","Lid","TotalRevenue","LFpctage","NDO_20LF","MaxFare",
                         "MinFare","TimeBand","AvgFare","Pure_AvgFare","AvgRevenue"))
x <- which(is.na(FinalDataFrame$ClusterGroup) == TRUE ) # this serves to replace all values 


#Assigning value of 0 to any observations which had ClusterGroup, nu_LF, or nu_Rev = NA
FinalDataFrame$ClusterGroup[x] <- 0
FinalDataFrame$nu_LF[x] <- 0
FinalDataFrame$nu_Rev[x] <- 0

#Dedicated to exporting the #FinalDataFrame# we have just compiled back into our Desired Sandbox table in SQL for further Data analysis and cleansing.
sqlSave(SQLhandle, FinalDataFrame, tablename = "Sandbox.ClusterGroupDataFrame",
        fast = TRUE, append = TRUE, rownames= FALSE)
SQLClose <- odbcClose(SQLhandle) #this serves to close the connection to the Revenue Management SQL server, thus ending this iteration of our Clustering Script

