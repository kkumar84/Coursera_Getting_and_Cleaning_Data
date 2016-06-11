##  The following R script gathers smartphone data from different text files 
##  and consolidates them into a tidy data set. The data pertains to accelormeter/
##  gyroscope readings during 6 types of physical activity : walking, walking upstairs,
##  walking downstairs, laying, standing, and sitting. 

##  This script needs as input the files :
##  X_test.txt, y_test.txt, subject_test.txt 
##  X_train.txt, y_train.txt, subject_train.txt
##  activity_labels.txt, features.txt.
##  The output of the analysis is the tidy data set stored in samsung_tidy_data.txt.

##  NOTE : This analysis file needs to have the same parent directory as UCI HAR Dataset. 

## Importing packages that make it easier to deal with dataframes
  library(dplyr)
  library(tidyr)
  
## IMPORTING DATA
  ## test set data
  x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  ## training set data
  x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  ## Instead of data frames we'll work with dataframe tables so we can use the 
  ## many of the useful functions provided by dplyr and tidyr
  x_test <- tbl_df(x_test)
  y_test <- tbl_df(y_test)
  subject_test <- tbl_df(subject_test)
  
  x_train <- tbl_df(x_train)
  y_train <- tbl_df(y_train)
  subject_train <- tbl_df(subject_train)
  
  ## repeating the above procedue with text files that contain descriptors 
  ## for the 6 activities and labels for the 561 features.
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  activity_labels <- tbl_df(activity_labels)
  
  feature_labels <- read.table("./UCI HAR Dataset/features.txt")
  feature_labels <- tbl_df(feature_labels)
  
  
## CHOOSING RELEVANT COLUMNS (MEAN, STD. DEVIATION)
  
  ## Since we need just the columns that contain mean or standard deviation of
  ## measurements we can look for those patterns in the feature label set.
  
  mean_std_vec <- grepl("mean\\(|std\\(",feature_labels[[2]])
  
  ##  subset of features. We obtain 66 out of 561 features.
  feature_subset <- feature_labels[mean_std_vec,]
  
  ## getting rid of the round brackets
  feature_subset[[2]] <- gsub("\\(\\)","", feature_subset[[2]])

  ## Choose relevant columns from x_test and y_test   
  x_test <- select(x_test, feature_subset[[1]])
  names(x_test) <- feature_subset[[2]]
  
  x_train <- select(x_train, feature_subset[[1]])
  names(x_train) <- feature_subset[[2]]

## COMBINING TRAINING AND TEST DATA    
  
  # combining features with activity labels and subjects
  testdata <- bind_cols(subject_test, y_test, x_test)
  traindata <- bind_cols(subject_train, y_train, x_train)
  
  names(traindata)[c(1,2)] <- c("Subject", "Activity_Code")
  names(testdata)[c(1,2)] <- c("Subject", "Activity_Code")
  
  # Combining the rows from training and test data
  data <- bind_rows(traindata, testdata)
  
  names(activity_labels) <- c("Activity_Code","Activity")
  
  data <- merge(activity_labels, data, by = "Activity_Code", sort = FALSE)
  
  ## the output of the above statement is a data frame, so we convert it to a data
  ## frame table
  data <- tbl_df(data)
  data <- data[,c(3,2,4:69)]

  
## TIDY DATA WITH AVERAGES FOR EACH SUBJECT & ACTIVITY COMBINATION    
  by_subj_act <- group_by(data, Subject, Activity)
  
## Replacing multiple measurements of a subject+activity combination with an average(mean) 
  by_subj_act <- summarize_each(by_subj_act, funs(mean), 3:68)

  ## changing column headers to reflect the averaging procedure we just conducted    
  names(by_subj_act)[3:68] <- sapply(names(by_subj_act)[3:68], function(x) paste("avg-",x,sep = "")) 
  

## WRITING OUTPUT   
  write.table(by_subj_act, file = "samsung_tidy_data.txt", row.names = FALSE, quote = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  