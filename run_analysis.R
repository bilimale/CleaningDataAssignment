library(data.table)

getAssignmentData <- function() {
  
  #Check if the newly created directory is created; else fail
  if (dir.exists("./CleaningAssignment") == FALSE) quit(status = 1)
  
  # Get the working directory and read the data.
  dir.create("./CleaningAssignment", showWarnings = TRUE, mode = "0777")
  
  #Download the file and unzip it
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  download.file(url, destfile = "./CleaningAssignment/DataSet.zip")
  
  #Change working directory
  setwd("./CleaningAssignment")
  
  #Unzip command for linix system, abort otherwise
  try(system("unzip -o DataSet.zip",intern = TRUE))
  
  #We should be ready to go now.
  return
}

# Goal to read and merge assignment data
readMergeAssignmentData <- function() {
  unzipDir <- "./UCI HAR Dataset" 
  unzipDirTrain <- "./UCI HAR Dataset/train"
  unzipDirTest <- "./UCI HAR Dataset/test"
  
  listofAllFiles <- as.data.table(list.files(unzipDir, all.files = TRUE, full.names = TRUE,recursive = TRUE, include.dirs = TRUE))
  listofTrainFiles <- as.data.table(list.files(unzipDirTrain, all.files = TRUE,recursive = FALSE, full.names = TRUE))
  listofTestFiles <- as.data.table(list.files(unzipDirTest, all.files = TRUE,recursive = FALSE, full.names = TRUE))
  
  #Subject Data
  readTrainSubjects <- as.data.table(fread(as.character(listofTrainFiles[4])))
  readTestSubjects <- as.data.table(fread(as.character(listofTestFiles[4])))
  
  # Y data
  readActivityTrain <- as.data.table(fread(as.character(listofTrainFiles[6])))
  readActivityTest  <- as.data.table(fread(as.character(listofTestFiles[6])))
  # X data
  readXTrain <- as.data.table(fread(as.character(listofTrainFiles[5])))
  readXTest  <- as.data.table(fread(as.character(listofTestFiles[5])))
  
  #Start the merge
  mergeSubjects <- rbind(readTrainSubjects, readTestSubjects)
  mergeActivity <- rbind(readActivityTrain, readActivityTest)
  mergeReading <- rbind(readXTrain, readXTest)
  
  # Merge the columns now
  setnames(mergeSubjects, "V1", "subject")
  setnames(mergeActivity, "V1", "activity")
  
  subjectActivity <- cbind(mergeSubjects, mergeActivity)
  datamerge <- cbind(subjectActivity, mergeReading)
  
  #Return the data for further cleaning
  datamerge
}

#
# Add in the label and clean the data
#
labelCleanMergedData  <- function(mergedData) {
  descActivityName <- as.data.table(fread(as.character(listofFiles[3])))
  
  #Add activity and column to get the names correctly
  sudoColumn <- as.data.table(c(0,0))
  setnames(sudoColumn, "V1", "V1")
  
  secondColumn <- as.data.table(c("activity", "activityN"))
  setnames(secondColumn, "V1", "V2")
  
  allColumnNames <- rbind(cbind(sudoColumn, secondColumn), descActivityName)
  
  #Starting the the clean data table
  cleanData <- as.data.table(mergedData[,subject])
  setnames(cleanData, "V1", "subject")
  cleanData <- cbind(cleanData, mergedData[, activity] )
  print(colnames(cleanData))
  print(head(cleanData))
  
  setnames(cleanData, "V2", "activity")
  
  #Set the names for all the columns
  for(n in seq_along(mergedData)) {
    colSelected <- paste("V", allColumnNames[n,V1], sep = "" )
    colLabeled <-  allColumnNames[n,V2]
    
    if(colSelected == "V0")  {
      print("Skipping")
      next
    }  else {
      
      #if the column contains mean or std, cbind to cleandata
      if(grepl(colLabeled, pattern = "mean\\(\\)|std\\(\\)", ignore.case = FALSE)) {
        cleanData <- cbind(cleanData, as.data.table(mergedData[, get(colSelected)]))
        setnames(cleanData, "V1", colLabeled )
        
      }
      setnames(mergedData, colSelected, colLabeled)
    }
  }
  #print(head(colnames(cleanData)))
  cleanData
}

#
# Add in the description for the activity
#
descriptionActivity <- function(cleanData)  {
  
}

#
# Generate the mean for each activty for each subject
#
subjectAverage <- function(cleanData) {
  totalSubject <- as.data.table(sort(unique(cleanData$subject)))
  totalActivity <- as.data.table(sort(unique(cleanData$activity)))
  
  averageSubjectActivity <- data.table()
  
  for(n in seq(totalSubject)) {
    for(k in seq_along(totalActivity)) {
      calculateAverage <- as.data.table(lapply(fn$sqldf("select * from cleanData where subject = \" $n \" and  activity =\" $k \"" ), mean))
      averageSubjectActivity <- rbind(averageSubjectActivity, calculateAverage)
      #print(lapply(fn$sqldf("select * from cleanData where subject = \" $n \" and  activity =\" $k \"" ), mean))  
      print(n)
      print(k)
    }
  }
  
}

mergedData <- readMergeAssignmentData()
cleanData <- labelCleanMergedData(mergedData)
