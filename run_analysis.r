
#----------------------------------------------------------------------
# Function readData will read the data from files in "test" or "train" folders
# by:
# 1. setting the working directory to folderName
# 2. generating the filenames to read in folderName
# 3. reading the files from folderName
# 4. setting working directory to Inertial Signals
# 5. generating the filenames to read in Inertial Signals
# 6. reading the files from Inertial Signals
# 7. combining all the data read using cbind
# 8. setting working directory back 2 levels up
# 9. returning the combined data frame
#----------------------------------------------------------------------
readData <- function(folderName,featureLabels,activityLabels)
{
    setwd(folderName)
    filenames <- c("subject_","y_","X_")
    filenames <- lapply(filenames,paste,folderName,".txt",sep="")
    dataFrame <- lapply(filenames,read.table)
    dataFrame <- as.data.frame(dataFrame)
    dataFrame[[2]] <- factor(as.numeric(dataFrame[[2]]),labels=as.character(activityLabels))
    colnames(dataFrame)<-c("Subject","Activity",as.character(featureLabels))
    
    ## Go into folder Inertial Signals
    setwd("Inertial Signals")
    filenamesInInertialSignals <- c("body_acc_x_","body_acc_y_","body_acc_z_",
                               "body_gyro_x_","body_gyro_y_","body_gyro_z_",
                               "total_acc_x_","total_acc_y_","total_acc_z_")
    
    # generate the names of files to read data from
    filenamesInInertialSignals <- lapply(filenamesInInertialSignals,paste,folderName,".txt",sep="")
    
    # read data into table
    dataInertialSignals <- lapply(filenamesInInertialSignals,read.table)     
    dataFrame <- cbind(dataFrame,as.data.frame(dataInertialSignals))
    
    setwd("..")
    setwd("..")
    
    dataFrame  
}

featureLabels <- (read.table("features.txt"))[[2]]
activityLabels<-(read.table("activity_labels.txt"))[[2]]

#read data from "train" folder
trainData<-readData("train",featureLabels,activityLabels)

#read data from "test" folder
testData<-readData("test",featureLabels,activityLabels)

#merge the test and train data into mergedData
mergedData <- rbind(trainData,testData)

#----------------------------------------------------------------------
# Function extractData will extract the columns from mergedData which 
# matches the value of patternToExtract
# which is Activity, Subject, std() or mean()
#----------------------------------------------------------------------
extractData <- function(mergedData,patternToExtract)
{
    #retrieve measurements that have only std or mean
    dataExtract <- mergedData[grepl(patternToExtract,colnames(mergedData))]
    dataExtract
}

#extract data from Activity, Subject and columns containing std() and mean()
dataExtracted <- extractData(mergedData,"Activity|Subject|std\\(\\)|mean\\(\\)")

#----------------------------------------------------------------------
# Function relabelColumnNames will rename the variable names to be more 
# descriptive 
# 1. fBodyBody and fBody will be renamed to frequencyBody
# 2. tBody will be renamed to timeBody
# 3. tGravity will be renamed to timeGravity
# 4. () will be removed
#----------------------------------------------------------------------
relabelColumnNames <- function(dataExtracted)
{
    #generate more descriptive variable names
    colnames(dataExtracted) <- gsub("fBodyBody","fBody",colnames(dataExtracted))
    colnames(dataExtracted) <- gsub("tBody","timeBody",colnames(dataExtracted))
    colnames(dataExtracted) <- gsub("fBody","frequencyBody",colnames(dataExtracted))
    colnames(dataExtracted) <- gsub("tGravity","timeGravity",colnames(dataExtracted))
    colnames(dataExtracted) <- gsub("\\(\\)","",colnames(dataExtracted))
    dataExtracted
}

dataRelabeled <- relabelColumnNames(dataExtracted)

#----------------------------------------------------------------------
# Function generateMean will calculate the average of the measurement 
# columns for each subject and activity.
#----------------------------------------------------------------------
generateMean <- function(dataInput)
{
    # apply mean to all columns except the Activity and Subject
    dsAverage<-NULL
    for (s in unique(dataInput$Subject)){
        for (a in unique(dataInput$Activity))
        {
            df1 <- colMeans(subset(dataInput,dataInput$Subject == s & as.character(dataInput$Activity) == a,c(3:ncol(dataInput))))
            df2 <- data.frame(s,a,names(df1),as.vector(df1))
            dsAverage <- rbind(df2,dsAverage)
        }
    }
    names(dsAverage) <- c("Subject","Activity","Variable","Average")
    dsAverage
}

ds.average<-generateMean(dataRelabeled)
# write dataset 
write.table(ds.average[order(ds.average[[1]]),],file="datasetAverage.txt",row.names=FALSE,quote=FALSE,sep="\t")
