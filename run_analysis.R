# Ssee if "reshape2" package installed (this script heavily relies on it)
if(!library(reshape2, logical.return = TRUE)) {
  # It did not exist, so install the package asap, and then load it
  install.packages('reshape2')
  library(reshape2)
}

# Initialize some initial values
targetFolder <- 'UCI HAR Dataset'
filename <- 'getdata_dataset.zip'

# Check if the file already unzipped
if(!file.exists(targetFolder)) {
  # Is the zip file at least present?
  if(!file.exists(filename)) {
    
    # Zip file not, so downlaod it asap
    download.file(
      'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
      filename
    )
  }
  
  # Now, un-zip the file 
  unzip(filename)
}

# 1. Merge the training and the test sets to make one data set.

  # Reading in the data into the test and training sets
  test.data <- read.table(file.path(targetFolder, 'test', 'X_test.txt'))
  test.activities <- read.table(file.path(targetFolder, 'test', 'y_test.txt'))
  test.subjects <- read.table(file.path(targetFolder, 'test', 'subject_test.txt'))
  
  train.data <- read.table(file.path(targetFolder, 'train', 'X_train.txt'))
  train.activities <- read.table(file.path(targetFolder, 'train', 'y_train.txt'))
  train.subjects <- read.table(file.path(targetFolder, 'train', 'subject_train.txt'))
  
  # Binding the rows for each of the data sets together
  data.data <- rbind(train.data, test.data)
  data.activities <- rbind(train.activities, test.activities)
  data.subjects <- rbind(train.subjects, test.subjects)
  
  # Now combining all of of the different columns together into one table
  full_data <- cbind(data.subjects, data.activities, data.data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

  # Grabbing the complete list of features
  features <- read.table(file.path(targetFolder, 'features.txt'))
  
  # Filtering to the features wanted
  requiredFeatures <- features[grep('-(mean|std)\\(\\)', features[, 2 ]), 2]
  full_data <- full_data[, c(1, 2, requiredFeatures)]

# 3. Using descriptive activity names to name the activities in the data set
  
  # Reading in the activity labels
  activities <- read.table(file.path(targetFolder, 'activity_labels.txt'))
  
  # Updating the activity name
  full_data[, 2] <- activities[full_data[,2], 2]
  
# 4. Appropriately labelling the data set with descriptive variable names. 
  
  colnames(full_data) <- c(
    'subject',
    'activity',
    # Removing the brackets from the features columns
    gsub('\\-|\\(|\\)', '', as.character(requiredFeatures))
  )
  
  # Coercing the data into strings
  full_data[, 2] <- as.character(full_data[, 2])
  
# 5. From the data set in step 4, creating a second, independent tidy data set with the average of each variable for every activity and each subject.

  # Melting the data so a unique row for each combination of subject and acitivites is present
  final.melted <- melt(full_data, id = c('subject', 'activity'))
  
  # Casting it getting the mean value
  final.mean <- dcast(final.melted, subject + activity ~ variable, mean)

  # Emitting the data out to a file
  write.table(final.mean, file=file.path("tidy.txt"), row.names = FALSE, quote = FALSE)
