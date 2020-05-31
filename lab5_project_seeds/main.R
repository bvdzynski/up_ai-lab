#####
#note! - differences to original file - class names (changed with regex):
#class name = variety of wheat
#1 (original dataset) = Kama (changed dataset) 
#2 (original dataset) = Rosa (changed dataset)
#3 (original dataset) = Canadian (changed dataset)

##############################
#trainNeuralNetwork function
##############################

trainNeuralNetwork <- function(neuralCount){
  #training, gradient method
  nn <- neuralnet(Kama + Rosa + Canadian ~
                    area + perimeter + compactness + kernel_length + kernel_width + asymmetry_coefficient + length_kernel_groove,
                  data = neuralNet_seedsTrain,
                  hidden = c(neuralCount),
                  stepmax = 1e+06,
                  threshold = 0.1
  )
  
  #plot(nn)
  
  mypredict <- compute(nn, seedsValidation[-c(8)])$net.result
  maxidx <- function(arr) {
    return(which(arr == max(arr)))
  }
  idx <- apply(mypredict, c(1), maxidx)
  prediction <- c('Kama', 'Rosa', 'Canadian')[idx]
  print("Confusion matrix:")
  print(table(prediction, seedsValidation$class))
  A<-as.matrix(table(prediction, seedsValidation$class))
  print("Recognition rate:")
  print(sum(diag(A))/sum(A))
  cat("\n\n\n")
}

##############################
#main
##############################

library(neuralnet)

neuralCountVector = c(5, 10, 15, 20, 25)

#read file
seeds <- read.table("...\\lab5_project_seeds\\data\\seeds_dataset_changed.data", header = FALSE, dec=".")

#setup columns
colnames(seeds) <- c('area',
                     'perimeter',
                     'compactness',
                     'kernel_length',
                     'kernel_width',
                     'asymmetry_coefficient',
                     'length_kernel_groove',
                     'class')

table(seeds$class)

#setup training and test data
set.seed(101) #random generator
size.sample <- floor(1/2 * nrow(seeds))
samples_id <- sample(1:nrow(seeds), size.sample)

#sets
seedsTrain <- seeds[c(samples_id),]
seedsValidation <- seeds[-c(samples_id),]

#init
neuralNet_seedsTrain <- seedsTrain

neuralNet_seedsTrain$Kama <- seedsTrain$class == 'Kama'
neuralNet_seedsTrain$Rosa <- seedsTrain$class == 'Rosa'
neuralNet_seedsTrain$Canadian <- seedsTrain$class == 'Canadian'

for (i in 1:length(neuralCountVector)) {
  print(paste("Neural network number", i, "- number of neurals: ", neuralCountVector[i], sep=" "))
  neuralCount = neuralCountVector[i]
  trainNeuralNetwork(neuralCount)
}