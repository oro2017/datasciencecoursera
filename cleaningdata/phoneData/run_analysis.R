mergeTestTrain<-function() {
  setwd("/Users/lathasanker/localdatasciencecoursera/cleaningdata/phoneData")
  testData<-read.table("test/X_test.txt", header=FASLE)
  trainData<-read.table("train/X_train.txt", header=FALSE)
  
}