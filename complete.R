complete<-function(directory, id=1:332) {
  setwd(paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/", directory, sep=""))
  # files<-list.files(path = paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/" , directory, sep=""), pattern = ".csv")
  files<-sprintf("%03d", id)
  df_final <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("id", "nobs")
  colnames(df_final) <- x
  # j<-1
  for(i in 1:length(files)) {
    # filename<-as.numeric(substr(files[i], 1, nchar(files[i])-4))
    # if (is.element(filename, id) ) { 
      filename<-paste(files[i],".csv",sep='')
      df<-do.call(rbind,lapply(filename,read.csv)) 
      df<-df[complete.cases(df),]
      index<-as.numeric(substr(filename, 1, nchar(filename)-4))
      df_final[i,1]<-index
      df_final[i,2]<-nrow(df)
      # j<-j+1
    # }
    }
 df_final
}