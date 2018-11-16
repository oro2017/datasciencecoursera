pollutantmean<-function(directory="specdata", pollutant, id = 1:332) {
  setwd(paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/", directory, sep=""))
  files<-list.files(path = paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/" , directory, sep=""), pattern = ".csv")
  df_total = data.frame()
  for(i in 1:length(files)) {
    filename<-as.numeric(substr(files[i], 1, nchar(files[i])-4))
    if (is.element(filename, id) ) {      
      df<-do.call(rbind,lapply(files[i],read.csv)) 
      df_total <- rbind(df_total,df)
    }
  }
  mean(df_total[[pollutant]],na.rm=TRUE)
}

