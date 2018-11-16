corr<-function(directory, threshold=0) {
  
setwd(paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/", directory, sep=""))
files<-list.files(path = paste("/Users/lathasanker/localdatasciencecoursera/datasciencecoursera/" , directory, sep=""), pattern = ".csv")

final_v<-c()

for(i in 1:length(files)) {
 
  df<-do.call(rbind,lapply(files[i],read.csv)) 
  df<-df[complete.cases(df),]
  if (nrow(df)>threshold) {
      cor_val<-cor(df["sulfate"], df["nitrate"], "everything", method = "pearson")
      final_v <- c(final_v, cor_val)
  }

}
final_v
}