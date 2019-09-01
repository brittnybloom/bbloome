setwd("~/Desktop/ ")
array<-read.table("genotype.array.all.txt", as.is=T, header=T)

array$Tissue <- sapply(array$Sample, function(array) 
  {temp=unlist(strsplit(array,"-")); y="None"; 
    if(length(temp)==2)
    {y=temp[2]};y
  })

array$Sample <- sapply(strsplit(as.character(array$Sample), "-"), "[", 1)
array$Call<- gsub(-1, NA, array$Call)

caTail <- subset(array,Tissue == "Tail")
caSpleen <- subset(array,Tissue == "Spleen")

match(unique(caTail$Sample),unique(caSpleen$Sample))
samples<-as.data.frame(match(unique(caTail$Sample), unique(caSpleen$Sample)))
markers<-unique(array$Marker)
spleenx<-unique(caSpleen$Sample[match(caTail$Sample,caSpleen$Sample)])
tailx<-unique(caTail$Sample[match(caSpleen$Sample, caTail$Sample)])

spleen<-data.frame()
for (i in 1:length(caSpleen[,2])){
  sample<-caSpleen$Sample[i]
  if (sample %in% spleenx){
  mark<-caSpleen$Marker[i]
  spleen[sample,mark]<-caSpleen$Call[i]
}
}

tail<-data.frame()
for (i in 1:length(caTail[,2])){
  sample<-caTail$Sample[i]
  if (sample %in% tailx){
  mark<-caTail$Marker[i]
  tail[sample,mark]<-caTail$Call[i]
}
}
M<- merge(spleen,tail,by='row.names')
Mnew <- sapply(M,as.numeric)
S<-(Mnew[,grep("*\\.x$",names(M))] != Mnew[,grep("*\\.y$", names(M))])
cbind(M[,1,drop=FALSE],S)
spleenNtaildiff<-cbind(M[,1,drop=FALSE],S)
means<-rowMeans(spleenNtaildiff[2:17], na.rm=T)
color = c("red", "orangered2", "orange", "yellow", "green", "seagreen3", "blue", "slateblue4", "purple", "darkmagenta")
hist(means, main="mean of differences in tail and spleen tissue", xlab="percentage of spleen and tail tissues which do not match", col=color)


