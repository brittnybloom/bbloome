setwd("~/Desktop/ ")
x<-read.table("genotype.array.all.txt", as.is=T, header=T)

x$Tissue <- sapply(x$Sample, function(x) 
  {temp=unlist(strsplit(x,"-")); y="None"; 
   if(length(temp)==2){y=temp[2]}; y })
marker<-unique(x$Marker)
layout(matrix(1:16,4,4,byrow=TRUE))
par(ask=FALSE)

for (i in 1:16) {
  currentdata<-subset(x, Marker==marker[i])
  plot(currentdata$RX, currentdata$RY,col=currentdata$Call+2, pch=19)
}

 #data with only positive calls

z<-read.table("genotype.array.all.txt", as.is=T, header=T)

z$Tissue <- sapply(z$Sample, function(z) 
  {temp=unlist(strsplit(z,"-")); y="None"; 
  if (length(temp)==2){z=temp[2]}; z })

marker<-unique(z$Marker)
layout(matrix(1:16,4,4,byrow=TRUE))
z$Tissue<-substring(z$Tissue, 7)
for (i in 1:16) {
    currentdata2<-subset(z, Marker==marker[i] & Call != -1)
    if (nrow(currentdata2) == 0) { 
      next
    }
    zcall<-currentdata2$Call
    plot(currentdata2$RX, currentdata2$RY,col=zcall, pch=19)
}

#data with only spleen tissue
a<-read.table("genotype.array.all.txt", as.is=T, header=T)

a$Tissue <- sapply(a$Sample, function(a) 
{temp=unlist(substring(a,7)); y="None"; 
 if (length(temp)==a){z=temp[a]}; a })

marker<-unique(a$Marker)
layout(matrix(1:16,4,4,byrow=TRUE))
a$Tissue <- substring(a$Tissue,7)
for (i in 1:16) {
  currentdata3<-subset(a, Marker==marker[i] & Call !=-1 & Tissue != "Tail")
  if (nrow(currentdata3) == 0) { 
    next
  }
  spleentissue<-currentdata3$Call
  plot(currentdata3$RX, currentdata3$RY,col=spleentissue, pch=19)
}

#data with only tail tissue
b<-read.table("genotype.array.all.txt", as.is=T, header=T)

b$Tissue <- sapply(b$Sample, function(b) 
{temp=unlist(strsplit(b,"-")); y="None"; 
 if (length(temp)==b){z=temp[b]}; b })

marker<-unique(b$Marker)
layout(matrix(1:16,4,4,byrow=TRUE))
b$Tissue <- substring(b$Tissue,7)

for (i in 1:16) {
  
  currentdata4<-subset(b, Marker==marker[i] & Call !=-1 & Tissue != "Spleen")
  if (nrow(currentdata4) == 0) { 
    next
  }
  tailtissue<-currentdata4$Call
  plot(currentdata4$RX, currentdata4$RY,col=tailtissue, pch=19)
}