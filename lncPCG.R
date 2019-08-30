
#------------------------------------LR---------------------------------------

setwd("H:\\research\\interaction\\revision\\result")
library(pROC)

files<-dir("H:\\research\\interaction\\revision\\data\\train")
for(k in files )
{
  input<-paste("H:\\research\\interaction\\revision\\data\\train/",k,sep = "")
  read.table(input,sep = "\t",header = T)->data
  a<-rep(1,nrow(data)/2)
  b<-rep(0,nrow(data)/2)
  rbind(as.matrix(a),as.matrix(b))->c
  cbind(data,c)->data1
  ncol(data1)->n
  strsplit(k,"[.]")->new
  unlist(new)[1]->prefix
  for(i in 1:3)
  {
    start=1+82*(i-1)
    end=82*i
    testInd<-c(start:end,(247+82*(i-1)):(246+82*i))
    train<-data1[-testInd,]
    test<-data1[testInd,]
    train$c<-as.factor(as.character(train$c))
    test$c<-as.factor(as.character(test$c))
    model<-glm(c~.,family=binomial(link='logit'),data=train)
    p<-predict(model,newdata=test,type="response")
    data.frame(p)->p
    a1<-rep(1,82)
    b1<-rep(0,82)
    rbind(as.matrix(a1),as.matrix(b1))->c1
    cbind(p,c1)->data2
    paste("H:\\research\\interaction\\revision\\result/logistic","/prob",prefix,"_",i,".txt",sep = "")->out
    write.table(data2,out,sep = "\t",row.names = T,col.names = F,quote = F)
    
  }
  
  #pdfpath<-paste("H:\\research\\interaction\\revision\\result/logistic/",prefix,"_roc.auc.pdf",sep = "")
  #pdf(file=pdfpath)
  for(i in 1:3)
  {
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result/logistic/",prefix,"_",i,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    path<-paste("H:\\research\\interaction\\revision\\result/logistic","/prob",prefix,"_",i,".txt",sep = "")
    read.table(path,sep="\t",header=F)->data1
    a<-rep(1,82)
    b<-rep(0,82)
    rbind(as.matrix(a),as.matrix(b))->c
    cbind(data1,c)->data1
    r<-roc(data1$c,data1$V2,levels=c("0","1")) #auc<0.5,c("1","0")
    if((r$auc-0.5)<0)
    {
      r<-roc(data1$c,data1$V2,direction = "<") 
    }
    #xlab<-paste("Specificity",paste("(",i,")",sep = ""),collapse = "\t")
    
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
  }
  
}

#------------------------------------SVM---------------------------------------

library("e1071")
library(pROC)
setwd("H:\\research\\interaction\\revision\\result")
files<-dir("H:\\research\\interaction\\revision\\data\\train")

files<-dir("H:\\research\\interaction\\revision\\data\\train")
for(k in files )
{
  input<-paste("H:\\research\\interaction\\revision\\data\\train/",k,sep = "")
  read.table(input,sep = "\t",header = T)->data
  strsplit(k,"[.]")->new
  unlist(new)[1]->prefix
  
  for(i in 1:3)
  {
    start=1+82*(i-1)
    end=82*i
    testInd<-c(start:end,(247+82*(i-1)):(246+82*i))
    train<-data[-testInd,]
    test<-data[testInd,]
    a<--10:10
    b<--10:10
    gamma=2^(a);
    cost=2^(b);
    class<-c(rep(1,nrow(data)/2),rep(0,nrow(data)/2)); 
    class<-as.factor(class)
    obj<-tune.svm(data[-testInd,],class[-testInd],type="C", gamma=gamma,cost=cost);
    paste("H:\\research\\interaction\\revision\\result/SVM","/svm.result_",prefix,"_",i,".txt",sep = "")->name
    model<-obj$best.model;
    
    #test
    test<-predict(model,data[testInd,],decision.values = T);
    attr(test,"decision.values")->dc
    cbind(test,dc)->a
    as.matrix(a)->ss
    
    #result
    paste("H:\\research\\interaction\\revision\\result/SVM","/decision.values_",prefix,"_",i,".txt",sep = "")->name2
    write.table(ss,name2,sep = "\t",row.names = T,col.names = F,quote = F)
    test.table<-table(class[testInd],t(test));
    write.table(test.table,file=name,sep="\t")
    
  }
}


for(k in files )
{
  strsplit(k,"[.]")->new
  unlist(new)[1]->prefix
  for(i in 1:3)
  {
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result/SVM/",prefix,"_",i,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    
    path<-paste("H:\\research\\interaction\\revision\\result/SVM","/decision.values_",prefix,"_",i,".txt",sep = "")
    read.table(path,sep="\t",header=F)->data1
    a<-rep(1,82)
    b<-rep(0,82)
    rbind(as.matrix(a),as.matrix(b))->c
    cbind(data1,c)->data1
    r<-roc(data1$c,data1$V3,levels=c("0","1")) #auc<0.5,c("1","0")
    #xlab<-paste("Specificity",paste("(",i,")",sep = ""),collapse = "\t")
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
  }
}


#------------------------------------RF---------------------------------------
library(randomForest)
library(pROC)
setwd("H:\\research\\interaction\\revision\\result")
files<-dir("H:\\research\\interaction\\revision\\data\\train")

for(k in files )
{
  input<-paste("H:\\research\\interaction\\revision\\data\\train/",k,sep = "")
  read.table(input,sep = "\t",header = T)->data
  strsplit(k,"[.]")->new
  unlist(new)[1]->prefix
  a<-rep(1,nrow(data)/2)
  b<-rep(0,nrow(data)/2)
  rbind(as.matrix(a),as.matrix(b))->c
  cbind(data,c)->data1
  ncol(data1)->n
  for(i in 1:3)
  {
    start=1+82*(i-1)
    end=82*i
    testInd<-c(start:end,(247+82*(i-1)):(246+82*i))
    train<-data1[-testInd,]
    test<-data1[testInd,]
    train$c<-as.factor(as.character(train$c))
    test$c<-as.factor(as.character(test$c))
    model<-randomForest(c~.,data = train,mtry=3,ntree=1200,keep.forest=T)
    p<-predict(model,newdata=test,type="prob")
    as.matrix(p)->rr
    a1<-rep(1,82)
    b1<-rep(0,82)
    rbind(as.matrix(a1),as.matrix(b1))->c1
    cbind(rr,c1)->data2
    paste("H:\\research\\interaction\\revision\\result/RF","/prob",prefix,"_",i,".txt",sep = "")->name2
    write.table(data2,name2,sep = "\t",col.names = F,row.names = T,quote = F)
  }
  
  for(i in 1:3)
  {
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result/RF/",prefix,"_",i,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    path<-paste("H:\\research\\interaction\\revision\\result/RF","/prob",prefix,"_",i,".txt",sep = "")
    read.table(path,sep="\t",header=F)->roc_data
    r<-roc(roc_data$V4,roc_data$V3,levels=c("0","1")) #auc<0.5,c("1","0")
    #xlab<-paste("Specificity",paste("(",i,")",sep = ""),collapse = "\t")
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
  }
  
  
}
  

#--------------------------prediction-----------------------
library(randomForest)
library(e1071)

#train<-read.table("H:\\research\\interaction\\revision\\data\\train/train_inter_random1.txt",header = T,sep = "\t")
train_files<-dir("H:\\research\\interaction\\revision\\data\\train/")
test_files<-dir("H:\\research\\interaction\\revision\\data\\test")
for(i in train_files)
{
  train_path<-paste("H:\\research\\interaction\\revision\\data\\train/",i,sep = "")
  read.table(train_path,sep = "\t",header = T)->train
  strsplit(i,"[.]")->new
  unlist(new)[1]->train_prefix
  for(k in test_files)
  {
    input<-paste("H:\\research\\interaction\\revision\\data\\test/",k,sep = "")
    read.table(input,sep = "\t",header = T)->test
    strsplit(k,"[.]")->new
    unlist(new)[1]->test_prefix
    
    #SVM
    class<-c(rep(1,nrow(train)/2),rep(0,nrow(train)/2)); 
    class<-as.factor(class) 
    a<--10:10
    b<--10:10
    gamma=2^(a);
    cost=2^(b);
    obj<-tune.svm(train,class,type="C", gamma=gamma,cost=cost);
    model<-obj$best.model;
    res<-predict(model,test,decision.values = T);
    attr(res,"decision.values")->dc
    cbind(res,dc)->aa
    as.matrix(aa)->ss1
    paste("H:\\research\\interaction\\revision\\result\\test_result","/SVM_",train_prefix,"_",test_prefix,".txt",sep = "")->name2
    write.table(ss1,name2,sep = "\t",row.names = T,col.names = F,quote = F)
#SVM_roc
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result\\test_result","/SVM_",train_prefix,"_",test_prefix,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    path<-paste("H:\\research\\interaction\\revision\\result\\test_result","/SVM_",train_prefix,"_",test_prefix,".txt",sep = "")
    read.table(path,sep="\t",header=F)->data1
    e<-rep(1,nrow(ss1)/2)
    f<-rep(0,nrow(ss1)/2)
    rbind(as.matrix(e),as.matrix(f))->g
    cbind(data1,g)->data1
    r<-roc(data1$g,data1$V3,levels=c("0","1")) #auc<0.5,c("1","0")
    #xlab<-paste("Specificity",paste("(",i,")",sep = ""),collapse = "\t")
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
  
  #RF
    a1<-rep(1,nrow(train)/2)
    b1<-rep(0,nrow(train)/2)
    rbind(as.matrix(a1),as.matrix(b1))->c1
    cbind(train,c1)->train1
    train1$c1<-as.factor(as.character(train1$c1))
    # test$c<-as.factor(as.character(test$c))
    model<-randomForest(c1~.,data = train1,mtry=3,ntree=1200,keep.forest=T)
    p<-predict(model,newdata=test,type="prob")
    as.matrix(p)->rr
    a3<-rep(1,61)
    b3<-rep(0,61)
    rbind(as.matrix(a3),as.matrix(b3))->c3
    cbind(rr,c3)->data2
    paste("H:\\research\\interaction\\revision\\result\\test_result","/RF_",train_prefix,"_",test_prefix,".txt",sep = "")->name3
    write.table(data2,name3,sep = "\t",col.names = F,row.names = T,quote = F)
  #roc_RF
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result\\test_result","/RF_",train_prefix,"_",test_prefix,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    path<-paste("H:\\research\\interaction\\revision\\result\\test_result","/RF_",train_prefix,"_",test_prefix,".txt",sep = "")
    read.table(path,sep="\t",header=F)->roc_data
    r<-roc(roc_data$V4,roc_data$V3,levels=c("0","1")) #auc<0.5,c("1","0")
    #xlab<-paste("Specificity",paste("(",i,")",sep = ""),collapse = "\t")
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
    
  #LR
    model<-glm(c1~.,family=binomial(link='logit'),data=train)
    p<-predict(model,newdata=test,type="response")
    data.frame(p)->p
    a4<-rep(1,61)
    b4<-rep(0,61)
    rbind(as.matrix(a4),as.matrix(b4))->c4
    cbind(p,c4)->data4
    paste("H:\\research\\interaction\\revision\\result\\test_result","/LR_",train_prefix,"_",test_prefix,".txt",sep = "")->name4
    write.table(data4,name4,sep = "\t",col.names = F,row.names = T,quote = F)
  #roc_LR
    setEPS()
    EPSpath<-paste("H:\\research\\interaction\\revision\\result\\test_result","/LR_",train_prefix,"_",test_prefix,"_roc.auc.eps",sep = "")
    postscript(EPSpath)
    path<-paste("H:\\research\\interaction\\revision\\result\\test_result","/LR_",train_prefix,"_",test_prefix,".txt",sep = "")
    read.table(path,sep="\t",header=F)->data5
    r<-roc(data5$V3,data5$V2,levels=c("0","1")) #auc<0.5,c("1","0")
    plot.roc(r, col="red",xlab="Specificity",print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,cex.axis=1.5,cex.lab=1.5,print.thres.cex=1.5,print.auc.cex=1.5)
    dev.off()
    
    
  }
  
}
