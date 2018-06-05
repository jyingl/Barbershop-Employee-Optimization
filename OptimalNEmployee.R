library(plotly)
library(queueing)

OptBarb <- function(minserv, arrivalrate, capacity) {
  mu <- 1/(minserv/60)
  lambda <- arrivalrate 
  
  input1 <- NewInput.MMCK(lambda=lambda, mu=mu, c=1, k=capacity)
  input2 <- NewInput.MMCK(lambda=lambda, mu=mu, c=2, k=capacity)
  input3 <- NewInput.MMCK(lambda=lambda, mu=mu, c=3, k=capacity)
  input4 <- NewInput.MMCK(lambda=lambda, mu=mu, c=4, k=capacity)
  input5 <- NewInput.MMCK(lambda=lambda, mu=mu, c=5, k=capacity)
  input6 <- NewInput.MMCK(lambda=lambda, mu=mu, c=6, k=capacity)
  input7 <- NewInput.MMCK(lambda=lambda, mu=mu, c=7, k=capacity)
  input8 <- NewInput.MMCK(lambda=lambda, mu=mu, c=8, k=capacity)
  
  # Create Model
  
  model1 <- QueueingModel(input1)
  model2 <- QueueingModel(input2)
  model3 <- QueueingModel(input3)
  model4 <- QueueingModel(input4)
  model5 <- QueueingModel(input5)
  model6 <- QueueingModel(input6)
  model7 <- QueueingModel(input7)
  model8 <- QueueingModel(input8)
  
  
  MeanWait <- c(Wq(model1),Wq(model2),Wq(model3),Wq(model4),Wq(model5),Wq(model6),Wq(model7),Wq(model8))
  MeanVarrivalrate <- c(VTq(model1),VTq(model2),VTq(model3),VTq(model4),VTq(model5),VTq(model6),VTq(model7),VTq(model8))
  
  ExpL <- matrix(c(seq(1:8),rep(0,8)),ncol=2)
  
  for (i in 1:8) {
    ExpL[i,2]<- (MeanWait[i]-.25)^2
  }
  min <- min(ExpL[,2])
  opt<- 0
  
  for (i in 1:8) {
    if (ExpL[i,2]==min) {
      opt <- i
    }
  } 
  return(opt)
}




analysis <- function(ms,Ms,arm,arM,mc,Mc){
  ar <- seq(arm,arM)
  la <- length(ar)
  sr <- seq(ms,Ms)
  l <- length(sr)
  seats <- seq(mc,Mc)
  ls <- length(seats)
  input <- matrix(c(sort(rep(sr,ls*la)),c(sort(rep(ar,(ls*l)/2)),sort(rep(ar,(ls*l)/2))),rep(seats,la*l)),ncol=3,byrow=FALSE)
  Employees<- mapply(OptBarb,input[,1],input[,2],input[,3])
  lr<-length(Employees)
  imdata <- as.data.frame(input)
  colnames(imdata) <- c("ServiceTime","ArrivalRate","Capacity")
  mdata <- cbind(imdata,Employees)
  return(mdata)
}

OptInfo <- function(minserv, arrivalrate, capacity) {
  mu <- 1/(minserv/60)
  lambda <- arrivalrate 
  
  input1 <- NewInput.MMCK(lambda=lambda, mu=mu, c=1, k=capacity)
  input2 <- NewInput.MMCK(lambda=lambda, mu=mu, c=2, k=capacity)
  input3 <- NewInput.MMCK(lambda=lambda, mu=mu, c=3, k=capacity)
  input4 <- NewInput.MMCK(lambda=lambda, mu=mu, c=4, k=capacity)
  input5 <- NewInput.MMCK(lambda=lambda, mu=mu, c=5, k=capacity)
  input6 <- NewInput.MMCK(lambda=lambda, mu=mu, c=6, k=capacity)
  input7 <- NewInput.MMCK(lambda=lambda, mu=mu, c=7, k=capacity)
  input8 <- NewInput.MMCK(lambda=lambda, mu=mu, c=8, k=capacity)
  
  # Create Model
  
  model1 <- QueueingModel(input1)
  model2 <- QueueingModel(input2)
  model3 <- QueueingModel(input3)
  model4 <- QueueingModel(input4)
  model5 <- QueueingModel(input5)
  model6 <- QueueingModel(input6)
  model7 <- QueueingModel(input7)
  model8 <- QueueingModel(input8)
  
  
  MeanWait <- c(Wq(model1),Wq(model2),Wq(model3),Wq(model4),Wq(model5),Wq(model6),Wq(model7),Wq(model8))
  MeanVarrivalrate <- c(VTq(model1),VTq(model2),VTq(model3),VTq(model4),VTq(model5),VTq(model6),VTq(model7),VTq(model8))
  
  
  ExpL <- matrix(c(seq(1:8),rep(0,8)),ncol=2)
  
  
  for (i in 1:8) {
    ExpL[i,2]<- (MeanWait[i]-.25)^2
  }
  min <- min(ExpL[,2])
  opt<- 0
  
  for (i in 1:8) {
    if (ExpL[i,2]==min) {
      opt <- i
    }
  } 
  print(ExpL)
  Report(model6)
}