###############################################################################################################
#function bv.step()
### found at:
#https://menugget.blogspot.com/2011/06/clarke-and-ainsworths-bioenv-and-bvstep.html
### see also: https://github.com/marchtaylor/sinkr

###############################################################################################################
### one of the reasons why a variable biological similarity matrix is often less explored with the routine is that
### the number of possible subset combinations becomes computationally overwhelming when the number of
### species/groups is large - the total number of combinations being equal to 2^n - 1, where n is the total
### number of variables. For this reason, Clarke and Warwick (1998) presented a stepwise routine (BVSTEP) for
### a faster exploration of the subset combinations. They specifically looked at the BIOBIO type exploration and
### addressed the concept of structural redundancy in community composition through the identification of
### "response units", or Taxonomic/functional groupings of species that changed in abundance in the same way over
### time. 
###
### I have created another function called bv.step() (below) to do this stepwise exploration according to the
### description of the "forward selection/backward elimination" algorithm described by Clarke and Warwick (1998).
### As an example of the function, a BIOBIO type exploration of the varespec data was conducted to identify a
### subset of species that best describe the overall community pattern. This was done in two steps where I first
### allowed the algorithm to run for 50 times, randomly selecting 30% of the 44 possible species for inclusion
### in the stepwise exploration. From this initial information I was able to identify some of the most important
### species that should always be included in the routine (i.e. inclusion of species 23, 26, and 29 correlated
### at rho=0.68 with the full set), and then performed a "second run" of bv.step() using the option
### "var.always.include=c(23,26,29)". In this way, I was able to arrive at a good representation of sample
### similarities using a much reduced number of species. By the way, this routine would have taken agonizingly
### long in bio.env() as there are 2^44-1 (1.759219e+13) possible combinations to test.

###
#first round
# res.bv.step.biobio <- bv.step(wisconsin(varespec), wisconsin(varespec), 
#                               fix.dist.method="bray", var.dist.method="bray",
#                               scale.fix=FALSE, scale.var=FALSE, 
#                               max.rho=0.95, min.delta.rho=0.001,
#                               random.selection=TRUE,
#                               prop.selected.var=0.3,
#                               num.restarts=50,
#                               output.best=10,
#                               var.always.include=NULL)
# > res.bv.step.biobio
# $order.by.best
# 
# #second round
# res.bv.step.biobio2  <- bv.step(wisconsin(varespec), wisconsin(varespec), 
#                                 fix.dist.method="bray", var.dist.method="bray",
#                                 scale.fix=FALSE, scale.var=FALSE, 
#                                 max.rho=0.95, min.delta.rho=0.001,
#                                 random.selection=TRUE,
#                                 prop.selected.var=0.2,
#                                 num.restarts=50,
#                                 output.best=10,
#                                 var.always.include=c(23,26,29))
###############################################################################################################
bv.step <- function(fix.mat, var.mat, 
                    fix.dist.method="bray", var.dist.method="euclidean",
                    scale.fix=FALSE, scale.var=TRUE,
                    max.rho=0.95,
                    min.delta.rho=0.001,
                    random.selection=TRUE,
                    prop.selected.var=0.2,
                    num.restarts=10,
                    var.always.include=NULL,
                    var.exclude=NULL,
                    output.best=10
){
  
  if(dim(fix.mat)[1] != dim(var.mat)[1]){stop("fixed and variable matrices must have the same number of rows")}
  if(sum(var.always.include %in% var.exclude) > 0){stop("var.always.include and var.exclude share a variable")}
  require(vegan)
  
  if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
  if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}
  
  fix.dist <- vegdist(as.matrix(fix.mat), method=fix.dist.method)
  
  #an initial removal phase
  var.dist.full <- vegdist(as.matrix(var.mat), method=var.dist.method)
  full.cor <- suppressWarnings(cor.test(fix.dist, var.dist.full, method="spearman"))$estimate
  var.comb <- combn(1:ncol(var.mat), ncol(var.mat)-1)
  RES <- data.frame(var.excl=rep(NA,ncol(var.comb)), n.var=ncol(var.mat)-1, rho=NA)
  for(i in 1:dim(var.comb)[2]){
    var.dist <- vegdist(as.matrix(var.mat[,var.comb[,i]]), method=var.dist.method)
    temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
    RES$var.excl[i] <- c(1:ncol(var.mat))[-var.comb[,i]]
    RES$rho[i] <- temp$estimate
  }
  delta.rho <- RES$rho - full.cor
  exclude <- sort(unique(c(RES$var.excl[which(abs(delta.rho) < min.delta.rho)], var.exclude)))
  
  if(random.selection){
    num.restarts=num.restarts
    prop.selected.var=prop.selected.var
    prob<-rep(1,ncol(var.mat))
    if(prop.selected.var< 1){
      prob[exclude]<-0
    }
    n.selected.var <- min(sum(prob),prop.selected.var*dim(var.mat)[2])
  } else {
    num.restarts=1
    prop.selected.var=1  
    prob<-rep(1,ncol(var.mat))
    n.selected.var <- min(sum(prob),prop.selected.var*dim(var.mat)[2])
  }
  
  RES_TOT <- c()
  for(i in 1:num.restarts){
    step=1
    RES <- data.frame(step=step, step.dir="F", var.incl=NA, n.var=0, rho=0)
    attr(RES$step.dir, "levels") <- c("F","B")
    best.comb <- which.max(RES$rho)
    best.rho <- RES$rho[best.comb]
    delta.rho <- Inf
    selected.var <- sort(unique(c(sample(1:dim(var.mat)[2], n.selected.var, prob=prob), var.always.include)))
    while(best.rho < max.rho & delta.rho > min.delta.rho & RES$n.var[best.comb] < length(selected.var)){
      #forward step
      step.dir="F"
      step=step+1
      var.comb <- combn(selected.var, RES$n.var[best.comb]+1, simplify=FALSE)
      if(RES$n.var[best.comb] == 0){
        var.comb.incl<-1:length(var.comb)
      } else {
        var.keep <- as.numeric(unlist(strsplit(RES$var.incl[best.comb], ",")))
        temp <- NA*1:length(var.comb)
        for(j in 1:length(temp)){
          temp[j] <- all(var.keep %in% var.comb[[j]]) 
        }
        var.comb.incl <- which(temp==1)
      }
      
      RES.f <- data.frame(step=rep(step, length(var.comb.incl)), step.dir=step.dir, var.incl=NA, n.var=RES$n.var[best.comb]+1, rho=NA)
      for(f in 1:length(var.comb.incl)){
        var.incl <- var.comb[[var.comb.incl[f]]]
        var.incl <- var.incl[order(var.incl)]
        var.dist <- vegdist(as.matrix(var.mat[,var.incl]), method=var.dist.method)
        temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
        RES.f$var.incl[f] <- paste(var.incl, collapse=",")
        RES.f$rho[f] <- temp$estimate
      }
      
      last.F <- max(which(RES$step.dir=="F"))
      RES <- rbind(RES, RES.f[which.max(RES.f$rho),])
      best.comb <- which.max(RES$rho)
      delta.rho <- RES$rho[best.comb] - best.rho 
      best.rho <- RES$rho[best.comb]
      
      if(best.comb == step){
        while(best.comb == step & RES$n.var[best.comb] > 1){
          #backward step
          step.dir="B"
          step <- step+1
          var.keep <- as.numeric(unlist(strsplit(RES$var.incl[best.comb], ",")))
          var.comb <- combn(var.keep, RES$n.var[best.comb]-1, simplify=FALSE)
          RES.b <- data.frame(step=rep(step, length(var.comb)), step.dir=step.dir, var.incl=NA, n.var=RES$n.var[best.comb]-1, rho=NA)
          for(b in 1:length(var.comb)){
            var.incl <- var.comb[[b]]
            var.incl <- var.incl[order(var.incl)]
            var.dist <- vegdist(as.matrix(var.mat[,var.incl]), method=var.dist.method)
            temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
            RES.b$var.incl[b] <- paste(var.incl, collapse=",")
            RES.b$rho[b] <- temp$estimate
          }
          RES <- rbind(RES, RES.b[which.max(RES.b$rho),])
          best.comb <- which.max(RES$rho)
          best.rho<- RES$rho[best.comb]
        }
      } else {
        break()
      }
      
    }
    
    RES_TOT <- rbind(RES_TOT, RES[2:dim(RES)[1],])
    print(paste(round((i/num.restarts)*100,3), "% finished"))
  }
  
  RES_TOT <- unique(RES_TOT[,3:5])
  
  
  if(dim(RES_TOT)[1] > output.best){
    order.by.best <- RES_TOT[order(RES_TOT$rho, decreasing=TRUE)[1:output.best],]
  } else {
    order.by.best <-  RES_TOT[order(RES_TOT$rho, decreasing=TRUE), ]
  }
  rownames(order.by.best)<-NULL
  
  order.by.i.comb <- c()
  for(i in 1:length(selected.var)){
    f1 <- which(RES_TOT$n.var==i)
    f2 <- which.max(RES_TOT$rho[f1])
    order.by.i.comb <- rbind(order.by.i.comb, RES_TOT[f1[f2],])
  }
  rownames(order.by.i.comb)<-NULL
  
  if(length(exclude)<1){var.exclude=NULL} else {var.exclude=exclude}
  out <- list(
    order.by.best=order.by.best,
    order.by.i.comb=order.by.i.comb,
    best.model.vars=paste(colnames(var.mat)[as.numeric(unlist(strsplit(order.by.best$var.incl[1], ",")))], collapse=","),
    best.model.rho=order.by.best$rho[1],
    var.always.include=var.always.include,
    var.exclude=var.exclude
  )
  out
  
}