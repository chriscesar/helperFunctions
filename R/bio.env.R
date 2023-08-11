###############################################################################################################
#function bio.env()
### found at:
#https://menugget.blogspot.com/2011/06/clarke-and-ainsworths-bioenv-and-bvstep.html
### see also: https://github.com/marchtaylor/sinkr

###############################################################################################################
### Due to the inflexibility of the bioenv() function, one has little control over how the
### variable similarity matrix is calculated (derived from the environmental subsets in the
### above example) as the method assumes the subset data to be environmental and that the resulting
### similarity matrix should be based on normalized "euclidean" distances. This makes sense with
### environmental data where one normalizes the data to remove the effect of differing units between
### parameters, yet in cases where the variable matrix is biological one might want more flexibility
### (a Bray-Curtis measure of similarity is common given its non-parametric nature). The vegan function
### vegdist() comes with many other possible indices that could be applied ("manhattan", "euclidean",
### "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford",
### "raup", "binomial" and "chao"). For example, beyond the typical biological to environmental
### comparison (BIOENV setup), one can also use the routine to explore other other types of relationships; e.g.:
###
### ENVBIO: subset of biological variables that best correlate to the overall environmental pattern
### BIOBIO: subset of biological variables that best correlate to the overall biological pattern
### ENVENV: subset of environmental variables that best correlate to the overall environmental pattern

### In the latter two examples one can identify a smaller subset of variables that best capture the overall
### sample similarities to a defined level of correlation. For the ENVBIO and BIOBIO cases, a more flexable
### setup is desired. I have written a version of the routine called bio.env() (below) that allows this
### flexibility. The output of te results are similar to the bioenv() function, yet also include a dataframe
### of the top correlated variable combinations.
###############################################################################################################

bio.env <- function(fix.mat, var.mat, 
                    fix.dist.method="bray", var.dist.method="euclidean",
                    scale.fix=FALSE, scale.var=TRUE,
                    output.best=10,
                    var.max=ncol(var.mat)
){
  if(dim(fix.mat)[1] != dim(var.mat)[1]){stop("fixed and variable matrices must have the same number of rows")}
  if(var.max > dim(var.mat)[2]){stop("var.max cannot be larger than the number of variables (columns) in var.mat")}
  
  require(vegan)
  
  combn.sum <- sum(factorial(ncol(var.mat))/(factorial(1:var.max)*factorial(ncol(var.mat)-1:var.max)))
  
  if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
  if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}
  fix.dist <- vegdist(fix.mat, method=fix.dist.method)
  RES_TOT <- c()
  best.i.comb <- c()
  iter <- 0
  for(i in 1:var.max){
    var.comb <- combn(1:ncol(var.mat), i, simplify=FALSE)
    RES <- data.frame(var.incl=rep(NA, length(var.comb)), n.var=i, rho=0)
    for(f in 1:length(var.comb)){
      iter <- iter+1
      var.dist <- vegdist(as.matrix(var.mat[,var.comb[[f]]]), method=var.dist.method)
      temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
      RES$var.incl[f] <- paste(var.comb[[f]], collapse=",")
      RES$rho[f] <- temp$estimate
      if(iter %% 100 == 0){print(paste(round(iter/combn.sum*100, 3), "% finished"))}
    }
    
    order.rho <- order(RES$rho, decreasing=TRUE)
    best.i.comb <- c(best.i.comb, RES$var.incl[order.rho[1]])
    if(length(order.rho) > output.best){
      RES_TOT <- rbind(RES_TOT, RES[order.rho[1:output.best],])
    } else {
      RES_TOT <- rbind(RES_TOT, RES)
    }
  }
  rownames(RES_TOT)<-NULL
  
  if(dim(RES_TOT)[1] > output.best){
    order.by.best <- order(RES_TOT$rho, decreasing=TRUE)[1:output.best]
  } else {
    order.by.best <- order(RES_TOT$rho, decreasing=TRUE)
  }
  OBB <- RES_TOT[order.by.best,]
  rownames(OBB) <- NULL
  
  order.by.i.comb <- match(best.i.comb, RES_TOT$var.incl)
  OBC <- RES_TOT[order.by.i.comb,]
  rownames(OBC) <- NULL
  
  out <- list(
    order.by.best=OBB,
    order.by.i.comb=OBC,
    best.model.vars=paste(colnames(var.mat)[as.numeric(unlist(strsplit(OBB$var.incl[1], ",")))], collapse=",") ,
    best.model.rho=OBB$rho[1]
  )
  out
}