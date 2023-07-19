library(MASS)
library(sna)

BASEDIR<-"~/Dropbox/Research/DBA/MB/Graphs/"
graph<- function(name,directed=FALSE) {
  return (list(name=name,adjacency=matrix(0,0,0)))
}

gE<- function(m) {
  return (length(which(m>0)))
}

gEmax<- function(m) {
  return (gN(m)*(gN(m)-1))
}

gEmin<- function(m) {
  return (gN(m)-1)
}

gN<- function(m) {
  return (nrow(m))
}

gDensity<- function(m) {
  return ((gE(m)-gEmin(m))/(gEmax(m)-gEmin(m)))
}

gConnectedness<- function(m) {
  v<-c()
  for (i in (1:nrow(m))) {
    v <- append(v, length(which(m[i,]>0)))
  }
  return (mean(v))

}

gMinor<- function(m, order) {
  res <- matrix(0, nrow=nrow(m)-1, ncol=ncol(m)-1)
  res<-m[-order, -order]
  return (res)

}

gLaplace<- function(m) {
  d <- gInDeg(m)
  L <- gSum(d, gScalar(gAdj(m),-1))
  return (L)
}

gLaplacianFactor<- function(m) {
  L<-gLaplace(m)
  res<- -Inf
  for (i in (1:ncol(L))) {
    Lminor <- gMinor(L,i)
    res<-det(Lminor)
    if (res>0)
      break
  }
  return (res)
}


gBetweennes<- function(m) {
  return (betweenness(m))
}


gAvrgDeg<- function(m) {
  md <- gDeg(m)
  v<-c()
  for (i in (1:nrow(md))) {
    v <- append(v, md[i,i])
  }
  return (mean(v))
}

gWAvrgDeg<- function(m) {
  md <- gWDeg(m)
  v<-c()
  for (i in (1:nrow(md))) {
    v <- append(v, md[i,i])
  }
  return (mean(v))
}



gLoadTOY <- function(group) {
  res<-data.matrix(read.delim(paste(BASEDIR, group, ".FREQ.tsv",sep ="")))
  rownames(res) <- colnames(res)
  for (row in (1:nrow(res))){
    for (col in (1:ncol(res))){
      if (abs(res[row,col]) >1000*10000) {
        res[row,col]<-0
      }
    }
  }
  return(g2Matrix(res))
}


gFilter<-function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (abs(m[row,col]) >1000*10000) {
        res[row,col]<-0
      } else       if (abs(m[row,col]) < 1./100000) {
        res[row,col]<-0
      } else{
        res[row,col]<-m[row,col]
      }
    }
  }
  return (res)
}

gScalar<-function(m, d) {
  return (m*d)
}

gSum<-function(m1, m2) {
  res<-m1+m2
  colnames(res) <- colnames(m1)
  rownames(res) <- rownames(m2)
  return (gFilter(res))
}

gDiff<-function(m1, m2) {
  return (gSum(m1,gScalar(m2,-1)))
}


gProd<-function(m1, m2) {
  res<-m1%*%m2
  colnames(res) <- colnames(m2)
  rownames(res) <- rownames(m1)
  return (gFilter(res))
}


gInverse <-function(m) {
  res <- ginv(m)
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  return (res)
}

g2Matrix<-function(data) {
  resa<-data.matrix(data)
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  return (res)
}



# AutoInverse<-function(m) {
#   res<-data.matrix(0,nrow(m),ncol(m);
#   for(i in(1:nrow(M
# }


gBinarize<-function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (m[row,col]==0) {
        res[row,col]<-0
      } else {
        res[row,col]<-1
      }
    }
  }
  return (res)
}


gComplement<-function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (m[row,col]==0) {
        res[row,col]<-1
      } else {
        res[row,col]<-0
      }
    }
  }
  return (res)
}


gAdj<-function(m)
  return (gBinarize(m))

gMaxClosure<-function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      res[row,col]<-m[row,col]
    }
  }
  for (k in (1:nrow(res)))
    for (row in (1:nrow(res))){
      for (col in (1:ncol(res))) {
        if (res[row,k]*res[k,col] > 0 &
            res[row,k]+res[k,col] > res[row,col]){
          res[row,col] <- res[row,k]+res[k,col]
        }
      }
    }
  return(res)
}

gDAG<-function(graph) {
  m <- dotgetMatrix(graph)
  mc <-gMinClosure(m)
  npaths <-0
  n <- 0
  for (node in (1:nrow(mc))) {
    if (sum(mc[node,])==0) {
        ad<-dotgetAllDepths(graph,node)
        npaths <- npaths + length(ad)
        n<-n+1
    }
  }
  return(npaths/n)
}

gWDAG<-function(graph) {
  m <- dotgetMatrix(graph)
  mc <-gMinClosure(m)
  npaths <-0
  n <- 0
  for (node in (1:nrow(mc))) {
    if (sum(mc[node,])==0) {
      ad <- dotgetAllDepths(graph,node)
      npaths <- npaths + mean(ad)
      n<-n+1
    }
  }
  return(npaths/n)
}


gMinClosure<-function(m) {
  mymax <- 10^100
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (m[row,col] == 0) {
        res[row,col]<-m[row,col]
      }else {
        res[row,col]<-m[row,col]
      }
    }
  }
  for (k in (1:nrow(res)))
    for (row in (1:nrow(res))){
      for (col in (1:ncol(res))) {
        if (res[row,k]*res[k,col] > 0 &
            (res[row,k]+res[k,col] < res[row,col] | res[row,col] == 0)){
          res[row,col] <- res[row,k]+res[k,col]
        }
      }
    }
  return(res)
}


gRotateLeft <- function(x) t(apply(x, 2, rev))

gIdentity<-function(order=1, cnames=c()) {
  res <- matrix(0, nrow=order, ncol=order)
  colnames(res) <- cnames
  rownames(res) <- cnames
  for (row in (1:nrow(res))){
    for (col in (1:ncol(res))) {
      if (row != col) {
        res[row,col]<-0
      } else {
        res[row,col]<-1
      }
    }
  }
  return (res)
}

gHamilton<- function(m, visited=c(1), tovisit=(2:ncol(m)), solutions=c(3:ncol(m), by = 2), cost=0, ind="|   ") {
  solution <- list()
  bestsol <- list()
  partsol <- list()
  cat(ind,"--(",visited,")---(",cost,")---->",tovisit,"<\n")
  # readline(prompt="Press [enter] to continue")
  if (length(tovisit)==0 || length(solutions)==0) {
    cat(ind,"All visited\n")
    solution[[2]] <- c(cost)
    solution[[1]] <- visited
  } else{
    parent <- visited[length(visited)]
    succesors <- c()
    for (col in (1:length(tovisit))) {
      if (m[parent,tovisit[col]]>0 & parent != tovisit[col])
        succesors <- c(succesors, c(tovisit[col]))
    }

    if (length(succesors)>0) {
      bestsol[[1]] <- c()
      bestsol[[2]]<-c(Inf)
      cat(ind, "Found succcesors:",succesors,"\n")
      for (succ in (1:length(succesors))) {
        cat(ind,"Trying ", succesors[succ],"\n")
        nnext <- succesors[-c(succ)]
        partsol <-graphHamilton(m,c(visited,c(succesors[succ])), tovisit[! tovisit %in% c(succesors[succ])], solutions[! solutions %in% c(succesors[succ])], cost+m[parent,succesors[succ]], paste(ind,"|   "))
        if (partsol[[2]][1] < bestsol[[2]][1]) {
          bestsol <- partsol
        }
      }
    } else {
      cat(ind,"No more successors found, left: ", tovisit,"\n")

      bestsol[[1]] <- visited
      if (length(tovisit)==0)
        bestsol[[2]]<-c(cost)
      else
        bestsol[[2]]<-c(Inf)
    }
    solution<-bestsol
  }
  # solution[[3]]<-c()
  # for (n in solution[[1]])
  #   solution[[3]]<- c(solution[[3]], c(colnames(m)[solution[[1]]]))
  # cat(ind,solution[[3]],"   SOLUTION: ",solution[[1]],"-",solution[[2]][1],"\n")
  if (solution[[2]][1]<Inf) {
    cat("   SOLUTION: ",solution[[1]],"-",solution[[2]][1], " --> ")
    for (n in solution[[1]])
      cat( c(colnames(m)[n],"->"))
    cat("\n")
  }
  return(solution)
}

gUpper<-function(m, order=-1) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (order < 0) {
        if (row ==1) {
          res[row,col]<-1
        } else {
          res[row,col]<-0
        }
      } else {
        if (row == 1 & col == order) {
          res[row,col]<-1
        } else
          res[row,col] <- 0
      }
    }
  }
  return(res)
}


gIdentityVector<- function(m) {
  # res <- matrix(0,  nrow=nrow(m), ncol=1)
  # for (row in (1:nrow(m)))  {
  #   if (row > 1 & row %% 2 != 0)
  #     res[row,1]<-1
  #   else
  #     res[row,1]<-0
  # }
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m)))  {
    for (col in (1:ncol(m)))  {
      if (col > 1 & col %% 2 != 0 && row==1)
        res[row,col]<-1
      else
        res[row, col]<-0
    }
  }
  return(res)
}

gRootSIM<- function(m) {
  res <- matrix(0,  nrow=1, ncol=ncol(m) )
  colnames(res) <- colnames(m)
  for (col in (1:ncol(m)))  {
    if (col > 1 & col%%2==1)
      res[1,col]<-trunc((col/2))
    else
      res[1, col]<-0
  }
  res
}


gLoad <- function(group) {
  res<-data.matrix(read.delim(paste(BASEDIR, group, ".tsv",sep =""))[,(1:19)])
  rownames(res) <- colnames(res)
  for (row in (1:nrow(res))){
    for (col in (1:ncol(res))){
      if (abs(res[row,col]) >1000*10000) {
        res[row,col]<-0
      }
    }
  }
  res
}


gOperative <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      if (row == col)
        res[row,col] <- m[1,col]
    }
  }
  return (res)
}


gNormalize <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  v<-max(m)
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      res[row,col] <- m[row,col]/v
    }
  }
  return(res)
}

gOnlySolutions <- function(m) {
  res <- matrix(0, nrow=1, ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- c("Sol")
  for (col in (1:ncol(m))) {
    if (col>1 && col%%2==1)
      res[1,col] <- 1
    else
      res[1,col] <- 0
  }
  return (res)
}

gOutDeg <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for ( row in (1:nrow(res))) {
    # res[row,row] <- sum(m[,row])
    res[row,row] <- length(which(m[row,]>0))
  }
  return (res)
}

gInDeg <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for ( row in (1:nrow(res))) {
    # res[row,row] <- sum(m[,row])
    res[row,row] <- length(which(m[,row]>0))
  }
  return (res)
}

gDeg <- function(m) {
  return (gSum(gInDeg(m), gOutDeg(m)))
  # return (graphF2ID(m)%+%graphF2OD(m))
}


gWOutDeg <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for ( row in (1:nrow(res))) {
    # res[row,row] <- sum(m[,row])
    res[row,row] <- sum(which(m[row,]>0))
  }
  return (res)
}

gWInDeg <- function(m) {
  res <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  for ( row in (1:nrow(res))) {
    # res[row,row] <- sum(m[,row])
    res[row,row] <- sum(which(m[,row]>0))
  }
  return (res)
}

gWDeg <- function(m) {
  return (gSum(gWInDeg(m), gWOutDeg(m)))
  # return (graphF2ID(m)%+%graphF2OD(m))
}


# gID <- function(m) {
#   res <- matrix(0, nrow=1, ncol=ncol(m))
#   for (col in (1:ncol(m))) {
#     if (col>1 && col%%2==1)
#       res[1,col] <- 1
#     else
#       res[1,col] <- 0
#   }
#   res
# }
#

gModule <- function(m) {
  sqrt(sum(m^2))
}



gP<- function(m) {
  sum(gOnlySolutions(m) %*% grBinarize(gOperative(m)))
}
#
# gEffort <- function(m) {
# #  sum(graphOnlySolutions(m) %*% graphNormalize(graphOperative(m)))
#   graphModule(graphOnlySolutions(m) %*% graphNormalize(graphOperative(m)))
# }

gToLatex <- function(m) {
  header<-"{c"
  for (col in (2:ncol(m))) {
    header <- paste(header,"c")
  }
  header <- paste (header,"}")
  res <- paste("{\\scriptsize $$\\left( \\begin{array}",header,"\n")
  for (row in (1:nrow(m))){
    for (col in (1:ncol(m))) {
      cell <- round(m[row,col], 3)
      if (row == col) {
        cell <- paste("{\\mathbf ",cell, "}")
      }
      res <- paste(res, cell)
      if (col == ncol(m)) {
        res <- paste(res, "\\\\\n")
      } else{
        res <- paste(res, "&")
      }
    }
  }
  res <- paste(res, "\\end{array}\\right)$$}")
  cat(res)
}

gSimmilarity <- function(m) {
  cosine(as.vector(gRootSIM(m)), as.vector(gOnlySolutions(m) %*% gOperative(m)))
}


gs <- function(m) {
  return (sum(m))
}



gp <- function(m) {
  return (gBinarize(gProd(gOnlySolutions(Mc),gInDeg(Mc))))
}

got <- function(m) {

  return(
    gProd(gComplement(gOnlySolutions(m)),gOperative(gMaxClosure(m)))
  )
  # return (gScalar(gSum(
  #   gProd(gComplement(gOnlySolutions(m)),gOperative(gMinClosure(gAdj(m)))),
  #         gProd(gComplement(gOnlySolutions(m)),gOperative(gMaxClosure(gAdj(m))))),0.5)
  #         )
  # return (gScalar(gSum(
  #   gProd(gComplement(gOnlySolutions(m)),gOperative(gMinClosure(m))),
  #   gProd(gComplement(gOnlySolutions(m)),gOperative(gMaxClosure(m)))),0.5)
  # )
}

gst <- function(m) {
  return(
    gProd(gOnlySolutions(m),gOperative(gMinClosure(m)))
  )

    # return (gScalar(gSum(
  #   gProd(gOnlySolutions(m),gOperative(gMinClosure(m))),
  #   gProd(gOnlySolutions(m),gOperative(gMaxClosure(m)))),0.5)
  # )
  # return (
  #   gProd(gOnlySolutions(m),gOperative(gMaxClosure(m)))
  # )
}

grt <- function(m) {
  return (gDiff(gst(m), got(m)))
}



gft <- function(m) {
  return (
    gProd(gOnlySolutions(m),gOperative(gMaxClosure(gAdj(m))))
  )
}




