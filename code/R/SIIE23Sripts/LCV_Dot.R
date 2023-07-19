library(png)

dotpwd<<-"/home/lcv/Dropbox/Research/DBA/MB/"

dot<- function(name,directed=FALSE) {
  return (list(name=gsub(" ","_",name),title="",
               adjacency=matrix(0,0,0),
               landscape=FALSE,fill=FALSE, directed=directed, shapes=c(),
               engine="circo",dimx=10,dimy=10))
}

dotgetMatrix<-function(dot) {
  return (dot$adjacency)
}

dotsetMatrix<-function(dot, matrix, reset=FALSE) {
  dot$adjacency<-matrix
  if (reset) {
    for (i in (1:ncol(matrix))) {
      dot <- dotsetShape(dot,colnames(matrix)[i],"circle")
    }
  }
  return (dot)
}

dotsetShape<-function(dot, colname, shape) {
  icol <- which(colnames(dotgetMatrix(dot))==colname)
  dot$shapes[icol] <- shape
  return (dot)
}

dotAddNode<-function(dot,node, shape="circle") {
  m <-dotgetMatrix(dot)
  if (!node %in% colnames(m)) {
    m2<-rbind(m,rep(0,ncol(m)))
    m2 <- cbind(m2,rep(0,nrow(m2)))
    if (ncol(m)==0) {
      colnames(m2)<-c(node)
      dot$shapes<-c(shape)
    } else {
      colnames(m2) <- c(colnames(m),node)
      dot$shapes<-append(dot$shapes,shape)
    }
    rownames(m2) <- colnames(m2,2)
    dot <- dotsetMatrix(dot,m2)
  }
  return (dot)
}


dotAddEdge<-function(dot,from, to) {
    dot$adjacency[from,to]<-dot$adjacency[from,to]+1
    return (dot)
}

dotsetEngine<-function(dot, engine) {
  dot$engine <- engine
  return (dot)
}

dotgetAllDepths<-function(dot, node) {
  # cat("ALl ancestors of ", node,"\n")
  direct<-dotgetDirectAncestors(dot,node)
  if (length(direct)==0) {
    return (c(0));
  }
  res<-c()
  for (n in direct) {
    allpaths <- dotgetAllDepths(dot, n)+1
    res <- append(res, allpaths)
  }
  return (res)
}


dotgetAllFreqs<-function(dot, node) {
  # cat("ALl ancestors of ", node,"\n")
  direct<-dotgetDirectAncestors(dot,node)
  if (length(direct)==0) {
    return (c(0));
  }
  res<-c()
  for (n in direct) {
    allpaths <- dotgetAllFreqs(dot, n)+dot$adjacency[n,node]
    res <- append(res, allpaths)
  }
  return (res)
}

dotgetAllAncestors<-function(dot, node) {
  # cat("ALl ancestors of ", node,"\n")
  direct<-dotgetDirectAncestors(dot,node)
  res<-direct
  for (n in direct) {
    # if (!n %in% res){
      res<- union(res,setdiff(dotgetAllAncestors(dot,n),res))
    # }
  }
  return (res)
}

dotgetDirectAncestors<-function(dot, node) {
  # cat("Direct ancestors of ", node,"\n")
  res<-c()
  select <-dot$adjacency[,node]
  for (a in (1:length(select))) {
    if (select[a]>0)
      res<-append(res,colnames(dot$adjacency)[a])
  }
  return (res)
}

dotExport<-function(dot,basedir=".") {
  filename <-paste(basedir,"/",dot$name,sep="")
  fi<-file(paste(filename,".dot",sep=""))
  lines<-c()
  if (dot$directed)
    line<-paste("digraph",dot$name,"{")
  else
    line<-paste("graph",dot$name,"{")
  lines<-append(lines,line)
  lines<-append(lines,"labelloc=\"tl\"");
  lines<-append(lines,paste("label= \"", dot$title,"\""))
  if (dot$engine == "dot")
    lines<-append(lines,"rankdir=\"LR\";")
  else if (dot$engine == "circo")
    lines<-append(lines,"rankdir=\"RL\";")
  lines<-append(lines,paste("graph [ size=\"",dot$dimx,",",dot$dimy,"!\"]\n"))
  for (i in (1:ncol(dot$adjacency))) {
    if (i == 1 | sum(dot$adjacency[,i])>0) {
      line <- paste("\"",colnames(dot$adjacency)[i],"\" [shape=\"",dot$shapes[i],"\" label=\"",gsub("_", "\n",colnames(dot$adjacency)[i]),"\"]",sep="")
      lines<-append(lines,line)
    }
  }
  for (r in (1:nrow(dot$adjacency))) {
    for (c in (1:ncol(dot$adjacency))) {
      if (dot$adjacency[r,c]>0) {
        line<-paste("\"",colnames(dot$adjacency)[r],"\"",ifelse(dot$directed," -> "," -- "),"\"",colnames(dot$adjacency)[c],"\" [ label=",dot$adjacency[r,c], "]",sep="")
        lines<-append(lines,line)
      }
    }
  }
  lines<-append(lines,"}")
  writeLines(lines,fi)
  close(fi)
  system2(command = dot$engine,
          args    = c("-T", "png", paste(filename,".dot", sep=""), ">", paste(filename,".png", sep="")))
  # Sys.sleep(1)
  # dotshow(dot,basedir)
}

dotShow<-function(dot,basedir=".") {
  filename <-paste(basedir,"/",dot$name,sep="")
  dotExport(dot, basedir)
  clear_viewer_pane()
  dims <- dev.size(units = "in")
  img <- readPNG(paste(filename,".png", sep=""))
  grid::grid.newpage()
  grid::grid.raster(img)
  # rstudioapi::viewer(img)

}

clear_viewer_pane <- function() {
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile)
}


