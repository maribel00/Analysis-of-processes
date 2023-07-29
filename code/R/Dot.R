library(stringr)

dotpwd<<-"/home/maribel/Descargas/SIIE23Sripts/Graphs/"

dot<- function(name,directed=FALSE,engine="circo") {
  return (list(name=gsub(" ","_",name),title="",
               adjacency=matrix(0,0,0),
               landscape=FALSE,fill=FALSE, directed=directed, shapes=c(),
               engine=engine,dimx=10,dimy=10))
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
  direct<-dotgetDirectAncestors(dot,node)
  res<-direct
  for (n in direct) {
    res<- union(res,setdiff(dotgetAllAncestors(dot,n),res))
  }
  return (res)
}

dotgetDirectAncestors<-function(dot, node) {
  res<-c()
  select <-dot$adjacency[,node]
  for (a in (1:length(select))) {
    if (select[a]>0)
      res<-append(res,colnames(dot$adjacency)[a])
  }
  return (res)
}

dotExportDISCO <- function(dot, basedir=".") {
  dot <- dotAddNode(dot,"END","square")
  name <- dot$name
  edges <- dotgetMatrix(dot)
  labels <- colnames(dot$adjacency)
  
  for (i in 2:ncol(edges)-1){
    for (j in 2:ncol(edges)-1){
      first <- as.numeric(str_extract(labels[j], "\\d+(?=_.*$)"))
      second <- as.numeric(str_extract(labels[i], "\\d+(?=_.*$)"))
      print(first)
      print(second)
      if (!is.na(first) && !is.na(second) && edges[j,i] > 0 && (first != second)){
        edges[1,i] <- edges[1,i] + edges[j,i]
        edges[j,ncol(edges)] <- edges[j,ncol(edges)] + edges[j,i]
        edges[j,i] <- 0
      }
    }
  }
  
  for (i in 1:nrow(edges)){
    in_sum <- 0
    out_sum <- 0
    for (j in 1:nrow(edges)){
      out_sum <- out_sum + edges[i,j]
      in_sum <- in_sum + edges[j,i]
    }
    if (out_sum == 0 && i != nrow(edges)){
      edges[i,ncol(edges)] <- edges[i,ncol(edges)] + in_sum
    }
  }
  
  # Inicializar el vector de frecuencia de nodos
  freq <- rep(0, length(edges))
  
  # Recorrer cada fila de la matriz
  for (i in 1:nrow(edges)) {
    # Recorrer cada columna de la fila
    for (col in 1:ncol(edges)) {
      # Si hay una arista entre los nodos, aumentar la frecuencia de ambos nodos en 1
      if (edges[i,col] >= 1) {
        # Aumentar la frecuencia del nodo actual en edges[i,col]
        freq[col] <- freq[col] + edges[i,col]
      }
    }
  }
  
  # Recortar los labels
  for (i in 1:length(labels)) {
    labels[i] <- gsub('p', 'P', labels[i])
    labels[i] <- gsub('_1', '-20', labels[i])
    labels[i] <- gsub('_2', '-40', labels[i])
    labels[i] <- gsub('_3', '-60', labels[i])
    labels[i] <- gsub('_4', '-80', labels[i])
    labels[i] <- gsub('_5', '-100', labels[i])
  }
  
  filename <-paste(basedir,"/",dot$name,sep="")
  file <- paste0(filename, '.dot')
  f <- file(description = file, open = 'w')
  line <- paste('digraph ',dot$name,sep="")
  line <- paste(line,' {',sep="")
  writeLines(line, f)
  line <- paste('\tlabel=',dot$name,sep="")
  line <- paste(line,';',sep="")
  writeLines(line, f)
  writeLines('\tdpi = 150', f)
  writeLines('\tsize="16,11!";', f)
  writeLines('\tmargin = 0;', f)
  
  for (i in 1:length(labels)) {
    color <- 'aqua'
    if (freq[i] >= 10 && freq[i] < 20) {
      color <- 'lightskyblue'
    } else if (freq[i] >= 20 && freq[i] < 30) {
      color <- 'deepskyblue'
    } else if (freq[i] >= 30 && freq[i] < 40) {
      color <- 'dodgerblue'
    } else if (freq[i] >= 40) {
      color <- 'royalblue'
    }
    if (labels[i] == 'START' || labels[i] == 'END') {
      color <- 'white'
    }
    
    label <- paste0('<<table border="0" cellborder="1" cellspacing="0"><tr><td bgcolor="', color, '"><FONT face="Arial" POINT-SIZE="10"><b>', labels[i], '</b></FONT></td></tr>')
    label <- paste0(label, '<tr><td bgcolor="white"><FONT face="Arial" POINT-SIZE="8"><i>', freq[i], '</i></FONT></td></tr></table>>]\n')
    writeLines(paste0('"', labels[i], '"', ' [shape=plain, label=', label), f)
  }
  
  for (i in 1:nrow(edges)) {
    for (j in 1:ncol(edges)) {
      if (edges[i,j] >= 1) {
        if (labels[i] == 'START' || labels[j] == 'END') {
          writeLines(paste0('"', labels[i], '" -> "', labels[j], '" [ style = dashed color=grey label ="', edges[i,j], '" labelfloat=false fontname="Arial" fontsize=8]\n'), f)
        } else {
          writeLines(paste0('"', labels[i], '" -> "', labels[j], '" [ color=grey16 penwidth = "', max(1, log(edges[i,j])), '"label ="', edges[i,j], '" labelfloat=false fontname="Arial" fontsize=8]\n'), f)
        }
      }
    }
  }
  
  writeLines('}', f)
  close(f)
  system2(command = "dot",
          args    = c("-Tpng", paste(filename,".dot", sep=""), ">", paste(filename,".png", sep="")))
}

dotExportProblem <- function(dot, basedir=".") {
  name <- dot$name
  edges <- dotgetMatrix(dot)
  labels <- colnames(dot$adjacency)
  
  # Recortar los labels
  for (i in 1:length(labels)) {
    labels[i] <- gsub('p', 'P', labels[i])
  }
  
  color <- c(
    'P1' = 'greenyellow',
    'P1 OK' = 'greenyellow',
    'P1 FAIL' = 'greenyellow',
    'P2' = 'bisque',
    'P2 OK' = 'bisque',
    'P2 FAIL' = 'bisque',
    'P3' = 'cadetblue',
    'P3 OK' = 'cadetblue',
    'P3 FAIL' = 'cadetblue',
    'P4' = 'orange',
    'P4 OK' = 'orange',
    'P4 FAIL' = 'orange',
    'P5' = 'deepskyblue',
    'P5 OK' = 'deepskyblue',
    'P5 FAIL' = 'deepskyblue',
    'P6' = 'gold',
    'P6 OK' = 'gold',
    'P6 FAIL' = 'gold',
    'P7' = 'hotpink',
    'P7 OK' = 'hotpink',
    'P7 FAIL' = 'hotpink',
    'P8' = 'indianred1',
    'P8 OK' = 'indianred1',
    'P8 FAIL' = 'indianred1',
    'P9' = 'mediumpurple1',
    'P9 OK' = 'mediumpurple1',
    'P9 FAIL' = 'mediumpurple1'
  )
  
  # Inicializar el vector de frecuencia de nodos
  freq <- rep(0, length(edges))
  
  # Recorrer cada fila de la matriz
  for (i in 1:nrow(edges)) {
    # Recorrer cada columna de la fila
    for (col in 1:ncol(edges)) {
      # Si hay una arista entre los nodos, aumentar la frecuencia de ambos nodos en 1
      if (edges[i,col] >= 1) {
        # Aumentar la frecuencia del nodo actual en edges[i,col]
        freq[col] <- freq[col] + edges[i,col]
      }
    }
  }
  
  filename <-paste(basedir,"/",dot$name,sep="")
  file <- paste0(filename, '.dot')
  f <- file(description = file, open = 'w')
  line <- paste('digraph ',dot$name,sep="")
  line <- paste(line,' {',sep="")
  writeLines(line, f)
  writeLines('\tlabelloc="t";', f)
  line <- paste('\tlabel=',dot$name,sep="")
  line <- paste(line,';',sep="")
  writeLines(line, f)
  writeLines('\tdpi = 150', f)
  writeLines('\tsize="16,11!";', f)
  writeLines('\tmargin = 0;', f)
  
  if (dot$engine == 'circo') {
    writeLines('layout=circo;', f)
  }
  
  for (i in 1:length(labels)) {
    if (labels[i] == 'START' || labels[i] == 'END') {
      writeLines(paste0('"', labels[i], '"', ' [shape=box, fillcolor=white, style=filled, color=black]'), f)
    } else {
      if (endsWith(labels[i], "FAIL") && !endsWith(labels[i], "OK")) {
        writeLines(paste0('"', labels[i], '"', ' [shape=circle, color=', color[labels[i]], ', style=filled]'), f)
      } else {
        writeLines(paste0('"', labels[i], '"', ' [shape=circle, color=', color[labels[i]], ', peripheries=2, style=filled]'), f)
      }
    }
  }
  
  for (i in 1:nrow(edges)) {
    for (j in 1:ncol(edges)) {
      if (edges[i,j] >= 1) {
        if (labels[i] == 'START' || labels[j] == 'END') {
          writeLines(paste0('"', labels[i], '" -> "', labels[j], '" [ style = dashed color=grey label ="', edges[i,j], '" labelfloat=false fontname="Arial" fontsize=8]\n'), f)
        } else {
          writeLines(paste0('"', labels[i], '" -> "', labels[j], '" [ color=grey16 penwidth = "', max(1, log(edges[i,j])), '"label ="', edges[i,j], '" labelfloat=false fontname="Arial" fontsize=8]\n'), f)
        }
      }
    }
  }
  
  writeLines('}', f)
  close(f)
  
  system2(command = "dot",
          args    = c("-Tpng", paste(filename,".dot", sep=""), ">", paste(filename,".png", sep="")))
}