setwd('./Descargas/SIIE23Sripts/')

source("Dot.R")

edges <- matrix(1:16, nrow = 4, ncol = 4)
print(length(edges))
labels <- c('P1-20', 'P2-20', 'START', 'END')
name <- "NOMBRE"
layoutCircular <- TRUE
getDotsProblem(name, labels, edges, layoutCircular)
name <- "NOMBRE2"
getDotsCompound(name, labels, edges)

source("GraphMiner.R")

my_list <- c("elemento1", "elemento2")
nuevo_elemento <- "elemento3"

my_list <- append(my_list, nuevo_elemento)

print(my_list)
