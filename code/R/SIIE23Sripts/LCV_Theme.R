# https://rkabacoff.github.io/datavis/modifyingthemes.pdf

library(ggplot2)

LCV_colors <- c("lightgreen", "orange", "lightskyblue", "red", "purple", "yellow","black","brown")
textcolor<-"black"
linecolor <- "black"
fillcolor <- "lightgray"
backgroundcolor <- "white"
mainline <- "solid"
secondline <- "dashed"
thirdline <- "dotted"
alphafill <- 0.9

LCV_Graphite_Theme <- function() {
  linecolor <<- "black"
  fillcolor <<- "lightgray"
  backgroundcolor <<- "white"
  mainline <<- "solid"
  secondline <<- "dashed"
  thirdline <<- "dotted"
  alphafill <<- 0.75
  textcolor<<-"black"
}



LCV_Green_Theme <- function() {
  linecolor <<- "green"
  fillcolor <<- "lightgreen"
  backgroundcolor <<- "black"
  mainline <<- "solid"
  secondline <<- "dashed"
  thirdline <<- "dotted"
  alphafill <<- 0.5
}

LCV_Heaven_Theme <- function() {
  linecolor <<- "gray"
  fillcolor <<- "lightskyblue"
  backgroundcolor <<- "white"
  mainline <<- "solid"
  secondline <<- "solid"
  thirdline <<- "solid"
  alphafill <<- 0.5
  textcolor<<- "black"
  linethick<<-0.5
}

LCV_Orange_Theme <- function() {
  linecolor <<- "gray"
  fillcolor <<- "orange"
  backgroundcolor <<- "white"
  mainline <<- "solid"
  secondline <<- "solid"
  thirdline <<- "solid"
  alphafill <<- 0.5
  textcolor<<- "black"
  linethick<<-0.5
}

LCV_Apple_Theme <- function() {
  linecolor <<- "gray"
  fillcolor <<- "lightgreen"
  backgroundcolor <<- "white"
  mainline <<- "solid"
  secondline <<- "solid"
  thirdline <<- "solid"
  alphafill <<- 0.5
  textcolor<<- "black"
  linethick<<-0.5
}

LCV_Tomato_Theme <- function() {
  linecolor <<- "gray"
  fillcolor <<- "red"
  backgroundcolor <<- "white"
  mainline <<- "solid"
  secondline <<- "solid"
  thirdline <<- "solid"
  alphafill <<- 0.5
  textcolor<<- "black"
  linethick<<-0.5
}

LCV_theme <-   theme(
  panel.background = element_rect(fill =backgroundcolor,
                                  colour = linecolor,
                                  size = 0.5, linetype = mainline),
  panel.grid.major = element_line(size = 0.1, linetype = secondline,
                                  colour = linecolor),
  panel.grid.minor = element_line(size = 0.1, linetype = thirdline,
                                  colour =linecolor),
  panel.border = element_rect(colour =textcolor,fill = "transparent",linewidth = 0.5),
  plot.background = element_rect(fill =backgroundcolor),
  plot.title = element_text(color=textcolor),
  plot.caption = element_text(color=textcolor),
  axis.title.x = element_text(color = textcolor,face="bold" ),
  axis.title.y = element_text(color = textcolor, face="bold"),
  axis.text.x = element_text(color = textcolor),
  axis.text.y = element_text(color = textcolor)
)


LCV_theme2 <-   theme(
  panel.background = element_rect(fill =backgroundcolor,
                                  colour = linecolor,
                                  linetype = mainline),
  panel.grid.major = element_line(size = 0.1, linetype = secondline,
                                  colour = linecolor),
  panel.grid.minor = element_line(size = 0.1, linetype = thirdline,
                                  colour =linecolor),
  panel.border = element_rect(colour =textcolor,fill = "transparent",linewidth = 0.5),
  plot.background = element_rect(fill =backgroundcolor),
  plot.title = element_text(color=textcolor),
  plot.caption = element_text(color=textcolor),
  axis.title.x = element_text(color = textcolor,face="bold" ),
  axis.title.y = element_text(color = textcolor, face="bold"),
  axis.text.x = element_text(color = textcolor),
  axis.text.y = element_text(color = textcolor)
)

tuk_plot <- function (x, xlab, ylab, ylabels = NULL, ...) {
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4L, drop = FALSE]
    yvals <- nrow(xi):1L
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE, xlab = "", ylab = "", main = NULL,
         ...)
    axis(1, ...)
    # change for custom axis labels
    if (is.null(ylabels)) ylabels <- dimnames(xi)[[1L]]

    axis(2, at = nrow(xi):1, labels = ylabels,
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5, ...)
    segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi),
             rep.int(yvals + 0.1, 3L), ...)
    title(main = paste0(format(100 * attr(x, "conf.level"),
                               digits = 2L), "% family-wise confidence level\n"),
          # change for custom axis titles
          xlab = xlab, ylab = ylab)

    box()
    dev.flush()
    on.exit()
  }
}


# LCV_barplot_theme <-  geom_bar(fill=fillcolor, color=linecolor,alpha=alphafill)
# LCV_boxplot_theme <- geom_boxplot(fill = fillcolor, color=linecolor,alpha=alphafill)
#
# LCV_density <- function (data, variable) {
#   ddataset<-data
#   nVar<-variable
#   hVar=ddataset[[nVar]]
#   ddata <- data.frame(hVar)
#   ggplot(ddataset,aes(x=hVar))+
#     geom_density(color=textcolor,fill=fillcolor,alpha=alphafill)+
#     xlab(nVar)+
#     scale_x_continuous(n.breaks=nticks)+
#     ylab("Density")+
#     LCV_theme
# }

# LCV_densitiesFactor <- function (data, variable, factor) {
#   if (is.factor(data[[factor]])) {
#     ddataset <- data.frame(data[,c(variable,factor)])
#     var<-ddataset[[variable]]
#     fac<-ddataset[[factor]]
#     maxf <- vector()
#     for (f in unique(fac)) {
#       d<-density(ddataset[fac==f,][[variable]])
#       maxf[f] <- max(d$y)
#       name <- paste("Factor=",f,sep="")
#       cat(name,"/",maxf[f],"\n")
#       # maxf[f]<-1
#     }
#     col<-"NORM-Density"
#     for(row in (1:nrow(ddataset))) {
#       ddataset[row,col]<-ddataset[row,variable]/maxf[ddataset[row,factor]]
#     }
#     v<- ddataset[[col]]
#     cluster <- ddataset[[factor]]
#     res<-ggplot(data=ddataset, aes(x=v,fill=cluster))+
#       geom_density(alpha=0.25)+
#       LCV_theme
#     res
#   }
#

#   ddataset <- data
#   dvariable <- ddataset[[variable]]
#   fvariable <- as.factor(dvariable)
#   ddata <- data.frame(fvariable)
#   res<-ggplot(data=ddata, mapping=aes(x=fvariable))+
#     geom_text(stat="count", aes(label=..count..),color=textcolor,vjust=-0.5)+
#     xlab(variable)+
#     geom_bar(fill=fillcolor, color=textcolor,alpha=alphafill)+
#     LCV_theme
#   res
# }




GGTukey<-function(Tukey){
  # A<-require("tidyverse")
  # if(A==TRUE){

  #   library(tidyverse)
  # } else {
  #   install.packages("tidyverse")
  #   library(tidyverse)
  # }
  B<-as.data.frame(Tukey[1])
  colnames(B)[2:3]<-c("min",
                      "max")
  C<-data.frame(id=row.names(B),
                min=B$min,
                max=B$max)
  D<-C%>%
    ggplot(aes(id))+
    geom_errorbar(aes(ymin=min,
                      ymax=max),
                  width = 0.2)+
    geom_hline(yintercept=0,
               color=fillcolor)+
    labs(x=NULL)+
    coord_flip()+
    theme(text=element_text(family="TimesNewRoman"),
          title=element_text(color="black",size=15),
          axis.text = element_text(color="black",size=10),
          axis.title = element_text(color="black",size=10),
          panel.grid=element_line(color="grey75"),
          axis.line=element_blank(),
          plot.background=element_rect(fill="white",color="white"),
          panel.background=element_rect(fill="white"),
          panel.border = element_rect(colour = "black", fill = NA,size=0.59),
          legend.key= element_rect(color="white",fill="white")+
            LCV_theme2
    )
  return(D)
}

# ggHSD(data.tukey)+
#   labels(main ="")+
#   theme_bw()
# # labs(subtitle = "")+
# theme(
#   axis.text.x = element_text(size=10),
#   axis.text.y = element_text(size=8,angle=0),
#   line = element_line(color="blue"),
#   plot.title = element_text(size=0),
#   strip.background = element_rect(fill="lightblue", color="black")
# )


# LCV_boxplot <- function(data, variable, value, group="", log=FALSE, ordered = FALSE) {
#   if (nchar(group)>0) {
#     dataset <- subset(data,select = c(group,variable, value))
#     dvariable <- dataset[[variable]]
#     dvalue <- dataset[[value]]
#     res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue,colour=group))+
#       geom_boxplot(fill = fillcolor, alpha=alphafill)+
#       xlab(variable)+
#       ylab(value)+
#       LCV_theme2
#
#   } else {
#     dataset <- subset(data,select = c(variable, value))
#     dvariable <- dataset[[variable]]
#     dvalue <- dataset[[value]]
#     if (log) {
#       res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue))+
#         geom_boxplot(fill = fillcolor, color=textcolor,alpha=alphafill)+
#         xlab(variable)+
#         ylab(value)+
#         scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                       labels = trans_format("log10", math_format(10^.x))) +
#         LCV_theme2
#     } else {
#       res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue))+
#         geom_boxplot(fill = fillcolor, color=textcolor,alpha=alphafill)+
#         xlab(variable)+
#         ylab(value)+
#         LCV_theme2
#     }
#   }
#   res
# }
#
