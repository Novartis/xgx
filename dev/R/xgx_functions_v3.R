#Useful packages
library(ggplot2)
library(grid)
library(gridExtra)
library(stringr)
library(tidyr)
library(dplyr)
library(xtable)
library(readxl)
library(readr)
library(haven)

#Functions to help with plotting
#use best practices ggplot2 settings
theme_set(theme_bw())

#add draft to plots:draft.flag = TRUE or FALSE - indicates whether to add draft
xgx_draft = function(draft.flag,x=Inf,y=Inf,label="DRAFT",color="grey",hjust=1.2,vjust=1.2,draft.font=7,fontface="bold") {
  label.fig = ifelse(draft.flag,label,"")
  annotate("text",x=x,y=y,label=label.fig,color=color,hjust=hjust,vjust=vjust,cex=draft.font,fontface=fontface)
} 

#append filenames to bottom of the plot by greating a Grob
xgx_filename = function(dirs,g=last_plot(),fontsize=8) {
  bottom.txt = with(dirs,paste0(top.level,"\n",
                                Rscript.relative,Rscript.name,"\n",
                                results.relative,filename))
  g = arrangeGrob(g, bottom = textGrob(bottom.txt,gp=gpar(fontsize=fontsize)))
}

#save plot
xgx_save = function(width,height,dirs,filename.main,draft.flag,
                      print.filenames.flag=TRUE,g=last_plot(),draft.x=Inf,draft.y=Inf,draft.font=4,filetype="png") {
  filedir        = file.path(dirs$results.relative)
  dirs$filename  = paste0(dirs$output.prefix,filename.main,".",filetype)         #get the full filename
  g = g + xgx_draft(draft.flag,x=draft.x,y=draft.y,draft.font=draft.font)
  if (print.filenames.flag) g = xgx_filename(dirs)
  ggsave(plot=g,width=width,height=height,file.path(filedir,dirs$filename))
  return(g)
}


#nicer log scaling of axes
log.breaks    = function(dx=1,x1=-10,x2=10)     {signif(10^seq(x1,x2,dx),1) }
log.labels    = function(dx=1,x1=-10,x2=10)     {sprintf("%1g",log.breaks(dx,x1,x2)) }

xgx_scale_x_log10 <-  function(breaks = xgx_log_breaks_major, minor_breaks=  xgx_log_breaks_minor,...){
  ## xgx_scale_x_log10() modifies ggplot2's scale_x_log10(), 
  ## adding nicely spaced breaks on log scale. xgx_scale_x_log10()
  ## can be used in much the same was as scale_x_log10(), 
  ## e.g.: ggplot(data=data.frame(x=exp(rnorm(1000)),y=rnorm(1000)),aes(x=x,y=y)) + geom_point() + xgx_scale_x_log10()
  xgx_log_breaks_minor <-  function(x){
    r1 <- range(log10(x));
    r <-  r1;
    r[1] <-  floor(r[1])
    r[2] <-  ceiling(r[2])+1;
    breaks <- c()
    for (i in seq(r[1],r[2])){
      breaks <-  c(breaks,seq(2*10^(i-1),10^i-10^(i-1),by=10^(i-1)));
    }
    breaks <-  breaks[breaks <= 10^r1[2]]
    breaks <-  breaks[breaks >= 10^r1[1]]
    return(breaks)
  }
  xgx_log_breaks_major <-  function(x){
    r1 <- range(log10(x));
    r <-  r1;
    r[1] <-  floor(r[1])
    r[2] <-  ceiling(r[2])+1;
    breaks <- 10^seq(r[1],r[2])
    breaks <-  breaks[breaks <= 10^r1[2]]
    breaks <-  breaks[breaks >= 10^r1[1]]
    return(breaks)
  }
  ret <- try(list(scale_x_log10(...,breaks=breaks,minor_breaks = minor_breaks), 
                  theme(panel.grid.minor.x=element_line(color=rgb(0.75,0.75,0.75)),
                        panel.grid.major.x=element_line(color=rgb(0.65,0.65,0.65)))),silent=TRUE)
  if  (inherits(ret, "try-error")) return(scale_x_log10(...))
  return(ret);
}

# Function to add nicely spaced breaks on log scale (y-axis)
xgx_scale_y_log10 <-  function(breaks = xgx_log_breaks_major, minor_breaks=  xgx_log_breaks_minor,...){
  ## xgx_scale_y_log10() modifies ggplot2's scale_x_log10(), 
  ## adding nicely spaced breaks on log scale. xgx_scale_y_log10()
  ## can be used in much the same was as scale_x_log10(), 
  ## e.g.: ggplot(data=data.frame(x=rnorm(1000),y=exp(rnorm(1000))),aes(x=x,y=y)) + geom_point() + xgx_scale_y_log10()
  xgx_log_breaks_minor <-  function(y){
    r1 <- range(log10(y));
    r <-  r1;
    r[1] <-  floor(r[1])
    r[2] <-  ceiling(r[2])+1;
    breaks <- c()
    for (i in seq(r[1],r[2])){
      breaks <-  c(breaks,seq(2*10^(i-1),10^i-10^(i-1),by=10^(i-1)));
    }
    breaks <-  breaks[breaks <= 10^r1[2]]
    breaks <-  breaks[breaks >= 10^r1[1]]
    return(breaks)
  }
  xgx_log_breaks_major <-  function(y){
    r1 <- range(log10(y));
    r <-  r1;
    r[1] <-  floor(r[1])
    r[2] <-  ceiling(r[2])+1;
    breaks <- 10^seq(r[1],r[2])
    breaks <-  breaks[breaks <= 10^r1[2]]
    breaks <-  breaks[breaks >= 10^r1[1]]
    return(breaks)
  }
  ret <- try(list(scale_y_log10(...,breaks=breaks,minor_breaks = minor_breaks), 
                  theme(panel.grid.minor.y=element_line(color=rgb(0.75,0.75,0.75)),
                        panel.grid.major.y=element_line(color=rgb(0.65,0.65,0.65)))),silent=TRUE)
  if  (inherits(ret, "try-error")) return(scale_y_log10(...))
  return(ret);
}


#plotting a nice, scaled X axis
xgx_scale_x_units =
  function(units.output="d", units.input="d",increment=7, t.start=0, t.end=35) {
    # h = hours, d=days, w=weeks, m=months, y=years
    
    #allows for user to write out longer string for units
    units.output   = units.output %>% tolower() %>% substr(1,1)
    units.input    = units.input  %>% tolower() %>% substr(1,1) #units.input)
    
    day.scale  = data.frame(h=1/24,
                            d=1,
                            w=7,
                            m=30.4375,
                            y=365.25)
    
    input.scale  = day.scale[[units.input]]
    output.scale = day.scale[[units.output]]
    scale.factor = output.scale/input.scale
    
    xlabel.list = data.frame(h="Hour",
                             d="Day",
                             w="Week",
                             m="Month",
                             y="Year")
    xlabel = xlabel.list[[units.output]]
    
    sc     = scale.factor
    breaks = seq(t.start*sc, t.end*sc, increment*sc)
    limits = c(t.start*sc, t.end*sc)
    labels = breaks/sc
    
    return( list(
      scale_x_continuous(breaks=breaks,labels=labels), 
      coord_cartesian(xlim=limits), 
      xlab(xlabel)) )
  }
# Test code for scale_x_units
if(FALSE) {
  dummy_data <- data.frame(TIME=seq(0,700), CONC = 100*exp(-0.2*seq(0,700)))
  gg <- ggplot(data = dummy_data, aes(x = TIME, y = CONC)) + geom_point()
  gg
  gg + xgx_scale_x_units()
  gg + xgx_scale_x_units(units.output = "days", increment =7, t.start = 0, t.end=30)
  gg + xgx_scale_x_units(t.start = 0, t.end=24, increment =1, units.output = "hours")
  gg + xgx_scale_x_units(units.output = "WEEK", increment = 2, t.start = 0, t.end = 4)
}

#reverse log transform - requires teh scales package
#useful when plotting receptor occupancy that approaches 100%
library(scales)
xgx_reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}
#sample code for reverse log transform
if(FALSE) {
  ggplot(cars,aes(x=speed,y=dist)) + 
    geom_point() +
    scale_y_continuous(trans=xgx_reverselog_trans(10))
}
