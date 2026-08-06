#CODE TO ADD LATER ----
function2string   = function(function_name) {
  function_string = deparse(function_name)
  index.remove    = c(1,2,length(function_string))
  function_string = function_string[-index.remove]
}

#Useful packages
library(ggplot2)
library(grid)
library(gridExtra)
#library(stringr)
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
ggdraft = function(draft.flag,x=Inf,y=Inf,label="DRAFT",color="grey",hjust=1.2,vjust=1.2,cex=7,fontface="bold") {
  label.fig = ifelse(draft.flag,label,"")
  annotate("text",x=x,y=y,label=label.fig,color=color,hjust=hjust,vjust=vjust,cex=cex,fontface=fontface)
} 

#append filenames to bottom of the plot by greating a Grob
ggfilename = function(dirs,p=last_plot(),fontsize=8) {
  bottom.txt = with(dirs,paste0(top.level,"\n",
                                Rscript.relative,Rscript.name,"\n",
                                results.relative,filename))
  p = arrangeGrob(p, bottom = textGrob(bottom.txt,gp=gpar(fontsize=fontsize)))
}

#save plot
ggsaveplot = function(width,height,dirs,filename.main,draft.flag,
                      print.filenames.flag=TRUE,p=last_plot(),draft.x=Inf,draft.y=Inf,filetype="png") {
  filedir        = file.path(dirs$results.relative)
  dirs$filename  = paste0(dirs$output.prefix,filename.main,".",filetype)         #get the full filename
  p = p + draft(draft.flag,x=draft.x,y=draft.y)
  if (print.filenames.flag) p = filex(dirs)
  ggsave(plot=p,width=width,height=height,file.path(filedir,dirs$filename))
  return(p)
}


#nicer log scaling of axes
log.breaks    = function(dx=1,x1=-10,x2=10)     {signif(10^seq(x1,x2,dx),1) }
log.labels    = function(dx=1,x1=-10,x2=10)     {sprintf("%1g",log.breaks(dx,x1,x2)) }
scale.x.log10 = function(dx=1,x1=-10,x2=10,...) {
  return(list(scale_x_log10(breaks=log.breaks(dx,x1,x2),labels=log.labels(dx,x1,x2),...),
              annotation_logticks(base = 10, sides = "b", color = "grey50")))
}
scale.y.log10 = function(dx=1,x1=-10,x2=10,...) {
  return(list(scale_y_log10(breaks=log.breaks(dx,x1,x2),labels=log.labels(dx,x1,x2),...),
              annotation_logticks(base = 10, sides = "l", color = "grey50")))
}

#plotting a nice, scaled X axis
scale_x_units =
  function(units.output="h", units.input="h",increment=6, t.start=0, t.end=24) {
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
  gg + scale_x_units()
  gg + scale_x_units(units.output = "days", increment =7, t.start = 0, t.end=30)
  gg + scale_x_units(t.start = 0, t.end=24, increment =1, units.output = "hours")
  gg + scale_x_units(units.output = "WEEK", increment = 2, t.start = 0, t.end = 4)
}
