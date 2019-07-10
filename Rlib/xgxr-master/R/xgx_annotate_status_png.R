#' Annotate a png file or directory of png files
#'
#' These function annotates a single png file or all files within a
#' directory.
#'
#' If a png file has been annotated once, this function will not
#' annotate it again.  Therefore, you can run this function on
#' directories with different input script names and it will label
#' each file based on when each file was run.
#' 
#' Based on code from MrFlick on Stack Overflow: 
#' https://stackoverflow.com/questions/23807021/how-to-do-in-r-load-an-image-file-print-text-on-image-save-modified-image
#'
#' @param file_or_dir Png file to annotate or directory location for
#'   annotating png files.  Note this will annotate just once, so if
#'   you generate multiple png files and then annotate at the end of
#'   your script it will have the correct script name on it. Then if
#'   you create new images in a different script in the same directory
#'   and then annotate with the script name the second script, the PNG
#'   files will show the correct script location for each file.
#' @param script Script name to add as a footnote; By default this is
#'   empty, though it could name the script that
#' @param status Draft or other status; If \code{status="Final"} or
#'   \code{status=""} the status overlay will be removed.  By default
#'   the status is DRAFT.
#' @param date_format Date format for adding the time the png was
#'   annotated.
#' @param col Color for annotating the draft status
#' @param font Font to use for the annotation function
#' @param cex_status_mult Multiplication factor for the status
#'   annotation.  By default 7
#' @param cex_footnote_mult Multiplication factor for the footnote
#'   annotation. By default 0.8
#' @param status_angle Angle to rotate status
#' @param x11 Display on the X11/Windows device
#' @return nothing
#' 
#' @importFrom graphics par plot.new "plot.window"
#' 
#' @examples
#'
#' library(xgx)
#' library(ggplot2)
#'
#' # Using the examples from plot()
#' require(stats) # for lowess, rpois, rnorm
#' png("png-example.png")
#' plot(cars)
#' lines(lowess(cars))
#' dev.off()
#' xgx_annotate_status_png("png-example.png", "/tmp/script1.R") # annotate one file
#'
#' png("png-example2.png")
#' plot(sin, -pi, 2*pi) # see ?plot.function
#' dev.off()
#'
#' ## Using the example from ggplot
#'
#' # Generate some sample data, then compute mean and standard deviation
#' # in each group
#' df <- data.frame(
#'  gp = factor(rep(letters[1:3], each = 10)),
#'  y = rnorm(30)
#' )
#' ds <- df %>% group_by(gp) %>% summarise(mean = mean(y), sd   = sd(y))
#'
#' # The summary data frame ds is used to plot larger red points on top
#' # of the raw data. Note that we don't need to supply `data` or `mapping`
#' # in each layer because the defaults from ggplot() are used.
#' png("png-example3.png")
#' ggplot(df, aes(gp, y)) +
#'  geom_point() +
#'  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
#'
#' dev.off()
#' xgx_annotate_status_png(".", "other-script.R")
#'
#' # Notice that this is agnostic of ggplot/plot and will skip already
#' # labeled items
#'
#' @author Matthew Fidler, Alison M, ....
#' @export
xgx_annotate_status_png <- function(file_or_dir, script="", status="DRAFT",
                                    date_format="%a %b %d %X %Y",
                                    col=grey(.8,alpha=.7),
                                    font=2,
                                    cex_status_mult=7,
                                    cex_footnote_mult=0.8,
                                    status_angle=45,
                                    x11=FALSE){
  ##read file
  if (assertthat::has_extension(file_or_dir, "png")){
    files <- c(file_or_dir)
  } else {
    files <- list.files(file_or_dir,pattern=".png$",full.names=TRUE)
  }
  for(file in files){
    img<-png::readPNG(file,info=TRUE)
    info <- attr(img,"info");
    dpi <- round(mean(c(0, info$dpi), na.rm=TRUE),0)
    if (dpi < 10){
      dpi <- 75
    }
    metadata <- attr(img,"metadata");
    if (!identical(metadata,"I love xgx!")){
      message(sprintf("Add footnote to %s\n",file))
      ##get size
      h<-dim(img)[1]
      w<-dim(img)[2]
      ##open file for output
      png(file, width=w, height=h*1.05) ## make it slightly taller to add the text at the bottom

      ##par is for setting graphical parameters
      ##here, you're initializing a "state machine" setting all the graphical parameters
      ##from the state machine.  you can just set a few parameters differently.
      ##it is extremely fast and takes no memory.
      ##
      ##grid, lattice, ggplot are friendlier to use, but they are much slower
      ##and they require more memory
      ##
      ## mar=c(0,0,0,0): sets margins to zero
      ## xpd=NA: all plotting is clipped to the device region
      ## mgp=c(0,0,0): margin line (in mex units) for the axis title.  I guess we don't care?
      ## oma=c(0,0,0,0): more margins # ann=FALSE: do not add extra annotation to the plot
      old_par <- par();
      on.exit(suppressWarnings({par(old_par)}));
      suppressWarnings(par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=FALSE))

      ##THIS CREATES NEW PLOT.  I GUESS IT AUTOMATICALLY USES WHAT YOU SET WITH PAR?
      plot.new()
      plot.window(0:1, 0:1)

      ##fill plot with image
      usr<-par("usr")  #gives the extremes of the user coordinates
      graphics::rasterImage(img, usr[1], usr[3]+0.1, usr[2], usr[4]) #shifted up by .1 to make space for text

      ##add draft status to text if status isn't "Final"
      ##could be boolean, too.
      cx <- dpi/75


      if(!any(status==c("Final", ""))){
        graphics::text(.5,.5, status, cex=cex_status_mult*cx,
                       col=col, font=font, srt=status_angle)
      }
      ##add path to the bottom of the graphs
      bottom_txt = paste0(script, ifelse(script == "", "", "\n"),
                          "PNG: ", file,
                          ifelse(date_format == "", "",
                                 paste0("\n",
                                        "Date: ", format(Sys.time(), date_format))))
      graphics::text(0.5, 0.025, bottom_txt,cex=cx*cex_footnote_mult)
      ##close image
      invisible(dev.off())
      img<-png::readPNG(file)
      png::writePNG(img,file,metadata="I love xgx!");
      if (x11){
        par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=FALSE)
        lim <- par()
        graphics::plot.new()
        graphics::plot.window(0:1, 0:1)
        graphics::rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      }
    } else {
      message(sprintf("Already annotated %s; Need to regenerate figure to annotate again\n",file))
    }
  }
  return(invisible())
}
