# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot
# objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# e=0.15, # extra height needed for last plot (vertical layout),
# or extra width for first plot (horizontal layout)
multiplot <- function(..., plotlist=NULL, file, cols=4,
                      layout=NULL, horizontal=FALSE, e=0.15) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots = c(list(...), plotlist)
  
  numPlots = length(plots)
  #message(paste0('>>>>>>>INFO: num plots 2 = ', numPlots), '\n')
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    
    ## set up heights/widths of plots
    
    # extra height needed for last plot (vertical layout),
    # or extra width for first plot (horizontal layout)
    hei = rep(1, numPlots)
    # bottom plot is taller
    hei[numPlots] = hei[numPlots]*(1+e)
    wid = rep(1, numPlots)
    # first left plot is wider
    wid[1] = wid[1]*(1+e)
    # Set up the page
    grid.newpage()
    if(horizontal){
      # pushViewport(viewport(layout = grid.layout(nrow(layout),
      #                                            ncol(layout), widths=wid)))
      pushViewport(viewport(layout = grid.layout(nrow(layout),
                                                 ncol(layout), heights=wid)))
    }else{
      pushViewport(viewport(layout = grid.layout(nrow(layout),
                                                 ncol(layout), heights=hei)))
      
    }
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get i,j matrix positions of the regions containing this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
