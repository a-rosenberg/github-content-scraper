## ---- include=FALSE, waring=F--------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(CircStats)
library(openair)


## ----loadData, include=FALSE, waring=F-----------------------------------
data=read.csv("Wind.csv")
str(data)


## ----Preliminar figure---------------------------------------------------
   # WindRose.R
require(ggplot2)
require(RColorBrewer)
require(scales)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 22.5,
                          spdmin = 0,
                          spdmax = 15,
                          spdseq = NULL,
                          palette = "OrRd",
                          countmax = NA,
                          debug = 0){


  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  

  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1

  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)

  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")

  }  

  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned
                           ,y = (..count..)/sum(..count..)
                           ))+
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
labels = c("N","","NE","", "E", "", "SE","", 
             "S","", "SW","", "W","","NW","")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank(), strip.text.x = element_text(size = 12, colour = "black", face="bold", angle = 0), strip.background = element_rect(fill="grey95"),
          axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=18,face="bold"), 
        panel.background = element_rect(fill="white", color = "black"), 
        panel.grid.major = element_line(color = "grey80"), legend.position="right", legend.direction = "vertical", legend.box = "vertical", legend.text = element_text(colour="black", size = 12), legend.title = element_text(colour="black", size=12, face="bold"),
  panel.grid.minor = element_blank())  + facet_wrap(~year, ncol=2) +
    scale_y_continuous(labels = percent) + 
    ylab("Frequency") 

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  print(p.windrose)  

  # return the handle to the wind rose
  return(p.windrose)
}


## ----Figure--------------------------------------------------------------
plot<-plot.windrose(data = data,
              spd = "spd",
              dir = "dir") 



plot +  facet_wrap(~year) +  labs(caption = "Oliveira et al. (2018) 96:1051-1063, the Plant Journal") +
    ggsave("WindRose.png", units="in", width=10, height=5, dpi=600)

