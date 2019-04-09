library(RColorBrewer)

#' Wind rose plot
#'
#' Plot a wind rose diagram, showing the relative frequency of winds coming
#' from each point of the compass
#'
#' Radar plot code modified from answers to this Stackoverflow question: https://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r/10820387
#'
#' @param data A data frame containing wind speed and direction in degrees
#' @param spd A numeric vector of wind speed data OR the name of the column
#' in the data data frame containing wind speed information
#' @param dir A numeric vector of wind direction data OR the name of the column
#' in the data data frame containing wind direction information
#' @param spdres Resolution of wind speed bins
#' @param dirres Resolution of wind direction bins
#' @param spdmin Maximum wind speed to use in binning
#' @param spdmax Minimum wind speed to use in binning
#' @param palette Palette to use in plotting
#' @param countmax Maximum axis limit for plotting
#' @return A ggplot2 object containing the windrose plot
#' @examples
#' # Assuming EasyWeather data files called 2018.csv and 2019.csv are in local
#' # directory:
#' years <- c(2018,2019)
#' test_data <- merge_years(years)
#' p <- plot_windrose(test_data, "wind_km_h", "wind_direction_deg", dirres = 15)
#' p <- plot_windrose(NULL, test_data$wind_km_h, test_data$wind_direction_deg)

plot_windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          printplot=TRUE){


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
                          max(data[spd],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- c(seq(spdseq))
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

  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (knots)",
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank())

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  if (printplot) { print(p.windrose) }

  # return the handle to the wind rose
  return(p.windrose)
}
