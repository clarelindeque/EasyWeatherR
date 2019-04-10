library(RColorBrewer)

#' Wind rose plot
#'
#' Plot a wind rose diagram, showing the relative frequency of winds coming
#' from each point of the compass
#'
#' Radar plot code modified from answers to this Stackoverflow question: https://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r/10820387
#'
#' @param data A data frame containing wind speed and direction in degrees
#' @param speed The name of the column in the data frame containing wind speed
#' @param direction The name of the column in the data frame containing wind direction
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

plot_windrose <- function(data,
                          speed,
                          direction,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          printplot=TRUE){


  # Tidy up input data ----
  n_in <- NROW(data)
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
  n_spd_seq <- length(spdseq)
  n_colors_in_range <- n_spd_seq - 1

  # create the color map
  spd_colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n_colors_in_range),
                                                min(9,
                                                    n_colors_in_range)),
                                            palette))(n_colors_in_range)

  if (max(data[[spd]],na.rm = TRUE) > spdmax){
    spd_breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd_labels <- c(paste(c(spdseq[1:n_spd_seq-1]),
                          '-',
                          c(spdseq[2:n_spd_seq])),
                    paste(spdmax,
                          "-",
                          max(data[spd],na.rm = TRUE)))
    spd_colors <- c(spd_colors, "grey50")
  } else{
    spd_breaks <- c(seq(spdseq))
    spd_labels <- paste(c(spdseq[1:n_spd_seq-1]),
                        '-',
                        c(spdseq[2:n_spd_seq]))
  }
  data$spd_binned <- cut(x = data[[spd]],
                         breaks = spd_breaks,
                         labels = spd_labels,
                         ordered_result = TRUE)

  # figure out the wind direction bins
  dir_breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  dir_labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir_binned <- cut(data[[dir]],
                    breaks = dir_breaks,
                    ordered_result = TRUE)
  levels(dir_binned) <- dir_labels
  data$dir_binned <- dir_binned

  # create the plot ----
  p_windrose <- ggplot(data = data,
                       aes(x = dir_binned,
                           fill = spd_binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (knots)",
                      values = spd_colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank())

  # adjust axes if required
  if (!is.na(countmax)){
    p_windrose <- p_windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  if (printplot) { print(p_windrose) }

  # return the handle to the wind rose
  return(p_windrose)
}
