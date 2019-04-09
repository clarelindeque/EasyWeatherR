# EasyWeatherR

Suite of functions to import, manipulate and visualise data from an EasyWeather HP2000 weather station. More information and specifications of this type of station can be found [here](http://www.foshk.com/Wifi_Weather_Station/HP1000.html). These weather stations appear under various brand names but all run the same software and output identical file formats.

## Getting Started

The data acquisition and manipulation functions read code from csv files, two of which are included as examples in the test folder, and are used in the unit testing.

### Prerequisites

Required packages:

* tidyverse
* janitor
* [openair](https://github.com/cran/openair/)

This list may be tidied up once the future work (see below) has been completed.

## Authors

* **Clare Lindeque** - Hatchet job - [github](https://github.com/clarelindeque/)

## License

MIT + file LICENSE

## Acknowledgments

* Inspiration and guidance drawn from [The Jason & Doug Blog](http://jason-doug-climate.blogspot.com)
* Radar plot code modified from answers to [this Stackoverflow question](https://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r/10820387)

## Future Work

* Functions to draw wind roses, time series visualisations
* Functions to create summary statistics/dashboards
* Continue to write test cases as development progresses
* Upload example output files (png images or pdfs)
* Upload full data set and update periodically?
