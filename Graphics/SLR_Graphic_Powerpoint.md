Presentation Graphic for Sea Level Rise at Portland, Maine
================
Curtis C. Bohlen

-   [Introduction](#introduction)
-   [Import Libraries](#import-libraries)
-   [Import Data](#import-data)
-   [Estimating the Linear Trend](#estimating-the-linear-trend)
-   [Mimic the NOAA Graphic](#mimic-the-noaa-graphic)
    -   [English Units Based on MLLW](#english-units-based-on-mllw)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

Here we prepare a graphic for depicting local sea level rise in
Portland. All data is derived from NOAA Tides and Currents or COPS data
for the tide gauge in Portland, Maine. Since NOAA provides clean data as
monthly values, we need only plot NOAA data, with minimal processing.

This notebook generates a version of ht egraphic specifically for use in
Powerpoint.

# Import Libraries

``` r
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.2     v dplyr   1.0.6
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> Warning: package 'forcats' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readr)

library(zoo)     # for the rollmean function
#> Warning: package 'zoo' was built under R version 4.0.5
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric

library(nlme)    # for gls
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse

library(CBEPgraphics)

load_cbep_fonts()
```

# Import Data

Our primary source data is based on NOAA’s analysis of sea level trends.
The description on the source web site
(<https://tidesandcurrents.noaa.gov/datums.html?id=8418150>) says the
following, so this is apparently NOT raw data.

> “The plot shows the monthly mean sea level without the regular
> seasonal fluctuations due to coastal ocean temperatures, salinities,
> winds, atmospheric pressures, and ocean currents. … The plotted values
> are relative to the most recent Mean Sea Level datum established by
> CO-OPS.”

For convenience, we want to be able to report these elevations as
positive values, which makes it easier for readers to compare
elevations. NOAA uses a datum of MLLW for charting purposes. We follow
that practice here.

According to <https://tidesandcurrents.noaa.gov/datums.html?id=8418150>,
at Portland, MLLW has an elevation (in feet) of 0.0 , while MSL has an
elevation of 4.94. We can convert elevations in inches MSL to elevations
in inches MLLW) as follows:

$$
E\_{MLLW} = E\_{MSL} + (4.94 ft\\times \\frac{12 in}{1 ft})
$$
An alternative is to declare some other arbitrary sea level datum as a
“Relative Sea Level.” We prepare a graphic that way, below, but chose
not to use it.

``` r
sibfldnm <- 'Original Data'
parent <- dirname(getwd())
sibling <- file.path(parent,sibfldnm)

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

``` r
fn <- '8418150_meantrend.csv'

fpath <- file.path(sibling, fn)

slr_data  <- read_csv(fpath, 
    col_types = cols(Unverified = col_double())) %>%
  rename(MSL = Monthly_MSL) %>%
  mutate(theDate = as.Date(paste0(Year,'/', Month,'/',15))) %>%
  mutate(MSL_ft = MSL*  3.28084,
         MLLW_ft = MSL_ft +(4.94))
#> Warning: 1299 parsing failures.
#> row col  expected    actual                                                                                                                              file
#>   1  -- 7 columns 8 columns 'C:/Users/curtis.bohlen/Documents/State of the Bay 2020/Data/A6. Climate Change/Portland-SLR/Original Data/8418150_meantrend.csv'
#>   2  -- 7 columns 8 columns 'C:/Users/curtis.bohlen/Documents/State of the Bay 2020/Data/A6. Climate Change/Portland-SLR/Original Data/8418150_meantrend.csv'
#>   3  -- 7 columns 8 columns 'C:/Users/curtis.bohlen/Documents/State of the Bay 2020/Data/A6. Climate Change/Portland-SLR/Original Data/8418150_meantrend.csv'
#>   4  -- 7 columns 8 columns 'C:/Users/curtis.bohlen/Documents/State of the Bay 2020/Data/A6. Climate Change/Portland-SLR/Original Data/8418150_meantrend.csv'
#>   5  -- 7 columns 8 columns 'C:/Users/curtis.bohlen/Documents/State of the Bay 2020/Data/A6. Climate Change/Portland-SLR/Original Data/8418150_meantrend.csv'
#> ... ... ......... ......... .................................................................................................................................
#> See problems(...) for more details.
```

# Estimating the Linear Trend

We use a linear model analysis to compare results to the linear trend
reported by NOAA on the source web page. NOAA reports the rate of sea
level rise in millimeters as 1.9 ± 0.14*m**m*/*y**r*.

The NOAA data are reported monthly, but to take advantage of the Date
class in R, we expressed monthly data as relating to the fifteenth of
each month.

As a result, our model coefficients are expressed in units per DAY. To
find the relevant annual rate of sea level rise, we need to multiply
both estimate (slope) and its standard error by 365.25 (approximate
length of a year in days) and then multiply again by 1000 to convert
from meters to millimeters.

The estimate from a simple linear model matches NOAA’s reported
estimate, but the standard error and derived 95% confidence interval are
considerably narrower. NOAA appropriately treated this as an
auto-correlated time series, instead of simply as a linear model. We do
the same, specifying and autoregressive error function of order 1.

``` r
the_gls <- gls(MSL~theDate, data=slr_data, correlation = corAR1())
ccs <- as.data.frame(summary(the_gls)$tTable)
EST <- round(ccs$Value[2] * 365.25 * 1000, 2)
SE <- round(ccs$Std.Error[2]   * 365.25 * 1000, 4)
CI <- 1.96*SE
knitr::kable(tibble(Estimate = EST, Std_Err = SE, CI_95 =  CI), digits= c(2,4,4))
```

| Estimate | Std\_Err | CI\_95 |
|---------:|---------:|-------:|
|     1.89 |   0.0726 | 0.1423 |

Those results match the NOAA-reported estimate and 95% confidence
interval.

# Mimic the NOAA Graphic

This a redrawing of the NOAA “Mean Sea Level Trend” graphic for
Portland. I have added a 24 month (2 year) moving average.

## English Units Based on MLLW

``` r
plt <- ggplot(slr_data, aes(theDate, MSL_ft)) + 
  geom_line(color=cbep_colors()[1]) +
  geom_line(aes(y=rollmean(MSL_ft,60, na.pad=TRUE)), color=cbep_colors()[2],
            size = 1.25) +
  geom_smooth(method='lm', formula = y~x, se=FALSE, color=cbep_colors()[3],
              size = 1) + 
  theme_cbep() + 
  xlab('Date') + 
  ylab('Monthly Mean Tide (ft, MSL)')

# Add Annotation in inches Per Decade
EST <- round(ccs$Value[2]       * 365.25 * 39.3701 * 10, 2)  
SE  <- round(ccs$Std.Error[2]   * 365.25 * 39.3701 * 10, 3)
CI  <- round(1.96 * SE,2)

plt + geom_text(aes(x = as.Date('2020-01-01'), y = -0.5),
                label = paste0(EST, '%+-%', CI, '~inch~per~decade'),
                parse = TRUE, hjust = 1, family='Montserrat',
                size = 4.5)
#> Warning: Removed 59 row(s) containing missing values (geom_path).
```

<img src="SLR_Graphic_Powerpoint_files/figure-gfm/plot_slr_mllw-1.png" style="display: block; margin: auto;" />

``` r
# ggsave('figures/Portland_SLR_mllw_revised.png', type='cairo',
#         width = 7, height = 5)
# ggsave('figures/Portland_SLR_mllw_revised.pdf', 
#        device = cairo_pdf, width = 7, height = 5)
```
