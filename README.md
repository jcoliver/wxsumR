# wxsumR

Code for R package for calculating annual summary statistics of site-specific
climate data. Largely an R implementation of the Stata package available at [https://github.com/jdavidm/weather_command](https://github.com/jdavidm/weather_command).

## Dependencies

+ dplyr (>= 0.8.3),
+ lubridate (>= 1.7.4),
+ stringr (>= 1.3.1),
+ tidyr (>= 1.0.0)

## Installation

### Development version

The latest version is available through GitHub (you'll need the 
[devtools](https://cran.r-project.org/web/packages/devtools/index.html) 
package to install):

``` r
# install.packages("devtools")
devtools::install_github("jcoliver/wxsumR")
# to include vignette with devtools installation, pass build_vignettes = TRUE
# devtools::install_github("jcoliver/wxsumR", build_vignettes = TRUE)
```
