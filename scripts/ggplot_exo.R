
R version 4.3.0 (2023-04-21 ucrt) -- "Already Tomorrow"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from C:/Users/jcolpitt/SpatialDataScience/Rprojects/R/.RData]

> (.packages())
[1] "stats"     "graphics"  "grDevices" "utils"     "datasets" 
[6] "methods"   "base"     
> 
  > 
  > library(here)
here() starts at C:/Users/jcolpitt/SpatialDataScience/Rprojects/R
> library(tidyverse)
── Attaching core tidyverse packages ────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.2     ✔ readr     2.1.4
✔ forcats   1.0.0     ✔ stringr   1.5.0
✔ ggplot2   3.4.2     ✔ tibble    3.2.1
✔ lubridate 1.9.2     ✔ tidyr     1.3.0
✔ purrr     1.0.1     
── Conflicts ──────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> library(sf)
Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE
> (.packages())
[1] "sf"        "lubridate" "forcats"   "stringr"   "dplyr"    
[6] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"  
[11] "tidyverse" "here"      "stats"     "graphics"  "grDevices"
[16] "utils"     "datasets"  "methods"   "base"     
> library(tidyverse)
> (.packages())
[1] "sf"        "lubridate" "forcats"   "stringr"   "dplyr"    
[6] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"  
[11] "tidyverse" "here"      "stats"     "graphics"  "grDevices"
[16] "utils"     "datasets"  "methods"   "base"     
> library(here)
> library(ggplot2)
> exo <- read_csv(here("data", "all_exoplanets.csv"))
Error: 'C:/Users/jcolpitt/SpatialDataScience/Rprojects/R/data/all_exoplanets.csv' does not exist.
> exo <- read_csv(here("data", "all_exoplanets_2021.csv"))
Rows: 4575 Columns: 23                                                  
── Column specification ──────────────────────────────────────────────────
Delimiter: ","
chr  (6): Planet Name, Planet Host, Discovery Method, Discovery Facili...
dbl (17): No., Num Stars, Num Planets, Discovery Year, Orbital Period ...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> summary(exo)
No.       Planet Name        Planet Host          Num Stars   
Min.   :   1   Length:4575        Length:4575        Min.   :1.00  
1st Qu.:1144   Class :character   Class :character   1st Qu.:1.00  
Median :2288   Mode  :character   Mode  :character   Median :1.00  
Mean   :2288                                         Mean   :1.11  
3rd Qu.:3432                                         3rd Qu.:1.00  
Max.   :4575                                         Max.   :4.00  

Num Planets    Discovery Method   Discovery Year Discovery Facility
Min.   :1.000   Length:4575        Min.   :1989   Length:4575       
1st Qu.:1.000   Class :character   1st Qu.:2014   Class :character  
Median :1.000   Mode  :character   Median :2016   Mode  :character  
Mean   :1.788                      Mean   :2015                     
3rd Qu.:2.000                      3rd Qu.:2016                     
Max.   :8.000                      Max.   :2021                     

Orbital Period Days Orbit Semi-Major Axis      Mass         
Min.   :0.00e+00    Min.   :   0.004      Min.   :    0.02  
1st Qu.:4.00e+00    1st Qu.:   0.056      1st Qu.:   16.15  
Median :1.20e+01    Median :   0.115      Median :  220.89  
Mean   :9.49e+04    Mean   :  10.940      Mean   :  725.12  
3rd Qu.:4.10e+01    3rd Qu.:   0.678      3rd Qu.:  740.53  
Max.   :4.02e+08    Max.   :7506.000      Max.   :17668.17  
NA's   :162         NA's   :1812          NA's   :2569      
  Eccentricity    Insolation Flux    Equilibrium Temperature
 Min.   :0.0000   Min.   :    0.06   Min.   :  50           
 1st Qu.:0.0140   1st Qu.:    5.71   1st Qu.: 619           
 Median :0.0920   Median :   45.70   Median : 961           
 Mean   :0.1549   Mean   :  446.66   Mean   :1058           
 3rd Qu.:0.2200   3rd Qu.:  226.52   3rd Qu.:1457           
 Max.   :0.9500   Max.   :44900.00   Max.   :4050           
 NA's   :2868     NA's   :4205       NA's   :3650           
Spectral Type      Stellar Effective Temperature Stellar Radius  
Length:4575        Min.   :  575                 Min.   : 0.010  
Class :character   1st Qu.: 4989                 1st Qu.: 0.790  
Mode  :character   Median : 5574                 Median : 0.960  
Mean   : 5439                 Mean   : 1.535  
3rd Qu.: 5908                 3rd Qu.: 1.250  
Max.   :40000                 Max.   :83.800  
NA's   :349                   NA's   :447     
Stellar Mass     Stellar Metallicity Stellar Metallicity Ratio
Min.   : 0.0100   Min.   :-1.0000     Length:4575              
1st Qu.: 0.7900   1st Qu.:-0.0700     Class :character         
Median : 0.9600   Median : 0.0200     Mode  :character         
Mean   : 0.9728   Mean   : 0.0129                              
3rd Qu.: 1.1100   3rd Qu.: 0.1100                              
Max.   :10.9400   Max.   : 0.5450                              
NA's   :731       NA's   :1371                                 
Stellar Surface Gravity    Distance        Gaia Magnitude  
Min.   :1.200           Min.   :   1.301   Min.   : 2.926  
1st Qu.:4.290           1st Qu.: 123.739   1st Qu.:10.750  
Median :4.450           Median : 434.754   Median :13.359  
Mean   :4.364           Mean   : 656.376   Mean   :12.432  
3rd Qu.:4.560           3rd Qu.: 856.417   3rd Qu.:14.780  
Max.   :7.920           Max.   :8200.000   Max.   :20.186  
NA's   :603             NA's   :104        NA's   :175     
> glimpse(exo)
Rows: 4,575
Columns: 23
$ No.                             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1…
$ `Planet Name`                   <chr> "11 Com b", "11 UMi b", "14 And …
$ `Planet Host`                   <chr> "11 Com", "11 UMi", "14 And", "1…
$ `Num Stars`                     <dbl> 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 1,…
$ `Num Planets`                   <dbl> 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2,…
$ `Discovery Method`              <chr> "Radial Velocity", "Radial Veloc…
$ `Discovery Year`                <dbl> 2007, 2009, 2008, 2002, 1996, 20…
$ `Discovery Facility`            <chr> "Xinglong Station", "Thueringer …
$ `Orbital Period Days`           <dbl> 326.0300, 516.2200, 185.8400, 17…
$ `Orbit Semi-Major Axis`         <dbl> 1.290, 1.530, 0.830, 2.930, 1.66…
$ Mass                            <dbl> 6165.6000, 4684.8142, 1525.5000,…
$ Eccentricity                    <dbl> 0.231, 0.080, 0.000, 0.370, 0.68…
$ `Insolation Flux`               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, …
$ `Equilibrium Temperature`       <dbl> NA, NA, NA, NA, NA, NA, NA, 1700…
$ `Spectral Type`                 <chr> "G8 III", NA, "K0 III", NA, NA, …
$ `Stellar Effective Temperature` <dbl> 4742, 4213, 4813, 5338, 5750, 41…
$ `Stellar Radius`                <dbl> 19.00, 29.79, 11.00, 0.93, 1.13,…
$ `Stellar Mass`                  <dbl> 2.70, 2.78, 2.20, 0.90, 1.08, 1.…
$ `Stellar Metallicity`           <dbl> -0.350, -0.020, -0.240, 0.410, 0…
$ `Stellar Metallicity Ratio`     <chr> "[Fe/H]", "[Fe/H]", "[Fe/H]", "[…
$ `Stellar Surface Gravity`       <dbl> 2.31, 1.93, 2.63, 4.45, 4.36, 1.…
$ Distance                        <dbl> 93.1846, 125.3210, 75.4392, 17.9…
$ `Gaia Magnitude`                <dbl> 4.44038, 4.56216, 4.91781, 6.383…
> print("x axis will be discovery year, y axis will be distance")
[1] "x axis will be discovery year, y axis will be distance"
> exovis <- ggplot(data = exo, mapping = aes(Discovery Year, Distance))
Error: unexpected symbol in "exovis <- ggplot(data = exo, mapping = aes(Discovery Year"
> exovis <- ggplot(data = exo, mapping = aes("Discovery Year", "Distance"))
> exovis
> exovis <- ggplot(data = exo, mapping = aes(x = Discovery Year, y = Distance))
Error: unexpected symbol in "exovis <- ggplot(data = exo, mapping = aes(x = Discovery Year"
> exovis <- ggplot(data = exo, mapping = aes(x = "Discovery Year", y = "Distance"))
> exovis
> exovis <- ggplot(data = exo, mapping = aes(x = Discovery_Year, y = Distance))
> exovis
Error in `geom_blank()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
! object 'Discovery_Year' not found
Run `rlang::last_trace()` to see where the error occurred.
> exovis <- ggplot(data = exo, mapping = aes(x = Discovery Year, y = Distance))
Error: unexpected symbol in "exovis <- ggplot(data = exo, mapping = aes(x = Discovery Year"
> 
> library(janitor)
Error in library(janitor) : there is no package called ‘janitor’
> install.packages("janitor")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/jcolpitt/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
also installing the dependency ‘snakecase’

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/snakecase_0.11.0.zip'
Content type 'application/zip' length 166695 bytes (162 KB)
downloaded 162 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/janitor_2.2.0.zip'
Content type 'application/zip' length 285239 bytes (278 KB)
downloaded 278 KB

package ‘snakecase’ successfully unpacked and MD5 sums checked
package ‘janitor’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\jcolpitt\AppData\Local\Temp\RtmpWUgUTl\downloaded_packages
> library(janitor)

Attaching package: ‘janitor’

The following objects are masked from ‘package:stats’:

    chisq.test, fisher.test

> exodata <- clean_names(exo)
> glimpse(exodata)
Rows: 4,575
Columns: 23
$ no                            <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, …
$ planet_name                   <chr> "11 Com b", "11 UMi b", "14 And b",…
$ planet_host                   <chr> "11 Com", "11 UMi", "14 And", "14 H…
$ num_stars                     <dbl> 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 1, 2,…
$ num_planets                   <dbl> 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1,…
$ discovery_method              <chr> "Radial Velocity", "Radial Velocity…
$ discovery_year                <dbl> 2007, 2009, 2008, 2002, 1996, 2020,…
$ discovery_facility            <chr> "Xinglong Station", "Thueringer Lan…
$ orbital_period_days           <dbl> 326.0300, 516.2200, 185.8400, 1773.…
$ orbit_semi_major_axis         <dbl> 1.290, 1.530, 0.830, 2.930, 1.660, …
$ mass                          <dbl> 6165.6000, 4684.8142, 1525.5000, 14…
$ eccentricity                  <dbl> 0.231, 0.080, 0.000, 0.370, 0.680, …
$ insolation_flux               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ equilibrium_temperature       <dbl> NA, NA, NA, NA, NA, NA, NA, 1700, N…
$ spectral_type                 <chr> "G8 III", NA, "K0 III", NA, NA, "K3…
$ stellar_effective_temperature <dbl> 4742, 4213, 4813, 5338, 5750, 4157,…
$ stellar_radius                <dbl> 19.00, 29.79, 11.00, 0.93, 1.13, 25…
$ stellar_mass                  <dbl> 2.70, 2.78, 2.20, 0.90, 1.08, 1.22,…
$ stellar_metallicity           <dbl> -0.350, -0.020, -0.240, 0.410, 0.06…
$ stellar_metallicity_ratio     <chr> "[Fe/H]", "[Fe/H]", "[Fe/H]", "[Fe/…
$ stellar_surface_gravity       <dbl> 2.31, 1.93, 2.63, 4.45, 4.36, 1.70,…
$ distance                      <dbl> 93.1846, 125.3210, 75.4392, 17.9323…
$ gaia_magnitude                <dbl> 4.44038, 4.56216, 4.91781, 6.38300,…
> exovis <- ggplot(data = exo, mapping = aes(x = discovery_year, y = distance))
> exovis
Error in `geom_blank()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
! object 'discovery_year' not found
Run `rlang::last_trace()` to see where the error occurred.
> exovis <- ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance))
> exovis
> 
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance)) +
+     geom_point()
Warning message:
Removed 104 rows containing missing values (`geom_point()`). 
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance, colour = gaia_magnitude)) +
+     +     geom_point()
Error in `+.gg`:
! Cannot use `+` with a single argument
ℹ Did you accidentally put `+` on a new line?
Run `rlang::last_trace()` to see where the error occurred.
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance))
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance)) +
+     +     geom_point()
Error in `+.gg`:
! Cannot use `+` with a single argument
ℹ Did you accidentally put `+` on a new line?
Run `rlang::last_trace()` to see where the error occurred.
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance)) +geom_point()
Warning message:
Removed 104 rows containing missing values (`geom_point()`). 
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance, colour = gaia_magnitude)) +geom_point()
Warning message:
Removed 104 rows containing missing values (`geom_point()`). 
> ggplot(data = exodata, mapping = aes(x = discovery_year, y = distance, colour = mass)) +geom_point()
Warning message:
Removed 104 rows containing missing values (`geom_point()`)