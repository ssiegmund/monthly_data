summary for camera data set
================
Sascha Siegmund
2021-07-25

## purpose of notebook

-   [ ] summarize all insights and ideas from the other notebooks, as
    well as good exploratory plots

## information

found on: <https://www.kaggle.com/crawford/1000-cameras-dataset/>

with acknowledgements to:  
These datasets have been gathered and cleaned up by Petra Isenberg,
Pierre Dragicevic and Yvonne Jansen. The original source can be found
here <https://perso.telecom-paristech.fr/eagan/class/igr204/datasets>

licensed under: Creative Commons Attribution-ShareAlike 3.0 License
<https://creativecommons.org/licenses/by-sa/3.0/>

Context: Some camera enthusiast went and described 1,000 cameras based
on 13 properties!

## domain information

Content: The 13 properties of each camera  
1. Model  
2. Release date  
3. Max resolution  
4. Low resolution  
5. Effective pixels  
6. Zoom wide (W)  
7. Zoom tele (T)  
8. Normal focus range  
9. Macro focus range  
10. Storage included  
11. Weight (inc. batteries)  
12. Dimensions  
13. Price

-   <https://en.wikipedia.org/wiki/Image_resolution>
-   <https://www.adorama.com/alc/wide-angle-vs-telephoto-which-lens-should-you-choose/>
-   <https://photo.stackexchange.com/questions/86323/what-does-it-mean-when-macro-focus-range-and-normal-focus-range-are-equal>

## summary highlights

## stories

## load packages

``` r
library(tidyverse) # tidy data frame
library(plotly) # make ggplots interactive
```

## import data

``` r
df <- read_csv(file = '../data/camera_dataset_processed.csv') %>% column_to_rownames(var = 'model')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   model = col_character(),
    ##   brand = col_character(),
    ##   release_date = col_double(),
    ##   max_resolution = col_double(),
    ##   low_resolution = col_double(),
    ##   effective_pixels = col_double(),
    ##   zoom_wide_w = col_double(),
    ##   zoom_tele_t = col_double(),
    ##   normal_focus_range = col_double(),
    ##   macro_focus_range = col_double(),
    ##   storage_included = col_double(),
    ##   weight_inc_batteries = col_double(),
    ##   dimensions = col_double(),
    ##   price = col_double()
    ## )

## overview

``` r
head(df)
```

    ##                        brand release_date max_resolution low_resolution
    ## Agfa ePhoto 1280        Agfa         1997           1024            640
    ## Agfa ePhoto 1680        Agfa         1998           1280            640
    ## Agfa ePhoto CL18        Agfa         2000            640             NA
    ## Agfa ePhoto CL30        Agfa         1999           1152            640
    ## Agfa ePhoto CL30 Clik!  Agfa         1999           1152            640
    ## Agfa ePhoto CL45        Agfa         2001           1600            640
    ##                        effective_pixels zoom_wide_w zoom_tele_t
    ## Agfa ePhoto 1280                      0          38         114
    ## Agfa ePhoto 1680                      1          38         114
    ## Agfa ePhoto CL18                      0          45          45
    ## Agfa ePhoto CL30                      0          35          35
    ## Agfa ePhoto CL30 Clik!                0          43          43
    ## Agfa ePhoto CL45                      1          51          51
    ##                        normal_focus_range macro_focus_range storage_included
    ## Agfa ePhoto 1280                       70                40                4
    ## Agfa ePhoto 1680                       50                NA                4
    ## Agfa ePhoto CL18                       NA                NA                2
    ## Agfa ePhoto CL30                       NA                NA                4
    ## Agfa ePhoto CL30 Clik!                 50                NA               40
    ## Agfa ePhoto CL45                       50                20                8
    ##                        weight_inc_batteries dimensions price
    ## Agfa ePhoto 1280                        420         95   179
    ## Agfa ePhoto 1680                        420        158   179
    ## Agfa ePhoto CL18                         NA         NA   179
    ## Agfa ePhoto CL30                         NA         NA   269
    ## Agfa ePhoto CL30 Clik!                  300        128  1299
    ## Agfa ePhoto CL45                        270        119   179

``` r
summary(df)
```

    ##     brand            release_date  max_resolution low_resolution
    ##  Length:1038        Min.   :1994   Min.   : 512   Min.   : 320  
    ##  Class :character   1st Qu.:2002   1st Qu.:2048   1st Qu.:1280  
    ##  Mode  :character   Median :2004   Median :2560   Median :2048  
    ##                     Mean   :2004   Mean   :2477   Mean   :1871  
    ##                     3rd Qu.:2006   3rd Qu.:3072   3rd Qu.:2560  
    ##                     Max.   :2007   Max.   :5616   Max.   :4992  
    ##                                    NA's   :1      NA's   :54    
    ##  effective_pixels  zoom_wide_w    zoom_tele_t    normal_focus_range
    ##  Min.   : 0.000   Min.   :23.0   Min.   : 28.0   Min.   :  1.00    
    ##  1st Qu.: 3.000   1st Qu.:35.0   1st Qu.:102.0   1st Qu.: 40.00    
    ##  Median : 4.000   Median :36.0   Median :111.0   Median : 50.00    
    ##  Mean   : 4.596   Mean   :35.9   Mean   :132.4   Mean   : 50.86    
    ##  3rd Qu.: 7.000   3rd Qu.:38.0   3rd Qu.:117.0   3rd Qu.: 60.00    
    ##  Max.   :21.000   Max.   :52.0   Max.   :518.0   Max.   :120.00    
    ##                   NA's   :85     NA's   :85      NA's   :137       
    ##  macro_focus_range storage_included weight_inc_batteries   dimensions   
    ##  Min.   : 1.000    Min.   :  1.0    Min.   : 100.0       Min.   : 30.0  
    ##  1st Qu.: 4.000    1st Qu.:  8.0    1st Qu.: 180.0       1st Qu.: 92.0  
    ##  Median : 7.000    Median : 16.0    Median : 230.0       Median :102.0  
    ##  Mean   : 8.875    Mean   : 19.8    Mean   : 325.9       Mean   :106.8  
    ##  3rd Qu.:10.000    3rd Qu.: 24.0    3rd Qu.: 350.0       3rd Qu.:116.0  
    ##  Max.   :85.000    Max.   :450.0    Max.   :1860.0       Max.   :240.0  
    ##  NA's   :128       NA's   :125      NA's   :23           NA's   :16     
    ##      price       
    ##  Min.   :  14.0  
    ##  1st Qu.: 149.0  
    ##  Median : 199.0  
    ##  Mean   : 457.4  
    ##  3rd Qu.: 399.0  
    ##  Max.   :7999.0  
    ## 

## observations from clean nb

## insights from describe uni

## insights from describe multi
