summary for camera data set
================

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
df <- read_csv(file = '../data/camera_dataset_processed.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Model = col_character(),
    ##   `Release date` = col_double(),
    ##   `Max resolution` = col_double(),
    ##   `Low resolution` = col_double(),
    ##   `Effective pixels` = col_double(),
    ##   `Zoom wide (W)` = col_double(),
    ##   `Zoom tele (T)` = col_double(),
    ##   `Normal focus range` = col_double(),
    ##   `Macro focus range` = col_double(),
    ##   `Storage included` = col_double(),
    ##   `Weight (inc. batteries)` = col_double(),
    ##   Dimensions = col_double(),
    ##   Price = col_double()
    ## )

## overview

``` r
head(df)
```

    ## # A tibble: 6 x 13
    ##   Model        `Release date` `Max resolution` `Low resolution` `Effective pixe~
    ##   <chr>                 <dbl>            <dbl>            <dbl>            <dbl>
    ## 1 Agfa ePhoto~           1997             1024              640                0
    ## 2 Agfa ePhoto~           1998             1280              640                1
    ## 3 Agfa ePhoto~           2000              640                0                0
    ## 4 Agfa ePhoto~           1999             1152              640                0
    ## 5 Agfa ePhoto~           1999             1152              640                0
    ## 6 Agfa ePhoto~           2001             1600              640                1
    ## # ... with 8 more variables: Zoom wide (W) <dbl>, Zoom tele (T) <dbl>,
    ## #   Normal focus range <dbl>, Macro focus range <dbl>, Storage included <dbl>,
    ## #   Weight (inc. batteries) <dbl>, Dimensions <dbl>, Price <dbl>

``` r
summary(df)
```

    ##     Model            Release date  Max resolution Low resolution
    ##  Length:1036        Min.   :1994   Min.   :   0   Min.   :   0  
    ##  Class :character   1st Qu.:2002   1st Qu.:2048   1st Qu.:1120  
    ##  Mode  :character   Median :2004   Median :2560   Median :2048  
    ##                     Mean   :2004   Mean   :2473   Mean   :1775  
    ##                     3rd Qu.:2006   3rd Qu.:3072   3rd Qu.:2560  
    ##                     Max.   :2007   Max.   :5616   Max.   :4992  
    ##  Effective pixels Zoom wide (W)   Zoom tele (T)   Normal focus range
    ##  Min.   : 0.00    Min.   : 0.00   Min.   :  0.0   Min.   :  0.00    
    ##  1st Qu.: 3.00    1st Qu.:35.00   1st Qu.: 96.0   1st Qu.: 30.00    
    ##  Median : 4.00    Median :36.00   Median :108.0   Median : 50.00    
    ##  Mean   : 4.59    Mean   :32.96   Mean   :121.5   Mean   : 44.13    
    ##  3rd Qu.: 7.00    3rd Qu.:38.00   3rd Qu.:117.0   3rd Qu.: 60.00    
    ##  Max.   :21.00    Max.   :52.00   Max.   :518.0   Max.   :120.00    
    ##  Macro focus range Storage included Weight (inc. batteries)   Dimensions   
    ##  Min.   : 0.000    Min.   :  0.00   Min.   :   0.0          Min.   :  0.0  
    ##  1st Qu.: 3.000    1st Qu.:  8.00   1st Qu.: 180.0          1st Qu.: 92.0  
    ##  Median : 6.000    Median : 16.00   Median : 226.0          Median :101.0  
    ##  Mean   : 7.786    Mean   : 17.45   Mean   : 319.3          Mean   :105.4  
    ##  3rd Qu.:10.000    3rd Qu.: 20.00   3rd Qu.: 350.0          3rd Qu.:115.0  
    ##  Max.   :85.000    Max.   :450.00   Max.   :1860.0          Max.   :240.0  
    ##      Price       
    ##  Min.   :  14.0  
    ##  1st Qu.: 149.0  
    ##  Median : 199.0  
    ##  Mean   : 457.9  
    ##  3rd Qu.: 399.0  
    ##  Max.   :7999.0

## observations from clean nb

## insights from describe uni

## insights from describe multi
