describe univariate for camera data set
================
Sascha Siegmund
2021-08-09

## purpose of notebook

-   [x] describe & visualize single variables (univariate)
-   [x] gather interesting observations for further investigation
-   [x] gather possible new features for extraction

## insights

-   brand column shows the count of cameras per brand, over 50% are
    produced by just 5 brands: Olympus, Sony, Canon, Kodak, Fujifilm

-   uni release\_date: with increasing year the number of cameras in the
    data also increase, almost linear, this could be a sign of more
    cameras available on the market or just from the data acquisition

-   comparing max\_resolution and low\_resolution shows that on median,
    the max resolution is higher, but both have a big spike around 2560
    (resp 2592)

-   effective\_pixels range between 0 and 13, mostly around 4 to 6, with
    two big spikes at 1 and 3

-   zoom\_wide\_W and zoom\_tele\_t are really far spread out but mostly
    centered around peaks, for zoom\_wide\_w its 28, 35, 38, for
    zoom\_tele\_t its 105a nd 114

-   normal\_focus\_range ranges mostly from 10 to 90 with only specific
    values and peaks at 30, 40, 50, 60, 90, but also rare values in
    between, macro\_focus\_range is centered around 7 with maximal 20 an
    modus 10, but quite some values in between compared to
    normal\_focus\_range

-   uni storage\_included: most data range from 1 to 48 mb with spikes
    at 8, 16, 32, but there are some big outliers, eg 450; not much
    information in this column except for technological progress and
    higher effective pixels require bigger storage, but these knowledge
    we already have from other variables, so lets drop this column in
    future

-   uni weight\_inc\_batteries: ranging from 100g to 600g with median
    230g, and a lot of outliers up to 1800g, distribution is left skewed

-   uni dimensions: ranging from 60 to 150 with median 100, also a big
    spike at 90, and some outliers up to 240

-   uni price: range is 14 to 600, with median 200 and spikes at 120,
    140, 220, and another isle at 400, outliers in the 1000 - 2000, but
    up to 8000, left skewed distribution

-   columns which can be dropped for future investigation:
    low\_resolution, storage\_included

## load packages

``` r
library(tidyverse) # tidy data frame
library(ggthemes) # for extra plot themes
library(plotly) # make ggplots interactive
```

## import data

``` r
df <- read_csv(file = '../data/camera_dataset_processed.csv') %>% column_to_rownames(var = 'model')
```

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

## univariate categorical brand

-   brand column shows the count of cameras per brand, over 50% are
    produced by just 5 brands: Olympus, Sony, Canon, Kodak, Fujifilm

``` r
# one variable, categorical x, show distribution
name = 'brand'
tmp_df <- df %>% rename(value = brand) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = fct_infreq(value))) +
    geom_bar() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.7, size=10))
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = fct_infreq(value))) +
    geom_boxplot(fill = 'lightgrey') +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) +
    theme(axis.text.x = element_text(angle=45, vjust=0.7, size=10))
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.9, 0.1), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-5-1.png)<!-- -->

``` r
# one variable, categorical x, 
# comparing the distribution of a variable against a hypothetical uniform distribution of that variable
name = c('camera count', 'brand share')
tmp_df <- df %>% rename(value = brand) %>% select(value)

library(gglorenz) #transformations for plotting Lorenz curve, https://github.com/jjchern/gglorenz

p1 <- tmp_df %>% 
  ggplot(aes(as.numeric(fct_infreq(value)))) +
    stat_lorenz(desc = FALSE) +
    coord_fixed() +
    geom_abline(linetype = 'dashed') +
    theme_minimal() +
    hrbrthemes::scale_x_percent() +
    hrbrthemes::scale_y_percent() +
    annotate_ineq(as.numeric(fct_infreq(tmp_df$value))) +
    ggtitle(paste("Lorenz curve for", name[1], sep=" ")) 
fig <- ggplotly(p1) %>% 
  layout(yaxis = list(title = paste("cumulative percentage of", name[1], sep=" ")), 
         xaxis = list(title = paste("cumulative percentage of", name[2], sep=" "))) 

fig
```

![](nb_figs/uni_unnamed-chunk-6-1.png)<!-- -->

## univariate numeric release\_date

-   uni release\_date: with increasing year the number of cameras in the
    data also increase, almost linear, this could be a sign of more
    cameras available on the market or just from the data acquisition

``` r
# one variable, continuous x, show distribution
name = 'release_date'
tmp_df <- df %>% rename(value = release_date) %>% select(value)

# https://ggplot2.tidyverse.org/reference/geom_dotplot.html
p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 1) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-7-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'release_date'
tmp_df <- df %>% rename(value = release_date) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-8-1.png)<!-- -->

## compare univariate max\_resolution and low\_resolution

-   comparing max\_resolution and low\_resolution shows that on median,
    the max resolution is higher, but both have a big spike around 2600

``` r
# two variables, both continuous x, compare distributions
name = c('max_resolution', 'low_resolution')
tmp_df <- df %>% rename(max_res = max_resolution, low_res = low_resolution) %>%
  select(max_res, low_res) %>% pivot_longer(cols = c(max_res, low_res))

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name)) +
    # geom_density(aes(colour = name), alpha = 0.5) +
    geom_histogram(binwidth = 100, alpha = 0.5, position = "identity") +
    theme_minimal() 
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, colour = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title = paste(name[1], "<br>", name[2], sep="")))

fig
```

![](nb_figs/uni_unnamed-chunk-9-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = c('max_resolution', 'low_resolution')
tmp_df <- df %>% rename(max_res = max_resolution, low_res = low_resolution) %>%
  select(max_res, low_res) %>% pivot_longer(cols = c(max_res, low_res)) %>% 
  add_count(value, name)

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name, color = name)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    # stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity', trim = TRUE) +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, color = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" "))
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

p3 <- tmp_df %>%
  ggplot(aes(x = value, color = name)) +
    stat_density(geom="line", position = 'identity', trim = TRUE) + 
    theme_minimal()
p3 <- ggplotly(p3) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

# https://plotly.com/r/subplots/
fig <- subplot(p3, p1, p2, nrows = 3, margin = 0, heights = c(0.1, 0.7, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title =name[1]))

fig
```

![](nb_figs/uni_unnamed-chunk-10-1.png)<!-- -->

## univariate numeric effective\_pixels

-   effective\_pixels range between 0 and 13, mostly around 4 to 6, with
    two big spikes at 1 and 3

``` r
# one variable, continuous x, show distribution
name = 'effective_pixels'
tmp_df <- df %>% rename(value = effective_pixels) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 1) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-11-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'effective_pixels'
tmp_df <- df %>% rename(value = effective_pixels) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-12-1.png)<!-- -->

## compare univariate zoom\_wide\_w and zoom\_tele\_t

-   zoom\_wide\_W and zoom\_tele\_t are really far spread out but mostly
    centered around peaks, for zoom\_wide\_w its 28, 35, 38, for
    zoom\_tele\_t its 105a nd 114

``` r
# two variables, both continuous x, compare distributions
name = c('zoom_wide_w', 'zoom_tele_t')
tmp_df <- df %>% rename(wide = zoom_wide_w, tele = zoom_tele_t) %>%
  select(wide, tele) %>% pivot_longer(cols = c(wide, tele))

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name)) +
    # geom_density(aes(colour = name), alpha = 0.5) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
    theme_minimal() 
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, colour = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title = paste(name[1], "<br>", name[2], sep="")))

fig
```

![](nb_figs/uni_unnamed-chunk-13-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = c('zoom_wide_w', 'zoom_tele_t')
tmp_df <- df %>% rename(wide = zoom_wide_w, tele = zoom_tele_t) %>%
  select(wide, tele) %>% pivot_longer(cols = c(wide, tele)) %>% 
  add_count(name, value)

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name, color = name)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    # stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity', trim = TRUE) +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, color = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" "))
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

p3 <- tmp_df %>%
  ggplot(aes(x = value, color = name)) +
    stat_density(geom="line", position = 'identity', trim = TRUE) + 
    theme_minimal()
p3 <- ggplotly(p3) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

# https://plotly.com/r/subplots/
fig <- subplot(p3, p1, p2, nrows = 3, margin = 0, heights = c(0.1, 0.7, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title =name[1]))

fig
```

![](nb_figs/uni_unnamed-chunk-14-1.png)<!-- -->

## compare univariate normal\_focus\_range and macro\_focus\_range

-   normal\_focus\_range ranges mostly from 10 to 90 with only specific
    values and peaks at 30, 40, 50, 60, 90, but also rare values in
    between, macro\_focus\_range is centered around 7 with maximal 20 an
    modus 10, but quite some values in between compared to
    normal\_focus\_range

``` r
# two variables, both continuous x, compare distributions
name = c('normal_focus_range', 'macro_focus_range')
tmp_df <- df %>% rename(normal = normal_focus_range, macro = macro_focus_range) %>%
  select(normal, macro) %>% pivot_longer(cols = c(normal, macro))

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name)) +
    # geom_density(aes(colour = name), alpha = 0.5) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
    theme_minimal() 
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, colour = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title = paste(name[1], "<br>", name[2], sep="")))

fig
```

![](nb_figs/uni_unnamed-chunk-15-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = c('normal_focus_range', 'macro_focus_range')
tmp_df <- df %>% rename(normal = normal_focus_range, macro = macro_focus_range) %>%
  select(normal, macro) %>% pivot_longer(cols = c(normal, macro)) %>% 
  add_count(name, value)

p1 <- tmp_df %>%
  ggplot(aes(x = value, fill = name, color = name)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    # stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity', trim = TRUE) +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = name, y = value, color = name)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("compare ", name[1], "and", name[2], sep=" "))
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

p3 <- tmp_df %>%
  ggplot(aes(x = value, color = name)) +
    stat_density(geom="line", position = 'identity', trim = TRUE) + 
    theme_minimal()
p3 <- ggplotly(p3) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

# https://plotly.com/r/subplots/
fig <- subplot(p3, p1, p2, nrows = 3, margin = 0, heights = c(0.1, 0.7, 0.2), shareX = TRUE) %>%
  layout(xaxis = list(title =name[1]))

fig
```

![](nb_figs/uni_unnamed-chunk-16-1.png)<!-- -->

## univariate numeric storage\_included

-   uni storage\_included: most data range from 1 to 48 mb with spikes
    at 8, 16, 32, but there are some big outliers, eg 450; not much
    information in this column except for technological progress and
    higher effective pixels require bigger storage, but these knowledge
    we already have from other variables, so lets drop this column in
    future

``` r
# one variable, continuous x, show distribution
name = 'storage_included'
tmp_df <- df %>% rename(value = storage_included) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 1) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-17-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'storage_included'
tmp_df <- df %>% rename(value = storage_included) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-18-1.png)<!-- -->

## univariate numeric weight\_inc\_batteries

-   uni weight\_inc\_batteries: ranging from 100g to 600g with median
    230g, and a lot of outliers up to 1800g, distribution is left skewed

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 10) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-19-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-20-1.png)<!-- -->

## univariate numeric dimensions

-   uni dimensions: ranging from 60 to 150 with median 100, also a big
    spike at 90, and some outliers up to 240

``` r
# one variable, continuous x, show distribution
name = 'dimensions'
tmp_df <- df %>% rename(value = dimensions) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 10) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-21-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'dimensions'
tmp_df <- df %>% rename(value = dimensions) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-22-1.png)<!-- -->

## univariate numeric price

-   uni price: range is 14 to 600, with median 200 and spikes at 120,
    140, 220, and another isle at 400, outliers in the 1000 - 2000, but
    up to 8000, left skewed distribution

``` r
# one variable, continuous x, show distribution
name = 'price'
tmp_df <- df %>% rename(value = price) %>% select(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_density() +
    geom_histogram(binwidth = 20) +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-23-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'price'
tmp_df <- df %>% rename(value = price) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y =..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1, tooltip = 'text') %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-24-1.png)<!-- -->

## experimental plot area

-   <https://cran.r-project.org/web/packages/ggbeeswarm/vignettes/usageExamples.pdf>
-   <https://stackoverflow.com/questions/32533356/is-there-a-way-to-make-the-density-function-in-r-use-counts-vs-probability>

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

tmp_sum_n <- tmp_df %>% distinct(value, .keep_all = TRUE) %>% select(n)
p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_histogram(aes(y = ..density..), binwidth = 10) +
    geom_point(aes(y = n), alpha = 0.5, stat = 'unique') +
    geom_spoke(aes(y = 0, angle = pi/2, radius = n), alpha = 0.5, stat = "unique") +
    stat_density(aes(y = ..density..* nrow(tmp_df %>%  distinct) * density(tmp_df$value, na.rm = TRUE)$bw), geom = 'line') +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-25-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

p3 <- tmp_df %>%
  distinct(value) %>%
  ggplot(aes(x = value)) +
    stat_density(geom = 'line') + 
    theme_minimal()
p3 <- ggplotly(p3) %>% layout()

p3
```

![](nb_figs/uni_unnamed-chunk-26-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

library(ggbeeswarm)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_quasirandom(aes(y = 0), groupOnX=FALSE, width = max(tmp_df$n), alpha = 0.25, bandwidth = 1/length(unique(tmp_df$value))) +
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line') +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-27-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.3, lwd = 1, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
  # ..density.. * sum(tmp_df$n) or ..scaled.. * max(tmp_df$n) or ..density.. * nrow(tmp_df) * density(tmp_df$value)$bw or ..count..
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-28-1.png)<!-- -->

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value) %>% na.omit()

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_spoke(aes(y = -n, radius = 2*n, angle = pi/2, text = paste0("value: ", value, "\ncount: ", n)),
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-29-1.png)<!-- -->

``` r
(max(tmp_df$value)-min(tmp_df$value))
```

    ## [1] 1760

``` r
# one variable, continuous x, show distribution
name = 'weight_inc_batteries'
tmp_df <- df %>% rename(value = weight_inc_batteries) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    geom_point(aes(y = n), alpha = 0.5, stat = 'unique') +
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line', position = 'identity') +
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-30-1.png)<!-- -->

``` r
set.seed(3)
# dat <- c(rnorm(100), rnorm(100,5))
# dat <- c(rbinom(500, 100, 0.4), rpois(500, 25))
# dat <- c(rbinom(500, 100, 0.4), rnorm(1000,5))
dat <- c(rbinom(100, 10, 0.3), rnorm(100,5), rep(10, 16))


name = 'weight_inc_batteries'
tmp_df <- as.tibble(dat) %>% rename(value = value) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_histogram(aes(y = ..density..), binwidth = 10) +
    # geom_point(aes(y = n), alpha = 0.2) +
    geom_spoke(aes(y = -n, angle = pi/2, radius = 2*n), 
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line') + # or ..scaled.. * max(tmp_df$n)
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-31-1.png)<!-- -->

``` r
set.seed(3)
# dat <- c(rnorm(100), rnorm(100,5))
# dat <- c(rbinom(500, 100, 0.4), rpois(500, 25))
# dat <- c(rbinom(500, 100, 0.4), rnorm(1000,5))
dat <- c(rbinom(100, 10, 0.3), rnorm(100,5), rep(10, 16))


name = 'weight_inc_batteries'
tmp_df <- as.tibble(dat) %>% rename(value = value) %>% select(value) %>% add_count(value)

p1 <- tmp_df %>%
  ggplot(aes(x = value)) +
    # geom_histogram(aes(y = ..density..), binwidth = 10) +
    # geom_point(aes(y = n), alpha = 0.2) +
    geom_spoke(aes(y = 0, angle = pi/2, radius = n), 
               alpha = 0.5, stat = "unique") +  # y = 0, radius = n for one-sided spoke plot
    stat_density(aes(y = ..scaled.. * max(tmp_df$n)), geom = 'line') + # or ..scaled.. * max(tmp_df$n)
    theme_minimal()  
p1 <- ggplotly(p1) %>% layout()

p2 <- tmp_df %>%
  ggplot(aes(x = 1, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste("distribution of", name, sep=" ")) 
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE))

# https://plotly.com/r/subplots/
fig <- subplot(p1, p2, nrows = 2, margin = 0, heights = c(0.8, 0.2), shareX = TRUE) %>% 
  layout(xaxis = list(title = name))

fig
```

![](nb_figs/uni_unnamed-chunk-32-1.png)<!-- -->
