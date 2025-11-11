cross validation
================
Mukta Patwari
2025-11-11

Loading libraries

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-4. For overview type '?mgcv'.

``` r
library(p8105.datasets)

set.seed(1)
```

Load LIDAR

``` r
data("lidar")

lidar_df = 
  lidar |> 
  as_tibble() |>
  mutate(id = row_number())

lidar_df |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

## Create dataframes

``` r
train_df =
  sample_frac(lidar_df, size = .8) %>% 
  arrange(id)

test_df = anti_join(lidar_df, train_df, by = "id")
```

Look at these

``` r
ggplot(train_df, aes(x = range, y = logratio)) +
  geom_point() +
  geom_point(data = test_df, color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

Fit a few models to `train_df`

``` r
linear_mod = lm(logratio ~ range, data = train_df)
smooth_mod = mgcv::gam(logratio ~ s(range), data = train_df)
wiggly_mod = mgcv::gam(logratio ~ s(range, k = 30), sp = 10e-6, data = train_df)
```

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = range, y = logratio)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

RMSEs

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.127317

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.08302008

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.08848557

## iterate

``` r
cv_df =
  crossv_mc(lidar_df, n = 100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
```

Did this work?

``` r
cv_df %>% pull(train) %>% nth(3)
```

    ## # A tibble: 176 × 3
    ##    range logratio    id
    ##    <dbl>    <dbl> <int>
    ##  1   390  -0.0504     1
    ##  2   391  -0.0601     2
    ##  3   393  -0.0419     3
    ##  4   394  -0.0510     4
    ##  5   396  -0.0599     5
    ##  6   399  -0.0596     7
    ##  7   400  -0.0399     8
    ##  8   402  -0.0294     9
    ##  9   403  -0.0395    10
    ## 10   405  -0.0476    11
    ## # ℹ 166 more rows

Fitting models

``` r
cv_df %>% 
  mutate(
    linear_fits = map(train, \(df) lm(logratio ~ range, data = df)),
    smooth_fits = map(train, \(df) mgcv::gam(logratio ~ s(range), data = df)),
    wiggly_fits = map(train, \(df) mgcv::gam(logratio ~ s(range, k =50), sp = 10e-8, data = df))
    ) %>% 
  mutate(
    rmse_linear = map2_dbl(linear_fits, test, rmse),
    rmse_smooth = map2_dbl(smooth_fits, test, rmse),
    rmse_wiggly = map2_dbl(wiggly_fits, test, rmse)
  )
```

    ## # A tibble: 100 × 9
    ##    train    test     .id   linear_fits smooth_fits wiggly_fits rmse_linear
    ##    <list>   <list>   <chr> <list>      <list>      <list>            <dbl>
    ##  1 <tibble> <tibble> 001   <lm>        <gam>       <gam>             0.129
    ##  2 <tibble> <tibble> 002   <lm>        <gam>       <gam>             0.133
    ##  3 <tibble> <tibble> 003   <lm>        <gam>       <gam>             0.118
    ##  4 <tibble> <tibble> 004   <lm>        <gam>       <gam>             0.123
    ##  5 <tibble> <tibble> 005   <lm>        <gam>       <gam>             0.140
    ##  6 <tibble> <tibble> 006   <lm>        <gam>       <gam>             0.141
    ##  7 <tibble> <tibble> 007   <lm>        <gam>       <gam>             0.125
    ##  8 <tibble> <tibble> 008   <lm>        <gam>       <gam>             0.131
    ##  9 <tibble> <tibble> 009   <lm>        <gam>       <gam>             0.117
    ## 10 <tibble> <tibble> 010   <lm>        <gam>       <gam>             0.136
    ## # ℹ 90 more rows
    ## # ℹ 2 more variables: rmse_smooth <dbl>, rmse_wiggly <dbl>
