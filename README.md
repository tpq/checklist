
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

<!-- badges: start -->
<!-- badges: end -->

At its core, checklist is a base-R implementation of recursive feature
elimination (RFE), a search algorithm that seeks to identify a minimal
set of features that minimise the total loss of a fitted model. The
algorithm is written in a generic way so that it can use any
user-defined loss function that takes the arguments ‘x’ and ‘y’ and
returns a numeric value.

The package offers two additional features:

1)  Feature elimination can be sped up by computing the loss on a
    proportion (‘prop’) of the data, or by eliminating several (‘k’)
    features per step.

2)  Custom functions enable RFE to be used to select features for a
    linear model where all features are binary variables and all
    coefficients are integers, yielding a model that is simply a
    point-based checklist.

You can install the development version of checklist like so:

``` r
devtools::install_github("tpq/checklist")
library(checklist)
```

## Recursive Feature Elimination

Let us make some data where features 2 and 7 are very important, while
features 1, 9, and 10 are not important at all.

``` r
M <- matrix(runif(1000), 100, 10)
colnames(M) <- paste0("V", 1:10)
y <- M[,2] + M[,7] + rowSums(M[,2:8]) + 2*runif(100)
```

We can eliminate unimportant features, but first we need to define the
model and its loss. For this, we will use linear regression with mean
absolute error.

``` r
my_loss <- function(x, y) {
  m = lm(y ~ ., data.frame(x, y))
  yhat = predict(m, x)
  sum(abs(y - yhat))
}
```

Now, let us eliminate unimportant features.

``` r
res <- rfe(M, y, my_loss)
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 9 ... 10 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 9 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 
#> 1 ... 2 ... 3 ... 4 ... 
#> 1 ... 2 ... 3 ... 
#> 1 ... 2 ... 
#> 1 ...
res
#>    Step Rank Column Loss..Before.Elim.
#> 1    10    1     V7           72.50245
#> 2     9    2     V4           65.26097
#> 3     8    3     V2           60.27359
#> 4     7    4     V6           53.50381
#> 5     6    5     V5           50.97691
#> 6     5    6     V8           48.39547
#> 7     4    7     V3           48.04816
#> 8     3    8     V1           47.60356
#> 9     2    9    V10           47.52314
#> 10    1   10     V9           47.48091
```

Judging from above, we can do pretty well with just 4 of the 10
features.

## Making A Checklist

In checklist, we define a checklist as a linear model with integer
coefficients. When coefficients are integers, it is feasible for a human
to calculate predictions “by hand”. When the number of features are few,
the model is easy to interpret and thus easy to implement in practice.

We wrote this tool to make it easy to make a checklist from your data.
The tool will calculate the integer coefficients and eliminate the
unimportant features. Ideally, the features used in a checklist are
binary, but it is up to you to make them binary. Here are some binary
data.

``` r
M <- matrix(sample(0:1, 1000, replace = T), 100, 10)
colnames(M) <- paste0("V", 1:10)
y <- M[,2] + M[,7] + rowSums(M[,2:8]) + 2*runif(100)
```

A checklist is fit like a typical model. We use non-parametric
correlation as the loss because we expect that the final tallied score
will have a different scale than the target variable. Note that the
checklist has no Intercept. Use `u` to tune the size of the integers in
the checklist.

``` r
chk = checklist(M, y, u = 5)
yhat = predict(chk, M)
cor(yhat, y, method = "spearman")
#> [1] 0.9495614
```

The package contains a checklist loss for use with RFE. The loss is 1
minus the Spearman correlation between the tallied score and the target
variable.

``` r
checklist_loss <-
  function(x, y) {
    chk = checklist(x, y, u = 5)
    yhat = predict(chk, x)
    1 - cor(yhat, y, method = "spearman")
  }
```

We can now eliminate features.

``` r
res <- rfe(M, y, checklist_loss)
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 9 ... 10 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 9 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 8 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 7 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 6 ... 
#> 1 ... 2 ... 3 ... 4 ... 5 ... 
#> 1 ... 2 ... 3 ... 4 ... 
#> 1 ... 2 ... 3 ... 
#> 1 ... 2 ... 
#> 1 ...
#> Warning in cor(yhat, y, method = "spearman"): the standard deviation is zero
res
#>    Step Rank Column Loss..Before.Elim.
#> 1    10    1     V2         0.44405147
#> 2     9    2     V7         0.30433920
#> 3     8    3     V8         0.21081588
#> 4     7    4     V6         0.15476059
#> 5     6    5     V4         0.11587638
#> 6     5    6     V3         0.07982728
#> 7     4    7     V5         0.05043856
#> 8     3    8    V10         0.05043856
#> 9     2    9     V9         0.05043856
#> 10    1   10     V1         0.05043856
```

We can now fit a 4 feature model.

``` r
best4 <- res$Column[1:4]
M_reduced <- M[,best4]
chk <- checklist(M_reduced, y, u = 5)
yhat = predict(chk, M_reduced)
cor(yhat, y, method = "spearman")
#> [1] 0.8452394
```

The ‘$points’ slot stores the points per feature.

``` r
chk$points
#> V2 V7 V8 V6 
#>  6  5  3  4
```
