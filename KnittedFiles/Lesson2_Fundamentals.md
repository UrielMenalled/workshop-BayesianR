Bayesian data analysis in R: Working with Bayesian models
================

Remember, the fundamental interest of a Bayesian approach is to use
probabilities, which are defined by **prior** hypotheses and
**posterior** corrections to prior hypotheses based on the data. You
then work with probability distributions to describe and compare data.
Working with probability distributions is based on taking large random
samples of the probability distribution if this sample is large enough,
each value will occur roughly proportionally often to how probable it
is.

``` r
source("SetUp_Code.R") #loading in packages and the prop_model function
```

    ## ℹ Loading metadata database✔ Loading metadata database ... done
    ##  
    ## ℹ No downloads are needed
    ## ✔ 5 pkgs + 142 deps: kept 137 [5s]

    ## Warning: package 'ggplot2' was built under R version 4.5.2

    ## Warning: package 'readr' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.2.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## Loading required package: usethis

``` r
set.seed(123)
```

To understand what fundamentally happening, let’s start by doing a
Bayesian analysis by hand. Let’s assume we have an experiment with 2/13
successes, meaning we KNOW that the success rate is 15%

``` r
#replicating the example above, if there are 2/13 = 15% successes
prop_success <- 0.15
n_samples <- 13

#simulating data using a binomial probability distribution
rbinom(n = 1, size = 13, prob = 0.15) #same as before, this shows number of successes with one experiment of 13 samples
```

    ## [1] 1

``` r
rbinom(n = 10000, size = 13, prob = 0.15) #now shows number of successes with 10,000 experiments, each with 13 samples
```

    ##     [1] 3 2 4 4 0 2 4 2 2 4 2 2 2 0 4 1 0 1 4 4 3 2 6 2 3 2 2 1 1 4 4 2 3 0 2 3
    ##    [37] 1 1 1 1 2 2 1 1 1 1 2 1 3 0 2 3 1 2 1 1 3 4 1 2 0 1 1 3 2 3 3 3 2 3 2 3
    ##    [73] 0 2 1 1 2 1 0 1 2 2 3 0 2 5 4 4 1 1 2 1 2 1 1 3 0 2 2 2 1 2 4 2 4 4 2 2
    ##   [109] 1 4 1 0 4 3 1 2 4 2 2 2 1 1 1 1 5 1 0 1 2 2 4 2 3 2 2 3 3 5 2 1 2 0 1 3
    ##   [145] 1 1 0 1 3 3 2 1 1 0 1 2 1 2 1 2 1 2 1 1 2 3 1 2 1 2 1 3 3 2 2 1 2 3 2 3
    ##   [181] 1 3 1 2 2 1 2 4 4 1 1 5 2 4 2 2 2 1 2 1 4 2 2 2 3 1 1 1 1 2 1 1 2 0 3 1
    ##   [217] 2 3 4 1 4 3 2 0 1 2 2 3 4 2 2 2 0 1 1 1 3 1 3 2 2 1 2 1 3 2 5 5 3 1 1 2
    ##   [253] 1 2 3 1 2 2 3 4 3 2 4 2 2 1 1 0 2 3 0 0 1 3 3 5 2 0 2 3 1 1 1 0 1 0 1 0
    ##   [289] 2 1 0 0 3 3 3 5 0 0 3 3 0 3 3 2 2 1 0 2 2 1 2 3 0 1 3 3 1 1 3 3 1 1 3 0
    ##   [325] 0 7 0 1 4 2 1 3 3 1 2 3 2 2 5 2 0 2 1 2 1 5 1 1 2 1 5 2 1 2 5 2 2 1 3 1
    ##   [361] 1 4 1 1 3 1 0 2 2 2 1 3 2 2 4 3 1 1 5 2 3 1 3 2 4 1 1 0 1 5 1 3 3 0 1 1
    ##   [397] 0 0 5 5 1 4 2 1 2 3 0 1 2 1 3 1 2 1 1 4 1 2 0 2 2 2 1 4 2 1 1 1 3 4 2 1
    ##   [433] 6 1 2 0 2 0 1 1 2 3 1 3 3 3 1 1 2 2 1 1 0 1 4 3 3 1 1 5 2 2 1 2 1 1 2 1
    ##   [469] 3 0 3 2 3 2 2 2 0 1 3 1 3 1 2 4 0 1 2 2 3 5 0 2 3 0 6 1 0 2 3 1 1 1 0 1
    ##   [505] 1 2 2 4 1 2 0 3 1 3 1 2 3 0 3 1 1 1 0 2 2 4 0 5 1 4 2 2 3 0 3 1 3 2 1 3
    ##   [541] 0 3 1 3 4 0 3 3 1 1 1 2 4 2 2 2 2 2 0 0 4 3 1 2 2 1 3 2 4 2 4 2 2 4 3 2
    ##   [577] 0 2 2 3 3 3 1 3 1 1 2 5 2 2 0 5 0 0 3 2 1 4 0 1 2 1 1 1 3 1 3 1 1 2 0 0
    ##   [613] 6 2 3 4 3 5 0 4 3 3 1 0 1 1 3 1 1 3 3 2 3 0 1 4 3 3 2 2 3 4 1 0 2 3 0 1
    ##   [649] 2 3 4 2 2 3 3 4 0 2 1 3 3 0 3 1 0 4 1 2 1 0 2 0 1 2 2 3 1 2 2 3 3 3 4 4
    ##   [685] 2 2 1 2 1 0 2 2 1 1 2 1 0 2 1 3 1 0 3 1 2 2 1 1 2 2 2 2 3 2 1 0 0 6 0 2
    ##   [721] 3 3 3 2 3 3 2 3 0 2 3 2 4 1 1 3 5 1 0 5 1 0 2 1 2 3 0 3 4 1 1 1 0 2 2 2
    ##   [757] 4 0 1 2 1 2 2 2 1 1 2 2 2 4 3 2 3 1 1 1 2 2 3 1 3 0 2 3 2 1 0 1 4 4 2 1
    ##   [793] 2 3 2 0 1 1 1 2 1 1 0 1 5 2 2 3 2 0 3 1 4 3 1 1 1 2 2 2 1 2 5 3 0 2 0 1
    ##   [829] 3 0 3 2 3 2 2 3 2 1 0 4 3 2 0 2 1 4 0 1 1 1 3 5 3 2 2 4 0 0 4 2 0 2 2 2
    ##   [865] 1 1 0 2 2 1 1 3 2 3 2 0 3 4 2 0 4 2 1 3 4 2 1 1 1 2 3 2 2 3 3 0 1 0 3 4
    ##   [901] 2 3 2 2 2 3 4 3 0 0 0 3 2 1 4 2 2 1 1 3 0 1 3 3 2 1 2 1 3 4 1 3 2 4 1 2
    ##   [937] 3 3 0 5 2 1 2 0 1 2 1 2 1 2 1 4 0 1 4 2 0 2 2 2 2 2 1 4 2 2 0 3 2 3 0 3
    ##   [973] 1 1 2 4 0 1 2 2 3 4 2 4 3 1 1 1 2 0 1 2 2 2 3 2 1 3 0 1 2 1 3 3 2 3 1 0
    ##  [1009] 2 2 1 1 2 0 3 2 2 2 1 2 3 2 1 3 1 2 2 1 2 1 2 2 1 2 2 1 2 2 2 3 3 2 2 1
    ##  [1045] 0 1 0 3 6 3 3 4 2 2 1 2 1 1 3 3 1 1 1 0 3 2 1 5 2 3 3 3 3 2 2 1 0 2 1 1
    ##  [1081] 1 1 2 3 1 0 0 3 2 3 0 0 1 3 3 5 2 1 1 3 3 1 3 3 2 2 4 4 2 0 3 2 1 1 1 2
    ##  [1117] 4 1 2 1 1 3 1 1 0 4 2 0 2 2 3 3 2 3 3 1 1 2 1 3 2 0 4 1 2 2 2 1 1 4 2 3
    ##  [1153] 4 2 1 0 3 2 1 1 1 2 1 0 0 2 1 1 5 2 2 2 4 1 2 3 1 0 2 0 1 2 3 3 1 1 3 3
    ##  [1189] 2 1 2 2 3 0 1 3 0 5 1 3 4 2 3 2 5 0 1 3 2 2 7 1 2 0 3 1 3 1 4 1 2 0 4 4
    ##  [1225] 0 2 1 3 2 0 1 1 2 1 0 0 3 4 1 0 1 2 2 3 1 2 0 1 3 3 2 2 0 1 3 0 1 5 2 1
    ##  [1261] 2 0 1 3 0 2 4 0 5 0 3 2 2 3 2 3 2 1 1 1 3 2 3 2 1 3 2 1 2 3 2 1 3 1 1 3
    ##  [1297] 3 3 2 0 2 2 3 3 2 0 1 2 0 1 1 3 1 3 0 3 1 2 0 3 1 4 1 5 1 2 2 0 2 2 1 2
    ##  [1333] 2 2 1 3 3 3 2 1 2 1 1 2 4 1 2 2 2 0 0 1 0 0 2 1 3 1 2 2 1 3 2 1 1 1 2 1
    ##  [1369] 1 1 1 2 2 2 3 1 4 0 2 2 1 4 0 4 1 1 3 0 1 6 2 2 1 2 3 2 2 0 1 1 1 0 2 1
    ##  [1405] 3 2 4 0 4 3 4 2 1 1 3 2 1 3 1 3 2 2 4 2 0 2 5 4 2 3 1 3 3 1 1 2 3 2 2 1
    ##  [1441] 2 0 2 3 3 0 2 2 1 2 3 3 0 2 3 1 3 3 0 4 3 3 1 0 3 4 1 2 2 3 0 3 1 2 3 2
    ##  [1477] 7 3 3 3 0 3 3 5 0 3 2 2 3 0 0 6 3 1 3 5 1 2 2 4 5 2 1 3 2 2 2 2 2 2 4 3
    ##  [1513] 1 2 2 0 1 2 1 3 0 3 2 3 0 0 2 2 1 3 3 4 2 2 1 1 1 4 2 0 3 1 2 2 1 0 2 2
    ##  [1549] 5 5 0 2 1 1 1 2 1 1 5 3 1 3 1 2 1 0 4 0 3 1 0 3 1 0 3 0 2 1 2 2 0 1 0 3
    ##  [1585] 2 2 1 2 1 3 4 2 2 1 0 2 1 3 1 2 1 1 3 3 0 3 1 2 2 2 3 2 5 5 1 2 1 2 1 0
    ##  [1621] 0 0 3 2 3 2 1 2 4 3 2 2 2 4 4 1 1 0 3 0 3 3 4 2 4 3 3 1 0 1 3 1 1 0 2 1
    ##  [1657] 4 0 3 3 0 1 2 3 2 4 1 2 1 2 2 0 3 1 2 2 2 2 1 1 2 6 1 1 0 2 2 3 4 2 0 1
    ##  [1693] 3 3 3 4 2 2 1 3 0 2 4 3 3 1 2 5 2 1 0 0 3 1 4 2 2 4 0 3 1 2 1 0 3 1 2 2
    ##  [1729] 1 6 2 0 4 3 2 1 3 4 3 1 5 2 0 1 0 3 3 4 1 5 3 3 2 5 2 4 2 2 4 2 0 1 2 2
    ##  [1765] 5 4 1 3 4 2 2 1 2 1 2 1 1 2 2 1 1 3 2 1 0 5 1 2 3 1 0 2 3 1 1 2 4 2 2 1
    ##  [1801] 3 1 2 2 0 4 2 3 2 1 2 3 2 2 2 2 2 2 0 6 2 0 3 2 3 0 3 3 3 3 2 4 1 0 1 2
    ##  [1837] 1 4 3 1 2 2 3 2 0 2 1 3 1 4 3 2 0 2 2 6 1 2 4 2 2 0 3 2 1 2 4 2 2 1 3 1
    ##  [1873] 1 1 4 2 1 4 1 3 1 1 1 0 3 1 1 1 2 2 5 1 3 5 2 1 3 0 2 2 1 1 3 2 2 2 3 2
    ##  [1909] 2 0 2 2 2 2 0 0 2 3 1 3 2 5 2 2 1 2 0 2 4 2 2 0 0 0 1 2 3 1 3 2 1 0 2 0
    ##  [1945] 3 1 2 3 1 1 3 1 1 2 2 1 3 2 1 5 1 1 1 1 1 1 0 4 2 1 3 2 2 3 2 1 1 4 1 3
    ##  [1981] 2 1 1 2 1 2 2 2 1 2 1 3 1 0 4 1 3 2 2 1 1 1 2 2 2 2 0 0 1 3 1 2 2 5 1 3
    ##  [2017] 1 1 0 6 2 6 1 0 1 2 2 2 3 2 2 2 1 0 3 2 4 4 3 2 1 2 3 4 4 3 1 0 2 2 0 4
    ##  [2053] 1 4 1 2 3 3 2 0 3 2 3 1 0 1 1 3 4 1 4 2 2 2 3 2 1 4 0 1 3 3 0 2 2 1 5 2
    ##  [2089] 1 1 1 0 0 1 3 4 2 2 0 2 1 2 4 2 3 1 1 3 3 3 1 1 2 2 1 3 0 3 3 4 2 1 1 3
    ##  [2125] 0 3 2 2 4 1 1 1 1 2 2 2 3 4 2 3 1 1 0 1 0 1 1 2 3 1 4 3 1 1 1 2 3 1 2 5
    ##  [2161] 1 2 3 2 6 0 5 3 2 1 2 1 2 3 0 1 3 0 3 4 2 0 2 3 0 3 3 1 3 2 1 2 4 3 3 2
    ##  [2197] 2 2 1 3 3 3 0 1 2 0 3 2 1 1 2 1 0 2 0 0 2 2 1 1 2 3 0 5 0 1 1 3 1 3 3 2
    ##  [2233] 2 1 2 4 1 0 5 1 2 0 5 4 3 2 3 1 1 7 5 2 3 1 1 2 1 0 5 2 3 2 1 2 0 1 4 1
    ##  [2269] 1 1 1 1 0 4 4 4 2 2 0 1 4 1 2 2 1 2 0 2 2 1 4 1 2 4 1 1 3 2 3 2 4 1 2 3
    ##  [2305] 1 2 2 4 1 2 0 1 2 2 3 0 2 1 2 3 2 1 3 2 1 2 0 1 2 0 1 2 3 3 0 0 4 4 4 2
    ##  [2341] 1 4 4 1 0 2 1 2 2 3 3 1 3 2 0 0 1 1 2 2 3 1 4 2 1 1 2 3 2 3 2 1 2 1 1 4
    ##  [2377] 2 0 4 1 0 5 1 5 1 2 3 1 3 2 0 1 2 4 6 1 2 2 4 3 2 1 4 3 1 1 2 3 2 3 2 1
    ##  [2413] 0 3 0 1 2 2 1 2 3 1 1 2 2 0 3 2 3 3 1 0 3 0 2 1 2 0 3 2 1 1 0 0 3 1 0 1
    ##  [2449] 2 3 1 1 3 3 5 2 1 2 2 1 2 5 1 2 1 0 0 2 0 0 1 1 2 1 2 3 1 3 1 1 1 3 1 4
    ##  [2485] 1 1 2 1 3 3 1 2 4 3 3 4 1 2 1 3 2 3 1 2 2 2 3 4 2 1 1 1 1 3 2 2 2 2 1 3
    ##  [2521] 2 3 1 1 4 1 0 2 4 3 2 2 2 4 0 2 3 3 4 3 1 3 3 2 4 1 4 2 1 2 1 2 2 3 2 2
    ##  [2557] 1 2 1 1 3 2 2 2 3 0 1 0 3 2 2 1 2 0 0 0 0 1 3 3 0 4 1 2 2 4 3 2 1 2 3 2
    ##  [2593] 0 1 3 2 3 3 1 1 2 1 4 1 1 2 1 2 2 3 1 0 2 2 3 1 2 2 4 4 1 1 3 3 2 1 1 1
    ##  [2629] 0 2 3 1 2 0 0 4 2 2 2 1 4 0 1 5 3 7 1 2 0 3 3 4 0 1 2 1 3 0 2 1 1 3 1 1
    ##  [2665] 2 2 2 2 1 0 1 3 1 0 0 2 3 1 2 2 1 4 4 1 3 1 4 0 5 1 1 1 2 2 6 1 3 2 5 2
    ##  [2701] 3 0 2 1 4 3 3 3 2 5 2 2 1 0 4 1 1 2 2 5 0 2 4 3 4 3 1 1 0 2 3 4 2 3 2 2
    ##  [2737] 3 2 3 1 1 0 0 4 3 0 0 1 2 3 0 0 2 1 2 0 4 3 2 2 1 2 1 2 2 3 1 3 2 0 1 2
    ##  [2773] 2 2 3 3 0 0 1 1 3 1 5 3 2 4 0 3 4 0 2 2 2 3 2 2 2 1 4 1 2 1 0 3 2 1 4 2
    ##  [2809] 4 4 1 3 3 4 1 0 5 1 3 2 1 3 3 1 1 3 1 2 1 2 2 3 3 2 1 3 2 3 3 2 2 3 3 2
    ##  [2845] 3 3 1 1 1 4 1 2 0 3 2 2 4 1 3 3 1 0 1 2 3 2 4 0 1 2 0 0 1 3 3 1 1 1 4 2
    ##  [2881] 3 4 2 1 4 3 0 2 3 0 4 0 1 0 1 3 2 2 0 5 2 3 1 3 2 4 1 1 3 3 2 3 1 4 2 3
    ##  [2917] 3 2 3 1 2 3 1 2 0 3 4 2 2 2 1 2 0 3 1 2 1 1 2 1 3 0 0 1 4 2 2 0 2 0 2 3
    ##  [2953] 1 1 3 0 3 2 0 4 1 3 2 2 1 3 0 1 1 3 1 1 0 2 2 5 1 3 0 3 1 1 1 1 4 5 1 1
    ##  [2989] 2 2 1 3 1 3 1 3 0 2 2 1 4 1 2 1 2 3 2 3 3 5 2 2 0 1 0 3 2 3 1 2 3 0 1 3
    ##  [3025] 3 3 4 1 4 3 2 1 1 3 2 2 0 1 4 2 0 1 0 1 3 1 2 2 3 2 1 4 0 0 3 2 3 3 5 4
    ##  [3061] 1 2 0 2 3 1 4 2 2 3 3 2 1 2 3 2 2 3 3 3 0 2 1 2 1 4 2 2 0 4 3 2 1 3 2 0
    ##  [3097] 0 2 1 2 5 1 3 1 2 3 1 3 1 0 1 2 2 1 5 1 4 3 2 1 1 3 4 2 4 3 2 1 4 1 2 3
    ##  [3133] 1 2 1 2 1 2 2 4 1 1 1 1 1 1 4 0 2 3 3 2 1 2 1 2 0 1 2 1 1 3 1 0 3 2 2 1
    ##  [3169] 1 2 0 1 3 1 0 4 0 3 3 0 3 2 1 1 2 1 1 2 3 2 2 1 4 5 3 0 2 3 1 1 2 3 1 1
    ##  [3205] 2 1 2 0 2 2 0 6 2 3 1 4 1 3 0 3 0 3 2 2 3 1 2 1 2 0 1 2 2 3 2 3 2 2 2 1
    ##  [3241] 2 7 4 3 3 4 2 1 3 2 0 2 2 4 1 1 4 3 2 0 1 1 2 0 4 3 1 2 1 1 1 4 1 3 3 1
    ##  [3277] 1 2 2 0 1 1 2 4 2 4 0 3 1 1 1 0 0 0 1 3 0 1 4 0 1 2 2 4 2 3 2 1 2 0 3 1
    ##  [3313] 3 2 1 3 0 0 2 3 2 2 1 3 2 0 3 1 2 1 0 3 2 3 1 2 2 4 2 1 4 3 2 2 3 1 3 3
    ##  [3349] 2 4 0 3 2 1 2 1 2 4 2 2 2 3 3 1 2 4 2 1 2 3 2 1 4 0 4 0 3 2 2 3 1 0 1 0
    ##  [3385] 1 4 1 1 3 3 1 1 2 1 4 2 2 2 2 2 1 1 1 0 1 5 2 4 1 1 2 2 3 4 2 3 1 4 1 1
    ##  [3421] 2 6 2 1 1 3 2 2 3 1 1 1 2 1 1 4 5 2 0 2 3 2 2 1 0 1 1 2 1 0 1 0 1 1 1 1
    ##  [3457] 2 2 1 4 2 0 1 2 4 3 2 1 1 2 1 3 3 2 2 2 1 2 4 2 2 1 2 2 1 5 4 3 3 1 0 0
    ##  [3493] 3 1 3 4 2 2 0 1 2 3 1 2 2 1 0 2 3 4 0 2 1 2 1 1 1 3 1 1 2 0 2 2 2 2 3 5
    ##  [3529] 3 0 3 2 2 3 2 3 1 2 0 1 4 2 2 5 5 2 3 0 3 4 2 3 3 2 3 2 0 2 2 2 2 1 2 2
    ##  [3565] 5 1 5 2 3 3 2 3 6 3 3 1 0 2 1 3 2 1 3 4 1 1 3 2 0 2 2 2 1 2 4 2 4 1 2 0
    ##  [3601] 2 1 2 2 3 4 2 4 1 2 2 2 1 2 1 2 2 0 3 2 2 1 1 2 4 1 1 1 2 2 1 1 2 3 2 2
    ##  [3637] 2 2 2 0 0 4 2 2 4 1 1 2 3 3 1 0 3 4 2 2 2 3 2 2 3 1 4 0 1 1 1 2 3 3 2 0
    ##  [3673] 4 4 2 4 2 1 1 2 4 1 4 2 0 3 5 3 1 1 1 1 1 1 0 2 3 0 0 0 1 4 1 5 3 1 1 3
    ##  [3709] 2 2 1 4 2 3 1 2 4 0 2 3 1 1 4 0 2 2 1 2 1 3 2 1 0 1 4 4 1 4 0 3 0 4 1 2
    ##  [3745] 3 1 0 3 1 3 2 5 0 0 2 2 0 3 0 2 4 0 4 3 3 1 6 3 1 3 4 4 1 0 3 3 0 1 3 0
    ##  [3781] 0 4 0 3 0 2 1 3 2 1 2 2 4 2 4 1 3 3 1 4 2 1 2 2 3 4 2 1 1 0 3 3 0 1 2 1
    ##  [3817] 1 4 1 6 3 1 2 3 3 2 2 2 1 4 1 2 2 1 5 1 0 3 2 0 1 3 1 0 3 2 2 2 4 4 1 2
    ##  [3853] 0 2 0 3 0 1 3 2 2 3 3 3 2 0 3 3 4 0 3 1 1 4 1 0 3 0 3 4 1 1 3 4 1 2 1 2
    ##  [3889] 1 3 1 1 0 1 2 1 1 6 1 2 1 2 2 1 1 2 3 4 3 1 2 2 0 2 2 1 0 5 5 0 2 5 0 1
    ##  [3925] 2 4 2 0 3 3 3 3 0 0 3 1 0 2 3 3 0 1 2 2 2 4 5 1 1 3 5 2 0 1 1 1 1 3 1 2
    ##  [3961] 2 4 2 1 0 0 1 3 0 1 4 2 3 4 2 2 5 4 2 1 2 3 1 2 5 1 2 0 1 2 1 2 5 2 1 3
    ##  [3997] 2 1 2 1 3 2 3 1 1 4 0 2 5 1 5 0 1 1 4 5 3 1 2 1 3 3 2 2 3 3 2 1 2 3 2 0
    ##  [4033] 2 2 4 1 3 3 2 2 2 3 2 5 0 2 1 1 3 1 2 1 1 2 3 5 1 1 2 1 1 1 1 3 3 3 1 1
    ##  [4069] 2 2 2 2 1 4 5 3 3 1 2 2 0 3 3 0 4 1 3 3 2 1 1 2 0 2 2 1 4 1 2 2 1 1 6 3
    ##  [4105] 1 4 3 2 3 2 1 2 2 3 1 2 2 2 4 3 3 1 1 4 4 2 3 2 0 1 2 2 4 2 0 0 1 1 3 3
    ##  [4141] 2 1 1 1 3 1 1 2 1 0 0 3 3 0 2 1 2 1 4 0 0 1 1 2 1 2 2 1 5 0 3 0 1 2 3 1
    ##  [4177] 1 1 1 3 3 2 3 1 1 1 0 1 3 1 2 1 3 2 0 3 3 2 2 2 0 2 2 3 2 2 3 2 0 1 3 2
    ##  [4213] 2 1 2 2 1 2 1 2 2 1 1 1 2 2 2 4 1 1 2 1 4 2 2 1 0 2 1 2 3 1 0 3 1 3 0 3
    ##  [4249] 3 4 1 1 4 1 4 2 2 3 2 1 2 1 3 1 3 4 2 0 4 2 3 2 2 1 0 2 1 0 0 3 3 0 3 0
    ##  [4285] 2 1 2 2 2 1 1 1 1 2 0 1 2 2 2 2 8 4 4 2 3 4 1 2 0 2 3 0 1 0 3 1 2 3 1 1
    ##  [4321] 1 3 2 1 1 1 2 2 1 1 4 2 2 1 1 2 1 2 1 2 4 0 1 1 2 0 2 0 1 1 2 2 5 2 1 3
    ##  [4357] 1 2 4 3 0 1 2 1 3 3 2 2 3 0 1 1 3 3 3 1 0 2 3 2 1 1 1 1 2 2 2 2 3 4 4 2
    ##  [4393] 2 1 0 2 3 3 2 5 6 1 1 1 2 3 3 4 3 1 1 3 1 1 4 1 1 3 2 2 3 2 3 1 5 4 0 3
    ##  [4429] 1 1 0 3 1 0 3 2 0 4 3 2 1 3 3 2 1 0 1 1 2 2 1 4 3 1 3 2 2 1 0 1 2 1 0 4
    ##  [4465] 3 3 2 3 3 3 3 1 2 3 2 2 1 1 1 1 1 2 4 2 1 1 2 1 2 0 0 2 1 1 1 3 2 0 2 0
    ##  [4501] 2 2 2 0 1 0 1 3 3 3 0 1 2 1 1 1 1 2 2 2 3 1 3 1 1 1 4 2 2 3 3 2 4 1 2 0
    ##  [4537] 1 3 3 2 1 2 0 3 2 1 2 3 1 1 3 1 0 2 1 2 2 2 1 1 2 1 0 0 4 1 4 2 3 2 4 3
    ##  [4573] 1 2 4 3 2 3 0 0 1 2 4 4 2 1 2 2 3 2 0 3 3 1 1 1 0 2 2 2 4 2 1 2 0 2 3 1
    ##  [4609] 1 3 1 5 3 4 3 1 1 0 4 2 1 3 3 3 2 0 4 1 3 2 2 3 1 3 3 3 2 1 3 0 0 0 2 3
    ##  [4645] 1 3 3 2 4 1 0 4 3 1 2 4 0 3 2 4 3 2 1 2 2 1 3 1 2 2 2 2 0 2 1 2 2 2 1 1
    ##  [4681] 0 1 3 0 5 3 4 2 1 2 0 1 1 2 2 1 2 1 2 0 3 3 1 2 2 1 3 3 4 3 0 3 1 2 0 1
    ##  [4717] 1 2 3 2 1 3 1 4 3 1 1 1 4 2 4 7 2 1 2 3 5 2 3 1 2 1 3 2 2 4 0 2 3 2 3 3
    ##  [4753] 4 5 2 3 2 0 2 1 2 0 3 0 0 4 5 1 2 2 2 3 3 1 0 2 2 2 3 2 2 1 2 3 1 3 3 1
    ##  [4789] 1 0 2 3 2 4 0 4 2 3 2 1 2 1 0 1 1 1 0 3 2 2 0 0 1 0 4 2 3 2 0 0 2 1 3 2
    ##  [4825] 2 4 0 0 2 0 1 1 1 2 0 2 3 1 3 0 2 1 1 1 2 2 2 4 2 1 3 2 1 1 0 0 2 3 2 1
    ##  [4861] 3 6 0 2 3 2 1 2 1 5 3 3 0 2 2 1 1 1 2 1 2 4 2 2 1 1 0 3 4 4 1 1 1 1 2 2
    ##  [4897] 0 2 0 2 4 4 4 2 1 1 4 2 2 2 3 3 2 2 2 2 2 0 2 2 1 1 1 0 3 1 0 2 3 2 2 3
    ##  [4933] 4 3 2 3 1 1 2 0 0 1 1 3 2 2 2 0 3 2 1 2 2 1 2 2 1 4 0 4 3 1 2 2 2 0 1 1
    ##  [4969] 0 5 1 4 3 2 3 1 3 0 1 5 0 2 2 1 2 3 2 1 1 3 0 4 2 2 1 4 1 3 0 1 5 3 2 1
    ##  [5005] 0 1 2 3 1 1 4 2 5 3 5 0 1 4 2 5 5 3 1 2 1 1 3 2 3 1 2 2 1 1 3 3 0 3 4 4
    ##  [5041] 0 5 4 1 4 3 2 0 3 1 1 3 5 1 0 2 3 1 2 1 3 2 1 1 3 0 2 2 2 2 2 2 3 1 0 3
    ##  [5077] 3 3 0 0 2 3 2 0 2 1 2 1 2 1 2 4 2 3 0 2 3 0 0 2 3 3 1 3 2 1 2 3 3 0 0 4
    ##  [5113] 0 1 2 3 3 2 1 1 3 3 1 2 2 3 4 2 3 3 1 2 1 3 3 1 6 3 1 2 1 3 1 1 0 2 0 1
    ##  [5149] 1 3 0 1 2 4 1 3 1 2 2 4 1 2 2 3 0 3 2 2 2 1 1 0 1 0 2 1 1 3 1 2 3 0 1 5
    ##  [5185] 5 3 0 5 3 4 1 1 2 1 3 0 4 3 4 4 1 1 1 0 2 3 3 0 3 5 1 3 2 0 2 2 3 2 3 0
    ##  [5221] 2 2 3 1 2 1 3 2 1 3 2 4 1 0 1 3 2 1 0 3 1 2 2 4 2 0 2 2 2 2 2 3 1 2 3 3
    ##  [5257] 2 1 2 1 1 2 0 1 1 1 1 3 1 4 2 3 1 1 1 2 1 2 1 0 1 2 2 1 2 1 1 3 2 1 3 2
    ##  [5293] 1 1 5 2 2 2 3 3 3 1 1 4 1 4 1 2 2 2 2 2 1 2 1 2 2 2 1 3 3 1 2 3 2 2 2 0
    ##  [5329] 1 2 2 2 2 4 2 2 1 0 1 2 2 0 1 1 1 1 2 1 0 3 1 1 1 1 3 2 1 3 4 1 3 1 3 1
    ##  [5365] 3 0 3 3 3 0 0 1 1 0 3 0 3 2 3 4 2 4 1 1 5 1 3 1 3 3 1 0 2 2 2 1 2 1 1 0
    ##  [5401] 2 2 5 0 2 2 0 1 2 2 4 2 2 1 1 1 1 1 4 1 3 0 1 1 3 2 1 2 1 2 3 4 2 2 2 4
    ##  [5437] 5 1 1 4 1 3 3 0 3 2 0 0 2 3 1 1 4 2 2 1 1 1 2 4 2 3 0 3 1 2 0 3 4 3 0 4
    ##  [5473] 1 3 2 2 3 2 2 1 2 1 3 2 2 4 2 0 2 1 4 2 2 0 1 4 2 2 2 1 2 1 1 3 2 5 1 2
    ##  [5509] 2 2 2 1 3 4 1 3 4 0 2 1 2 2 3 1 3 1 2 1 3 2 2 2 4 3 4 2 2 4 2 1 1 0 2 2
    ##  [5545] 0 3 1 1 1 4 2 3 2 3 1 2 4 1 1 2 4 2 2 3 1 1 0 1 2 6 3 3 2 1 5 3 5 2 3 3
    ##  [5581] 2 2 3 2 1 2 2 2 4 1 1 1 2 2 1 2 1 1 1 2 1 1 0 0 2 5 3 2 2 1 2 1 1 3 3 2
    ##  [5617] 2 2 4 1 1 1 0 3 4 2 1 4 1 1 4 2 2 2 1 1 3 3 1 2 1 0 3 1 2 1 2 3 0 6 3 1
    ##  [5653] 3 2 0 2 2 6 2 0 0 1 1 0 2 2 1 1 2 1 0 5 1 2 4 5 2 3 0 0 1 1 1 2 1 1 0 2
    ##  [5689] 3 3 2 2 3 2 1 1 2 0 0 2 2 3 2 2 3 3 1 5 1 5 2 1 3 3 2 1 2 3 2 2 2 2 2 1
    ##  [5725] 2 4 0 2 3 1 2 2 3 2 5 4 2 1 1 6 2 1 1 2 0 0 1 1 0 3 3 3 0 2 3 1 2 0 2 1
    ##  [5761] 3 2 4 3 2 3 1 1 2 2 3 2 2 2 4 4 4 1 2 2 1 2 4 0 2 1 1 2 0 2 2 3 1 1 2 4
    ##  [5797] 0 1 3 0 5 0 2 3 1 2 2 2 4 1 1 4 0 4 1 1 0 0 1 3 2 3 4 1 3 2 1 0 1 1 2 2
    ##  [5833] 1 2 1 0 1 3 2 3 3 1 3 2 2 3 4 3 4 1 0 1 3 2 2 0 0 1 6 3 2 0 1 0 3 2 4 1
    ##  [5869] 2 0 2 3 3 1 2 2 3 2 2 3 1 2 1 2 4 4 2 2 4 3 4 2 1 3 2 4 4 2 2 2 3 5 2 1
    ##  [5905] 3 4 2 1 2 4 2 1 2 1 2 3 0 3 1 3 2 2 2 2 2 5 3 0 2 1 2 0 0 3 4 2 1 2 2 4
    ##  [5941] 3 1 2 5 2 0 0 4 3 2 1 1 1 2 3 4 3 4 1 0 1 7 2 6 3 1 4 2 3 1 4 2 1 3 1 2
    ##  [5977] 2 2 2 4 1 0 1 1 4 2 1 1 0 3 0 1 2 1 3 4 4 3 4 2 1 1 2 0 3 1 3 6 0 2 2 3
    ##  [6013] 3 2 4 3 1 2 4 4 1 1 1 0 4 3 2 2 1 4 1 2 2 4 2 2 1 1 4 3 3 1 0 4 2 2 1 0
    ##  [6049] 1 1 2 0 1 0 1 2 1 3 4 1 5 0 3 1 1 0 1 3 1 3 1 0 3 1 1 3 0 2 0 2 5 2 4 3
    ##  [6085] 2 4 2 3 2 0 2 1 2 2 1 2 0 0 0 1 1 3 2 2 2 0 3 2 3 2 2 3 3 5 1 0 2 1 2 2
    ##  [6121] 2 0 1 2 4 1 1 5 2 0 0 4 2 3 3 3 2 2 2 1 2 3 3 1 1 3 0 2 3 3 1 1 3 1 3 2
    ##  [6157] 1 2 0 4 4 2 4 2 3 3 1 1 1 3 3 1 3 2 2 0 1 2 4 0 2 2 2 0 3 1 2 1 4 3 2 1
    ##  [6193] 3 1 1 4 2 4 3 3 2 0 2 4 0 0 2 2 5 3 3 2 6 2 5 2 3 2 3 2 0 0 4 2 2 2 4 3
    ##  [6229] 1 1 2 1 2 0 4 1 1 2 2 1 1 3 2 2 2 0 2 2 1 3 4 1 1 2 0 1 1 1 2 1 0 2 2 0
    ##  [6265] 2 1 0 1 2 1 2 1 3 1 2 1 1 2 2 0 3 2 1 3 1 0 1 0 1 5 0 2 3 2 2 2 2 2 5 2
    ##  [6301] 2 1 1 1 1 1 2 1 2 1 0 3 2 2 2 1 2 3 0 2 3 2 3 2 2 1 3 2 0 1 3 0 2 1 2 2
    ##  [6337] 4 1 1 2 3 0 0 1 1 3 2 1 0 0 3 2 5 3 1 4 2 6 0 3 5 3 3 5 1 1 1 2 2 2 2 0
    ##  [6373] 2 0 1 4 2 1 0 1 4 1 1 2 2 3 3 2 1 2 1 3 1 2 0 1 3 2 2 1 1 2 3 1 1 4 1 0
    ##  [6409] 1 2 2 1 1 5 3 2 0 2 2 2 3 2 0 3 0 5 4 3 1 2 1 4 3 3 3 3 1 4 0 2 1 3 2 1
    ##  [6445] 3 2 3 2 2 1 1 5 6 1 2 1 5 4 2 2 1 3 4 1 3 2 2 2 0 1 0 1 3 1 2 3 3 1 3 2
    ##  [6481] 1 1 0 2 5 0 3 0 5 5 3 3 2 3 2 0 2 3 0 3 3 2 1 3 2 3 2 2 2 5 2 3 1 4 0 1
    ##  [6517] 2 2 1 1 2 2 2 2 2 2 3 1 2 2 2 5 2 1 4 1 3 0 0 0 3 1 3 1 1 1 2 1 3 2 3 3
    ##  [6553] 4 1 2 0 1 1 1 0 1 0 0 1 1 1 2 1 3 5 3 4 4 6 1 3 3 2 2 1 0 1 3 1 1 4 2 3
    ##  [6589] 2 3 2 2 2 0 2 2 1 2 1 1 3 5 2 3 4 2 4 0 2 1 1 3 2 2 1 2 4 3 6 2 3 1 0 2
    ##  [6625] 2 3 1 1 2 3 2 0 2 0 4 2 4 3 2 1 3 1 1 0 1 2 3 1 5 2 3 4 2 1 3 3 3 2 3 2
    ##  [6661] 1 1 2 2 0 4 1 4 0 0 2 3 1 1 3 3 3 2 1 2 1 3 2 3 2 2 2 2 1 2 1 2 3 2 3 3
    ##  [6697] 2 3 4 3 4 3 1 3 3 2 3 1 2 1 2 1 5 1 3 1 2 2 3 1 1 1 3 2 3 2 6 1 1 3 1 0
    ##  [6733] 0 2 3 4 2 1 1 2 1 1 2 2 1 3 1 1 0 1 1 3 2 2 1 3 1 4 0 3 0 1 1 2 2 1 2 2
    ##  [6769] 1 3 4 2 1 2 2 2 2 2 3 0 3 1 3 4 1 3 2 1 1 0 0 1 2 3 2 2 0 1 4 1 1 3 3 1
    ##  [6805] 2 2 1 2 3 2 2 1 0 1 1 1 2 2 1 4 1 2 2 3 2 1 2 2 2 2 4 0 1 2 2 2 0 3 1 4
    ##  [6841] 2 0 1 3 2 3 1 2 3 3 1 0 2 2 0 0 1 2 3 4 2 4 1 0 0 3 4 1 1 2 4 0 0 2 1 1
    ##  [6877] 3 2 1 2 2 1 2 2 2 3 4 2 2 3 1 1 1 4 5 1 2 1 1 3 2 3 2 3 0 2 0 4 2 1 2 3
    ##  [6913] 0 4 3 2 3 3 6 3 2 4 1 1 0 3 3 0 4 5 3 1 2 1 0 2 1 0 0 1 1 2 1 1 0 5 2 0
    ##  [6949] 2 2 2 3 0 1 0 2 1 5 1 0 2 1 2 0 1 3 1 1 0 2 1 1 1 2 2 3 4 2 2 0 4 1 1 3
    ##  [6985] 0 5 3 1 0 2 0 2 3 1 1 0 1 5 2 4 2 0 3 0 3 1 1 2 2 2 3 3 5 2 1 1 5 4 4 2
    ##  [7021] 3 1 0 1 4 2 2 0 2 3 2 3 2 3 3 1 3 2 3 0 1 2 0 1 2 4 2 1 3 0 2 2 1 0 0 1
    ##  [7057] 1 3 1 5 2 1 2 3 0 0 4 4 1 2 2 1 3 2 2 1 3 2 1 2 1 3 3 1 1 1 2 2 2 3 1 5
    ##  [7093] 2 4 2 3 5 0 1 3 2 3 3 1 1 1 0 0 2 2 1 2 2 0 2 0 1 2 1 1 1 1 1 3 1 1 2 2
    ##  [7129] 0 3 2 1 3 4 4 3 2 4 1 1 3 0 2 3 3 3 4 6 1 4 2 1 0 1 1 3 1 5 1 0 1 1 4 4
    ##  [7165] 1 5 1 2 0 0 2 1 2 3 3 5 0 2 1 1 1 2 3 2 1 1 1 3 4 4 2 2 3 3 2 2 4 2 2 2
    ##  [7201] 0 4 5 2 4 1 2 3 2 3 2 2 1 0 4 4 2 1 3 1 2 4 2 0 4 1 1 1 1 0 2 3 0 4 3 2
    ##  [7237] 2 1 1 1 2 1 2 1 2 2 0 3 0 1 2 2 3 0 4 2 1 1 2 3 2 4 0 0 1 3 1 1 1 2 3 3
    ##  [7273] 2 3 3 1 1 2 1 1 1 4 3 0 2 3 2 4 2 0 3 3 2 1 1 2 1 1 1 3 0 3 4 3 1 2 1 1
    ##  [7309] 1 2 3 2 2 2 2 1 3 3 1 1 2 0 1 2 1 2 2 3 1 3 4 2 5 3 5 1 3 1 1 0 3 2 3 3
    ##  [7345] 3 3 3 4 2 2 1 2 3 1 3 2 1 2 3 1 1 1 1 1 1 1 4 4 2 2 2 1 2 0 0 2 0 1 0 2
    ##  [7381] 2 2 3 3 2 2 3 2 3 5 2 3 4 2 2 1 2 3 2 1 1 2 1 3 0 1 1 1 2 2 1 2 2 2 2 2
    ##  [7417] 0 2 1 2 1 4 3 1 4 4 0 0 3 1 1 0 3 2 1 2 3 0 0 2 4 1 2 2 0 0 2 3 6 2 4 1
    ##  [7453] 4 2 1 1 3 1 5 0 2 1 1 2 3 0 3 2 1 4 3 3 1 1 4 1 2 1 2 3 2 2 0 1 3 0 4 3
    ##  [7489] 2 1 2 2 2 2 2 3 1 3 4 0 3 3 2 3 3 1 1 2 0 2 4 3 1 0 2 4 4 0 2 0 5 3 0 0
    ##  [7525] 2 2 3 0 3 0 2 1 3 3 2 2 2 1 0 2 3 2 3 2 2 1 0 2 2 1 1 2 2 1 1 1 4 3 1 1
    ##  [7561] 1 1 5 0 0 3 1 0 3 1 2 2 0 0 3 2 2 3 1 0 2 1 1 4 1 1 2 1 2 1 2 1 2 2 1 1
    ##  [7597] 4 2 2 3 0 2 2 3 2 1 2 2 2 3 1 2 1 2 1 2 3 2 2 2 4 1 2 2 2 1 3 4 4 1 2 2
    ##  [7633] 3 0 2 1 2 1 1 4 2 2 2 2 2 2 1 1 3 2 2 4 2 3 1 3 3 1 2 2 2 2 1 1 3 2 1 5
    ##  [7669] 2 1 1 3 0 1 2 2 0 1 1 3 2 0 1 2 2 4 1 2 2 2 0 4 3 5 4 6 1 1 3 1 2 3 1 2
    ##  [7705] 2 4 1 2 0 2 2 4 2 2 1 3 1 2 1 2 0 1 1 1 2 0 2 4 1 0 1 1 2 1 2 4 2 2 1 3
    ##  [7741] 1 1 2 4 1 2 0 3 4 3 1 0 2 2 3 2 2 1 1 2 4 1 1 2 0 0 3 2 3 1 3 3 1 0 1 2
    ##  [7777] 6 2 2 3 3 0 2 1 1 3 0 3 1 2 0 2 1 0 3 3 3 2 2 0 1 2 2 3 1 3 2 1 2 3 4 1
    ##  [7813] 3 2 1 3 3 2 1 1 1 3 3 4 0 1 2 2 1 3 1 1 0 0 2 1 1 3 4 0 2 2 1 3 2 2 2 3
    ##  [7849] 3 4 4 3 1 3 2 2 1 1 2 3 3 3 4 2 1 2 1 0 0 2 3 4 3 1 3 0 2 5 2 1 1 3 1 3
    ##  [7885] 1 4 5 2 2 1 1 4 2 5 1 2 4 2 3 3 0 1 1 2 0 1 2 1 2 3 3 1 1 3 0 3 1 4 0 5
    ##  [7921] 3 1 1 2 0 4 1 2 3 1 4 1 4 4 2 6 2 1 3 2 5 0 1 2 2 1 1 2 4 6 4 0 0 2 0 5
    ##  [7957] 1 1 2 1 1 2 1 4 1 1 5 1 1 2 2 0 2 0 4 3 1 2 0 0 3 3 4 0 0 1 1 2 2 0 2 4
    ##  [7993] 1 4 1 1 3 3 1 2 2 3 1 3 3 0 1 0 2 5 3 2 1 2 0 0 2 3 0 5 4 4 3 0 2 1 2 2
    ##  [8029] 3 3 4 1 2 1 3 2 2 1 4 5 5 0 1 3 0 0 0 1 2 4 1 3 3 0 0 0 2 2 2 1 0 2 0 1
    ##  [8065] 2 0 0 1 1 0 1 3 3 1 0 2 1 4 0 5 1 2 1 2 4 4 2 2 5 1 1 3 3 0 2 2 1 1 4 1
    ##  [8101] 4 0 1 1 0 4 2 1 4 1 3 1 0 1 0 2 2 3 3 4 2 0 1 2 4 2 5 2 2 1 2 2 2 1 1 3
    ##  [8137] 1 1 0 2 1 0 3 2 5 3 2 3 3 3 3 0 2 2 2 2 2 3 2 2 2 2 1 4 1 3 3 0 1 1 2 1
    ##  [8173] 4 6 3 1 1 3 4 2 2 2 2 2 3 2 2 3 2 3 2 0 1 1 1 1 3 1 3 3 2 4 3 1 0 3 0 1
    ##  [8209] 2 1 2 2 3 2 1 1 0 0 2 1 1 2 3 3 0 1 0 3 2 1 1 1 5 2 1 1 2 1 2 2 3 2 1 3
    ##  [8245] 4 2 1 0 3 1 2 1 1 1 2 2 4 6 1 3 1 2 3 3 1 4 2 1 3 3 2 3 1 2 4 2 1 3 3 2
    ##  [8281] 3 1 1 1 6 2 1 2 0 1 2 2 2 1 3 1 3 4 1 1 5 2 4 1 1 2 0 2 1 1 3 3 1 2 3 1
    ##  [8317] 1 2 0 2 3 1 2 0 1 2 2 1 1 2 0 0 0 1 3 2 3 3 6 2 0 1 1 1 2 3 3 3 1 3 2 3
    ##  [8353] 1 2 2 3 0 1 1 2 1 1 2 2 3 4 1 3 1 1 2 1 1 1 2 2 1 1 1 3 1 1 2 3 2 3 3 1
    ##  [8389] 2 4 2 2 1 1 2 2 2 1 0 3 1 1 1 4 2 0 0 1 4 1 2 1 3 4 2 2 2 2 0 3 2 2 2 3
    ##  [8425] 0 2 3 3 0 0 1 1 0 0 1 2 3 4 2 1 1 2 4 2 4 2 1 1 1 1 2 1 2 2 4 4 1 1 2 2
    ##  [8461] 2 2 1 3 0 2 0 3 2 0 2 3 1 2 2 1 0 0 3 1 2 3 1 2 1 1 3 2 1 2 5 1 3 2 1 2
    ##  [8497] 1 1 3 2 2 2 3 3 1 3 2 2 2 2 3 2 2 2 1 0 1 3 2 1 1 5 2 1 0 0 1 4 3 1 2 3
    ##  [8533] 4 2 2 3 3 1 2 0 1 3 3 1 3 0 1 3 2 2 4 5 2 2 3 1 4 1 2 4 0 0 2 3 3 3 1 1
    ##  [8569] 1 4 1 1 0 2 3 2 1 5 0 3 1 4 1 1 2 0 0 3 1 0 1 0 2 3 3 2 1 3 2 4 3 3 0 2
    ##  [8605] 2 3 3 2 2 4 3 2 2 3 3 1 2 2 1 0 2 2 1 0 4 2 4 2 2 3 1 1 1 1 1 0 1 3 2 3
    ##  [8641] 2 2 5 4 3 1 1 1 3 2 0 3 1 0 0 0 2 2 0 2 4 0 3 0 3 2 2 3 3 1 2 4 3 1 1 2
    ##  [8677] 1 4 2 2 1 2 5 1 2 2 3 1 2 0 4 1 1 2 2 2 2 3 2 2 2 2 1 3 3 5 2 2 2 3 2 2
    ##  [8713] 1 2 2 0 1 1 2 1 2 1 2 1 2 1 2 3 3 1 1 2 1 2 2 3 1 2 3 2 2 1 2 3 1 3 3 2
    ##  [8749] 2 7 2 2 1 6 1 2 1 0 4 0 2 1 5 4 2 4 3 2 1 2 3 1 1 1 3 1 0 1 3 1 2 1 0 3
    ##  [8785] 1 3 1 4 3 0 3 2 1 1 2 1 3 0 0 2 2 1 2 2 2 1 1 7 1 1 2 1 2 0 3 1 3 5 2 1
    ##  [8821] 4 3 2 1 2 4 2 2 1 1 2 2 1 1 2 3 2 1 1 2 0 4 1 4 0 2 2 2 2 2 3 3 2 2 1 3
    ##  [8857] 1 3 0 2 1 3 6 2 3 3 0 1 0 2 1 2 2 1 2 4 1 1 1 4 2 1 3 1 6 3 2 2 1 0 2 4
    ##  [8893] 1 0 3 1 0 0 2 1 0 2 3 1 2 1 4 2 1 2 0 1 4 0 2 1 0 0 3 3 1 5 2 1 3 1 2 1
    ##  [8929] 3 3 4 2 1 2 1 3 3 2 2 2 2 1 2 2 2 1 3 0 0 1 2 3 5 0 2 1 1 3 3 2 4 3 2 2
    ##  [8965] 3 2 1 0 1 1 1 0 4 1 4 5 1 2 2 1 2 3 0 2 2 2 1 2 0 2 3 2 1 3 0 2 5 1 3 3
    ##  [9001] 3 1 2 4 0 1 1 3 0 4 1 4 2 2 3 3 2 0 0 2 3 2 1 3 0 2 0 4 3 2 2 3 1 2 0 3
    ##  [9037] 3 0 3 3 2 1 1 2 0 1 1 3 1 6 1 4 1 3 3 1 1 2 2 2 2 0 2 2 1 1 3 2 2 1 4 3
    ##  [9073] 2 5 1 3 4 3 1 2 2 2 4 3 2 4 3 1 0 3 0 0 1 0 2 4 3 2 0 2 3 3 7 2 3 1 1 2
    ##  [9109] 0 0 1 1 4 2 2 0 1 2 3 1 4 3 1 1 2 1 1 2 1 2 3 2 1 2 3 1 1 1 2 3 0 4 1 0
    ##  [9145] 3 2 1 3 2 0 1 3 0 1 2 3 3 4 1 3 2 4 4 0 2 0 2 3 2 1 2 1 3 4 1 2 3 2 1 2
    ##  [9181] 2 0 3 0 1 2 2 2 2 0 2 0 1 4 1 3 4 2 0 3 3 3 2 1 0 4 1 0 1 1 2 2 1 2 4 3
    ##  [9217] 2 3 1 1 1 0 1 1 2 3 1 1 2 2 2 3 0 1 1 3 2 1 2 3 0 2 2 1 2 3 3 3 2 1 1 2
    ##  [9253] 3 6 1 1 3 2 4 4 1 1 3 5 0 1 0 1 0 2 2 3 0 7 0 2 5 3 2 0 2 2 2 0 4 1 2 4
    ##  [9289] 3 2 2 2 1 2 1 2 0 2 2 0 0 1 1 1 0 3 2 2 4 1 3 3 1 4 0 0 2 2 1 1 1 1 2 2
    ##  [9325] 2 3 5 2 2 1 3 3 1 3 2 1 3 2 3 1 0 4 1 2 2 3 2 2 1 2 0 3 4 1 3 0 3 1 2 2
    ##  [9361] 3 1 2 1 2 3 1 1 0 3 2 1 3 2 2 1 1 1 3 1 1 2 2 3 2 2 3 4 0 0 3 1 1 3 1 3
    ##  [9397] 2 2 2 4 2 4 1 0 2 1 2 0 1 1 3 2 4 2 2 1 3 2 2 1 3 2 2 2 1 1 2 1 3 1 1 2
    ##  [9433] 2 1 4 1 1 1 2 1 2 2 4 1 1 1 2 1 1 3 1 5 5 1 2 2 3 4 3 1 3 1 5 3 2 1 3 2
    ##  [9469] 4 1 4 1 6 2 2 2 2 5 2 0 3 3 3 1 2 1 2 1 3 3 0 4 4 2 1 1 2 4 4 2 2 2 1 2
    ##  [9505] 1 2 2 2 1 1 2 3 2 3 2 2 2 3 2 0 1 3 1 2 1 3 1 1 1 1 1 1 1 3 2 3 2 4 4 2
    ##  [9541] 1 1 3 4 1 0 2 2 0 3 1 1 4 0 3 2 2 4 2 1 1 1 2 1 1 4 1 1 4 1 3 1 6 1 3 1
    ##  [9577] 2 3 2 2 2 2 4 1 1 3 5 0 1 3 2 2 2 1 2 2 4 0 1 1 3 2 0 0 1 2 3 2 1 1 2 0
    ##  [9613] 1 3 1 0 1 0 2 1 2 4 1 2 0 2 1 2 0 2 1 3 2 3 1 4 0 1 3 2 0 2 1 3 2 1 0 3
    ##  [9649] 3 1 2 1 3 3 3 0 0 2 2 3 0 1 3 3 4 1 4 2 2 2 2 2 1 0 3 2 1 1 1 1 2 3 1 1
    ##  [9685] 2 2 3 2 5 2 1 3 1 2 1 2 5 1 3 1 2 1 1 2 0 3 3 1 5 0 1 1 1 1 3 3 5 2 0 4
    ##  [9721] 2 2 2 0 3 2 3 1 1 2 4 2 0 2 2 1 2 5 0 4 7 1 3 2 3 3 2 3 3 1 1 4 0 1 4 2
    ##  [9757] 3 2 2 0 2 2 1 0 3 0 2 2 1 2 3 3 1 2 2 1 3 1 2 3 0 4 2 3 2 2 1 2 2 4 3 0
    ##  [9793] 3 3 1 2 1 1 1 3 3 1 2 2 1 2 3 2 1 0 2 2 4 1 2 1 3 0 1 1 3 1 2 4 1 3 2 2
    ##  [9829] 3 4 1 4 2 3 1 0 2 4 2 2 2 2 1 2 0 2 2 1 3 3 1 1 5 1 3 2 2 1 4 2 4 2 1 1
    ##  [9865] 3 2 3 0 1 2 2 3 2 3 1 5 5 1 2 1 1 2 3 0 4 4 3 1 3 2 0 1 2 0 2 2 1 1 3 3
    ##  [9901] 1 0 1 2 2 1 2 3 4 1 1 3 4 1 2 2 1 4 3 1 2 4 3 3 4 4 3 0 3 2 1 0 0 3 2 4
    ##  [9937] 0 1 1 1 3 0 4 3 2 4 2 0 3 0 0 3 1 4 1 2 2 2 1 1 1 2 1 1 1 5 2 1 3 0 1 2
    ##  [9973] 3 0 4 3 2 0 3 1 2 2 2 2 3 0 5 0 1 1 3 4 0 2 2 4 0 2 4 1

``` r
hist(rbinom(n = 10000, size = 13, prob = 0.15))
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
quantile(rbinom(n = 10000, size = 13, prob = 0.15), c(0.05, 0.95)) #based on 10,000 re-runs of the experiment, we have 90% confidence that there will be 0 to 4 successes if a treatment with a 15% success rate is given to 13 samples.
```

    ##  5% 95% 
    ##   0   4

The example above shows how we can create data from known parameters (we
knew 15% success). But often we have data and want to predict
parameters.

First we create a prior. If we don’t want to make any assumptions of the
data we can use an uninformative prior, where all outcomes are equally
probable.

``` r
proportion_success_uninformative <- runif(n = 10000, min = 0.0, max = 1.0)
hist(proportion_success_uninformative) #this tells you that there is equal likelihood of 0-100% success
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
n_success_uninformative <- rbinom(100000, size = 13, 
                     prob = proportion_success_uninformative) #prior of an experiment with 13 samples an unknown success rate
median(n_success_uninformative) #without knowing anything we get a 50% success rate which is 13*.5 = 6.5
```

    ## [1] 6

``` r
prior_uninformative <- data.frame(proportion_success_uninformative, n_success_uninformative) #this is our prior where we don't assume anything
```

Then, we modify the prior probability distribution based on the data
from the experiment. Sticking with the example, say we run an experiment
and find that out of the 13 samples there are 2 successes

``` r
posterior_uninformative <- prior_uninformative[prior_uninformative$n_success_uninformative == 2,] #telling out model that we ran an experiment an saw 2 successes out of 13 samples
hist(posterior_uninformative$proportion_success_uninformative)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
median(posterior_uninformative$proportion_success_uninformative) #19% success
```

    ## [1] 0.1838316

Our Bayesian model predicts that the success rate is 19%, that’s close
to 15% success but not exact… which is because this is a modification of
an uninformative prior. We can get closer to the “truth” through 1) by
feeding new results into the Bayesian model or 2) using a more
informative prior

Let’s say we re-run the experiment and again find 2/13 successes again

``` r
#starting with the proportion successes from the first set of results
proportion_success_update <- posterior_uninformative$proportion_success_uninformative #stripping the proportion success from posterior_uninformative and using it for priors in for the update
hist(proportion_success_update)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
n_success_update <- rbinom(length(proportion_success_update), size = 13, 
                     prob = proportion_success_update) #simulating the results of the experiment given and binomial probability distribution based off of a probability of success defined by the first result 2/13

prior_update <- data.frame(proportion_success_update, n_success_update) #this is our prior based off of the results of the first replication

posterior_update <- prior_update[prior_update$n_success_update == 2,] #telling out model that we ran an experiment an saw 2 successes out of 13 samples AGAIN
hist(posterior_update$proportion_success_update)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
median(posterior_update$proportion_success_update) #17% success
```

    ## [1] 0.1698875

This idea of iteratively improving the model is key to Bayesian
analysis. prop_model shows how this narrows model fit.

``` r
data <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
prop_model(data)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Instead of an uninformative prior, let’s assume we *know* that the
success rate will be between 10 and 20%

``` r
proportion_success_informative <- runif(n = 10000, min = 0.10, max = 0.20)
hist(proportion_success_informative, xlim = c(0,1))
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
n_success_informative <- rbinom(100000, size = 13, 
                     prob = proportion_success_informative) #prior of an experiment with 13 samples an prior success rate between 10 and 20%
prior_informative <- data.frame(proportion_success_informative, n_success_informative) #this is our prior where we state that success should be between 0 and 20%

posterior_informative <- prior_informative[prior_informative$n_success_informative == 2,]
hist(posterior_informative$proportion_success_informative)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
median(posterior_informative$proportion_success_informative) #15% success, perfect!
```

    ## [1] 0.1510417

Note that if your prior is wrong, your Bayesian model will ALWAYS be
wrong. For example, say that you state that the success rate should be
between 30 and 40%. The model will break apart.

``` r
proportion_success_informativeWRONG <- runif(n = 10000, min = 0.30, max = 0.40)
hist(proportion_success_informativeWRONG, xlim = c(0,1))
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
n_success_informativeWRONG <- rbinom(100000, size = 13, 
                     prob = proportion_success_informativeWRONG) #prior of an experiment with 13 samples an prior success rate between 30 and 40%
prior_informativeWRONG <- data.frame(proportion_success_informativeWRONG, n_success_informativeWRONG) #this is our prior where we state that success should be between 30 and 40%

posterior_informativeWRONG <- prior_informativeWRONG[prior_informativeWRONG$n_success_informativeWRONG == 2,]
hist(posterior_informativeWRONG$proportion_success_informativeWRONG)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/Lesson2_Fundamentals_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
median(posterior_informativeWRONG$proportion_success_informativeWRONG) #33% success
```

    ## [1] 0.3372633
