Bayesian data analysis in R: Introduction
================

Bayesian inference is a method for figuring out unknown quantities given
some known facts. There are other inference methods that do this, but
what makes Bayesian inference special is that it uses probability to
describe the uncertainty. It’s really just about using probability,
where probability is a number between 0 and 1. Fundamentally, what
Bayesian analysis is doing is *updating* the probability distribution to
reflect data.

The function prop_model implements a Bayesian model that assumes that: -
The data is a vector of successes and failures represented by 1s and
0s. - There is an unknown underlying proportion of success. - Prior to
being updated with data, any underlying proportion of success is equally
likely.

Let’s start by seeing what happens when we run the model with no data.

``` r
data <- c()
prop_model(data)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/figures/L1.1-1.png)<!-- -->

``` r
#The function prop_model implements a Bayesian model that assumes that: 1) The data is a vector of successes and failures represented by 1s and 0s; 2) There is an unknown underlying proportion of success; 3) Prior to being updated with data, any underlying proportion of success is equally likely.
```

We get a big blue square representing a uniform probability
distribution, indicating that any proportion of successes has equal
probability. It’s labeled “Prior” because one assumption of the model
was that **prior** to seeing any data, any underlying proportion of
success is equally likely. Now, let’s add a data point: say this is from
an experiment on whether a drug cured patients. The first run of the
experiment showed that the patient was not cured.

``` r
data <- c(0)
prop_model(data)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/figures/L1.2-1.png)<!-- -->

If we re-run the experiment and the second patient was cured, the
probability of having a success changes…

``` r
data <- c(0,1)
prop_model(data)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/figures/L1.3-1.png)<!-- -->

Remember, the fundamental idea of Bayesian analysis is that you are
updating a prior probability with data to look at increasingly powerful
**posterior** probability distributions. So, say we have six data points
from the experiment. Now we know that, given an uninformative prior with
equal likelihood of success/failure, the probability of being cured from
the drug is approximately 40%, with substantial uncertainty, as the
drug’s efficacy could range from approximately 10% to 70%.

``` r
data <- c(0,1,0,0,0,1)
prop_model(data)
```

![](/Users/urielmenalled/Desktop/workshop-BayesianR/KnittedFiles/figures/L1.4-1.png)<!-- -->

**CONGRATULATIONS, you’ve just used a Bayesian model!**
