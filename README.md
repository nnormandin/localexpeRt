[![Github Issues](http://githubbadges.herokuapp.com/nnormandin/localexpeRt/issues.svg)](https://github.com/nnormandin/localexpeRt/issues)
[![GitHub version](https://badge.fury.io/gh/nnormandin%2FlocalexpeRt.svg)](http://badge.fury.io/gh/nnormandin%2FlocalexpeRt)

*This project was partially funded via the Omar N. Bradley Officer Research Fellowship in Mathematics.*


# localexpeRt
This R package is for Binary Local Expert Regression, which is a proprietary ensemble method for regression tasks. Briefly, it involves decomposing the task of predicting a continuous target variable into many simpler probabilistic class estimation tasks. These 'local expert' models are reconstructed to form a unique distributional understanding of each separate instance, and a separate regression model is trained on top of this model output (see David Wolpert's work on [stacked generalization](http://www.cs.utsa.edu/~bylander/cs6243/wolpert92stacked.pdf)).

# Read the [vignette](https://htmlpreview.github.io/?https://raw.githubusercontent.com/nnormandin/localexpeRt/master/vignettes/Understanding_localexpeRt.html) for this project to see the method in action.
This vignette walks through an application of Binary Local Expert Regression to chemical research data from Max Kuhn's Applied Predictive Modeling package, and serves as an introduction to the methodology.


# Install the development version from github
If you have the `devtools` package installed in `R` you can download the (unstable) development version like this:

```r
devtools::install_github('nnormandin/localexpeRt')
```
