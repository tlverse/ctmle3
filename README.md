
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`ctmle3`: modern Super Learning with pipelines

[![Travis-CI Build
Status](https://travis-ci.org/tlverse/ctmle3.svg?branch=master)](https://travis-ci.org/tlverse/ctmle3)
[![Build
status](https://ci.appveyor.com/api/projects/status/lfv64jnygnmx6txi?svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/ctmle3)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tlverse/ctmle3/master.svg)](https://codecov.io/github/tlverse/ctmle3?branch=master)
<!-- [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) -->
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1342294.svg)](https://doi.org/10.5281/zenodo.1342294) -->
<!-- [![Join the chat at https://gitter.im/ctmle3-Rpkg/Lobby](https://badges.gitter.im/ctmle3-Rpkg/Lobby.svg)](https://gitter.im/ctmle3-Rpkg/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) -->

> A modern implementation of the Collaborative Targeted Maximum
> Likelihood Estimator (C-TMLE) algorithm for xxxxx

**Authors:** [Weixin Cai](https://github.com/wilsoncai1992), [Jeremy
Coyle](https://github.com/jeremyrcoyle)

-----

## What’s `ctmle3`?

<!-- `ctmle3` is a modern implementation of the Super Learner algorithm of
@vdl2007super. The Super Learner algorithm performs ensemble learning in one of
two fashions:

1. The _discrete_ Super Learner can be used to select the best prediction
   algorithm from among a supplied library of machine learning algorithms
   ("learners" in the `ctmle3` nomenclature) -- that is, the discrete Super Learner
   is the single learning algorithm that minimizes the cross-validated risk with
   respect to an appropriate loss function.
2. The _ensemble_ Super Learner can be used to assign weights to a set of
   specified learning algorithms (from a user-supplied library of such
   algorithms) so as to create a combination of these learners that minimizes
   the cross-validated risk with respect to an appropriate loss function. This
   notion of weighted combinations has also been referred to as _stacked
   regression_ [@breiman1996stacked] and _stacked generalization_
   [@wolpert1992stacked].
 -->

-----

## Installation

<!--
For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via


```r
install.packages("ctmle3")
```
-->

Install the most recent *stable release* from GitHub via
[`devtools`](https://www.rstudio.com/products/rpackages/devtools/):

``` r
devtools::install_github("tlverse/ctmle3")
```

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an
issue](https://github.com/tlverse/ctmle3/issues).

-----

## Examples

<!-- `ctmle3` makes the process of applying screening algorithms, learning algorithms,
combining both types of algorithms into a stacked regression model, and
cross-validating this whole process essentially trivial. The best way to
understand this is to see the `ctmle3` package in action:
 -->

-----

## Contributions

It is our hope that `ctmle3` will grow to be widely used for creating
stacked regression models and the cross-validation of pipelines that
make up such models, as well as the variety of other applications in
which the Super Learner algorithm plays a role. To that end,
contributions are very welcome, though we ask that interested
contributors consult our [contribution
guidelines](https://github.com/tlverse/ctmle3/blob/master/CONTRIBUTING.md)
prior to submitting a pull request.

-----

After using the `ctmle3` R package, please cite the following:

``` 
    @misc{coyle2018ctmle3,
      author = {Cai, Weixin and Coyle, Jeremy R},
      title = {{ctmle3}: Modern Pipelines for Machine Learning and {Super
        Learning}},
      year  = {2018},
      howpublished = {\url{https://github.com/tlverse/ctmle3}},
      note = {{R} package version 0.1.0},
    }
```

-----

## License

© 2017-2018 [Weixin Cai](https://github.com/wilsoncai1992), [Jeremy R.
Coyle](https://github.com/jeremyrcoyle)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

-----

## References
