
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varameta

The goal of `varameta` is to provide a solution to meta-analysing
medians. Existing solutions either convert medians to means, or omit the
studies that report medians. `varameta` provides all existing methods,
including a new esitmator for the variance of the sample median,
enabling meta-analysis of medians.

This package has some [secondary objectives](#secondary-objectives),
however, for the creator. As my first foray into reproducible packaged
analyses, version control with git, and package development, much that
is pedestrian in `varameta` was dazzlingly fascinating in its novelty.

## installation

You can install varameta from github with:

``` r
# install.packages("devtools")
devtools::install_github("softloud/varameta")
```

## talk

| where                                    | when           | title                                                                                       |
| ---------------------------------------- | -------------- | ------------------------------------------------------------------------------------------- |
| Fenner School of Environment and Society | September 2018 | [`varameta::` meta-analysis of medians](http://cantabile.rbind.io/talks/fenner-2018/slides) |

## example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

## secondary objectives

### packaged analyses

This was the first time I really (figuratively) cracked open the
canonical [*R packages*](http://r-pkgs.had.co.nz/).

As someone who has built [towering
pillars](https://ropensci.org/blog/2018/03/13/ode-to-testing/) of script
files, it was quite the revelation to switch to storing my functions
reproducibly *with* documentation.

Unit [testing as a
debugging](http://cantabile.rbind.io/2018/07/01/testing-as-debugging/)
tool, was also a highlight discovery.

> “If you’re using automated testing, this is also a good time to create
> an automated test case. If your existing test coverage is low, take
> the opportunity to add some nearby tests to ensure that existing good
> behaviour is preserved. This reduces the chances of creating a new
> bug.” ([*Advanced
> R*](http://adv-r.had.co.nz/Exceptions-Debugging.html))

### git

A doctorate is a grand time of firsts. First package, first paper, first
conference talk, and first collaboration. Suddenly sharing my code,
collaborating on it, and tracking large projects are pressing concerns.

Learning git via the entertaining [*Happy Git and GitHub for the
useR*](http://happygitwithr.com/) was the solution I went with. A
[frustrating learning
curve](https://stackoverflow.blog/2017/05/23/stack-overflow-helping-one-million-developers-exit-vim/)
at times; e.g.,

``` bash
git config --global core.editor "nano"
```

is a command I wish I learnt a lot earlier. I’m convinced that vim was
designed by [GLaDOS](https://en.wikipedia.org/wiki/GLaDOS).

![](https://upload.wikimedia.org/wikipedia/en/b/bf/Glados.png)
