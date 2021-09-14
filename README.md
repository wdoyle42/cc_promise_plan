# cc_promise_plan
Implications for states of America's Promise Act

## To run code

From the `scripts` directory

1. Run `ipeds.R`
2. Run `rmarkdown::render('acp_plan.Rmd')`

You can use RStudio or the terminal. From the terminal (assuming you
are in the top-level directory of `cc_promise_plan`:

``` shell
$ cd ./scripts
$ Rscript ipeds.R
$ Rscript -e "rmarkdown::render('acp_plan.Rmd')"
```

## Required R libraries

- tidyverse
- writexl
- here
- crosswalkr
- scales
- rmarkdown

