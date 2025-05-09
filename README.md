# cma Credit Mutuel Analysis alias cma.git

aka package 'lotz_track', which is now superseded, relayed by lotz_scribe as convenient quarto_book 

## run

- quarto render (in terminal )

## data

-   cma_raw.csv with \*group **enrichment results from read_sheet\_ read_year\_ read_all\_ group_cma\_** fwrite_cma\_ **OR** fread_cma\_\*\*
-   main dataset to start any analysis from is **mini_long\_**

## logs

1.  create **AS repo bare** with \[README,LICENSE\] : this https://github.com/fischerpj/cma.git
2.  in Docker/xv2 clone **AS R-project-package** "cma" with \[DESCRIPTION, NAMESPACE, R/, man/\]
3.  import data to inst/extdata

-   where docker ps \[CONTAINER ID= **f3de914afe04**\]
-   C:\_myHUL_MERTZ_legacy\CreditMutuel\_DATA\CM\_CC\XLSX is the source of external xlsx files
-   docker cp C:\_myHUL_MERTZ_legacy\CreditMutuel\_DATA\CM\_CC\XLSX f3de914afe04:/home/rstudio/projects/cma/inst/extdata/cmcc then \$ sudo chown -R rstudio data/cm_cc \$ sudo chgrp -R rstudio data/cm_cc

4.  gh auth login seems already done and not requiring password

## issues

-   fix LICENSE in DESCRIPTION and LICENSE
