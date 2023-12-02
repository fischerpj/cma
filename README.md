# cma
Credit Mutuel Analysis
## logs
1. create **AS repo bare** with [README,LICENSE] : this https://github.com/fischerpj/cma.git
2. in Docker/xv2 clone **AS R-project-package** "cma"  with  [DESCRIPTION, NAMESPACE, R/, man/]
3. import data to inst/extdata
- where docker ps  [CONTAINER ID= **f3de914afe04**]
- C:\_myHUL\_MERTZ_legacy\CreditMutuel_DATA\CM_CC\XLSX is the source of external xlsx files
- docker cp C:\_myHUL\_MERTZ_legacy\CreditMutuel_DATA\CM_CC\XLSX f3de914afe04:/home/rstudio/projects/cma/inst/extdata/cmcc
then
$ sudo chown -R rstudio data/cm_cc
$ sudo chgrp -R rstudio data/cm_cc

4. gh auth login seems already done and not requiring password
## issues
- fix LICENSE in DESCRIPTION and LICENSE