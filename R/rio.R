
#' fread_raw_
#' fast read of csv file holding raw cma dataset
#' 
#' @param x path
#' @export
fread_raw_ <- function(x= path_(x= "cma_raw.csv")){
  data.table::fread(x)
}

#' fwrite_raw_
#' fast write of csv file
#' 
#' @param x dataframe
#' @param y filename
fwrite_raw_ <- function(x= read_all_(), y= "cma_raw.csv"){
  x |> group_cma_() |>
    data.table::fwrite(path_(y))
}

#' group_cma_
#' grouping the records of cma
#' 
#' @param x dataframe
group_cma_ <- function(x= fread_raw_()){
  group <- NULL
  nowork <- noworkid_()
  
  x |> dplyr::mutate(group = dplyr::case_when(stringr::str_detect(libelle, "ZANETTE|^SOUS CAP EXP|PSB AVENIR|^CHEQUE 0667488|^CHEQUE 0880812|^CHEQUE 0636297|^CHEQUE 0636297|^SOLDE PEL 01896|^VIR 102780189600013058360$") ~"zanette-exception",
                                              stringr::str_detect(libelle, "INTERETS PARTS SOCIALES|REMUNERATION") & rid != 2136 & rid != 2321 & rid != 100 & rid != 321    ~"zanette-exception",
                                              rid == 1852 ~"zanette-exception",
                                              rid == 1313 ~"zanette-exception",
                                              (stringr::str_detect(libelle, "CHQ|CHEQUE") & debit <= -1000 & rid %in% nowork) ~ "grochq",
                                              (stringr::str_detect(libelle, "CHQ|CHEQUE") & debit <= -1000) ~ "grotravo",
                                              stringr::str_detect(libelle, "CHQ|CHEQUE|VIR SEPA")~"chqvir",
                                              stringr::str_detect(libelle, "SALZEMAN|THIBAUT|HUGO|THEO|LEPRE|VIR MLE MICHELE|VIR M PIERRE FISCHER|MISSIONSWERK|PORTES OUVERTES")~"libe",
                                              stringr::str_detect(libelle, "PEL|^MLE MICHELE FISCHER$|CLOT 01896 20200401|^VIR 10278018960001305836065")~"exception",
                                              stringr::str_detect(libelle, "LOYER|JESSICA|ROHR|MEHL|WABEAL|SEBESCAN|ERNENWEIN|PIASNY|BARTHEL")~"loyer",
                                              stringr::str_detect(libelle, "URSSAF|PREV. ARTISANALE|CPAM|CAISSE REGIONALE D ASSUR|PREVOYANCE|U R S S A F|VIR SECU INDEP|^PRLV SEPA ALSACE")~"secu",
                                              stringr::str_detect(libelle, "ENERGIES|EAU|TIP LDEF|GARDE|FRAIS|VEOLIA|CHAUFFAGE|CIRRUS|CHEQUIER|CORAIL|AURORE 2000|PARTS SOCIALES|REMUNERATION|PART B|ESSAI VIREMENT|ANNU RET")~"charges",
                                              stringr::str_detect(libelle, "ASSURANCE RETRA|RSI|CARSAT|AG2R|REUNICA|ASSUR RETRAITE")~"pension",
                                              stringr::str_detect(libelle, "SEPA DIRECTION GENERALE|TIP IMPOT|VIR DGFIP|DRFIP")~"taxe",
                                              stringr::str_detect(libelle, "DAB|^RET ")~"cash",
                                              stringr::str_detect(libelle, "Solde|^$") & month== 1 ~"solde_report",
                                              stringr::str_detect(libelle, "Solde|^$") & month== 12 ~"solde_fin",
                                              TRUE~NA))
}

## PRLV SEPA DIRECTION GENERALE 

#' read_all_
#' read all years of cma datasets
#' 
#' @param x range of years
read_all_ <- function(x=2012:2021){
  x |> purrr::map_dfr(read_year_) |>
    dplyr::arrange(date)  |>
    tibble::rowid_to_column( "rid")
}

#' read_year_
#' read one year of data
#' 
#' @param year integer
#' @param x vector of sheet names
#' @param y file path
read_year_ <- function(year=2021, x= sheets_(y), y= path_(paste0("CMCC",year,".xlsx"))){
  purrr::map_dfr(x, read_sheet_, y, .id="tab") |>
    data.table::data.table()
}

#' read_sheet_
#' read one sheet of data
#' 
#' @param x sheet name
#' @param y file path
read_sheet_ <- function(x= sheets_(y)[1], y= path_("CMCC2019.xlsx")){
  debit <- credit <- comptable <- NULL 
  result <- readxl::read_excel(y,sheet=x) |>
    dplyr::select(-dplyr::matches("^Origine|^Column")) |>
    dplyr::mutate(credit = as.numeric(stringr::str_replace((stringr::str_replace(as.character(credit)," ","" )),",",".")),
                  debit = as.numeric(stringr::str_replace(stringr::str_replace_all(debit," ",""),",","."))
    ) |>
    tidyr::replace_na(list(debit = 0, credit = 0))
  
  # Convert column names tolower and ASCII
  names(result) <- tolower(names(result)) |> 
    stringi::stri_trans_general('Latin-ASCII')
  
  result <- result |>
    dplyr::mutate(comptable = dplyr::case_when(is.na(comptable)~libelle,
                                               TRUE~comptable),
                  libelle =  dplyr::case_when(is.na(libelle)~comptable,
                                              TRUE~libelle),
                  date = lubridate::dmy(stringr::str_extract(comptable,"\\d{2}/\\d{2}/\\d{4}")),
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  period = lubridate::floor_date(
                    date,
                    unit = "month")
    )
  return(result)
}

#' sheets_
#' retrieve sheet names in excel file
#' 
#' @param x file path
#' @param y last filename
sheets_ <- function(x= path_(y), y= "CMCC2021.xlsx"){
  x |>
    readxl::excel_sheets()
}

#' path_
#' make absolute path
#' 
#' @param x last filename
path_ <- function(x= "CMCC2021.xlsx"){
##  here::here("inst/extdata/cmcc/XLSX", x)
  paste("/home/rstudio/projects/lotz_track/inst/extdata/cmcc/XLSX", x, sep="/")
}

#========================================================

#' noworkid_
#' cheque NOT WORK
#' 
#' @param x transaction
#' @export
noworkid_ <- function(x = grochq_()){
  nowork <- c(2082,1851,1659,106,2097,673)
  return(nowork)
}


#' grochq_
#' gros cheques
#' 
#' @param x transaction
#' @export
grochq_ <- function(x = nosolde_()){
  var <- NULL
  x |> 
    dplyr::filter(stringr::str_detect(var,"^gro"))
}

#' exception
#' pinpoint the exceptional transactions in mainly Zanette and a few others 
#' 
#' @param x path
#' @export
exception_ <- function(x= nosolde_()){
  year <- month <- var <- group <- NULL  
  x |> 
    dplyr::filter(stringr::str_detect(var,"exception")) |>
    dplyr::group_by(var) |>
    dplyr::arrange(year,month) |>
    data.table::data.table()
}

#' nozanette_
#' transactions hors Zanette
#' 
#' @param x path
#' @export
nozanette_ <- function(x= nosolde_()){
  var <- group <- NULL  
  x |> dplyr::filter(!stringr::str_detect(var,"^zanette")) |>
    data.table::data.table()
}

#' nosolde_
#' transactions no_solde
#' 
#' @param x path
#' @export
nosolde_ <- function(x= mini_long_()){
var <- group <- NULL  
  x |> dplyr::filter(!stringr::str_detect(var,"^solde")) |>
    data.table::data.table()
}

#' fread_long_
#' fast read of cube file
#' 
#' @param x path
#' @export
fread_long_ <- function(x= path_(x= "cma_long.csv")){
  data.table::fread(x)
}

#' fwrite_long_
#' fast write of cube file
#' 
#' @param x dataframe
#' @param y filename
fwrite_long_ <- function(x= mini_long_(), 
                         y= "cma_long.csv"){
  data.table::fwrite(x, path_(y))
}

#' mini_long_
#' mini_tibble long for extensive 2454 rows ( soldes and Zanette rows inlcuded) of selected columns 
#' 
#' @param x dataframe
#' @param omit regex
#' @export
mini_long_ <- function(x= long_cma_(), omit= "^Xdc$|period|group|^date|^tab|^code|^operation|^valeur|^bank|^comptable"){
  dc <- var <- group <- NULL
  x |> 
    dplyr::mutate(var= dplyr::case_when(stringr::str_detect(group, "grochq|grotravo")~"gros-debit",
                                        TRUE~ paste(group,dc,sep="-"))) |>
    dplyr::select(-dplyr::matches(omit)) |>
    dplyr::group_by(var) |>
    data.table::data.table()
}

#' long_cma_
#' data_cube of cma
#' 
#' @param x dataframe
#' @param omit regex
long_cma_ <- function(x= fread_raw_(), omit="Xlibelle"){
  group <- value <- NULL
  x |> dplyr::select(-dplyr::matches(omit)) |>
    tidyr::pivot_longer(cols= c('debit','credit'), names_to="dc") |>
    dplyr::filter(value!= 0) |>
    dplyr::mutate(year= as.character(year)) |>
    ##
    dplyr::mutate(isZanette = stringr::str_detect(group,"^zanette"),
                  dc = dplyr::case_when(group == "grochq"~"gros_cheques",
                                        group == "grotravo"~"gros_travaux",
                                        TRUE~dc),
                  ts = lubridate::dmy(paste("1",month,year,sep="/"))) |>
    data.table::data.table()
}


#' gt_cma_
#' gt rendering of cma
#' 
#' @param x dataframe
gt_cma_ <- function(x= group_cma_()){
  x |> gt::gt()
}

# cube_cma_()[,.(gt=sum(value),.N),.(group,var)][order(-gt)]