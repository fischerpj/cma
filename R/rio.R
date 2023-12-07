
#' fread_raw_
#' fast read of csv file
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
  x |> dplyr::mutate(group = dplyr::case_when(stringr::str_detect(libelle, "SALZEMAN|THIBAUT|HUGO|THEO|LEPRE|VIR MLE MICHELE|VIR M PIERRE FISCHER|MISSIONSWERK|PORTES OUVERTES")~"libe",
                                              stringr::str_detect(libelle, "CHQ|CHEQUE|VIR SEPA")~"chq-vir",
                                              stringr::str_detect(libelle, "LOYER|JESSICA|ROHR|MEHL|WABEAL|SEBESCAN|ERNENWEIN|PIASNY|BARTHEL")~"loyer",
                                              stringr::str_detect(libelle, "URSSAF|PREV. ARTISANALE|CPAM|CAISSE REGIONALE D ASSUR|PREVOYANCE|U R S S A F|VIR SECU INDEP")~"secu",
                                              stringr::str_detect(libelle, "ENERGIES|EAU|TIP LDEF|GARDE|FRAIS|VEOLIA|CHAUFFAGE|CIRRUS|CHEQUIER|CORAIL|AURORE 2000|PARTS SOCIALES|REMUNERATION|PSB AVENIR|PART B|ESSAI VIREMENT|ANNU RET")~"charges",
                                              stringr::str_detect(libelle, "ASSURANCE RETRA|RSI|CARSAT|AG2R|REUNICA|ASSUR RETRAITE")~"pension",
                                              stringr::str_detect(libelle, "SEPA DIRECTION GENERALE|TIP IMPOT|VIR DGFIP|DRFIP")~"taxe",
                                              stringr::str_detect(libelle, "DAB|^RET ")~"cash",
                                              stringr::str_detect(libelle, "ZANETTE|PEL|VIR 1027801896|^MLE MICHELE FISCHER$|SOUS CAP EXP|SEPA ALSACE FRANCHE COMTE|CLOT 01896 20200401")~"exception",
                                              stringr::str_detect(libelle, "Solde|^$") & month== 1 ~"solde_report",
                                              stringr::str_detect(libelle, "Solde|^$") & month== 12 ~"solde_fin",
                                              TRUE~NA))
}

#' read_all_
#' read all years of data
#' 
#' @param x range of years
read_all_ <- function(x=2012:2021){
  x |> purrr::map_dfr(read_year_) |>
    dplyr::arrange(date)
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
  here::here("inst/extdata/cmcc/XLSX", x)
}

#========================================================

#' nosolde_
#' transactions no_solde
#' 
#' @param x path
#' @export
nosolde_ <- function(x= fread_long_()){
group <- NULL  
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
#' mini_tibble long of selected columns 
#' 
#' @param x dataframe
#' @param omit regex
mini_long_ <- function(x= long_cma_(), omit= "group|^date|^tab|^code|^operation|^valeur|^bank|^comptable"){
  group <- NULL
  x |> 
    dplyr::mutate(var= paste(group,var,sep="-")) |>
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
  value <- NULL
  x |> dplyr::select(-dplyr::matches(omit)) |>
    tidyr::pivot_longer(cols= c('debit','credit'), names_to="var") |>
    dplyr::filter(value!= 0) |>
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