#' path_
#' make absolute path
#' 
#' @param x last filename
path_ <- function(x= "CMCC2021.xlsx"){
  here::here("inst/extdata/cmcc/XLSX", x)
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
                  date = lubridate::dmy(stringr::str_extract(comptable,"\\d{2}/\\d{2}/\\d{4}")),
                  year = lubridate::year(date)
                  )
  return(result)
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

#' read_all_
#' read all years of data
#' 
#' @param x range of years
read_all_ <- function(x=2012:2021){
  x |> purrr::map_dfr(read_year_) |>
    dplyr::arrange(date)
}

#' fwrite_cma_
#' fast write of csv file
#' 
#' @param x path
fwrite_cma_ <- function(x= read_all_()){
  data.table::fwrite(x, path_("cma.csv"))
}

#' fread_cma_
#' fast read of csv file
#' 
#' @param x path
#' @export
fread_cma_ <- function(x= path_("cma.csv")){
  data.table::fread(x)
}

#' group_cma_
#' grouping the records of cma
#' 
#' @param x dataframe
group_cma_ <- function(x= fread_cma_()){
  x |> dplyr::mutate(group = dplyr::case_when(stringr::str_detect(libelle, "CHQ|CHEQUE|VIR SEPA")~"chq-vir",
                                              stringr::str_detect(libelle, "LOYER|JESSICA|ROHR|MEHL|WABEAL|SEBESCAN|ERNENWEIN|PIASNY|BARTHEL")~"loyer",
                                              stringr::str_detect(libelle, "URSSAF|PREV. ARTISANALE|CPAM|CAISSE REGIONALE D ASSUR|PREVOYANCE|U R S S A F|VIR SECU INDEP")~"secu",
                                              stringr::str_detect(libelle, "SALZEMAN|THIBAUT|HUGO|THEO|LEPRE|VIR MLE MICHELE|VIR M PIERRE FISCHER|MISSIONSWERK|PORTES OUVERTES")~"libe",
                                              stringr::str_detect(libelle, "ENERGIES|EAU|TIP LDEF|GARDE|FRAIS|VEOLIA|CHAUFFAGE|CIRRUS|CHEQUIER|CORAIL|AURORE 2000|PARTS SOCIALES|REMUNERATION|PSB AVENIR|PART B|ESSAI VIREMENT|ANNU RET")~"charges",
                                              stringr::str_detect(libelle, "ASSURANCE RETRA|RSI|CARSAT|AG2R|REUNICA|ASSUR RETRAITE")~"pension",
                                              stringr::str_detect(libelle, "SEPA DIRECTION GENERALE|TIP IMPOT|VIR DGFIP|DRFIP")~"taxe",
                                              stringr::str_detect(libelle, "DAB|^RET ")~"cash",
                                              stringr::str_detect(libelle, "ZANETTE|PEL|VIR 1027801896|^MLE MICHELE FISCHER$|SOUS CAP EXP|SEPA ALSACE FRANCHE COMTE|CLOT 01896 20200401")~"exception",
                                              stringr::str_detect(libelle, "Solde|^$")~"solde",
                                              TRUE~NA))
}

#' mini_cma_
#' mini_tibble of selected columns 
#' 
#' @param x dataframe
#' @param omit regex
mini_cma_ <- function(x= group_cma_(), omit= "^date|^tab|^code|^operation|^valeur|^bank|^comptable"){
  group <- NULL
  x |> dplyr::select(-dplyr::matches(omit)) |>
    dplyr::group_by(group)
}

#' cube_cma_
#' data_cube of cma
#' 
#' @param x dataframe
#' @param omit regex
cube_cma_ <- function(x= mini_cma_(), omit="libelle"){
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