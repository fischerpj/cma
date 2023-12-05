path_ <- function(x= "CMCC2021.xlsx"){
  here::here("inst/extdata/cmcc/XLSX", x)
}

sheets_ <- function(x= path_(y), y= "CMCC2021.xlsx"){
  x |>
    readxl::excel_sheets()
}

read_sheet_ <- function(x= sheets_(y)[1], y= path_("CMCC2019.xlsx")){
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

read_year_ <- function(year=2021, x= sheets_(y), y= path_(paste0("CMCC",year,".xlsx"))){
  purrr::map_dfr(x, read_sheet_, y, .id="tab") |>
    data.table::data.table()
}

read_all_ <- function(x=2012:2021){
  x |> purrr::map_dfr(read_year_)
}