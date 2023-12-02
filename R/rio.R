path_ <- function(x= "CMCC2021.xlsx"){
  here::here("inst/extdata/cmcc/XLSX", x)
}

sheets_ <- function(x= path_(y), y= "CMCC2021.xlsx"){
  x |>
    readxl::excel_sheets()
}

read_sheet_ <- function(x= sheets_(y)[1], y= path_("CMCC2021.xlsx")){
  readxl::read_excel(y,sheet=x) |>
    dplyr::select(-dplyr::matches("^Origine|Column7")) |>
    dplyr::mutate(credit = as.numeric(stringr::str_replace((stringr::str_replace(as.character(credit)," ","" )),",",".")),
                  debit = as.numeric(stringr::str_replace(stringr::str_replace_all(debit," ",""),",","."))
    )
}

read_all_ <- function(x= sheets_(y), y= path_("CMCC2021.xlsx")){
  purrr::map_df(x, read_sheet_, y)
}