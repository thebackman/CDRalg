
#' Assignment of CDR rating
#'
#' code_CDR adds a variable called "CDR" to a data set according to the scoring
#' algorithm by Morris (1993).
#'
#' @param df  a data frame
#' @param id_name the name of the primary ID variable in the data set
#' @param deb debug yes or no (will slow function down)
#'
#' @return a data frame
#' @export
#' @examples
#' data("CDRexamples")
#' code_CDR(df = CDRexamples, id_name = "id", deb = FALSE)
#' code_CDR(df = CDRexamples, id_name = "id", deb = TRUE)
code_CDR <- function(df, id_name = "lopnr", deb = FALSE) {

  # -- Intro steps

  # check df
  if (is.data.frame(df) == F) {
    stop("input must be a data frame", call. = F)
  }

  # conversion from tibble for easier use of conventional subsetting
  df <- as.data.frame(df)

  # some name stuff
  orig_names <- names(df) #  store orig names before lower case conversion
  id_name <- tolower(id_name)
  names(df) <- tolower(names(df))
  glos <- c("glo5", "glo6", "glo7", "glo8", "glo9", "glo10")
  varnames <- c(id_name, glos)

  # check if varnames exists in file
  if (all(varnames %in% names(df)) == F) {
    stop("variables not in file, check spelling / variable names")
  }

  # are there any missings in the id-variable
  if (any(is.na(df[,id_name, drop = T])) == T) {
    stop("missing obsservations in the id-variable, check")
  }

  # are all glos numeric
  numers <- purrr::map_lgl(df[, glos], function(x) is.numeric(x))
  if (all(numers) == F) {
    stop("all glo-variables must be numeric, check input data frame")
  }

  # -- Subset and split

  # subset working df
  df_limited <- df[, varnames]

  # split limited in two parts, missing obs, no missin obs
  comp_cases <- stats::complete.cases(df_limited)
  df_limited_comp <- df_limited[comp_cases, , drop = F]
  df_limited_na <- df_limited[!comp_cases, , drop = F]

  # do any variables have values that are not allowed
  ok_vals <- c(0, 0.5, 1, 2, 3)
  vals <- purrr::map_lgl(df_limited_comp[, glos], function(x) all(x %in% ok_vals))
  if (all(vals) == F) {
    stop("some observations in glo-variables contain non allowed values")
  }

  # -- Run workhorse function on OK data frame

  df_limited_comp <- cdr_coder(df_limited_comp, deb = deb)

  # -- Wrap it up and return it

  # assign NA to missing obs in case there are any
  if (nrow(df_limited_na) == 0) {  # no missing obs, early return
    finished <- dplyr::left_join(df, df_limited_comp[,c(id_name, "CDRGLOBAL")], by = id_name)
    names(finished) <- c(orig_names, "CDRGLOBAL")
    return(finished)
  }

  df_limited_na$CDRGLOBAL <- NA_real_

  # stack CDR rating and merge with original data
  df_id_CDR <- rbind(df_limited_comp[,c(id_name, "CDRGLOBAL")], df_limited_na[,c(id_name, "CDRGLOBAL")])
  finished <- dplyr::left_join(df, df_id_CDR, by = id_name)
  names(finished) <- c(orig_names, "CDRGLOBAL")
  return(finished)
}


