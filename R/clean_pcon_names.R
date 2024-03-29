#' Clean British Parliamentary Constituency Names
#'
#' British parliamentary constituencies are not often coded in a consistent manner. Fixing this can also be very time-consuming. This function uses the constituencies dataset in the package to standardise a vector of constituencies names. Note that where strings are uncertain this function can take a while to run.
#'
#' @param x A vector of constituency names.
#' @param year A specific year to consider names for. Defaults to NULL.
#' @return A vector of cleaned constituency names.
#' @examples
#' clean_pcon_names(c("Ynys Môn", "Derby North", "North, Derby"), year = 2019)
#' @export

clean_pcon_names <- function(x, year = NULL){

  # Rename original data

  orig <- x


  # Convert names vector to lower case and remove punctuation, spaces, and diacritics

  x <-
    x %>%
    tolower() %>%
    stringr::str_replace("&", "and") %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_remove_all("\\bthe\\b") %>%
    stringr::str_replace("\\bsiar\\b", "an iar") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_remove("kingstonupon") %>%
    stringr::str_remove("upontyne") %>%
    stringr::str_split("") %>%
    lapply(sort) %>%
    lapply(paste0, collapse = "") %>%
    unlist()



  # Simplify reference names and remove remove punctuation, etc.

  if(is.null(year) == T){

    pcon <- britpol::constituency_results$constituency

  } else {

    pcon <- britpol::constituency_results$constituency[as.numeric(format(britpol::constituency_results$date, "%Y")) == year]

  }

  if(length(pcon) == 0){
    stop("The year you provide must be one in which an election occurred.")
  }

  ref <-
    pcon %>%
    tolower() %>%
    stringr::str_replace("&", "and") %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_remove_all("\\bthe\\b") %>%
    stringr::str_replace("\\bsiar\\b", "an iar") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_remove("kingstonupon") %>%
    stringr::str_remove("upontyne") %>%
    stringr::str_split("") %>%
    lapply(sort) %>%
    lapply(paste0, collapse = "") %>%
    unlist()


  # Convert to names in constituencies dataset
  # (let me know if you know how to speed this up)

  for(i in 1:length(ref)){
    x[x == ref[i]] <- pcon[i]
  }


  # Count non-perfect matches

  non_perf <- length(x[x == tolower(x)])
  non_perf_names <- unique(orig[x == tolower(x)])


  # Make best guess of missing strings based on length of data

  if(length(x[x == tolower(x)]) > 0){
    for(i in which(x == tolower(x))){
      x[i] <- pcon[which.max(comparator::JaroWinkler(max_prefix = 5)(ref, x[i]))]
    }
  }


  # Print number of non-perfect matches

  if(non_perf > 0){
    warning(
      paste0(
        "There ",
        ifelse(non_perf == 1, "was ", "were "),
        non_perf, " non-perfect ",
        ifelse(non_perf == 1, "match: ", "matches: "),
        paste0(non_perf_names, collapse = ", ")
      )
    )
  }


  # Return the simplified data to the user

  return(x)


}
