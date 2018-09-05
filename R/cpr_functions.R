#' This function checks if the cpr-number is a real cpr-number.
#'
#' @param cpr a character vector representating the cpr-numbers. must be 10 digits long and containg no '-'.
#' @return logical indicating whether the cpr-number is real or not
#' @author Magnus Ahlström
#' @details
#' A real cpr number contains numbers from 0-9 is 10 digits long and when the digits are multiplied by
#' a specific vector, the rowsum should be dividable by 11.
#' @seealso \code{cpr2BD} \code{cpr2Sex}

cpr_correct <- function(cpr) {
  if(!is.character(cpr)) stop("cpr must be a character string")
  if(sum(grepl("[[:digit:]]{6}-[[:digit:]]{4}", cpr), na.rm = T) > 0) stop("cpr should be without a dash ('-')")
  if(sum(nchar(cpr) != 10) > 0) stop("cpr should be 10 digit long")
  cdd <- suppressWarnings(as.numeric(substr(cpr,1,2)))
  splitted <- t(matrix(as.numeric(do.call(rbind, strsplit(cpr, ""))), ncol = 10))
  out <- colSums(splitted * c(4,3,2,7,6,5,4,3,2,1)) %% 11 == 0
  as.logical((is.na(out) * F) + (!is.na(cdd) & cdd <= 31 & cdd >= 1))
}

#' This function returns a birthday based on a real cpr-number
#'
#' @param cpr a character vector representating the cpr-numbers. must be 10 digits long and containg no '-'.
#' @return a date vector of birthdates of the individuals.
#' @author Magnus Ahlström
#' @details
#' Based on the digits 1-2 day of birth is calculated, the month is calculated based on digits 3-4 and year is
#' calculated based on digits 5-7.
#' @export
#' @seealso \code{cpr_correct} \code{cpr2Sex}

cpr2BD <- function(cpr) {
  kor <- cpr_correct(cpr)
  if (sum(!kor) > 0) warning("Some or more cprs where invalid Danish cprs")
  cdd <- suppressWarnings(as.numeric(substr(cpr, 1, 2)))
  cmm <- suppressWarnings(as.numeric(substr(cpr, 3, 4)))
  cyy <- suppressWarnings(as.numeric(substr(cpr, 5, 6)))
  c7 <- as.numeric(substr(cpr, 7, 7))
  year <-
    (c7 %in% c("0", "1", "2", "3")) * 1900  +
    (c7 %in% c("4") & cyy <= 36) * 2000 +
    (c7 %in% c("4") & cyy > 36) * 1900 +
    (c7 %in% c("5", "6", "7", "8") & cyy <= 57) * 2000 +
    (c7 %in% c("5", "6", "7", "8") & cyy > 57) * 1800 +
    (c7 %in% c("9") & cyy <= 36) * 2000 +
    (c7 %in% c("9") & cyy > 36) * 1900 +
    cyy
  ds <- paste(year, cmm, cdd, sep = "-")
  ds <- replace(ds, !kor, NA)
  date.temp <- as.Date(ds)
  date.temp <- as.numeric(date.temp) * kor + as.numeric(!kor) * c(-25567)
  #date.temp <- replace(date.temp, !kor, NA)
  as.Date(date.temp, origin = "1970-01-01")
}

#' This function returns sex based on a real cpr-number
#'
#' @param cpr a character vector representating the cpr-numbers. must be 10 digits long and containg no '-'.
#' @param output class of the output either 'numeric' 1 is male and 2 is female. Or factor with labels
#' 'M', 'F' or 'Invalid'
#' @return a date vector of birthdates of the individuals.
#' @author Magnus Ahlström
#' @details
#' Based on digit 10 sex is calculated, if the number is dividable by 2 output is female if not then
#' output is male.
#' @export
#' @seealso \code{cpr_correct} \code{cpr2BD}

cpr2Sex <- function(cpr, output = c("as.numeric", "factor")) {
  kor <- cpr_correct(cpr)
  if (sum(!kor) > 0) warning("Some or more cprs where invalid Danish cprs")
  output <- match.arg(output)
  L10 <- substr(cpr, 10, 10)
  N10 <- ((as.numeric(L10) %% 2 == 1) * 1 + (as.numeric(L10) %% 2 == 0) * 2) * kor
  out <- eval(call(output, x = N10, levels = c(0,1,2), labels = c("Invalid", "M", "F")))
  out[!kor] <- NA
  out
}
