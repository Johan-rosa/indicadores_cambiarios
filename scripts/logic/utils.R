#' Change month encoding
#'
#' Take month from text to number or form number to text. This work with any
#' month name (Spanish or English) to create a number. From number to text only
#' creates Spanish months
#'
#' @param mes a number or character with the month
#' @param type a character indicating the type of conversion,
#' can be any of these:
#' \code{c("text_to_number", "number_to_text", "number_to_shorttext")}
#'
#' @export
#'
#' @examples
#' crear_mes("Enero", "text_to_number")
crear_mes <- function(mes, type = "text_to_number") {
  # Input validation
  checkmate::assertChoice(
    type, c("text_to_number", "number_to_text", "number_to_shorttext"))

  if (is.character(mes)) {
    checkmate::assert_choice(type, c("text_to_number"))
  } else if (is.numeric(mes)) {
    checkmate::assert(
      checkmate::check_choice(type, c("number_to_text", "number_to_shorttext")),
      all(mes %in% 1:12),
      combine = "and"
    )
  }

  if (type == "number_to_text") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Enero",
      `2` = "Febrero",
      `3` = "Marzo",
      `4` = "Abril",
      `5` = "Mayo",
      `6` = "Junio",
      `7` = "Julio",
      `8` = "Agosto",
      `9` = "Septiembre",
      `10` = "Octubre",
      `11` = "Noviembre",
      `12` = "Diciembre")
  }

  if (type == "number_to_shorttext") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Ene",
      `2` = "Feb",
      `3` = "Mar",
      `4` = "Abr",
      `5` = "May",
      `6` = "Jun",
      `7` = "Jul",
      `8` = "Ago",
      `9` = "Sep",
      `10` = "Oct",
      `11` = "Nov",
      `12` = "Dic")
  }

  if (type == "text_to_number") {
    mes  <-  stringr::str_to_title(mes)
    new_mes <- dplyr::recode(
      mes,
      "Jan" = 01,
      "Ene" = 01,
      "Feb" = 02,
      "Mar" = 03,
      "Abr" = 04,
      "Apr" = 04,
      "May" = 05,
      "Jun" = 06,
      "Jul" = 07,
      "Aug" = 08,
      "Ago" = 08,
      "Sep" = 09,
      "Sept" = 09,
      "Oct" = 10,
      "Nov" = 11,
      "Dec" = 12,
      "Dic" = 12,

      "Enero" = 01,
      "Febrero" = 02,
      "Marzo" = 03,
      "Abril" = 04,
      "Mayo" = 05,
      "Junio" = 06,
      "Julio" = 07,
      "Agosto" = 08,
      "Septiembre" = 09,
      "Octubre" = 10,
      "Noviembre" = 11,
      "Diciembre" = 12,

      "January" = 01,
      "February" = 02,
      "March" = 03,
      "April" = 04,
      "May" = 05,
      "June" = 06,
      "July" = 07,
      "August" = 08,
      "September" = 09,
      "October" = 10,
      "November" = 11,
      "December" = 12)
  }

  return(new_mes)
}

#' @export
weekday_en_to_es <- function(x) {
  dplyr::recode(
    tolower(x),
    monday    = "lunes",
    tuesday   = "martes",
    wednesday = "miércoles",
    thursday  = "jueves",
    friday    = "viernes",
    saturday  = "sábado",
    sunday    = "domingo",
    .default  = x
  )
}