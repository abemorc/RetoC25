
# helper para convertir a hora
as_sexagesimal <- function(variable) {
  hms::round_hms(hms::as_hms(variable*60*60*24), digits = 0)
}
# ##