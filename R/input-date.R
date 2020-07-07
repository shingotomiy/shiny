#' Create date input
#'
#' Creates a text input which, when clicked on, brings up a calendar that
#' the user can click on to select dates.
#'
#' The date `format` string specifies how the date will be displayed in
#' the browser. It allows the following values:
#'
#' \itemize{
#'   \item `yy` Year without century (12)
#'   \item `yyyy` Year with century (2012)
#'   \item `mm` Month number, with leading zero (01-12)
#'   \item `m` Month number, without leading zero (1-12)
#'   \item `M` Abbreviated month name
#'   \item `MM` Full month name
#'   \item `dd` Day of month with leading zero
#'   \item `d` Day of month without leading zero
#'   \item `D` Abbreviated weekday name
#'   \item `DD` Full weekday name
#' }
#'
#' @inheritParams textInput
#' @param value The starting date. Either a Date object, or a string in
#'   `yyyy-mm-dd` format. If NULL (the default), will use the current date
#'   in the client's time zone.
#' @param min The minimum allowed date. Either a Date object, or a string in
#'   `yyyy-mm-dd` format.
#' @param max The maximum allowed date. Either a Date object, or a string in
#'   `yyyy-mm-dd` format.
#' @param format The format of the date to display in the browser. Defaults to
#'   `"yyyy-mm-dd"`.
#' @param startview The date range shown when the input object is first clicked.
#'   Can be "month" (the default), "year", or "decade".
#' @param weekstart Which day is the start of the week. Should be an integer
#'   from 0 (Sunday) to 6 (Saturday).
#' @param language The language used for month and day names. Default is "en".
#'   Other valid values include "ar", "az", "bg", "bs", "ca", "cs", "cy", "da",
#'   "de", "el", "en-AU", "en-GB", "eo", "es", "et", "eu", "fa", "fi", "fo",
#'   "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id", "is", "it-CH", "it",
#'   "ja", "ka", "kh", "kk", "ko", "kr", "lt", "lv", "me", "mk", "mn", "ms",
#'   "nb", "nl-BE", "nl", "no", "pl", "pt-BR", "pt", "ro", "rs-latin", "rs",
#'   "ru", "sk", "sl", "sq", "sr-latin", "sr", "sv", "sw", "th", "tr", "uk",
#'   "vi", "zh-CN", and "zh-TW".
#' @param autoclose Whether or not to close the datepicker immediately when a
#'   date is selected.
#' @param datesdisabled Which dates should be disabled. Either a Date object,
#' or a string in `yyyy-mm-dd` format.
#' @param daysofweekdisabled Days of the week that should be disabled. Should be
#'   a integer vector with values from 0 (Sunday) to 6 (Saturday).
#'
#' @family input elements
#' @seealso [dateRangeInput()], [updateDateInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   dateInput("date1", "Date:", value = "2012-02-29"),
#'
#'   # Default value is the date in client's time zone
#'   dateInput("date2", "Date:"),
#'
#'   # value is always yyyy-mm-dd, even if the display format is different
#'   dateInput("date3", "Date:", value = "2012-02-29", format = "mm/dd/yy"),
#'
#'   # Pass in a Date object
#'   dateInput("date4", "Date:", value = Sys.Date()-10),
#'
#'   # Use different language and different first day of week
#'   dateInput("date5", "Date:",
#'           language = "ru",
#'           weekstart = 1),
#'
#'   # Start with decade view instead of default month view
#'   dateInput("date6", "Date:",
#'             startview = "decade"),
#'
#'   # Disable Mondays and Tuesdays.
#'   dateInput("date7", "Date:", daysofweekdisabled = c(1,2)),
#'
#'   # Disable specific dates.
#'   dateInput("date8", "Date:", value = "2012-02-29",
#'             datesdisabled = c("2012-03-01", "2012-03-02"))
#' )
#'
#' shinyApp(ui, server = function(input, output) { })
#' }
#'
#' @section Server value:
#' A [Date] vector of length 1.
#'
#' @export
dateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
  language = "en", width = NULL, autoclose = TRUE,
  datesdisabled = NULL, daysofweekdisabled = NULL,
  sassVariables = list()) {

  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  datesdisabled <- dateYMD(datesdisabled, "datesdisabled")

  value <- restoreInput(id = inputId, default = value)

  tags$div(id = inputId,
    class = "shiny-date-input form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

    shinyInputLabel(inputId, label),
    tags$input(type = "text",
               class = "form-control",
               `data-date-language` = language,
               `data-date-week-start` = weekstart,
               `data-date-format` = format,
               `data-date-start-view` = startview,
               `data-min-date` = min,
               `data-max-date` = max,
               `data-initial-date` = value,
               `data-date-autoclose` = if (autoclose) "true" else "false",
               `data-date-dates-disabled` =
                   # Ensure NULL is not sent as `{}` but as 'null'
                   jsonlite::toJSON(datesdisabled, null = 'null'),
               `data-date-days-of-week-disabled` =
                   jsonlite::toJSON(daysofweekdisabled, null = 'null')
    ),
    datePickerDependencies(sassVariables)
  )
}

datePickerDependencies <- function(sassVariables = list()) {

  if (useBsTheme() || length(sassVariables) == 0) {
    scssDir <- system.file(package = "shiny", "www", "shared", "datepicker", "scss")
    tmpDir <- tempfile("datepicker-scss")
    dir.create(tmpDir)
    outFile <- file.path(tmpDir, "custom.css")
    sassFunc <- if (useBsTheme()) bootstraplib::bootstrap_sass else sass::sass
    defaults <- list(
      sassVariables %OR% "",
      bsThemeDateDefaults()
    )
    sassFunc(
      list(
        defaults,
        sass::sass_file(file.path(scssDir, "build3.scss"))
      ),
      output = outFile
    )
    # Hash the skin and defaults so that the html dependency from one date-picker won't stomp another
    cssDependency <- htmlDependency(
      paste0("datepicker-datepicker-css-", digest::digest(defaults, "xxhash64")),
      version = "1.9.0",
      src = dirname(outFile),
      stylesheet = basename(outFile)
    )
  } else {
    cssDependency <- htmlDependency(
      "bootstrap-datepicker-css",
      version = "1.9.0",
      src = c(href = "shared/datepicker"),
      stylesheet = "css/bootstrap-datepicker3.min.css",
    )
  }

  list(
    cssDependency,
    htmlDependency(
      "bootstrap-datepicker-js",
      version = "1.9.0",
      c(href = "shared/datepicker"),
      script = "js/bootstrap-datepicker.min.js",
      # Need to enable noConflict mode. See #1346.
      head = "<script>(function() {
        var datepicker = $.fn.datepicker.noConflict();
        $.fn.bsDatepicker = datepicker;
      })();
     </script>"
    )
  )
}



bsThemeDateDefaults <- function() {
  if (!useBsTheme()) {
    return("")
  }

  version <- bootstraplib::theme_version()
  if ("3" %in% version) {
    return(list(
      "@function color-contrast($color, $dark: $gray-darker, $light: $text-color) {
        @return if(
          red($color) * 0.299 + green($color) * 0.587 + blue($color) * 0.114 > 150,
          $dark, $light
        );
      }",
      # No !default flag because these are variables already defined by Bootstrap
      list(
        "btn-primary-color" = "color-contrast($btn-primary-bg)",
        "btn-primary-color-hover" = "color-contrast($gray-lighter)",
        "state-info-bg" = "mix($body-bg, $brand-primary, 20%)",
        "dropdown-bg" = "$body-bg",
        "dropdown-border" = "mix($text-color, $body-bg, 15%)"
      )
    ))
  }

  list(
    "gray" = "gray('300')",
    "gray-light" = "gray('500')",
    "gray-lighter" = "gray('900')",
    "brand-primary" = "mix($black, theme-color('primary'), 6.5%)",
    "btn-primary-color" = "color-yiq($brand-primary)",
    "btn-primary-color-hover" = "color-yiq($gray-lighter)",
    "btn-primary-bg" = "$brand-primary",
    "btn-primary-border" = "darken($btn-primary-bg, 5%)",
    "btn-link-disabled-color" = "$gray-light",
    "state-info-bg" = "mix($white, $brand-primary, 20%)",
    "line-height-base" = "20/14",
    "border-radius-base" = "4px",
    "dropdown-bg" = "$white",
    "dropdown-border" = "mix($black, $white, 15%)"
  )
}
