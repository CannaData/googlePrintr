#' List printers associated with Google account
#'
#' @param name Query printer names
#' @param type Query printer type, see notes
#' @param connection_status Query connection status, see notes
#' @param use_cdd Indicate whether to include printer info
#' @param extra_fields Comma separated list of extra fields to include
#' @importFrom googleAuthR gar_api_generator
#' @note For info about parameters see \url{https://developers.google.com/cloud-print/docs/appInterfaces#search}
#' @export
#'

gcp_search <- function(name = NULL,
                       type = NULL,
                       connection_status = NULL,
                       use_cdd = NULL,
                       extra_fields = NULL) {
  if (!is.null(type)) {
    type <- match.arg(
      type,
      c(
        "GOOGLE",
        "HP",
        "DOCS",
        "DRIVE",
        "FEDEX",
        "ANDROID_CHROME_SNAPSHOT",
        "IOS_CHROME_SNAPSHOT"
      )
    )
  }
  
  if (!is.null(connection_status)) {
    connection_status <- match.arg(connection_status,
                                   c("ALL", "ONLINE", "UNKNOWN", "OFFLINE", "DORMANT"))
  }
  
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/search",
    data_parse_function = function(x) {
      x$printers
    },
    pars_args = list(
      q = '',
      type = '',
      connection_status = '',
      use_cdd = '',
      extra_fields = ''
    ),
    checkTrailingSlash = FALSE
  )
  
  f(
    pars_arguments = list(
      q = name,
      type = type,
      connection_status = connection_status,
      use_cdd = use_cdd,
      extra_fields = extra_fields
    )
  )
}

#' Get details about printer
#'
#' @param printerid The ID of the printer
#' @param client See notes
#' @inheritParams gcp_search
#' @note Client field lets user augment printer capabilities see \url{https://developers.google.com/cloud-print/docs/appInterfaces#printer}
#' @export
#'

gcp_printer <-
  function(printerid,
           client = NULL,
           extra_fields = NULL) {
    f <- googleAuthR::gar_api_generator(
      "https://www.google.com/cloudprint/printer",
      data_parse_function = function(x) {
        x$printers
      },
      pars_args = list(
        printerid = '',
        client = '',
        extra_fields = ''
      ),
      checkTrailingSlash = FALSE
    )
    
    f(pars_arguments = list(
      printerid = printerid,
      client = client,
      extra_fields = extra_fields
    ))
  }

#' Accept printer invitation
#'
#' @inheritParams gcp_printer
#' @param accept Boolean indicating whether to accept request from printer
#' @export
#' @note Undocumented API discovered at \url{https://stackoverflow.com/a/36366114/4564432}
#'

gcp_processinvite <- function(printerid, accept = TRUE) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/processinvite",
    http_header = "POST",
    data_parse_function = function(x) {
      x$status
    },
    pars_args = list(printerid = '',
                     accept = TRUE),
    checkTrailingSlash = FALSE
  )
  
  f(pars_arguments = list(printerid = printerid,
                          accept = accept))
  
}

#' Submit print job to printer
#'
#' @importFrom jsonlite toJSON unbox
#' @inheritParams gcp_printer
#' @param title Title of print job
#' @param ticket Print ticket, see notes
#' @param content Document to print, see notes
#' @param contentType MIME type of document to print
#' @param tag Tags to add to print job
#' @param http_header Either GET or POST, but only GET works at moment
#' @note See \url{https://developers.google.com/cloud-print/docs/appInterfaces#submit}
#'
#' @export
#'

gcp_submit <- function(printerid,
                       title,
                       ticket = NULL,
                       content,
                       contentType = NULL,
                       tag = NULL,
                       http_header = c("GET", "POST")) {
  http_header <- match.arg(http_header)
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/submit",
    http_header = http_header,
    data_parse_function = function(x) {
      x$success
    },
    pars_args = list(
      printerid = '',
      title = '',
      ticket = jsonlite::toJSON(list(
        version = jsonlite::unbox("1.0"),
        print = c()
      ), auto_unbox = FALSE),
      content = '',
      contentType = '',
      tag = ''
    ),
    customConfig = list(
      # https://developers.google.com/cloud-print/docs/pythonCode#multipart-form-data
      encode = "multipart"
    ),
    checkTrailingSlash = FALSE
  )
  
  f(
    pars_arguments = list(
      printerid = printerid,
      title = title,
      ticket = ticket,
      content = content,
      contentType = contentType,
      tag = tag
    )
  )
}
