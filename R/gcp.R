#' List printers associated with Google account
#'
#' @param name String for searching printer names
#' @importFrom googleAuthR gar_api_generator
#' @export
#'

gcp_search <- function(name = NULL,
                       type = NULL,
                       connection_status = NULL,
                       use_cdd = NULL,
                       extra_fields = NULL) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/search",
    data_parse_function = function(x) {
      # print(x)
      x$printers
    },
    pars_args = list(
      q = '',
      type = '',
      connection_status = '',
      use_cdd = '',
      extra_fields = ''
    )
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
#' @param printerid
#' @export
#' 

gcp_printer <- function(printerid, client = NULL, extra_fields = NULL) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/printer",
    data_parse_function = function(x) {
      # print(x)
      x$printers
    },
    pars_args = list(
      printerid = '',
      client = '',
      extra_fields = ''
    )
  )
  
  f(pars_arguments = list(
    printerid = printerid,
    client = client,
    extra_fields = extra_fields
  ))
}

#' Submit print job to printer
#' @export
#' 

gcp_submit <- function(printerid,
                       title,
                       ticket = NULL,
                       content,
                       contentType = NULL,
                       tag = NULL) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/submit",
    # http_header = "POST",
    data_parse_function = function(x) {
      x$status
    },
    pars_args = list(
      printerid = '',
      title = '',
      ticket = '',
      content='',
      contentType='',
      tag=''
    )
  )
  
  f(the_body = content, 
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
