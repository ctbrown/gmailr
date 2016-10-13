
TEXT_PART = 1L  # CONSTANT
HTML_PART = 2L  # CONSTANT

#' Create a mime formatted message object
#'
#' @param ... name-value pairs used to create fields for the MIME header. 
#'   You can set header values initially, or use \code{common_fields}.  
#' @param attr attributes to pass to the model
#' @param body \strong{text} to use for the body
#' @param parts mime parts to pass to the model \code{\link{common_fields}} 
#'   set functions to set them after object creation.
#' 
#' @details 
#'   \code(...) must be properly name-value pairs that are incorporated into 
#'   the MIME header.
#'   
#'   \code{attr} fields that are used are: \code{content_type}, \code{encoding}
#'   and \code{boundary}. The last of which is probably unnessary. 
#'   
#'   There is little checking of \code{attr} field for correctness.
#'   
#'   There seems to be little rationale for a distinction between \code{...} 
#'   and \code{attr}.
#'     
#' 
#' @seealso \code{\link{common_fields}}, \code{\link{body}}
#' @examples
#' # using the field functions
#'   msg = mime() %>%
#'    from("james.f.hester@@gmail.com") %>%
#'    to("CRAN@@R-project.org") %>%
#'    text_body("Please don't reject my package")
#'
#' # alternatively you can set the fields (...) using mime, however you must
#' # use properly formatted MIME names
#'   msg = mime(
#'     From="james.f.hester@@gmail.com",
#'     To="CRAN@@R-project.org"
#'   ) %>%
#'   html_body("<b>Please<\b> don't reject my package")
#'   
#' @export

mime = function( ..., attr = NULL, body = NULL, parts = list() ) {
  
  structure( 
    list(
      parts  = parts ,
      header = with_defaults( 
        c( 
          "MIME-Version" = "1.0",
          Date = http_date(Sys.time())
        ) ,
        ...
      ),
      body=body, 
      attr=attr
      ), 
    class="mime"
  )
  
}

#' Accessor functions for mime messages
#' @param x the (mime) object whose fields you are setting
#' @param val the value to set
#' @param vals one or more values to use, will be joined by commas
#' @param ... other arguments ignored
#' @rdname common_fields
#' @export
to.mime = function(x, vals, ...){
  if( missing(vals) ) { return(x$header$To) }
  x$header$To = paste0( collapse=", ", vals )
  x
}

#' @name common_fields
#' @rdname common_fields
#' @export
from.mime = function(x, val, ...){
  if(missing(val)){ return(x$header$From) }
  x$header$From = val
  x
}

#' @rdname common_fields
#' @export
cc = function(x, vals, ...){
  if(missing(vals)){ return(x$header$Cc) }
  x$header$Cc = paste0(collapse=", ", vals)
  x
}

#' @rdname common_fields
#' @export
bcc = function(x, vals, ...){
  if(missing(vals)){ return(x$header$Bcc) }
  x$header$Bcc = paste0(collapse=", ", vals)
  x
}

#' @rdname common_fields
#' @export
subject.mime = function(x, val, ...){
  if(missing(val)){ return(x$header$Subject) }
  x$header$Subject = val
  x
}

#' Add a text or html body to a mime message
#'
#' if called without a body returns the current body
#' 
#' @param mime message to add the body to
#' @param body the body to add to the message
#' @param ... additional parameters to put in the attr field
#' 
#' @details 
#' 
#' @section text_body: 
#' 
#'  The useful values for \code{...} are: \cr
#'  * content-type : textplain \cr
#'  * charset : utf-8\cr
#'  * encoding : quoted-printable \cr
#'  * format : flowec \cr
#'  
#' Because \code{...} is passed into the attr, there is no way to get them into
#' the header, but we need to   
#' 
#' @rdname mime_body
#' @export

text_body = function(mime, body, ...){
  
  if( missing(body) ) return( mime$parts[[TEXT_PART]] ) 
  
  mime$parts[[TEXT_PART]]  = 
    mime(
      attr=list(
        content_type = 'text/plain',
        charset      = 'utf-8',
        encoding     = 'quoted-printable',
        format       = 'flowed',
        ...
      ),
      body = body
    )
  
  mime
}



#' @rdname mime_body
#' @export
html_body = function( mime, body, ... ) {
  
  if( missing(body) ){ return( mime$parts[[HTML_PART]] ) }
  
  mime$parts[[HTML_PART]] = 
    mime( 
      attr=list(
        content_type = 'text/html',
        charset      = 'utf-8',
        encoding     = 'base64',
        ...
      ),
      body = body
  )
  
  mime
}


#'  @references 
#'    \url{https://datatracker.ietf.org/doc/draft-ietf-appsawg-text-markdown/?include_text=1}
#' @rdname mime_body
#' @export

markdown_body = function(mime, body, ...){
  
  if( missing(body) ) return( mime$parts[[TEXT_PART]] ) 
  
  mime$parts[[TEXT_PART]]  = 
    mime(
      attr=list(
        content_type = 'text/markdown',
        charset      = 'utf-8',
        encoding     = 'quoted-printable',
        format       = 'flowed' ,
        variant      = NULL,
        ...
      ),
      body = body
    )
  
  mime
}



#' Attach an object to a mime message
#' 
#' @param mime object to attach to
#' @param body data to attach
#' @param filename name of file to attach
#' @param type mime type of the attached file
#' @param ... additional arguments put into the \code{attr} field of the object
#' 
#' @details 
#'   \code{...} is unlike \code{...} in \code{mime}.  It populates the 
#'   \code{attr} array and not the header which is set automatically.
#' 
#' @rdname attach
#' @export

attach_part = function(mime, body, ...) {
  
  if( missing(body) ){ 
    return( mime$parts[[3L:length(mime$parts)]] ) 
  }
  
  part_num = if( length(mime$parts) < 3L) 3L else length(mime$parts) + 1L
  
  mime$parts[[part_num]] = 
    mime( 
      attr = c(encoding = 'base64', list(...) ),
      body = body
    )
  
  mime
}


#' @param attr list (see \code{\link{mime}} ).
#' 
#' @rdname attach
#' @export

attach_part2 = function(mime, body, ..., attr = list() ) {
  
  if( missing(body) ){ 
    return( mime$parts[[3L:length(mime$parts)]] ) 
  }
  
  part_num = if( length(mime$parts) < 3L) 3L else length(mime$parts) + 1L
  
  mime$parts[[part_num]] = 
    mime( 
      attr = c(encoding = 'base64', attr ),
      body = body,
      ...
    )
  
  mime
}


#' @rdname attach
#' @export

attach_file = function(mime, filename, type = NULL, ...) {
  
  if( missing(filename) ){ return( mime$parts[[3L:length(mime$parts)]] ) }

  if ( is.null(type) ) {
    type = mime::guess_type(filename, empty = NULL)
  }

  con  = file( filename, "rb" )
  # info = file.info(filename)
  body = readBin(con, "raw", file.size(filename) )
  close(con)

  base_name = basename(filename)

  attach_part(
    mime, 
    body,
    content_type = type,
    name = base_name,
    filename = base_name,
    modification_date = http_date(info$mtime),
    ...
  )
  
}


#' @rdname attach
#' @export

attach_file2 = function(mime, filename, type = NULL, ..., attr=list() ) {
  
  if( missing(filename) ){ return( mime$parts[[3L:length(mime$parts)]] ) }

  if ( is.null(type) ) {
    type = mime::guess_type(filename, empty = NULL)
  }

  con  = file( filename, "rb" )
  info = file.info(filename)
  body = readBin(con, "raw", info$size )
  close(con)

  base_name = basename(filename)

  attach_part2(
    mime, 
    body,
    content_type      = type,
    name              = base_name,
    filename          = base_name,
    modification_date = http_date(info$mtime),
    ...,
    attr=attr
  )
  
}


#' Convert a mime object to character representation
#'
#' These functions convert a mime object into the final mime character
#' representation.
#' @param x object to convert
#' @param ... additional arguments ignored
#' @param newline value to use as newline character
#' @rdname as.character.mime
#' @export

as.character.mime = function( x, ..., newline="\r\n" ) {

  # if we have both the text part and html part, we have to embed them in a 
  # multipart/alternative message
  if( x$attr$content_type %!=% 'multipart/alternative' && 
      exists_list(x$parts, TEXT_PART) && 
      exists_list(x$parts, HTML_PART)
  ){
    new_msg = mime( 
      attr=list(content_type = 'multipart/alternative' ),
      parts=c( x$parts[TEXT_PART], x$parts[HTML_PART] )
    )
    x$parts[TEXT_PART] = list(NULL)
    x$parts[HTML_PART] = list(NULL)
    x$parts[[1]] = new_msg          # create new multipart/alternative
  }

  # if a multipart message
  if( length(x$parts) > 0L ){

    x$attr$content_type = x$attr$content_type %||% 'multipart/mixed'

    # random hex boundary if multipart, otherwise nothing
    boundary = x$attr$boundary = random_hex(32)

    # sep is --boundary newline if multipart, otherwise newline
    sep = paste0('--', boundary, newline)

    # end is --boundary-- if mulitpart, otherwise nothing
    end = paste0('--', boundary, '--', newline)

    body_text = 
      paste0( collapse=sep,
        Filter(
            function(x) length(x) > 0L
          , c( lapply(x$parts, as.character), x$body)  # recursively apply 
        )
      )
    
  } else {
    
    boundary = NULL
    sep = newline
    end = newline

    body_text = x$body
  }

  x$header$"Content-Type" = parse_content_type(x$attr)
  x$header$"Content-Transfer-Encoding" = x$attr$encoding
  
  # CTB:
  # This change is suggested by issue 16 by @brenoea to match MIME specs which
  # has an additional "\r\n" after after every part's attributes block 
  
  # x$header$"Content-Disposition" = parse_content_disposition(x$attr)
  x$header$"Content-Disposition" =    
    paste0( parse_content_disposition(x$attr), "\r\n" )
  
  
  encoding = x$attr$encoding %||% ''

  encoded_body = switch( encoding,
    'base64'           = encode_base64(body_text, 76L, newline),
    'quoted-printable' = quoted_printable_encode(body_text),
    body_text
  )
  
  headers = format_headers( x$header, newline=newline )
  
  # CTB:
  # if( ! grepl( paste0( newline, "$"), headers ) )
  #  headers <- paste0( headers, newline )
  
  paste0(headers, sep, encoded_body, end)
  
}

parse_content_type = function(header) {
  paste0(header$content_type %||% 'text/plain',
         header$charset %|||% paste0('; charset=', header$charset),
         header$format %|||% paste0('; format=', header$format),
         header$name %|||% paste0('; name=', header$name),
         header$boundary %|||% paste0('; boundary=', header$boundary)
         )
}


parse_content_disposition = function(header) {
  paste0( 
    header$disposition %||% 'inline' ,
    header$filename %|||% paste0('; filename=', header$filename) ,
    header$modification_date %|||% paste0( '; modification-date=', header$modification_date) )
}

# CTB: used for MIME boundary separator
random_hex = function(width=4) {
  paste(sprintf("%x", sample(16, size=width, replace=TRUE) - 1L), collapse="")
}


# CTB: Produces a single element character vector (string) reprensentation of
#      the MIME headers
format_headers = function( headers, newline ) {
  
  empty = vapply(headers, function(x) { is.null(x) || length(x) %==% 0L }, logical(1L))
  keep_headers = headers[!empty]
  if(length(keep_headers) %==% 0L){
    return(NULL)
  }
  # CTB: issue 16 fix
  # paste0(paste(sep=": ", collapse=newline, names(keep_headers), keep_headers), newline)
  headers = paste0(
    paste(sep=": ", collapse=newline, names(keep_headers), keep_headers), 
    newline
  )
  
  # if( ! grepl("\r\n\r\n$", headers ) )
  #   headers <- paste0( headers, "\r\n" )
  return(headers)
  # CTB: END
}

with_defaults = function(defaults, ...){
  args = list(...)
  missing = setdiff(names(defaults), names(args))
  c(defaults[missing], args)
}

