#' Run XPath query
#' 
#' This function finds matches for an XPath query in a corpus.
#'
#' @param pattern An XPath query.
#' @param x A corpus: an [`fnames`] object, a character vector of an XML source,
#'   a list of documents already parsed with [XML::xmlParse()].
#' @param handlers Optional collection of functions used to map the different
#'   XML nodes to R objects.
#' @param trim Logical. Whether to strip of whitespace from the begginng and end
#'  of text strings.
#' @param fun Optional function to be applied to the individual matches prior
#'   to returning the result.
#' @param final_fun Optional final function to be applied to the complete list
#'  of matches prior to returning the result.
#' @param namespaces Optional namespaces.
#' @param ... Additional arguments.
#'
#' @return A list with matches (see [XML::xpathApply()]).
#' @export
#' 
#' @examples 
#' test_xml <- '
#' <p>
#'   <w pos="at">The</w>
#'   <w pos="nn">example</w>
#'   <punct>.</punct>
#' </p>'
#' 
#' find_xpath("//w", test_xml)
#' find_xpath("@pos", test_xml)
#' find_xpath("//w[@pos='nn']", test_xml)
find_xpath <- function(pattern,
                       x,
                       handlers = NULL,
                       trim = TRUE,
                       fun = NULL,
                       final_fun = NULL,
                       namespaces = NULL,
                       ...) {
   # TODO use xml2 instead of XML
   if (!is.vector(x)) {
      x <- list(x)
   }
   res <- vector("list", length(x))
   for (i in seq_along(x)) {
      cur <- x[[i]]
      # --
      # - if cur turns out to be XML source or a filename,
      #   which is tested with (is.character(cur)), we
      #   parse it with xmlParse()
      # - otherwise, we assume cur is a parse XML document
      #   this could be tested with something along the
      #   lines of ("XMLInternalDocument" %in% class(cur));
      #   however, currently this default case is not tested,
      #   just assumed.
      # case of cur.x is XML source or filename
      # --
      if (is.character(cur)) { 
         cur <- XML::xmlParse(cur,
                              handlers = handlers,
                              trim = trim)
      }   
      #res[[i]] <- cur[pattern]
      if (is.null(namespaces)) {
         namespaces = XML::xmlNamespaceDefinitions(cur, simplify = TRUE)
      }
      res[[i]] <- XML::xpathApply(cur, pattern,
                                  namespaces = namespaces, ...)
   }
   res <- unlist(res)
   if (!is.null(fun)) {
     res <- sapply(res, fun, ...)
   }
   if (!is.null(final_fun)) {
     res <- do.call(final_fun, list(res))
   }    
   res
}
