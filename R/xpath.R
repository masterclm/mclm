# =============================================================================
# functions in support of XPath
# =============================================================================

find_xpath <- function(pattern,
                       x,
                       handlers = NULL,
                       trim = TRUE,
                       fun = NULL,
                       final_fun = NULL,
                       namespaces = NULL,
                       ...) {
   # ----------------------------------------------------------
   # - x can be any of the following:
   #      - a vector of filenames
   #      - a character vector of XML source
   #      - a list of parsed XML documents  
   #  - asText, trim, ignoreBlanks=TRUE
   #       - passed on the xmlParse
   #       if x contains parsed XML documents,
   #       then these arguments are ignored.
   #  - ...
   #       passed on to sapply
   # ----------------------------------------------------------
   if (!is.vector(x)) {
      x <- list(x)
   }
   res <- vector("list", length(x))
   for (i in seq_along(x)) {
      cur <- x[[i]]
      # -------------------------------------------------------
      # - if cur turns out to be XML source or a filename,
      #   which is tested with (is.character(cur)), we
      #   parse it with xmlParse()
      # - otherwise, we assume cur is a parse XML document
      #   this could be tested with something along the
      #   lines of ("XMLInternalDocument" %in% class(cur));
      #   however, currently this default case is not tested,
      #   just assumed.
      # case of cur.x is XML source or filename
      # -------------------------------------------------------
      if (is.character(cur)) { 
         cur <- XML::xmlParse(cur,
                              handlers = handlers,
                              trim = trim)
      }   
      # -------------------------------------------------------
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
