
#
#    name: mclm.R  [short for: mastering corpus linguistics methods]
#    purpose: library of simple R functions in support of corpus linguistics
#    author: Dirk Speelman 
#    version: 2017-10-16
#

.onUnload <- function(libpath) {
  library.dynam.unload("mclm", libpath)
}

