# ============================================================================
# generics
# ============================================================================

explore <- function(x, ...) UseMethod("explore")

explore.default <- function(x, ...) invisible(x)

as_character <- function(x, ...) UseMethod("as_character")

as_character.default <- function(x, ...) as.character(x, ...)

as_numeric <- function(x, ...) UseMethod("as_numeric")

as_numeric.default <- function(x, ...) as.numeric(x, ...)

n_tokens <- function(x, ...) UseMethod("n_tokens")

n_tokens.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.numeric(NA)
}

n_types <- function(x, ...) UseMethod("n_types")

n_types.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.numeric(NA)
}

type_names <- function(x, ...) UseMethod("type_names")

type_names.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.character(NA)
}

keep_pos <- function(x,
                     pos,
                     invert = FALSE,
                     ...) UseMethod("keep_pos")

keep_pos.default <- function(x,
                             pos,
                             invert = FALSE,
                             ...) {
  warning("unsupported type of x; simply returning x")
  x
}

keep_re <- function(x,
                    pattern,
                    perl = TRUE,
                    invert = FALSE,
                    ...) UseMethod("keep_re")

keep_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            invert = FALSE,
                            ...) {
  warning("unsupported type of x; simply returning x")
  x
}

keep_types <- function(x,
                       types,
                       invert = FALSE,
                       ...) UseMethod("keep_types")

keep_types.default <- function(x,
                               types,
                               invert = FALSE,
                               ...) {
   warning("unsupported type of x; simply returning x")
  x
}
  
keep_bool <- function(x,
                      bool,
                      invert = FALSE,
                      ...) UseMethod("keep_bool")

keep_bool.default <- function(x,
                              bool,
                              invert = FALSE,
                              ...) {
   warning("unsupported type of x; simply returning x")
  x
}

drop_pos <- function(x,
                     pos,
                     ...) UseMethod("drop_pos")

drop_pos.default <- function(x,
                             pos,
                             ...) {
   warning("unsupported type of x; simply returning x")
  x
}

drop_re <- function(x,
                    pattern,
                    perl = TRUE,
                    ...) UseMethod("drop_re")

drop_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            ...) {
   warning("unsupported type of x; simply returning x")
  x
}
drop_types <- function(x,
                       types,
                       ...) UseMethod("drop_types")

drop_types.default <- function(x,
                               types,
                               ...) {
   warning("unsupported type of x; simply returning x")
  x
}
drop_bool <- function(x,
                      bool,
                      ...) UseMethod("drop_bool")

drop_bool.default <- function(x,
                              bool,
                              ...) {
   warning("unsupported type of x; simply returning x")
  x
}

"tot_n_tokens<-" <- function(x, value) UseMethod("tot_n_tokens<-")

"tot_n_tokens<-.default" <- function(x, value) x

tot_n_tokens <- function(x) UseMethod("tot_n_tokens")

tot_n_tokens.default <- function(x) NULL

"orig_ranks<-" <- function(x, value) UseMethod("orig_ranks<-")

"orig_ranks<-.default" <- function(x, value) x

orig_ranks <- function(x, ...) UseMethod("orig_ranks")

orig_ranks.default <- function(x, ...) NULL

ranks <- function(x, ...) UseMethod("ranks")

ranks.default <- function(x, ...) NULL

as_data_frame <- function(x, row.names = NULL,
                          optional = FALSE, ...) UseMethod("as_data_frame")

as_data_frame.default <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}

trunc_at <- function(x, pattern, ...) UseMethod("trunc_at")

trunc_at.default <- function(x, pattern, ...) invisible(x)

