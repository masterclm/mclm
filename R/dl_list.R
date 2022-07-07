## ---------------------------------------------------------------------------
##
## Simple implementation of a doubly linked list using environments
##
## ---------------------------------------------------------------------------
##
## A doubly linked list is implemented as an environment x with the following
## objects in it:
##
##  x$sentinel: which points to a 'node' which is at the same time the
##              starting point and the end point of the chain of items in
##              the list.
##  x$current : which points to the 'current node' in the list.
##  x$n       : which stores the size of the list (i.e. the number of
##              non-sentinel 'nodes' in the list
##
## All the 'nodes' in the list are themselves also implemented as environments
## z with the following ojects in them:
##
##  z$value   : the actual value contained by that node
##  z$nxt     : pointer to the next node in the list
##  z$prev    : pointer to the previous node in the list
##
## The sentinel of a list x, i.e. x$sentinel, is a node like any other node,
## except that its value is always NULL. In an empty list, x$sentinel$nxt
## points to x$sentinel itself and x$sentinel$prev also points to
## x$sentinel itself. In a non-empty list, e.g. a list with the 'real' nodes
## z1, z2, ..., zn, x$sentinel$nxt points to z1, z1$nxt points to z2, z2$nxt
## points to z3, etc., and zn$nxt again points to x$sentinel. ALso,
## x$sentinel$prev will point to zn, zn$prev will point to the one but last
## 'real' node, etc., and z1$prev will again point to x$sentinel.
## ---------------------------------------------------------------------------
##
## If x is a doubly linked list, then:
##
## - x$sentinel$next gives you:
##      - x$sentinel if the list is empty
##      - the first 'real' item in the list otherwise
##
## - x$sentinel$prev gives you:
##      - x$sentinel if the list is empty
##      - the last 'real' item in the list otherwise
##
## - x$current gives you:
##      - x$sentinel if no other item has been made the current item
##      - the last selected current item otherwise
##
## - x$n gives you:
##      - the number of 'real' (i.e. non-sentinel) items in the list
##
## ---------------------------------------------------------------------------


## ---------------------------------------------------------------------------
## wish list / to do
## ---------------------------------------------------------------------------
##  insert_at_current(x, y, before = TRUE)
##  pop_at_current(x, before = TRUE)
##  insert_at_pos(x, i)
## ---------------------------------------------------------------------------


## doubly linked list constructor
dl_list <- function() {
  x <- new.env(parent = emptyenv())
  x$sentinel <- new.env(parent = emptyenv())
  x$sentinel$nxt <- x$sentinel
  x$sentinel$prev <- x$sentinel
  x$sentinel$value <- NULL          # sentinel always has value NULL
  x$current <- x$sentinel           # current item in list
  x$n <- 0                          # number of non-sentinel items in list 
  x
}

## ---------------------------------------------------------------------------

## test if list is empty; alternative method would be x$n == 0
is_empty <- function(x) {
  identical(x$sentinel$nxt, x$sentinel)
}

## test if list is empty; alternative method would be x$n > 0
has_items <- function(x) {
  !identical(x$sentinel$nxt, x$sentinel)
}

## test if the current node is a "real node" (as opposed to being the sentinel)
current_exists <- function(x) {
  !identical(x$current, x$sentinel)
}

## test if the next node is a "real node" (as opposed to being the sentinel)
next_exists <- function(x) {
  !identical(x$current$nxt, x$sentinel)
}

## ---------------------------------------------------------------------------

## get number of 'real nodes' in list x
get_size <- function(x) {
  x$n
}

## get value of 'current node' in list; NULL if sentinel is 'current node'
## does not remove any nodes
get_cur <- function(x) {
  x$current$value
}

## get value of 'first real node' in list; NULL if list is empty 
## does not remove any nodes
get_first <- function(x) {
  x$sentinel$nxt$value
}

## get value of 'last real node' in list; NULL if list is empty
## does not remove any nodes
get_last <- function(x) {
  x$sentinel$prev$value
}

## ---------------------------------------------------------------------------

##  make first the sentinel the current node
goto_sentinel <- function(x) {
  x$current <- x$sentinel
  invisible(TRUE)                # return success; always succeeds
}

##  make first "real node" the current node, if it exists; if it doesn't, the
##  sentinel is made the current node (or rather, is confirmed as current
##  node, because it already should have been the current node)
goto_first <- function(x) {
  x$current <- x$sentinel$nxt
  invisible(current_exists(x))   # return success
}

##  make next node the current node; this next node can be the sentinel
goto_next <- function(x) {
  x$current <- x$current$nxt
  invisible(current_exists(x))   # return success
}

##  make previous node the current node; this next node can be the sentinel
goto_prev <- function(x) {
  x$current <- x$current$prev
  invisible(current_exists(x))   # return success
}

##  make last "real node" the current node, if it exists; if it doesn't, the
##  sentinel is made the current node (or rather, is confirmed as current
##  node, because it already should have been the current node)
goto_last <- function(x) {
  x$current <- x$sentinel$prev
  invisible(current_exists(x))   # return success  
}

## ---------------------------------------------------------------------------

## add new node with value y to beginning of list x
push_first <- function(x, y) {
  old_first <- x$sentinel$nxt
  new_node <- new.env(parent = emptyenv())
  new_node$value <- y
  new_node$nxt <- old_first
  new_node$prev <- x$sentinel
  old_first$prev <- new_node
  x$sentinel$nxt <- new_node
  x$n <- x$n + 1
  invisible(x)                      # return the whole list
}

## add new node with value y to beginning of list x
push_last <- function(x, y) {
  old_last <- x$sentinel$prev
  new_node <- new.env(parent = emptyenv())
  new_node$value <- y
  new_node$nxt <- x$sentinel
  new_node$prev <- old_last
  x$sentinel$prev <- new_node
  old_last$nxt <- new_node
  x$n <- x$n + 1
  invisible(x)                       # return the whole list
}

## same as push (we copy code to reduce number of function calls)
push <- function(x, y) {
  old_last <- x$sentinel$prev
  new_node <- new.env(parent = emptyenv())
  new_node$value <- y
  new_node$nxt <- x$sentinel
  new_node$prev <- old_last
  x$sentinel$prev <- new_node
  old_last$nxt <- new_node
  x$n <- x$n + 1
  invisible(x)                       # return the whole list
}


## ---------------------------------------------------------------------------

## get value of first real node from list and remove this node from list
pop_first <- function(x) {
  if (is_empty(x)) {
    stop("Cannot remove item from empty dl_list")
  }
  target_node        <- x$sentinel$nxt
  target_is_current  <- identical(x$current, target_node)
  value              <- target_node$value
  rm("value", envir = target_node)
  node_to_left       <- target_node$prev
  node_to_right      <- target_node$nxt
  node_to_left$nxt   <- node_to_right
  node_to_right$prev <- node_to_left
  x$n <- x$n - 1
  if (target_is_current) {
    x$current <- x$sentinel$nxt
  }
  value
}

## get value of first real node from list and remove this node from list
pop_last <- function(x) {
  if (is_empty(x)) {
    stop("Cannot remove item from empty dl_list")
  }
  target_node        <- x$sentinel$prev
  target_is_current  <- identical(x$current, target_node)
  value              <- target_node$value
  rm("value", envir = target_node)
  node_to_left       <- target_node$prev
  node_to_right      <- target_node$nxt
  node_to_left$nxt   <- node_to_right
  node_to_right$prev <- node_to_left
  x$n <- x$n - 1 
  if (target_is_current) {
    x$current <- x$sentinel$prev
  }
  value
}

## same as pop_last (we copy code to reduce the number of function calls)
pop <- function(x) {
  if (is_empty(x)) {
    stop("Cannot remove item from empty dl_list")
  }
  target_node        <- x$sentinel$prev
  target_is_current  <- identical(x$current, target_node)
  value              <- target_node$value
  rm("value", envir = target_node)
  node_to_left       <- target_node$prev
  node_to_right      <- target_node$nxt
  node_to_left$nxt   <- node_to_right
  node_to_right$prev <- node_to_left
  x$n <- x$n - 1 
  if (target_is_current) {
    x$current <- x$sentinel$prev
  }
  value
}

## get value of first real node from list and remove this node from list
pop_cur <- function(x) {
  if (!current_exists(x)) {
    stop("Cannot remove sentinel from dl_list")
  }
  target_node        <- x$current
  target_is_current  <- identical(x$current, target_node)
  value              <- target_node$value
  rm("value", envir = target_node)
  node_to_left       <- target_node$prev
  node_to_right      <- target_node$nxt
  node_to_left$nxt   <- node_to_right
  node_to_right$prev <- node_to_left
  x$n <- x$n - 1 
  if (target_is_current) {
    if (!identical(node_to_right, x$sentinel)) {
      x$current <- node_to_right
    } else if (!identical(node_to_left, x$sentinel)) {
      x$current <- node_to_left
    } else {
      x$current <- x$sentinel
    }
  }
  value
}

# ----------------------------------------------------------------------------

print_dl_list <- function(x, n = 6) {
  old_cur <- x$current
  i <- 0
  cat(paste0("Doubly linked list (number of items: ", x$n, ")\n"))
  goto_sentinel(x)
  while ((i < n) && goto_next(x)) {
    i <- i + 1
    cat(paste0("[[", i, "]] ", get_cur(x), "\n"))
  }
  if (x$n > n) {
    cat("...\n")
  }
  x$current <- old_cur
}
