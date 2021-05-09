##########  insert tidyr verbs ##########
#' pivot_longer function in an R script with cursor between brackets
#'
#' @return pivot_longer function in an R script with cursor between brackets
#'
#' @export
insert_pivot_longer <- function() {
  insert(pivot_longer)
  reposition(1)
}

#' pivot_wider function in an R script with cursor between brackets
#'
#' @return pivot_wider function in an R script with cursor between brackets
#'
#' @export
insert_pivot_wider <- function() {
  insert(pivot_wider)
  reposition(1)
}

#' nest function in an R script with cursor between brackets
#'
#' @return nest function in an R script with cursor between brackets
#'
#' @export
insert_nest <- function() {
  insert(nest)
  reposition(1)
}

#' nest function in an R script with cursor between brackets
#'
#' @return nest function in an R script with cursor between brackets
#'
#' @export
insert_unnest <- function() {
  insert(unnest)
  reposition(1)
}
##########  insert dplyr verbs ##########

#' filter function in an R script with cursor between brackets
#'
#' @return filter function in an R script with cursor between brackets
#'
#' @export
insert_filter <- function() {
  insert(filter)
  reposition(1)
}

#' full_join function in an R script with cursor between brackets
#'
#' @return full_join function in an R script with cursor between brackets
#'
#' @export
insert_full_join <- function() {
  insert(full_join)
  reposition(1)
}

#' group_by function in an R script with cursor between brackets
#'
#' @return group_by function in an R script with cursor between brackets
#'
#' @export
insert_group_by <- function() {
  insert(group_by)
  reposition(1)
}

#' inner_join function in an R script with cursor between brackets
#'
#' @return inner_join function in an R script with cursor between brackets
#'
#' @export
insert_inner_join <- function() {
  insert(inner_join)
  reposition(1)
}

#' left_join function in an R script with cursor between brackets
#'
#' @return left_join function in an R script with cursor between brackets
#'
#' @export
insert_left_join <- function() {
  insert(left_join)
  reposition(1)
}

#' mutate function in an R script with cursor between brackets
#'
#' @return mutate function in an R script with cursor between brackets
#'
#' @export
insert_mutate <- function() {
  insert(mutate)
  reposition(1)
}

#' right_join function in an R script with cursor between brackets
#'
#' @return right_join function in an R script with cursor between brackets
#'
#' @export
insert_right_join <- function() {
  insert(right_join)
  reposition(1)
}

#' select function in an R script with cursor between brackets
#'
#' @return select function in an R script with cursor between brackets
#'
#' @export
insert_select <- function() {
  insert(select)
  reposition(1)
}

#' summarise function in an R script with cursor between brackets
#'
#' @return summarise function in an R script with cursor between brackets
#'
#' @export
insert_summarise <- function() {
  insert(summarise)
  reposition(1)
}

#' case_when function in an R script with cursor between brackets
#'
#' @return case_when function in an R script with cursor between brackets
#'
#' @export
insert_case_when <- function() {
  insert(case_when)
  reposition(1)
}

##########  insert selection helper verbs ##########

#' across function in an R script with cursor between brackets
#'
#' @return across function in an R script with cursor between brackets
#'
#' @export
insert_across <- function() {
  insert(across)
  reposition(1)
}

#' contains function in an R script with cursor between brackets
#'
#' @return contains function in an R script with cursor between brackets
#'
#' @export
insert_contains <- function() {
  insert(contains)
  reposition(2)
}

#' ends_with function in an R script with cursor between brackets
#'
#' @return ends_with function in an R script with cursor between brackets
#'
#' @export
insert_ends_with <- function() {
  insert(ends_with)
  reposition(2)
}

#' everything function in an R script with cursor between brackets
#'
#' @return everything function in an R script with cursor between brackets
#'
#' @export
insert_everything <- function() {
  insert(everything)
}

#' is.numeric function in an R script with cursor between brackets
#'
#' @return is_numeric function in an R script with cursor between brackets
#'
#' @export
insert_is.numeric <- function() {
  insert(is.numeric)
  reposition(1)
}

#' starts_with function in an R script with cursor between brackets
#'
#' @return starts_with function in an R script with cursor between brackets
#'
#' @export
insert_starts_with <- function() {
  insert(starts_with)
  reposition(2)
}

#' where function in an R script with cursor between brackets
#'
#' @return where function in an R script with cursor between brackets
#'
#' @export
insert_where <- function() {
  insert(where)
  reposition(1)
}

##########  insert ggplot ##########
#' ggplot and aes functions in an R script with cursor between bracket of aes
#'
#' @return ggplot and aes functions in an R script with cursor between brackets
#'
#' @export
insert_ggplot <- function() {
  insertmulti(ggplot, aes)
  reposition(2)
  if (!pipe_toggle()$pipe == "+") {
    invisible(toggle_pipe())
  }
}
