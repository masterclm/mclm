#' Learnr question wrappers with desired defaults
#' 
#' These are wrappers for [learnr::question()] and related functions that
#' always allow retries, give answers in a random order, and use
#' [learnr::random_praise()] and encouragement for the reactions.
#' To be used in tutorials.
#'
#' @param ... A question and a series of [learnr::answer()].
#' @param q A question.
#' @param a The correct answer.
#'
#' @return A [learnr::question()] with desired defaults.
#' @export
#' @keywords internal
mclm_question <- function(...){
  learnr::question(...,
           allow_retry = TRUE,
           random_answer_order = TRUE,
           correct = learnr::random_praise(),
           incorrect = learnr::random_encouragement()
  )
} 

#' @rdname mclm_question
#' @export
mclm_question_num <- function(q, a, min = 5000, step = 100, tolerance = 0.001) {
  learnr::question_numeric(q,
                           learnr::answer(a, correct = TRUE),
                           allow_retry = TRUE,
                           correct = learnr::random_praise(),
                           incorrect = learnr::random_encouragement(),
                           min = min,
                           max = 900000,
                           step = step,
                           tolerance = tolerance)
}

#' @rdname mclm_question
#' @export
mclm_question_text <- function(...) {
  learnr::question_text(...,
                allow_retry = TRUE,
                correct = learnr::random_praise(),
                incorrect = learnr::random_encouragement())
}
