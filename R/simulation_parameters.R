#' Create simulation parameter dataframe
#'
#' This function creates a dataframe for \code{\link{metasims}} where each row represents a different set of simulataion parameters.
#'
#' @param k Vector of the different number of studies to simulate over.
#' @inheritParams metasim
#'
#'
#' @export

simulation_parameters <- function(k = c(3, 7, 20, 50),
                                  sample_lb = 20,
                                  sample_ub = 200,
                                  sample_p = 0.3,
                                  between_study_var = c(0, 0.4),
                                  within_study_var = c(0, 0.2),
                                  distribution = c("norm", "exp"),
                                  parameters = list(list(mean = 5, sd = 0.2), rate = 3),
                                  median_ratio = c(1, 1.2)) {
  # list of distributions
  tibble::tibble(distribution = distribution,
                 parameters = parameters) %>%
    dplyr::mutate(distn = purrr::map2(distribution, parameters, function(x, y) {
      list(distribution = x, parameters = y)
    })) %>% dplyr::pull(distn) %>% {
      list(
        distn = .,
        k = k,
        between_study_var = between_study_var,
        within_study_var = within_study_var,
        median_ratio = median_ratio
      )
    } %>% purrr::cross_df() %>%
    dplyr::mutate(
      distribution = purrr::map_chr(distn, 1),
      parameters = purrr::map(distn, 2),
      sample_sizes = purrr::map(
        k,
        simulate_meta_n,
        sample_lb = sample_lb,
        sample_ub = sample_ub,
        sample_p = sample_p
     )
  )
}
