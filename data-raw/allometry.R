allometry.data

allometry.data <- read.table("./data-raw/sapwood_allometry.csv",
                       sep = ",",
                       header = TRUE,
                       stringsAsFactors = FALSE)
usethis::use_data(allometry.data)


library(dplyr)
library(tidyr)
library(broom)
library(purrr)
# make_models <- function(allo, parameter){
#
#     formula <- as.formula(sprintf("%s ~ alpha * DBH_cm ^beta", parameter))
#
#     allo %>%
#         dplyr::group_by(Species) %>%
#         tidyr::nest() %>%
#         dplyr::mutate(fit = purrr::map(data, ~ stats::nls(formula = formula,
#                                                           data = .,
#                                                           start = list(alpha = 1, beta = 1))),
#                       model_parameters = purrr::map(fit, broom::tidy)) %>%
#         tidyr::unnest(model_parameters)
#
# }
#
# options(dplyr.print_max = 1e9)
#
# purrr::map(c("Saparea_cm2", "Bark_cm", "Sap_cm"),
#            ~make_models(allo = allometry.data, parameter = .x))

# area
fits_AS <- allometry.data %>%
    dplyr::group_by(Species) %>%
    tidyr::nest() %>%
    dplyr::mutate(fit = purrr::map(data, ~stats::nls(Saparea_cm2 ~ alpha * DBH_cm ^beta,
                                                      data = .x,
                                                      start = list(alpha = 1, beta = 1))),
                  model_parameters = purrr::map(fit, broom::tidy))
# sapwood thickness
fits_DS <- allometry.data %>%
    dplyr::group_by(Species) %>%
    tidyr::nest() %>%
    dplyr::mutate(fit = purrr::map(data,  ~stats::lm(Sap_cm ~ DBH_cm,
                                      data = .x)),
           model_parameters = purrr::map(fit, broom::tidy))
# bark thickness
fits_DB <- allometry.data %>%
    dplyr::group_by(Species) %>%
    tidyr::nest() %>%
    dplyr::mutate(fit = purrr::map(data,  ~lm(Bark_cm ~ DBH_cm,
                                      data = .x)),
           model_parameters = purrr::map(fit, broom::tidy))


allometry.models <- list(Saparea_cm2 = fits_AS,
                          Sap_cm = fits_DS,
                          Bark_cm = fits_DB)



usethis::use_data(allometry.models)

