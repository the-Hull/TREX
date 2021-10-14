#' Predict allometric parameters from DBH
#'
#' @param input_dbh_cm numeric, single value or vector of DBH measurements in cm
#' @param parameter character, one or multiple of \code{"Bark_cm"}, \code{"Sap_cm"}, \code{"Saparea_cm2"}
#' @param species character, one of the species given in \code{\link{allometry.data}}
#' @param ... additional arguments passed to \code{predict()}
#'
#' @return A data.frame containing predictions for each provided parameter and species
#' @export
#'
#' @examples
#'   predict_allometry(input_dbh_cm = 10:15)
#'
#'   predict_allometry(input_dbh_cm = 10:15,
#'     parameter = "Bark_cm",
#'     species   = c("Pinus banksiana",
#'                 "Betula alleghaniensis"))
predict_allometry <- function(input_dbh_cm,
                              parameter = c("Bark_cm", "Sap_cm", "Saparea_cm2"),
                              species = NULL,
                              ...){

    `%nin%` <-  Negate(`%in%`)

    denv <- new.env()
    utils::data("allometry.data", envir = denv)
    utils::data("allometry.models", envir = denv)

        allometry.data <- denv$allometry.data
    allometry.models <- denv$allometry.models

    rm(denv)

    if(any(parameter %nin% names(allometry.models))){
        stop("Please choose from the existing outputs: Bark_cm, Sap_cm, Saparea_cm2.")
    }

    if(!is.numeric(input_dbh_cm) |
       any(input_dbh_cm <= 0)){
        stop("Please provide a positive dbh in cm.")
    }

    if(!is.null(species) & any(species %nin% unique(allometry.data$Species))) {

        stop(sprintf("Please choose a species from %s"), unique(allometry.data$Species))

    }

    if(is.null(species)){
        species <- unique(allometry.data$Species)
    }

    # make named list for every parameter - setNames(parameter_predictions, parameter_names)
    predictions <- setNames(

        # parameter looping
        lapply(parameter,
                          function(parm){

                              # make named list for every species - setNames(species_predictions, species_names)
                              spec_preds <- setNames(
                              # species looping
                                  lapply(species,
                                         function(spec){

                                             preds <- predict(allometry.models[[parm]][['fit']][allometry.models[[parm]][['Species']] %in% spec][[1]],
                                                     newdat = data.frame(DBH_cm = input_dbh_cm),
                                                     ...)
                                             preds <- data.frame(value = preds)

                                         }),
                              species)
                              spec_preds <- dplyr::bind_rows(spec_preds, .id = "species")
                          }),
        parameter)

    predictions <- dplyr::bind_rows(predictions, .id = "parameter")
    rownames(predictions) <- NULL

    return(predictions)
}



