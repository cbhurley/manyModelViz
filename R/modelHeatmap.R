

#' Draws a heatmap for model comparison
#'
#' @param d A dataframe with fit information, or a list of fits, from which the data.frame is constructed using broom::tidy(car::Anova())
#' @param x The x variable
#' @param y The y variable
#' @param z Used for heatmap fill
#' @param xorder "none" means no ordering. Otherwise, use "increasing"/"decreasing" for ordering by mean z, or specify xorder as a vector of xlevels.
#' @param yorder "none" means no ordering. Otherwise, use "increasing"/"decreasing" for ordering by mean z, or specify xorder as a vector of xlevels.
#' @param zfunction If not NULL, applied to z values
#' @param zlevels If not NULL, specifies cut levels for the z variable
#' @details If zlevels is present, then ordering is done by a weighted mean of the category levels.
#' @return a ggplot
#' @export
#'
#' @import ggplot2
#' @examples
#' \donttest{
#' if (requireNamespace("rrr", quietly = TRUE)){
#'    fits <- purrr::map(rrr::tobacco[,1:3], ~ lm(.x ~ ., data=rrr::tobacco[,4:9]))
#'    cols <- c("blue", "cyan", "grey95")
#'    modelHeatmap(fits, "term", "response", "p.value", xorder="increasing", yorder="increasing")+
#'       ggplot2::scale_fill_manual(values = cols)
#'    modelHeatmap(fits, "term", "response", "p.value", xorder="increasing", yorder="increasing",
#'    zfunction=function(p) p.adjust(p, method = "BH"))+
#'       ggplot2::scale_fill_manual(values = cols)
#'
#'    if (requireNamespace("ranger", quietly = TRUE)){
#'      rfs <- purrr::map(rrr::tobacco[,1:3], ~
#'          ranger::ranger(.x ~ ., data=rrr::tobacco[,4:9], importance="permutation",
#'                            xorder="decreasing", yorder="decreasing"))
#'      rfdf <- purrr::map_dfr(rfs, ~ {
#'      imp <- ranger::importance(.x)
#'       terms <- names(imp)
#'       names(imp) <- NULL
#'        data.frame( term=terms,importance=imp)
#'        }, .id="response")

#'      modelHeatmap(rfdf, "term", "response", "importance")
#'    }
#' }
#' }
#' @export
modelHeatmap <- function(d, x,y,z,
                         xorder= "none",
                         yorder= "none",
                         zfunction=NULL,
                         zlevels=NULL){

  if (z == "p.value" & is.null(zlevels)) zlevels <- c(0,0.01,0.05,1)
  if (!is.data.frame(d)){
    fits <- d
    d <- purrr::map_dfr(fits, ~ broom::tidy(car::Anova(.x)), .id="response")
    d <- dplyr::filter(d, .data$term != "Residuals")
    y <- "response"
  }

  if (!(x %in% names(d))) stop("Input x must be a variable in the data.frame")
  if (!(y %in% names(d))) stop("Input y must be a variable in the data.frame")
  if (!(z %in% names(d))) stop("Input z must be a variable in the data.frame")
  if (! is.null(zfunction)) d[[z]] <- zfunction(d[[z]])

  if (! is.null(zlevels))
    d[[z]] <- cut(d[[z]], breaks = zlevels)

  if (xorder== "increasing" | xorder == "decreasing"){
    if (is.numeric(d[[z]]))
      xord <- names(sort(tapply(d[[z]], d[[x]], mean,na.rm=TRUE)))
    else  xord <- names(sort(tapply(d[[z]], d[[x]], function(z1) {
      tab <- table(z1)
      weighted.mean(tab, 1:length(tab), na.rm=TRUE)
    })))

    if (xorder== "decreasing") xord <- rev(xord)
    xorder <- xord

  }

  if (yorder== "increasing" | yorder == "decreasing"){
    if (is.numeric(d[[z]]))
      yord <- names(sort(tapply(d[[z]], d[[y]], mean,na.rm=TRUE)))
    else  yord <- names(sort(tapply(d[[z]], d[[y]], function(z1) {
      tab <- table(z1)
      weighted.mean(tab, 1:length(tab), na.rm=TRUE)
    })))

    if (yorder== "decreasing") yord <- rev(yord)
    yorder <- yord
  }


  if (!identical(yorder, "none"))
    d[[y]] <-  factor(d[[y]], levels=yorder)
  else d[[y]] <-  factor(d[[y]])

  if (!identical(xorder, "none"))
    d[[x]] <-  factor(d[[x]], levels=xorder)
  else d[[x]] <-  factor(d[[x]])




  ggplot(data = d, aes(x=.data[[x]], y=.data[[y]])) +geom_tile(aes(fill=.data[[z]] ), color="grey50")+
    scale_x_discrete(position = "top") +scale_y_discrete(limits=rev)+
    theme(axis.text.x = element_text(angle = 30, hjust = 0)) + xlab("")+ ylab("")

}
