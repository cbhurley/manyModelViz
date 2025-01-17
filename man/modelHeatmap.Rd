% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelHeatmap.R
\name{modelHeatmap}
\alias{modelHeatmap}
\title{Draws a heatmap for model comparison}
\usage{
modelHeatmap(
  d,
  x,
  y,
  z,
  xorder = "none",
  yorder = "none",
  zfunction = NULL,
  zlevels = NULL
)
}
\arguments{
\item{d}{A dataframe with fit information, or a list of fits, from which the data.frame is constructed using broom::tidy(car::Anova())}

\item{x}{The x variable}

\item{y}{The y variable}

\item{z}{Used for heatmap fill}

\item{xorder}{"none" means no ordering. Otherwise, use "increasing"/"decreasing" for ordering by mean z, or specify xorder as a vector of xlevels.}

\item{yorder}{"none" means no ordering. Otherwise, use "increasing"/"decreasing" for ordering by mean z, or specify xorder as a vector of xlevels.}

\item{zfunction}{If not NULL, applied to z values}

\item{zlevels}{If not NULL, specifies cut levels for the z variable}
}
\value{
a ggplot
}
\description{
Draws a heatmap for model comparison
}
\details{
If zlevels is present, then ordering is done by a weighted mean of the category levels.
}
\examples{
\donttest{
if (requireNamespace("rrr", quietly = TRUE)){
   fits <- purrr::map(rrr::tobacco[,1:3], ~ lm(.x ~ ., data=rrr::tobacco[,4:9]))
   cols <- c("blue", "cyan", "grey95")
   modelHeatmap(fits, "term", "response", "p.value", xorder="increasing", yorder="increasing")+
      ggplot2::scale_fill_manual(values = cols)
   modelHeatmap(fits, "term", "response", "p.value", xorder="increasing", yorder="increasing",
   zfunction=function(p) p.adjust(p, method = "BH"))+
      ggplot2::scale_fill_manual(values = cols)

   if (requireNamespace("ranger", quietly = TRUE)){
     rfs <- purrr::map(rrr::tobacco[,1:3], ~
         ranger::ranger(.x ~ ., data=rrr::tobacco[,4:9], importance="permutation",
                           xorder="decreasing", yorder="decreasing"))
     rfdf <- purrr::map_dfr(rfs, ~ {
     imp <- ranger::importance(.x)
      terms <- names(imp)
      names(imp) <- NULL
       data.frame( term=terms,importance=imp)
       }, .id="response")
     modelHeatmap(rfdf, "term", "response", "importance")
   }
}
}
}
