#' RNA-binding proteins (RBPs) and a list of features obtained from publicly
#' available datasets.
#'
#' A dataset containing 1072 RBPs and other attributes to be used in the
#' logistic regression model.
#'
#' @source University of Toronto, Canada.
#'
#' @format A data frame with 1079 rows and 12 variables:
#' \describe{
#' \item{Human Genes}{1079 human RNA-binding proteins (RBPs) and their gene
#' names}
#' \item{hasCanonicalRBDs}{indicates whether the RBP has a canonical
#' RNA-binding domains (RBDs) or not, 1 if yes and 0 if not}
#' \item{Neuron Enrichment (Mouse)} {the average fold change of the gene's
#' expression in neurons versus non-neurons in mouse}
#' \item{Neuron Enrichment (Human)} {the average fold change of the gene's
#' expression in neurons versus non-neurons in human}
#' \item{Brain Enrichment} {the average fold change of the gene's
#' expression in brains versus non-brain regions}
#' \item{day3/day1_diff} {differential expression of the RBP between day 3
#' and day 1 during neuron differentiation}
#' \item{day7/day1_diff} {differential expression of the RBP between day 7
#' and day 1 during neuron differentiation}
#' \item{pLI} {the tolerance of the gene to the loss of function on the basis
#' of the number of protein truncating variants}
#' \item{medium_pLI} {1 if the gene's pLI score falls within the range 0.5-0.9,
#' 0 elsewise}
#' \item{high_pLI} {1 if the gene's pLI score falls within the range 0.9-0.99,
#' 0 elsewise}
#' \item{extreme_pLI} {1 if the gene's pLI score falls within the range 0.9-0.99,
#' 0 elsewise}
#' \item{autism_genes} {1 if the gene is associated with autism based on the
#' Simons Foundation Autism Research Initiate (SFARI) list, 0 elsewise}
#' }
"rbps"
