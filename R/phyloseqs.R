#' Replaces phyloseq::otu_table and returns a matrix instead
#'
#' @param physeq
#'
#' @return matrix of feature table
#' @export
otu_table2 <- function(physeq) {
  if(!(class(physeq) == "phyloseq"))
    stop("Must have phyloseq object as input!")
  phyloseq::otu_table(physeq)@.Data
}

#' Replaces phyloseq::sample_data and returns a data frame instead
#'
#' @param physeq
#'
#' @return data frame of metadata
#' @export
sample_data2 <- function(physeq) {
  if(!(class(physeq) == "phyloseq"))
    stop("Must have phyloseq object as input!")
  data.frame(phyloseq::sample_data(physeq),
             check.names = FALSE,
             stringsAsFactors = FALSE)
}

#' Replaces phyloseq::tax_table and returns a matrix instead
#'
#' @param physeq
#'
#' @return matrix of taxonamy table
#' @export
tax_table2 <- function(physeq) {
  if(!(class(physeq) == "phyloseq"))
    stop("Must have phyloseq object as input!")
  phyloseq::tax_table(physeq)@.Data
}

#' Create long format tibble object given phyloseq, aggregating otu table (must have),
#' metadata (optional), and tax table (optional) together.
#'
#' @param physeq
#'
#' @return a long tibble, each row corresponding to one feature and one sample
#' @importFrom magrittr "%>%"
#' @export
phyloseq_to_tb <- function(physeq) {
  if(!(class(physeq) == "phyloseq"))
    stop("Must have phyloseq object as input!")

  mat_otu <- otu_table2(physeq)
  tb <- t(mat_otu) %>%
    tibble::as_tibble(rownames = "rownames") %>%
    tidyr::gather(key = feature,
                  value = abundance,
                  -rownames)

  # If taxa table is present, add
  if(!is.null(phyloseq::access(physeq, "tax_table", errorIfNULL = FALSE))) {
    mat_tax <- tax_table2(physeq)
    if(any(colnames(tb) %in% colnames(mat_tax)))
      stop("There are overlapping column names, cannot proceed!")
    tb <- mat_tax %>%
      tibble::as_tibble(rownames = "feature") %>%
      dplyr::right_join(tb, by = "feature")
  }

  # If metadata is present, add
  if(!is.null(phyloseq::access(physeq, "sample_data", errorIfNULL = FALSE))) {
    df_metadata <- sample_data2(physeq)
    if(any(colnames(tb) %in% colnames(df_metadata)))
      stop("There are overlapping column names, cannot proceed!")
    tb <- df_metadata %>%
      tibble::as_tibble(rownames = "rownames") %>%
      dplyr::right_join(tb, by = "rownames")
  }

  return(tb)
}

#' Prune a phyloseq object to exclude empty taxa/samples, which
#' are prone to happen when a phyloseq object is subsetted.
#'
#' @param physeq
#' @param flist_taxa function for filtering taxa, default to non-empty pruning
#' @param flist_samples function for filtering sample, default to non-empty pruning
#' @param max.iter maximum number of iterations
#'
#' @return the pruned phyloseq object
#' @export
prune_taxaSamples <- function(physeq,
                              flist_taxa = kOverA2(k = 1, A = 0),
                              flist_samples = function(x) sum(x > 0) > 0,
                              max.iter = 3
) {
  i.iter <- 1
  taxa.ind <- apply(otu_table2(physeq), 1, flist_taxa)
  samples.ind <- apply(otu_table2(physeq), 2, flist_samples)
  if (phyloseq::ntaxa(physeq) != length(taxa.ind) |
      phyloseq::nsamples(physeq) != length(samples.ind)) {
    stop("Logic error in applying function(s). Logical result not same length as ntaxa(physeq)")
  }
  while(TRUE) {
    if(i.iter > max.iter)
      stop("Max iteration reached!")
    physeq <- phyloseq::prune_taxa(taxa.ind, physeq)
    samples.ind <- apply(otu_table2(physeq), 2, flist_samples)
    physeq <- phyloseq::prune_samples(samples.ind, physeq)
    taxa.ind <- apply(otu_table2(physeq), 1, flist_taxa)
    if(all(taxa.ind)) return(physeq)
    i.iter <- i.iter + 1
  }
}

#' Filtering utility to indicate if a feature is > A present in at least k samples
#'
#' @param k how many samples should the feature be present in?
#' @param A abundance threshold for the feature to be considered as present
#'
#' @return
kOverA2 <- function(k = 1, A = 0) {
  if(A >= 1) stop("Filtering is for relative abundance!")
  function(x) {
    if(any(is.na(x))) stop("Missing values in the data!")
    sum(x > A) >= k
  }
}
