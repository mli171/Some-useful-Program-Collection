# get the unique permutation index from a vector
#     1. complete permutation
#     2. within block permutation

library(vegan)

getPermuteMatrix <-
  function(perm, N,  strata = NULL)
  {
    ## 'perm' is either a single number, a how() structure or a
    ## permutation matrix
    if (length(perm) == 1) {
      perm <- how(nperm = perm)
    }
    ## apply 'strata', but only if possible: ignore silently other cases
    if (!missing(strata) && !is.null(strata)) {
      if (inherits(perm, "how") && is.null(getBlocks(perm)))
        setBlocks(perm) <- strata
    }
    ## now 'perm' is either a how() or a matrix
    if (inherits(perm, "how"))
      perm <- shuffleSet(N, control = perm)
    else { # matrix: check that it *strictly* integer
      if(!is.integer(perm) && !all(perm == round(perm)))
        stop("permutation matrix must be strictly integers: use round()")
    }
    ## now 'perm' is a matrix (or always was). If it is a plain
    ## matrix, set minimal attributes for printing. This is a dirty
    ## kluge: should be handled more cleanly.
    if (is.null(attr(perm, "control")))
      attr(perm, "control") <-
        structure(list(within=list(type="supplied matrix"),
                       nperm = nrow(perm)), class = "how")
    perm
  }

# 1. complete permutation
perm.id = as.matrix(getPermuteMatrix(how(nperm = 100), 1:12))

# 2. within block (4 elements each block) permutation
perm.id = as.matrix(getPermuteMatrix(how(within=Within("free"), blocks=factor(rep(1:3, each=4)), nperm = 100), 1:12))
