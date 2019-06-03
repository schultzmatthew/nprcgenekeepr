#' obfucateId creates a vector of ID aliases of specified length
#'
#' ID aliases are psuedorandom sequences of alphanumeric upper case characters.
#' User has the option of providing a character vector of aliases to avoid using.
#'
#' @return named character vector of aliases where the name is the original ID value.
#'
#' @param id character vector of IDs to be obfuscated (alias creation).
#' @param size character length of each alias
#' @param existingIds character vector of existing aliases to avoid duplication.
#' @importFrom stringi stri_c
#' @export
obfuscateId <- function(id, size = 10, existingIds = character(0)) {
  existingIds <- c(character(length(id)), existingIds)
  obfuscatedId <- character(length(id))
  for (i in seq_along(id)) {
    counter <- 0
    repeat {
      obfuscatedId[i] <- stri_c(sample(c(LETTERS, stri_c(0:9)), size = size, replace = TRUE), collapse = "")
      if (!any(obfuscatedId[i] %in% existingIds))
        break
      counter <- counter + 1
      if (counter > 100)
        stop("Character length of alias IDs is too short to easily avoid duplicates")
    }
    existingIds[i] <- obfuscatedId[i]
  }
  names(obfuscatedId) <- id
  obfuscatedId
}
