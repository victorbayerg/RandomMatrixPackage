#' Create a random matrix
#'
#' This function creates a random matrix of specified dimensions.
#'
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @return A matrix with random numbers.
#' @examples
#' create_random_matrix(3, 3)
#' @export
create_random_matrix <- function(nrow, ncol) {
  matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
}

#' Shuffle a matrix and extract a random number
#'
#' This function shuffles the elements of a matrix and returns a random number.
#'
#' @param mat A matrix to be shuffled.
#' @return A random number from the shuffled matrix.
#' @examples
#' mat <- create_random_matrix(3, 3)
#' extract_random_number(mat)
#' @export
extract_random_number <- function(mat) {
  shuffled_indices <- sample(length(mat))
  shuffled_matrix <- mat[shuffled_indices]
  return(sample(shuffled_matrix, 1))
}

#' Permute a vector
#'
#' This function generates a random permutation of a given vector.
#'
#' @param vec A vector to be permuted.
#' @return A permuted vector.
#' @examples
#' permute_vector(c(1, 2, 3, 4, 5))
#' @export
permute_vector <- function(vec) {
  return(vec[sample(lenght(vec))])
}

#' Select random elements from a matrix
#'
#' This function selects a specified number of random elements from a given matrix.
#'
#' @param mat A matrix from which to select random elements.
#' @param num_elements Number of random elements to select.
#' @return A vector of randomly selected elements from the matrix.
#' @examples
#' mat <- create_random_matrix(3, 3)
#' select_random_elements(mat, 2)
#' @export
select_random_elements <- function(mat, num_elements) {
  if (num_elements > length(mat)) {
    stop("Number of elements to select is greater than the number of elements in the matrix.")
  }
  random_indices <- sample(length(mat), num_elements)
  return(mat[random_indices])
}

#' Generate a random walk matrix
#'
#' This function generates a random walk matrix based on a given starting point and step probabilities.
#' Random walks are essential in modeling processes where outcomes are influenced by probabilistic steps or transitions. This function allows for simulating such processes over a matrix structure.
#'
#' @param size Integer. Size of the square matrix (n x n).
#' @param start Integer. Starting point or initial state in the matrix (1-based index).
#' @param steps List of numeric vectors. Each vector represents probabilities of moving to the next state from each current state.
#'
#' @return A square matrix where each element represents the probability of transitioning from one state to another.
#'
#' @examples
#' # Generate a random walk matrix for a 3x3 matrix
#' walk_matrix <- random_walk_matrix(size = 3, start = 1, steps = list(c(0.3, 0.5, 0.2), c(0.4, 0.1, 0.5), c(0.2, 0.6, 0.2)))
#' print(walk_matrix)
#'
#' @export
random_walk_matrix <- function(size, start, steps) {
  # Initialize the matrix
  walk_matrix <- matrix(0, nrow = size, ncol = size)

  # Set initial state
  current_state <- start

  # Generate random walk probabilities
  for (i in 1:size) {
    walk_matrix[current_state, ] <- steps[[current_state]]
    current_state <- sample.int(size, 1, prob = steps[[current_state]])
  }

  return(walk_matrix)
}

#' RandomMatrixPackage: A Package for Creating and Manipulating Random Matrices
#'
#' This package provides functions for creating and manipulating random matrices.
#' It includes functions to create random matrices, shuffle matrices, extract random numbers,
#' permute vectors, select random elements from matrices, and generate random walk matrices.
#'
#' @name RandomMatrixPackage
#' @keywords internal
#' @section Functions:
#' \itemize{
#'   \item \code{\link{create_random_matrix}}: Create a random matrix.
#'   \item \code{\link{extract_random_number}}: Shuffle a matrix and extract a random number.
#'   \item \code{\link{permute_vector}}: Permute a vector.
#'   \item \code{\link{select_random_elements}}: Select random elements from a matrix.
#'   \item \code{\link{random_walk_matrix}}: Generate a random walk matrix.
#' }
#'
"_PACKAGE"
