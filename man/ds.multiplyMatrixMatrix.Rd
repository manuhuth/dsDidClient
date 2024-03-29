% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.multiplyMatrixMatrix.R
\name{ds.multiplyMatrixMatrix}
\alias{ds.multiplyMatrixMatrix}
\title{Multiply Matrix Matrix}
\usage{
ds.multiplyMatrixMatrix(
  matrix1,
  matrix2,
  nrow1 = NULL,
  ncol1 = NULL,
  nrow2 = NULL,
  ncol2 = NULL,
  newobj = NULL,
  datasources = NULL
)
}
\arguments{
\item{matrix1}{The first matrix to be multiplied. Can be a string or a flattened matrix}

\item{matrix2}{The second matrix to be multiplied. Must be a string with the name of the matrix on the server side if the first matrix was a matrix object.}

\item{nrow1}{The number of rows in the first matrix. Only needed if the first matrix is a flattened matrix.}

\item{ncol1}{The number of columns in the first matrix. Only needed if the first matrix is a flattened matrix.}

\item{nrow2}{The number of rows in the second matrix. Only needed if the second matrix is a flattened matrix.}

\item{ncol2}{The number of columns in the second matrix. Only needed if the second matrix is a flattened matrix.}

\item{newobj}{The name of the new object to assign the result of the multiplication to on the server side.}

\item{datasources}{A specific Datashield data source to which the result should be assigned.}
}
\description{
The function is used to multiply two matrices and assign the result to a new object on the server side. One of the matrices can be a client side matrix.
}
