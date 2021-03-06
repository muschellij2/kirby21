% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_image_filenames.R
\name{get_image_filenames_df}
\alias{get_image_filenames_df}
\alias{get_image_filenames_list}
\alias{get_image_filenames_list_by_subject}
\alias{get_image_filenames_list_by_visit}
\alias{get_image_filenames_matrix}
\title{Get Image Filenames in a matrix}
\usage{
get_image_filenames_df(..., long = FALSE)

get_image_filenames_matrix(..., long = FALSE)

get_image_filenames_list(...)

get_image_filenames_list_by_visit(...)

get_image_filenames_list_by_subject(...)
}
\arguments{
\item{...}{arguments passed to \code{\link{get_image_filenames}}}

\item{long}{if \code{TRUE}, each row is a subject, visit, modality pair}
}
\description{
Return the filenames for the images
}

