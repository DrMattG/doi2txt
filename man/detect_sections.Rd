% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_sections.R
\name{detect_sections}
\alias{detect_sections}
\title{Detect all major section headers in plain text journal articles}
\usage{
detect_sections(text)
}
\arguments{
\item{text}{A character vector containing a scientific journal article in plain text format where each line represents one paragraph, section header, or other type of standalone text (e.g. a figure caption).}
}
\value{
A numeric vector of length 5 indicating the lines within the text that are the section headers for the introduction, methods, results, discussion, and literature cited sections, respectively.
}
\description{
Finds the lines in a plain text scientific journal article that correspond to the start of the introduction, methods, results, discussion, and references.
}
