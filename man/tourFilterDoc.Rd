% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/tourTableServer.R
\name{tourFilterDoc}
\alias{tourFilterDoc}
\title{tourFilterDoc}
\usage{
tourFilterDoc(session, selectedId, dbPath = "tour.sqlite",
  dbTableName = "tourMembers", editorId = "tour_ace_editor")
}
\arguments{
\item{session}{current shiny session}

\item{selectedId}{id selected}

\item{dbPath}{path to sqlite db}

\item{dbTableName}{name of table containing members data}

\item{editorID}{outputID of the editor as defined in tourEditor()}
}
\description{
Given an member id, update editor with "doc" field value
}

