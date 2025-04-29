\name{dceResponse}
\alias{dceResponse}
\alias{dceResponseSet}
\alias{dceResponseQ}

\title{Collect Responses to DCE Questions}

\description{
Collect user's responses to DCE questions.
}

\details{
Three types of dialog boxes are provided to collect user's responses to
DCE questions that are converted from the choice sets designed in 
the \sQuote{\code{Design Choice Sets for DCE}} dialog box.

The first dialog box (\sQuote{\code{Set Options for Response Collection}})
sets four options for response collections:

\itemize{
\item \code{Design}: An R object name containing DCE choice sets
generated in the \sQuote{\code{Design Choice Sets for DCE}} dialog box. 
The default name \sQuote{\code{DCEdesign}} is displayed automatically.
The correct name should be entered if the name of DCE choice sets 
(\code{DCEdesign}) was changed in the dialog box for DCE design.

\item \code{Opt-out option}: When the opt-out option will be used in
the DCE questions, check this box.

\item \code{Save to file}: When the user's responses should be saved
to an external file in CSV format (\code{.csv}), check this box.

\item \code{Block number}: When the DCE choice sets are divided into
two or more blocks, set the block number to which the user will be assigned.
}

After confirming all the settings on the first dialog box, press
the \sQuote{\code{OK}} button to continue.

A series of dialog boxes (\sQuote{\code{Collect Responses to DCE Questions}}) 
sequentially opens to collect the user's responses to DCE questions. 
In each dialog box, alternatives designed in the 
\sQuote{\code{Design Choice Sets for DCE}}
dialog box are presented. The user is asked to select their most preferred
alternative from the drop-down list box. After making the selection,
the user clicks the \sQuote{\code{OK}} button to open a new dialog box
corresponding to the next DCE question.

When all user responses to DCE questions have been collected, a dialog
box (\sQuote{\code{Save As}}) opens if the \sQuote{\code{Save to file}}
option was selected in the first dialog box. This saves the user's
responses to an external file in CSV format.
}
