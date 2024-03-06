\name{dceDataset}
\alias{dceDataset}
\alias{dceDataP}

\title{Create Data Set for DCE Analysis}

\description{
Create a data set for analysis from choice sets for DCEs and data set
containing responses to DCE questions.
}

\details{
This dialog box uses the function \code{\link[support.CEs]{ce.dataset}} 
in \pkg{support.CEs} to create a data set for DCE analysis from the choice 
sets designed in the \sQuote{\code{Design Choice Sets for DCE}} dialog box
and a data set containing responses to DCE questions.
This dialog box is activated if a data frame is selected as the active 
data set. 
For details on the results of this dialog box, refer to help for 
the function \code{\link[support.CEs]{ce.dataset}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for data set}: An R object name into which a data set
generated using this dialog box is stored.
The default name \sQuote{\code{DCEdata}} can be changed following R's
rules for naming objects. If changed, you should take note of the new
object name, which will be used later.

\item \code{Save to file}: A check box for saving the resultant data set
to an external file in RDA format (\code{.rda}). 
If necessary, check this box.

\item \code{Design}: An R object name containing DCE choice sets generated
in the \sQuote{\code{Design Choice Sets for DCE}} dialog box. 
The default name \sQuote{\code{DCEdesign}} is displayed automatically.
The correct name should be entered if the name 
for design (\code{DCEdesign}) was changed in the dialog box for DCE design.

\item \code{Responses to DCE questions}: Names of variables containing
responses to DCE questions in the active data set. Variable names are
separated by commas (e.g., q1, q2, q3, ...).

\item \code{Opt-out option}: A check box for an opt-out option. 
When the opt-out option was used in the DCE questions, check this box.
}
}

