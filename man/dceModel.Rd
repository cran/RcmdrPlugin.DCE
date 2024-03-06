\name{dceModel}
\alias{dceModel}
\alias{resetDceModel}
\alias{clogitP}

\title{Fit Model to DCE Data}

\description{
Fit a model to DCE data.
}

\details{
This dialog box fits a conditional logit model to the data set for DCE
analysis using the functions \code{\link[survival]{clogit}} in \pkg{survival}
and \code{\link[support.CEs]{gofm}} in \pkg{support.CEs}. 
This dialog box is activated if the DCE data set created in the 
\sQuote{\code{Create Data Set for DCE Analysis}} dialog box is selected 
as the active data set.
For details on the results of this dialog box, refer to helps for
\code{\link[survival]{clogit}} and \code{\link[support.CEs]{gofm}}. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for model}: An R object name into which the results of
a fitted model obtained using this dialog box are stored. 
The default name \sQuote{\code{DCEmodel.x}}, where \code{x} indicates
the integer number assigned automatically from \code{1}, can be changed
following R's rules for naming objects.

\item \code{Response variable}: A variable containing responses to DCE
questions in the active data set. This variable name is fixed as \code{RES},
which cannot be changed by users (A fixed variable is colored in green).

\item \code{Independent variables}: Variables used as independent variables
in the model. Select one or more variables from this list box.

\item \code{Stratification variable}: A variable for identifying each
combination of respondents and DCE questions. This variable name is fixed
as \code{STR}.

\item \code{Subset expression}: A logical expression for specifying
a subset of the observations in the active dataset to be used for
the fitting.
}
}

