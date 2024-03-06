\name{dceMwtp}
\alias{dceMwtp}

\title{Calculate Marginal Willingness to Pay}

\description{
Calculate marginal willingness to pay (MWTP) values of nonmonetary variables
from the fitted model.
}

\details{
This dialog box uses the function \code{\link[support.CEs]{mwtp}} in 
\pkg{support.CEs} to calculate MWTP values of nonmonetary variables 
from the model fitted in the \sQuote{\code{Fit Model to DCE Data}} 
dialog box.
This dialog box is activated if the fitted model is selected as 
the active model. For details on the results of this dialog
box, refer to help for the function \code{\link[support.CEs]{mwtp}}. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for output}: An R object name into which the calculated
results obtained through this dialog box are stored. The default name 
\sQuote{\code{MWTP}} can be changed following R's rules for naming objects. 

\item \code{Monetary variable}: A variable used as the monetary variable in
the calculation of MWTP(s). Select one from this drop-down list of variables.

\item \code{Nonmonetary variables}: Variables used as the nonmonetary
variable(s) in the calculation of MWTP(s). Select one or more variables
from this list box.

\item \code{Calculation method}: A method used to calculate confidence
intervals for MWTP(s). Select one from two radio buttons corresponding to
\sQuote{\code{Krinsky and Robb}} and \sQuote{\code{Delta}}. 
For the details on these methods, see help for \code{\link[support.CEs]{mwtp}}.

\item \code{Confidence level}: A confidence level used for calculating
the confidence intervals for MWTP(s). The default is \code{0.95}.

\item \code{Number of replications}: A total number of replications for
the Krinsky and Robb method.

\item \code{Seed for random number generator (optional)}: An integer value
used as a seed for generating random numbers. If reproducibility is needed,
type an arbitrary value into this
box.
}
}

