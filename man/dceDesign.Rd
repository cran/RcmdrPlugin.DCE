\name{dceDesign}
\alias{dceDesign}
\alias{resetDceTable}

\title{Design Choice Sets for DCE}

\description{
Design choice sets for DCEs.
}

\details{
This dialog box uses the function \code{\link[support.CEs]{rotation.design}} 
in package \pkg{support.CEs} to design choice sets for DCE with 
the rotation design method or mix-and-match method. 
For details on the results of this dialog box, refer to help for the
function \code{\link[support.CEs]{rotation.design}}. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for design}: An R object name into which choice sets 
generated using this dialog box are stored. The default name 
\sQuote{\code{DCEdesign}} can be changed following R's rules for naming
objects. If changed, you should take note of the new object name, 
which will be used later.

\item \code{Save to file}: A check box used to save the resultant design
to an external file in RDA format (\code{.rda}). 
If necessary, check this box.

\item \code{Design method}: A method used to generate DCE choice sets. 
Select one from two radio buttons corresponding to \sQuote{\code{Rotation}} 
and \sQuote{\code{Mix-and-Match}}. For the details on these methods, 
see help for function \code{\link[support.CEs]{rotation.design}}.

\item \code{Number of alternatives per set (without opt-out)}: The number
of alternatives per choice set (question), where an opt-out option is not
counted as this number even if the opt-out option will be used in your 
survey.

\item \code{Number of blocks}: The number of blocks into which the resultant
DCE choice sets are divided.

\item \code{Quantitative}: A check box showing that a quantitative attribute
is set in the corresponding row.

\item \code{Attribute}: An attribute variable name following R's rules for
naming variables. Type names without skipping from the first row.

\item \code{Level 1} to \code{Level 6}: Level variable names corresponding
to the attribute entered in the first column. These variable names must 
also follow R's rules for naming variables. Type names without skipping 
from the \sQuote{\code{Level 1}}.

\item \code{Seed for random number generator (optional)}: An integer value
used as a seed for generating random numbers. If reproducibility is needed,
type an arbitrary value into this box.

\item \code{Reproduce choice sets designed with R < 3.6.0}: A check box 
for reproducing DCE choice sets generated with R versions older than 3.6.0.
If necessary, check this box.
}
}


