\docType{package}
\name{RcmdrPlugin.DCE-package}
\alias{RcmdrPlugin.DCE-package}
\alias{RcmdrPlugin.DCE}
\alias{dceLoad}

\title{R Commander Plug-in for Discrete Choice Experiments}

\description{
This package adds menu items for discrete choice experiments (DCEs) 
to the R Commander. DCE is a question-based survey method that 
designs various combinations (profiles) of attribute levels 
using the experimental designs, asks respondents to select 
the most preferred profile in each choice set, and then measures
preferences for the attribute levels by analyzing the responses. 
For details on DCEs, refer to Louviere et al. (2000). 
}

\details{
This package is an R Commander plug-in package for DCEs. 
It depends on \pkg{support.CEs} (Aizaki 2012), 
\pkg{survival} (Therneau 2020; Therneau and Grambsch 2000), and 
\pkg{Rcmdr} (Fox 2005, 2017; Fox and Bouchet-Valat 2020). 
Refer to Aizaki (2012), Aizaki et al. (2014), and 
\sQuote{Non-Market Valuation with R} <\url{https://nmvr.skr.jp/}> 
for a brief introduction to DCEs in R.
A manual for this package is available at <\url{https://sites.google.com/view/r4sp/rcmdrplugin}>.

After successfully installing and loading \pkg{RcmdrPlugin.DCE}, 
the R Commander window will appear, and then you will find \sQuote{\code{DCE}} 
added to the top-level menus in the R Commander window.

Selecting \sQuote{\code{DCE}} displays menu items. When selecting a menu item, 
the corresponding dialog box opens. The following dialog boxes are 
provided by \pkg{RcmdrPlugin.DCE}:
\itemize{

   \item The dialog box for \sQuote{\code{Design choice sets...}} designs 
choice sets for DCE with the rotation design method or mix-and-match method 
using the function \code{\link[support.CEs]{rotation.design}} 
in \pkg{support.CEs}.

   \item The dialog box for \sQuote{\code{Display questions...}} 
shows DCE questions from the choice sets designed in the first dialog box 
using the function \code{\link[support.CEs]{questionnaire}} in 
\pkg{support.CEs}.

   \item The dialog box for \sQuote{\code{Collect responses...}} 
shows three types of dialog boxes to collect user's responses to
DCE questions according to choice sets designed in previous dialog box.

   \item The dialog box for \sQuote{\code{Create data set for analysis...}} 
creates a data set for DCE analysis from the choice sets designed in the first 
dialog box and a data set containing responses to DCE questions using 
the function \code{\link[support.CEs]{ce.dataset}} 
in \pkg{support.CEs}. The menu item is activated if a data frame is 
selected as the active data set.

   \item The dialog box for \sQuote{\code{Create interactions...}} creates 
interactions between attribute/level variables and covariates 
in the data set for DCE analysis. The menu item is activated 
if the data set for DCE analysis created in the previous dialog box is selected as 
the active data set.

   \item The dialog box for \sQuote{\code{Fit model...}} implements 
a conditional logit model analysis of the data set for analysis using 
the functions \code{\link[survival]{clogit}} in \pkg{survival} and 
\code{\link[support.CEs]{gofm}} in \pkg{support.CEs}. 
The menu item is activated if the data set for DCE analysis created in 
the previous dialog box is selected as the active data set.

   \item The dialog box for \sQuote{\code{Calculate MWTP...}} 
calculates marginal willingness to pays (MWTPs) of nonmonetary variables 
from the model fitted in the previous dialog box using the function 
\code{\link[support.CEs]{mwtp}} in \pkg{support.CEs}. The menu item 
is activated if the fitted model is selected as the active model.

   \item The dialog box for \sQuote{\code{Load saved objects...}} loads 
an R Data file saved in the dialog box for 
\sQuote{\code{Design choice sets...}} or 
\sQuote{\code{Create data set for analysis...}}
using the function \code{\link[base]{load}} in \pkg{base}. 
}
}

\section{Acknowledgments}{
This work was supported by JSPS KAKENHI Grant Number JP20K06251.
}

\author{
Hideo Aizaki
}

\references{
Aizaki H (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R.
\emph{Journal of Statistical Software}, 50(C2): 1--24.
\url{https://www.jstatsoft.org/article/view/v050c02}.

Aizaki H (2021) support.CEs: Basic Functions for Supporting an Implementation of Choice Experiments.
R package version 0.5-0. 
\url{https://CRAN.R-project.org/package=support.CEs}.

Aizaki H, Nakatani T, Sato K (2014) 
\emph{Stated Preference Methods Using R}. 
Chapman and Hall/CRC. 

Fox J (2005) The R Commander: A Basic Statistics Graphical User Interface to R. 
\emph{Journal of Statistical Software}, 14(9): 1--42. 
\url{https://www.jstatsoft.org/article/view/v014i09}.

Fox J (2017) \emph{Using the R Commander: A Point-and-Click Interface for R}.
Chapman and Hall/CRC. 

Fox J, Bouchet-Valat M (2020) Rcmdr: R Commander.
R package version 2.7-1.
\url{https://cran.r-project.org/package=Rcmdr}.

Louviere JJ, Hensher DA, Swait JD (2000) \emph{Stated Choice Methods: Analysis and Application}.
Cambridge University Press.

Therneau T (2020) survival: Survival Analysis.
R package version 3.2-7.
\url{https://CRAN.R-project.org/package=survival}.

Therneau TM, Grambsch PM (2000) \emph{Modeling Survival Data: Extending the Cox Model}.
Springer.
}

\examples{
if(interactive()) {
  library(RcmdrPlugin.DCE)
}
}

\keyword{
package
}

