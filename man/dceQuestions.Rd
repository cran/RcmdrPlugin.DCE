\name{dceQuestions}
\alias{dceQuestions}

\title{Display DCE Questions}

\description{
Display a series of questions for DCEs.
}

\details{
This dialog box displays a series of DCE questions that are converted from
the choice sets designed in the \sQuote{\code{Design Choice Sets for DCE}} 
dialog box. This dialog box uses the function 
\code{\link[support.CEs]{questionnaire}} in \pkg{support.CEs}. 
For details on the results of this dialog box,
refer to help for \code{\link[support.CEs]{questionnaire}}.

GUI element of this dialog box is as follows.

\itemize{
\item \code{Design}: An R object name containing DCE choice sets
generated in the \sQuote{\code{Design Choice Sets for DCE}} dialog box. 
The default name \sQuote{\code{DCEdesign}} is displayed automatically.
The correct name should be entered if the name of DCE choice sets 
(\code{DCEdesign}) was changed in the dialog box for DCE design.
}
}

