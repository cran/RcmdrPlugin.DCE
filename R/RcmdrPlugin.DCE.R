###############################################################################

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

###############################################################################

dceDesign <- function() {
  initializeDialog(title = gettextRcmdr("Design Choice Sets for DCE"))
  defaults <- list(
    designName = "DCEdesign",
    designMethod = "FALSE",
    nAlternativesName = "2",
    nBlocksName = "1",
    RNGseedName = "",
    RNGoptionVariable = "0",
    A1Var = "0",
    A2Var = "0",
    A3Var = "0",
    A4Var = "0",
    A5Var = "0",
    A6Var = "0")
  dialog.values <- getDialog("dceDesign", defaults)

  
  ##### Output Frame #####
  outputFrame <- tkframe(top)

  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design     <- ttkentry(outputFrame, width = "20", 
                         textvariable = designName)

  ##### Input Frame #####
  inputsFrame       <- tkframe(top)
  designMethodFrame <- tkframe(inputsFrame)
  AltBlkRngFrame    <- tkframe(inputsFrame)
  RNGoptionFrame    <- tkframe(inputsFrame)
  TABLEFrame        <- tkframe(inputsFrame)
  tableFrame        <- tkframe(TABLEFrame)
  rightFrame        <- tkframe(TABLEFrame)
  AttrCheckBoxFrame <- tkframe(rightFrame)
  
  # Design method
  radioButtons(
    designMethodFrame,
    name = "designmethod",
    buttons = c("FALSE", "TRUE"),
    labels = gettextRcmdr(c("Rotation", "Mix-and-Match")),
    initialValue = dialog.values$designMethod,
    title = gettextRcmdr("Design method"))

  # Number of alternatives per set (without the output option)
  nAlternativesName <- tclVar(dialog.values$nAlternativesName)
  nAlternatives     <- ttkentry(AltBlkRngFrame,
                                width = "10",
                                textvariable = nAlternativesName)
  
  # Number of blocks
  nBlocksName <- tclVar(dialog.values$nBlocksName)
  nBlocks     <- ttkentry(AltBlkRngFrame,
                          width = "10",
                          textvariable = nBlocksName)

  # Seed for RNG
  RNGseedName <- tclVar(dialog.values$RNGseedName)
  RNGseed     <- ttkentry(AltBlkRngFrame,
                          width = "10",
                          textvariable = RNGseedName)

  # RNG option
  RNGoptionVariable <- tclVar(dialog.values$RNGoptionVariable)
  RNGoptionCheckBox <- ttkcheckbutton(RNGoptionFrame,
                                      variable = RNGoptionVariable)

  # Table for alternatives and levels
  ## Initial settings
  env <- environment()
  assign(".tableFrame", tkframe(tableFrame), envir = env)
  tkdestroy(get(".tableFrame", envir = env))
  assign(".tableFrame", tkframe(tableFrame), envir = env)
  nrows <- 6
  ncols <- 7

  initial.table <- getRcmdr("savedTable")
  
  ## Names of columns
  make.col.names <- "labelRcmdr(.tableFrame, text='')"
  col.varname <- c("Attribute", "Level 1", "Level 2", "Level 3",
                                "Level 4", "Level 5", "Level 6")
  for (j in 1:ncols) {
    make.col.names <- 
      paste(make.col.names, ", ",
            "labelRcmdr(.tableFrame, text = '", col.varname[j], "')",
            sep = "")
  }
  eval(parse(text=paste("tkgrid(", make.col.names, ", sticky = 'w')", 
                        sep = "")), envir = env)

  ## Names of rows and cells in table
  for (i in 1:nrows) {
    varname <- paste(".tab.", i, ".1", sep = "")
    assign(varname, if (is.null(initial.table)) {
                      tclVar("")
                    } else {
                      tclVar(initial.table[i, 1])
                    }, envir = env)
    row.varname <- paste(".rowname.", i, sep = "")
    make.row <- paste("labelRcmdr(.tableFrame, text ='')")
    make.row <- paste(make.row, ", ",
                      "ttkentry(.tableFrame, width = '15', textvariable =", 
                      varname, ")", sep="")
    for (j in 2:ncols) {
      varname <- paste(".tab.", i, ".", j, sep = "")
      assign(varname, if (is.null(initial.table)) {
                        tclVar("")
                      } else {
                        tclVar(initial.table[i, j])
                      }, envir = env)
      make.row <- paste(make.row, ", ",
                        "ttkentry(.tableFrame, width = '10', textvariable =", 
                        varname, ")", sep="")
    }
    eval(parse(text = paste("tkgrid(", make.row, ")", sep = "")), envir = env)
  }
  tkgrid(get(".tableFrame", envir = env), sticky = "w")

  # Quantitative attributes
  A1Var <- tclVar(dialog.values$A1Var)
  A1CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A1Var)
  A2Var <- tclVar(dialog.values$A2Var)
  A2CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A2Var)
  A3Var <- tclVar(dialog.values$A3Var)
  A3CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A3Var)
  A4Var <- tclVar(dialog.values$A4Var)
  A4CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A4Var)
  A5Var <- tclVar(dialog.values$A5Var)
  A5CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A5Var)
  A6Var <- tclVar(dialog.values$A6Var)
  A6CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A6Var)
  
  
  ##### onOK Function #####
  onOK <- function() {

    putDialog("dceDesign", list(
      designName = tclvalue(designName),
      designMethod = tclvalue(designmethodVariable),
      nAlternativesName = tclvalue(nAlternativesName),
      nBlocksName = tclvalue(nBlocksName),
      RNGseedName = tclvalue(RNGseedName),
      RNGoptionVariable = tclvalue(RNGoptionVariable),
      A1Var = tclvalue(A1Var),
      A2Var = tclvalue(A2Var),
      A3Var = tclvalue(A3Var),
      A4Var = tclvalue(A4Var),
      A5Var = tclvalue(A5Var),
      A6Var = tclvalue(A6Var)))
    
    closeDialog()

    # Table of attributes and levels
    nrows <- 6
    ncols <- 7
    varNames <- matrix("", nrow = nrows, ncol = ncols)

    for (i in 1:nrows) {
      for (j in 1:ncols) {
        varname <- paste(".tab.", i, ".", j, sep = "")
        varNames[i, j] <- 
          eval(parse(text =
            paste("as.character(tclvalue(", varname, "))", sep = "")))
      }
    }

    # Store the table of attributes and levels into savedTable 
    putRcmdr("savedTable", varNames) 
    
    # Variables for attributes and levels
    attributeNames <- varNames[, 1]
    varidRows      <- which(attributeNames != "")
    nrows          <- length(varidRows)
    attributeNames <- attributeNames[varidRows]
    levelNames     <- varNames[varidRows, -1]

    attribute.names.list <- vector("list", nrows)

    for (i in 1:nrows) {
      levelnames <- levelNames[i, ]
      levelnames <- levelnames[levelnames != ""]
      attribute.names.list[[i]] <- levelnames
    }

    # Code for argument 'attribute.names'
    cmd.attributes <- paste("list(", attributeNames[1], " = ",
                            attribute.names.list[1], sep = "")
    for (i in 2:nrows) {
      cmd.attributes <- paste(cmd.attributes, ", ", attributeNames[i], " = ",
                              attribute.names.list[i], sep = "")
    }
    cmd.attributes <- paste(cmd.attributes, ")", sep = "")
    
    # Code for argument 'seed'
    if (is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste(", seed = NULL)", sep = "")
    } else {
      cmd.seed <- paste(", seed = ",  as.numeric(tclvalue(RNGseedName)), 
                        ")", sep = "")
    }

    # Code for attributes 'categorical attributes' and 'continuous attributes'
    ind <- as.logical(c(as.numeric(tclvalue(A1Var)),
                        as.numeric(tclvalue(A2Var)),
                        as.numeric(tclvalue(A3Var)),
                        as.numeric(tclvalue(A4Var)),
                        as.numeric(tclvalue(A5Var)),
                        as.numeric(tclvalue(A6Var))))

    if (all(ind)) { # all attributes are continuous
      cmd.cateA <- paste("''")
      cmd.contA <- paste("c('", 
                         paste(na.omit(attributeNames[ind]), collapse = "', '"),
                         "')", sep = "")
    } else if (all(!ind)){ # all attributes are categorical
      cmd.cateA <- paste("c('", 
                         paste(na.omit(attributeNames[!ind]), collapse = "', '"),
                         "')", sep = "")
      cmd.contA <- paste("''")
    } else { # some attributes are continuous, while the others are categorical
      cmd.contA <- paste("c('", 
                         paste(na.omit(attributeNames[ind]), collapse = "', '"),
                         "')", sep = "")
      cmd.cateA <- paste("c('",
                         paste(na.omit(attributeNames[!ind]), collapse = "', '"),
                         "')", sep = "")
    }
    
    # Reproduce choice sets designed on R < 3.6.0
    if (tclvalue(RNGoptionVariable) == 1) {
      doItAndPrint(paste(
        'if(getRversion() >= "3.6.0") RNGkind(sample.kind = "Rounding")'))
    }
    
    # Design choice sets
    doItAndPrint(
      paste(tclvalue(designName), " <- rotation.design(",
            "attribute.names = ", cmd.attributes,
            ", nalternatives = ", tclvalue(nAlternativesName),
            ", nblocks = ", tclvalue(nBlocksName),
            ", randomize = ", tclvalue(designmethodVariable),
            cmd.seed, sep = ""))
    doItAndPrint(
      paste("attributes(", tclvalue(designName), ")$contA <- ", cmd.contA,
            sep = ""))
    doItAndPrint(
      paste("attributes(", tclvalue(designName), ")$cateA <- ", cmd.cateA,
            sep = ""))
    doItAndPrint(paste(tclvalue(designName)))
    
    tkfocus(CommanderWindow())
  }

  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "rotation.design",
               reset       = "resetDceTable",
               apply       = "dceDesign")
  # Output
  tkgrid(
    labelRcmdr(outputFrame,
               text = gettextRcmdr("Name for design ")),
    design, sticky = "w")
  tkgrid(outputFrame, sticky = "w")

  # Blank line
  tkgrid(labelRcmdr(top, text = ""))

  # Input
  ## Design Method
  tkgrid(designmethodFrame, sticky = "w")
  tkgrid(designMethodFrame, sticky = "w")

  ## Number of alternatives per set
  tkgrid(
    labelRcmdr(
      AltBlkRngFrame,
      text = gettextRcmdr("Number of alternatives per set (without opt-out) ")),
    nAlternatives, sticky = "w")

  ## Number of blocks
  tkgrid(labelRcmdr(AltBlkRngFrame,
                    text = gettextRcmdr("Number of blocks ")),
         nBlocks, sticky = "w")

  ## Seed for RNG
  tkgrid(labelRcmdr(
           AltBlkRngFrame,
           text = gettextRcmdr("Seed for random number generator (optional) ")),
         RNGseed, sticky = "w")
  tkgrid(AltBlkRngFrame, sticky = "w")

  ## RNG option
  tkgrid(
    RNGoptionCheckBox,
      labelRcmdr(
        RNGoptionFrame,
        text = gettextRcmdr("Reproduce choice sets designed with R < 3.6.0")),
    sticky = "w")
  tkgrid(RNGoptionFrame, sticky = "w")
  
  ## Table
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Attributes and their levels:")),
    sticky = "w")

  ## Quantitative attributes
  tkgrid(labelRcmdr(AttrCheckBoxFrame, text = "Quantitative"), sticky = "ew")
  tkgrid(A1CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(A2CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(A3CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(A4CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(A5CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(A6CheckBox, labelRcmdr(AttrCheckBoxFrame, text = ""), sticky = "ew")
  tkgrid(AttrCheckBoxFrame, sticky = "ew")

  tkgrid(rightFrame, tableFrame, sticky = "ew")
  tkgrid(TABLEFrame, sticky = "ew")
    
  tkgrid(inputsFrame, sticky = "w")

  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetDceTable <- function() {
  putRcmdr("savedTable", NULL)
  putDialog("dceDesign", NULL)
  dceDesign()
}

###############################################################################

dceQuestions <- function() {
  initializeDialog(title = gettextRcmdr("Create DCE Questions"))
  defaults <- list(designName = "DCEdesign")
  dialog.values <- getDialog("dceQuestions", defaults)
  
  
  ##### Input Frame #####
  inputsFrame <- tkframe(top)

  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design <- ttkentry(inputsFrame, width = "20",
                     textvariable = designName)

  ##### onOK Function #####
  onOK <- function() {
    putDialog("dceQuestions", list(designName = tclvalue(designName)))

    designValue <- tclvalue(designName)

    closeDialog()

    doItAndPrint(paste("questionnaire(choice.experiment.design = ",
                       designValue, ", common = NULL, quote = TRUE)", 
                       sep = ""))
    tkfocus(CommanderWindow())
  }

  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "questionnaire",
               reset       = "dceQuestions",
               apply       = NULL)

  # Name of design
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Name of design ")),
    design, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################

dceDesignMatrix <- function() {
  initializeDialog(title = gettextRcmdr("Create Design Matrix"))
  defaults <- list(
    designmatrixName = "DCEdesign.matrix",
    designName       = "DCEdesign",
    optoutVariable   = "0",
    saveVariable     = "0")
  dialog.values <- getDialog("dceDesignMatrix", defaults)
  
  env <- environment()
  
  
  ##### Output Frame #####
  outputFrame <- tkframe(top)
  
  designmatrixName <- tclVar(dialog.values$designmatrixName)
  designmatrix     <- ttkentry(outputFrame, width = "20", 
                               textvariable = designmatrixName)
  
  
  ##### Input Frame #####
  inputsFrame <- tkframe(top)
  designFrame <- tkframe(inputsFrame)
  optoutFrame <- tkframe(inputsFrame)
  saveFrame   <- tkframe(inputsFrame)
  
  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design <- ttkentry(designFrame, width = "20",
                     textvariable = designName)

  # Opt-out option
  optoutVariable <- tclVar(dialog.values$optoutVariable)
  optoutCheckBox <- ttkcheckbutton(optoutFrame, variable = optoutVariable)
  
  # Save option
  saveVariable <- tclVar(dialog.values$saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)
  
  
  ##### onOK function #####
  onOK <- function() {
    
    putDialog("dceDesignMatrix", list(
      designmatrixName = tclvalue(designmatrixName),
      designName       = tclvalue(designName),
      optoutVariable   = tclvalue(optoutVariable),
      saveVariable     = tclvalue(saveVariable)))
    
    cedes <- tclvalue(designName)

    contA <- eval(parse(text = paste("attr(", cedes, ", 'contA')", sep = "")))
    cateA <- eval(parse(text = paste("attr(", cedes, ", 'cateA')", sep = "")))

    closeDialog()
    
    doItAndPrint(
      paste(
        tclvalue(designmatrixName), " <- make.design.matrix(",
        "choice.experiment.design = ", paste(cedes), 
        ", optout = ", tclvalue(optoutVariable), 
        ", categorical.attributes = c('", paste(cateA, collapse = "', '"), "')",
        ", continuous.attributes  = c('", paste(contA, collapse = "', '"), "')",
        ", unlabeled = TRUE, common = NULL, binary = FALSE)", sep = ""))

    if(tclvalue(saveVariable) == 1){
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          '{"All Files" {"*"}} {"CSV Files" {".csv" ".CSV"}}'),
        defaultextension = ".csv",
        initialfile = paste(tclvalue(designmatrixName), ".csv", sep=""),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      command <- paste("write.table(", tclvalue(designmatrixName), 
                       ', "', saveFile, 
                       '", sep = ",", col.names = TRUE,', 
                       'row.names = TRUE, quote= TRUE)',
                       sep = "")
      justDoIt(command)
      logger(command)
      Message(paste(gettextRcmdr("DCE design matrix exported to file"),
                    saveFile),
              type = "note")
    }
    tkfocus(CommanderWindow())
  }
  
  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "make.design.matrix",
               reset       = "dceDesignMatrix",
               apply       = "dceDesignMatrix")
  
  # Output
  tkgrid(
    labelRcmdr(outputFrame,
               text = gettextRcmdr("Name for design matrix ")),
    designmatrix, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  
  # Blank line
  tkgrid(labelRcmdr(top, text = ""))
  
  # Input
  ## Choice sets
  tkgrid(labelRcmdr(
    designFrame,
    text = gettextRcmdr("Name of design ")),
    design, sticky = "w")
  tkgrid(designFrame, sticky = "w")
  
  ## Opu-out option
  tkgrid(optoutCheckBox,
         labelRcmdr(optoutFrame, text = gettextRcmdr("Use opt-out option")),
         sticky = "w")
  tkgrid(optoutFrame, sticky = "w")

  ## Save as CSV file
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame, text = gettextRcmdr("Save as CSV file")),
         sticky = "w")
  tkgrid(saveFrame, sticky = "w")
  
  tkgrid(inputsFrame, sticky = "w")  

  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
  
}

###############################################################################

loadDceDesignMatrix <- function() {
  initializeDialog(title = gettextRcmdr("Import Design Matrix"))
  defaults <- list(designmatrixName = "DCEdesign.matrix")
  dialog.values <- getDialog("dceDesignMatrix", defaults)
  
  env <- environment()
  
  
  ##### Output Frame #####
  outputFrame <- tkframe(top)
  
  designmatrixName <- tclVar(dialog.values$designmatrixName)
  designmatrix     <- ttkentry(outputFrame, width = "20", 
                               textvariable = designmatrixName)
  
  
  ##### onOK function #####
  onOK <- function() {
    
    putDialog("dceDesignMatrix", list(
      designmatrixName = tclvalue(designmatrixName)))
    
    closeDialog()
    
    file <- tclvalue(tkgetOpenFile(filetypes = 
              gettextRcmdr(' {"CSV Files" {".csv"}} {"All Files" {"*"}}')))

    if (file == "") {
      return()
    }

    setBusyCursor()
    on.exit(setIdleCursor())
    
    doItAndPrint(paste0(tclvalue(designmatrixName), ' <- read.table("', 
                         file, '", header = TRUE, sep = ",")'))
    
    tkfocus(CommanderWindow())
  }
  
  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "read.table",
               reset       = "loadDceDesignMatrix")
  
  # Output
  tkgrid(
    labelRcmdr(outputFrame,
               text = gettextRcmdr("Name for design matrix ")),
    designmatrix, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################

dceDataset <- function() {
  initializeDialog(title = gettextRcmdr("Create Data Set for Analysis"))
  defaults <- list(
    datasetName  = "DCEdata",
    designName   = "DCEdesign.matrix",
    responseName = "")
  dialog.values <- getDialog("dceDataset", defaults)
  
  
  ##### Output Frame #####
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)

  # output name
  datasetName <- tclVar(dialog.values$datasetName)
  dataset     <- ttkentry(datasetnameFrame, width = "20", 
                          textvariable = datasetName)
  

  ##### Input Frame #####
  inputsFrame <- tkframe(top)
  tableFrame  <- tkframe(inputsFrame)
  
  # choice sets
  designName <- tclVar(dialog.values$designName)
  design <- ttkentry(inputsFrame, width = "20",
                     textvariable = designName)
  
  # response variables
  responseName <- tclVar(dialog.values$responseName)
  response <- ttkentry(inputsFrame, width = "40",
                         textvariable = responseName)
  

  ##### onOK function #####
  onOK <- function() {

    putDialog("dceDataset", list(
      datasetName   = tclvalue(datasetName),
      designName    = tclvalue(designName),
      responseName  = tclvalue(responseName)))
    
    closeDialog()
    responseVars <- unlist(strsplit(x = gsub(' +', '', tclvalue(responseName)),
                                    split = ","))

    doItAndPrint(
      paste(
        tclvalue(datasetName), " <- make.dataset(",
        "respondent.dataset = ", activeDataSet(), 
        ", design.matrix = ", tclvalue(designName), 
        ', choice.indicators = c("', 
          paste(responseVars, collapse = '", "'), '")',
        ", detail = FALSE)", sep = ""))

    justDoIt(paste0('class(', tclvalue(datasetName), 
                    ') <- c("dcedataset", "data.frame")'))
        
    tkfocus(CommanderWindow())
    
  }  
  
  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "make.dataset",
               reset       = "dceDataset",
               apply       = "dceDataset")
  
  # Output
  tkgrid(
    labelRcmdr(datasetnameFrame,
               text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(datasetnameFrame, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  
  # Blank line
  tkgrid(labelRcmdr(top, text = ""))
  
  # Input
  ## Design matrix
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Name of design matrix")),
    design, sticky = "w")

  ## Responses to DCE questions
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Responses to DCE questions ")),
    response, sticky = "w")
  
  tkgrid(inputsFrame, sticky = "w")  
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################

dceFitmodel <- function() {
  initializeDialog(title = gettextRcmdr("Fit Conditional Logit Model"))
  defaults <- list(
    ini.responseVarName = "RES",
    ini.strataVarName   = "STR")
  dialog.values <- getDialog("dceFitmodel", defaults)

  if (!any(Variables() == dialog.values$ini.responseVarName)) {
    dialog.values$ini.responseVarName = gettextRcmdr("<no variable selected>")
  }
  if (!any(Variables() == dialog.values$ini.strataVarName)) {
    dialog.values$ini.strataVarName = gettextRcmdr("<no variable selected>")
  }
    
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  
  # remove a term 'strata' from the current model formula
  if (currentModel) {
    currentRhs <- currentFields$rhs
    currentRhs <- gsub(' +', '', currentRhs)
    currentRhs <- unlist(strsplit(currentRhs, "\\+"))
    strataPos  <- grep("strata\\(", currentRhs)
    currentRhs <- currentRhs[-strataPos]
    currentRhs <- paste(currentRhs, collapse = " + ")
    currentFields$rhs <- currentRhs
  }
  
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }

  ##### Output Frame #####
  # Name for fitted model
  UpdateModelNumber()
  modelName  <- tclVar(paste("DCEmodel.", getRcmdr("modelNumber"), sep = ""))
  modelFrame <- tkframe(top)
  model      <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  
  ##### Input Frame #####
  # Response variable
  responseVarFrame <- tkframe(top)
  responseVarBox <- variableComboBox(
    responseVarFrame,
    Variables(),
    initialSelection = dialog.values$ini.responseVarName,
    title = "Response variable")

  # Independent variables (Added 0.1-3)
  independentVarFrame <- tkframe(top)
  independentVarBox <- variableListBox(
    independentVarFrame,
    Variables(),
    title = "Independent variables (pick one or more)",
    selectmode = "multiple",
    listHeight = 5)

  # Stratification variable
  strataFrame    <- tkframe(top)
  strataVarFrame <- tkframe(strataFrame)
  strataVarBox   <- variableComboBox(
    strataVarFrame,
    Variables(),
    initialSelection = dialog.values$ini.strataVarName,
    title = "Stratification variable")

    
  ##### onOK function #####
  onOK <- function () {

    responseVar <- getSelection(responseVarBox)
    strataVar   <- getSelection(strataVarBox)
    indVar      <- getSelection(independentVarBox)  # Added 0.1-3
    if(length(indVar) == 0) covVar <- "1"           # Added 0.1-3
    
    putDialog("dceFitmodel", list(
      ini.responseVarName = responseVar,
      ini.strataVarName   = strataVar))
    
    modelValue  <- trim.blanks(tclvalue(modelName))

    closeDialog()

    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == 
        gettextRcmdr("<all valid cases>") || trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste(", subset = ", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }

    rhsVars <- paste(indVar, collapse = " + ")    # Added 0.1-3
    
    formula <- paste(responseVar, " ~ ", rhsVars, # Modified 0.1-3 
                     " + strata(", strataVar ,")", sep = "")

    cmd <- paste("clogit(", formula, ", data = ", ActiveDataSet(), subset, ")",
                 sep = "")
    doItAndPrint(paste(modelValue, " <- ", cmd, sep = ""))
    doItAndPrint(paste(modelValue))
    doItAndPrint(paste("gofm(", modelValue,")", sep = ""))
    
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "clogit",
               model       = TRUE,
               reset       = "resetDceModel",
               apply       = "dceFitmodel")

  # Output
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")

  # Blank line
  tkgrid(labelRcmdr(top, text = ""))

  # Input
  ## Title
  tkgrid(labelRcmdr(top, text = gettextRcmdr("Model formula"),
                    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"),
         sticky = "w")

  ## Response variable
  tkgrid(getFrame(responseVarBox), sticky = "w") # Modified 0.1-3
  tkgrid(responseVarFrame, sticky = "w")

  ## Independent variables (Modified 0.1-3)
  tkgrid(getFrame(independentVarBox), sticky = "w")
  tkgrid(independentVarFrame, sticky = "w")

  ## Stratification variable (Modified 0.1-3)
  tkgrid(getFrame(strataVarBox), sticky = "w")
  tkgrid(strataVarFrame, sticky = "w")
  tkgrid(strataFrame, sticky = "w")

  ## Blank line
  tkgrid(labelRcmdr(top, text = ""))

  ## Subset
  subsetBox(model = TRUE)
  tkgrid(subsetFrame, sticky = "w")

  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix(preventDoubleClick = TRUE)
}

resetDceModel <- function(){
  putRcmdr("reset.model", TRUE)
  putDialog("dceFitmodel", NULL)
  putDialog("dceFitmodel", NULL, resettable = FALSE)
  dceFitmodel()
}

###############################################################################

dceMwtp <- function() {
  initializeDialog(
    title = gettextRcmdr("Calculate Marginal Willingness To Pays"))
  defaults <- list(
    outputName    = "MWTP",
    moneyName     = gettextRcmdr("<no variable selected>"),
    nonmoneyName  = NULL,
    calcmethod    = "1",
    NdrawsValue   = "1000",
    confLevelName = "0.95",
    RNGseedName   = "")
  dialog.values <- getDialog("dceMwtp", defaults)
  
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  

  ##### Output Frame #####
  # output
  outputFrame <- tkframe(top)
  outputName  <- tclVar(dialog.values$outputName)
  output      <- ttkentry(outputFrame, width = "20", textvariable = outputName)
  
  
  ##### Input Frame #####
  inputsFrame    <- tkframe(top)
  moneyFrame     <- tkframe(inputsFrame)
  clFrame        <- tkframe(inputsFrame)
  optionsKRFrame <- tkframe(inputsFrame)
  methodFrame    <- tkframe(inputsFrame)
  titleFrame     <- tkframe(inputsFrame)
  RNGseedFrame   <- tkframe(top)

  ## Monetary variable
  moneyVarBox <- variableComboBox(
    moneyFrame,
    variableList = names(coef(get(activeModel()))),
    initialSelection = dialog.values$moneyName, 
    title = "Monetary variable")
  
  ## Nonmonetary variables
  nonmoneyVarsBox <- variableListBox(
    moneyFrame,
    variableList = names(coef(get(activeModel()))),
    selectmode = "multiple",
    initialSelection = varPosn(dialog.values$nonmoneyName,
                               vars = names(coef(get(activeModel())))),
    title = "Nonmonetary variables (pick one or more)")
  
  ## Bootstrap method
  radioButtons(methodFrame, 
               name    = "methodtype",
               buttons = c("krinskyrobb", "delta"),
               values  = c("1", "2"),
               labels  = gettextRcmdr(c("Krinsky and Robb", "Delta")),
               initialValue = dialog.values$calcmethod,
               title   = gettextRcmdr("Calculation method"))
  
  ## Confidence level
  confLevelName <- tclVar(dialog.values$confLevelName)
  confLevel     <- ttkentry(clFrame, width = "6",
                            textvariable = confLevelName)

  NdrawsValue   <- tclVar(dialog.values$NdrawsValue)
  Ndraws        <- ttkentry(optionsKRFrame, width = "6",
                            textvariable = NdrawsValue)
  
  ## Random number generator seed
  RNGseedName <- tclVar(dialog.values$RNGseedName)
  RNGseed     <- ttkentry(optionsKRFrame, width = "10",
                          textvariable = RNGseedName)
  
  
  ### onOK button ###
  onOK <- function() {

    moneyVar <- getSelection(moneyVarBox)
    nonmoneyVars <- getSelection(nonmoneyVarsBox)
    nonmoneyName <- nonmoneyVars

    if (moneyVar == "<no variable selected>") {
      errorCondition(recall = dceMwtp,
                     message = gettextRcmdr("Select monetary variable"))
      return()
    }
    
    if (length(nonmoneyVars) == 0) {
      errorCondition(recall = dceMwtp,
                     message = gettextRcmdr("Select nonmonetary variable(s)"))
      return()
    } else {
      nonmoneyVars <- paste('c("', paste(nonmoneyVars, collapse = '", "'), '")',
                            sep = "")
      cmd.nonmoney <- paste('", nonmonetary.variables = ', nonmoneyVars,
                            sep = "")
    }

    putDialog("dceMwtp", list(
      outputName    = tclvalue(outputName),
      moneyName     = moneyVar,
      nonmoneyName  = nonmoneyName,
      calcmethod    = tclvalue(methodtypeVariable),
      NdrawsValue   = tclvalue(NdrawsValue),
      confLevelName = tclvalue(confLevelName),
      RNGseedName   = tclvalue(RNGseedName)))

    outputValue <- trim.blanks(tclvalue(outputName))
    closeDialog()

    if (tclvalue(methodtypeVariable) == "1") {
      cmd.method <- paste(', method = "kr"', sep = "")
    } else {
      cmd.method <- paste(', method = "delta"', sep = "")
    }
    
    if (is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste(", seed = NULL)", sep = "")
    } else {
      cmd.seed <- paste(", seed = ",  as.numeric(tclvalue(RNGseedName)), 
                        ")", sep = "")
    }

    doItAndPrint(paste(
      outputValue, ' <- mwtp(output = ', activeModel(),
      ', monetary.variables = "', moneyVar,
      cmd.nonmoney,
      ', nreplications = ', tclvalue(NdrawsValue),
      ', confidence.level = ', tclvalue(confLevelName),
      cmd.method, cmd.seed, sep = ""))
    doItAndPrint(paste0(outputValue))
    
    tkfocus(CommanderWindow())
  }
  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "mwtp",
               reset       = "dceMwtp",
               apply       = "dceMwtp")

  # Output
  tkgrid(labelRcmdr(outputFrame,
                    text = gettextRcmdr("Name for output ")),
         output, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  
  # Blank line
  tkgrid(labelRcmdr(top, text = ""))
  
  # Input
  ## Monetary variable
  tkgrid(getFrame(moneyVarBox), sticky = "w")

  ## Nonmonetary variable
  tkgrid(getFrame(nonmoneyVarsBox), sticky = "nw")
  tkgrid(moneyFrame, sticky = "w")

  ## Calculation method
  tkgrid(methodtypeFrame, sticky = "w")
  tkgrid(methodFrame, sticky = "w")
  
  ## Confidence level
  tkgrid(labelRcmdr(clFrame,
                    text = gettextRcmdr("Confidence level ")),
         confLevel, sticky = "w")
  tkgrid(clFrame, sticky = "w")
  
  ## Options for Krinsky and Robb
  tkgrid(
    labelRcmdr(
      titleFrame,
      text = gettextRcmdr("Options for Krinsky and Robb method:")),
     sticky = "w")
  tkgrid(titleFrame, sticky = "w")
  
  ### Number of replications
  tkgrid(labelRcmdr(optionsKRFrame,
                    text = gettextRcmdr("Number of replications ")),
         Ndraws, sticky = "w")
  
  ### Random number generator
  tkgrid(
    labelRcmdr(
      optionsKRFrame,
      text = gettextRcmdr("Seed for random number generator (optional) ")),
    RNGseed, sticky = "w")
  tkgrid(optionsKRFrame, sticky = "w")
  
  tkgrid(inputsFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  
  dialogSuffix()
}

###############################################################################

clogitP <- function() {
  activeModelP() && class(get(ActiveModel()))[1] == "clogit"
}

dcedataP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "dcedataset"
}

