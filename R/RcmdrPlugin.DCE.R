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
    ini.designName = "DCEdesign",
    ini.designMethod = "FALSE",
    ini.nAlternativesName = "2",
    ini.nBlocksName = "1",
    ini.RNGseedName = "",
    ini.RNGoptionVariable = "0",
    ini.A1Var = "0",
    ini.A2Var = "0",
    ini.A3Var = "0",
    ini.A4Var = "0",
    ini.A5Var = "0",
    ini.A6Var = "0",
    ini.saveVariable = "0")
  dialog.values <- getDialog("dceDesign", defaults)

  if(is.null(getDialog("dceDesign"))) putRcmdr("savedTableDceDesign", NULL)
  
  ##### Output Frame #####
  outputFrame <- tkframe(top)
  designFrame <- tkframe(outputFrame)
  saveFrame   <- tkframe(outputFrame)

  # Choice sets
  designName <- tclVar(dialog.values$ini.designName)
  design     <- ttkentry(designFrame, width = "14",
                         textvariable = designName)

  # Save option
  saveVariable <- tclVar(dialog.values$ini.saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)


  ##### Input Frame #####
  inputsFrame       <- tkframe(top)
  designMethodFrame <- tkframe(inputsFrame)
  AltBlkRngFrame    <- tkframe(inputsFrame)
  RNGFrame          <- tkframe(inputsFrame)
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
    initialValue = dialog.values$ini.designMethod,
    title = gettextRcmdr("Design method"))

  # Number of alternatives per set (without the output option)
  nAlternativesName <- tclVar(dialog.values$ini.nAlternativesName)
  nAlternatives     <- ttkentry(AltBlkRngFrame,
                                width = "7",
                                textvariable = nAlternativesName)
  
  # Number of blocks
  nBlocksName <- tclVar(dialog.values$ini.nBlocksName)
  nBlocks     <- ttkentry(AltBlkRngFrame,
                          width = "7",
                          textvariable = nBlocksName)

  # Seed for RNG
  RNGseedName <- tclVar(dialog.values$ini.RNGseedName)
  RNGseed     <- ttkentry(RNGFrame,
                          width = "7",
                          textvariable = RNGseedName)

  # RNG option
  RNGoptionVariable <- tclVar(dialog.values$ini.RNGoptionVariable)
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

  initial.table <- getRcmdr("savedTableDceDesign")
  
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
  A1Var <- tclVar(dialog.values$ini.A1Var)
  A1CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A1Var)
  A2Var <- tclVar(dialog.values$ini.A2Var)
  A2CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A2Var)
  A3Var <- tclVar(dialog.values$ini.A3Var)
  A3CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A3Var)
  A4Var <- tclVar(dialog.values$ini.A4Var)
  A4CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A4Var)
  A5Var <- tclVar(dialog.values$ini.A5Var)
  A5CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A5Var)
  A6Var <- tclVar(dialog.values$ini.A6Var)
  A6CheckBox <- ttkcheckbutton(AttrCheckBoxFrame, variable = A6Var)
  
  
  ##### onOK Function #####
  onOK <- function() {

    putDialog("dceDesign", list(
      ini.designName = tclvalue(designName),
      ini.designMethod = tclvalue(designmethodVariable),
      ini.nAlternativesName = tclvalue(nAlternativesName),
      ini.nBlocksName = tclvalue(nBlocksName),
      ini.RNGseedName = tclvalue(RNGseedName),
      ini.RNGoptionVariable = tclvalue(RNGoptionVariable),
      ini.A1Var = tclvalue(A1Var),
      ini.A2Var = tclvalue(A2Var),
      ini.A3Var = tclvalue(A3Var),
      ini.A4Var = tclvalue(A4Var),
      ini.A5Var = tclvalue(A5Var),
      ini.A6Var = tclvalue(A6Var),
      ini.saveVariable = tclvalue(saveVariable)))
    
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
    putRcmdr("savedTableDceDesign", varNames) 
    
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
    justDoIt(
      paste("attributes(", tclvalue(designName), ")$contA <- ", cmd.contA,
            sep = ""))
    justDoIt(
      paste("attributes(", tclvalue(designName), ")$cateA <- ", cmd.cateA,
            sep = ""))
    doItAndPrint(paste(tclvalue(designName)))
    
    # Save choice sets
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          ' {"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(tclvalue(designName), ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', tclvalue(designName),
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(paste0(gettextRcmdr("DCE design was exported to file: "),
                     saveFile),
              type = "note")
    }
    
    tkfocus(CommanderWindow())
  }

  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "dceDesign",
               reset       = "resetDceTable",
               apply       = "dceDesign")
  # Output
  ## Design
  tkgrid(
    labelRcmdr(designFrame,
               text = gettextRcmdr("Name for design ")),
    design, sticky = "w")

  ## Save choice sets
  tkgrid(
    saveCheckBox,
      labelRcmdr(
        saveFrame,
        text = gettextRcmdr("Save to file")),
    sticky = "w")
  
  tkgrid(designFrame, labelRcmdr(outputFrame, text = "   "),
         saveFrame, sticky = "w")
  tkgrid(outputFrame, sticky = "w")

  # Blank line
  tkgrid(labelRcmdr(top, text = ""))

  # Input
  ## Design Method
  tkgrid(designmethodFrame, sticky = "w")
  tkgrid(designMethodFrame, sticky = "w")

  ## Design parameter
  tkgrid(
    labelRcmdr(
      AltBlkRngFrame,
      text = gettextRcmdr("Design parameters:")),
    sticky = "w")

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
  tkgrid(AltBlkRngFrame, sticky = "w")

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

  ## Reproducibility
  tkgrid(labelRcmdr(
           RNGFrame,
           text = gettextRcmdr("Reproducibility:")),
         sticky = "w")

  ## Seed for RNG
  tkgrid(labelRcmdr(
           RNGFrame,
           text = gettextRcmdr("Seed for random number generator (optional) ")),
         RNGseed, sticky = "w")
  tkgrid(RNGFrame, sticky = "w")

  ## RNG option
  tkgrid(
    RNGoptionCheckBox,
      labelRcmdr(
        RNGoptionFrame,
        text = gettextRcmdr("Reproduce choice sets designed with R < 3.6.0")),
    sticky = "w")
  tkgrid(RNGoptionFrame, sticky = "w")
  
  tkgrid(inputsFrame, sticky = "w")

  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetDceTable <- function() {
  putRcmdr("savedTableDceDesign", NULL)
  putDialog("dceDesign", NULL)
  dceDesign()
}

###############################################################################

dceQuestions <- function() {
  initializeDialog(title = gettextRcmdr("Display DCE Questions"))
  defaults <- list(designName = "DCEdesign")
  dialog.values <- getDialog("dceQuestions", defaults)
  
  
  ##### Input Frame #####
  inputsFrame <- tkframe(top)

  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design <- ttkentry(inputsFrame, width = "14",
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
  OKCancelHelp(helpSubject = "dceQuestions",
               reset       = "dceQuestions",
               apply       = NULL)

  # Name of design
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Design ")),
    design, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################

dceDataset <- function() {
  initializeDialog(title = gettextRcmdr("Create Data Set for DCE Analysis"))
  defaults <- list(
    ini.datasetName  = "DCEdata",
    ini.designName   = "DCEdesign",
    ini.responseName = "",
    ini.optoutVariable = "0",
    ini.saveVariable = "0")
  dialog.values <- getDialog("dceDataset", defaults)
  
  
  ##### Output Frame #####
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  saveFrame        <- tkframe(outputFrame)

  # output name
  datasetName <- tclVar(dialog.values$ini.datasetName)
  dataset     <- ttkentry(datasetnameFrame, width = "14", 
                          textvariable = datasetName)
  
  # save option
  saveVariable <- tclVar(dialog.values$ini.saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)


  ##### Input Frame #####
  inputsFrame <- tkframe(top)
  tableFrame  <- tkframe(inputsFrame)
  optoutFrame <- tkframe(inputsFrame)
  
  # choice sets
  designName <- tclVar(dialog.values$ini.designName)
  design <- ttkentry(inputsFrame, width = "14",
                     textvariable = designName)
  
  # response variables
  responseName <- tclVar(dialog.values$ini.responseName)
  response <- ttkentry(inputsFrame, width = "40",
                         textvariable = responseName)
  
  # opt-out option
  optoutVariable <- tclVar(dialog.values$ini.optoutVariable)
  optoutCheckBox <- ttkcheckbutton(optoutFrame, variable = optoutVariable)
  

  ##### onOK function #####
  onOK <- function() {

    putDialog("dceDataset", list(
      ini.datasetName    = tclvalue(datasetName),
      ini.designName     = tclvalue(designName),
      ini.responseName   = tclvalue(responseName),
      ini.optoutVariable = tclvalue(optoutVariable),
      ini.saveVariable   = tclvalue(saveVariable)))
    
    cateA <- eval(parse(text = paste0("attr(", tclvalue(designName),
                                      ", 'cateA')")))
    contA <- eval(parse(text = paste0("attr(", tclvalue(designName),
                                      ", 'contA')")))

    if (tclvalue(optoutVariable) == 1) {
      optoutTF <- "TRUE"
    } else {
      optoutTF <- "FALSE"
    }
    
    closeDialog()
    responseVars <- unlist(strsplit(x = gsub(' +', '', tclvalue(responseName)),
                                    split = ","))

    # Create data set for analysis
    doItAndPrint(
      paste0(
        tclvalue(datasetName), ' <- ce.dataset(',
        'data = ', activeDataSet(), 
        ', response = c("', 
          paste(responseVars, collapse = '", "'), '")',
        ', design = ', tclvalue(designName), 
        ', categorical.attributes = c("',
          paste(cateA, collapse = '", "'), '")',
        ', continuous.attributes = c("',
          paste(contA, collapse = '", "'), '")',
        ', common = NULL',
        ', optout = ', optoutTF,
        ', unlabeled = TRUE',
        ', binary = FALSE',
        ', detail = FALSE)'))

    activeDataSet(tclvalue(datasetName))
    
    # Save data set
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          ' {"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(tclvalue(datasetName), ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', tclvalue(datasetName),
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(
        paste0(
          gettextRcmdr("DCE data set for analysis was exported to file: "),
          saveFile),
        type = "note")
    }
        
    tkfocus(CommanderWindow())
    
  }  
  
  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "dceDataset",
               reset       = "dceDataset",
               apply       = "dceDataset")
  
  # Output
  ## data set
  tkgrid(
    labelRcmdr(datasetnameFrame,
               text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")

  ## Save data set
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame,
                    text = gettextRcmdr("Save to file")),
         sticky = "w")

  tkgrid(datasetnameFrame, labelRcmdr(outputFrame, text = "   "),
         saveFrame, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  
  # Blank line
  tkgrid(labelRcmdr(top, text = ""))
  
  # Input
  ## Design matrix
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Design")),
    design, sticky = "w")

  ## Responses to DCE questions
  tkgrid(labelRcmdr(
    inputsFrame,
    text = gettextRcmdr("Responses to DCE questions ")),
    response, sticky = "w")
  
  ## Opt-out option
  tkgrid(optoutCheckBox,
         labelRcmdr(optoutFrame, text = gettextRcmdr("Opt-out option")),
         sticky = "w")
  tkgrid(optoutFrame, sticky = "w")

  tkgrid(inputsFrame, sticky = "w")  
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################

dceInteractions <- function() {
  initializeDialog(
    title = 
      gettextRcmdr("Create Interactions between Attributes/Levels and Covariates"))

  defaults <- list(
    ini.attrlvlVar   = NULL,
    ini.covariateVar = NULL)

  dialog.values <- getDialog("dceInteractions", defaults)


  ##### Input Frame #####
  inputFrame      <- tkframe(top)
  attrlvlVarFrame <- tkframe(inputFrame)
  covariateFrame  <- tkframe(inputFrame)
  
  # Attribute/level variables
  attrlvlVarVec <- eval(parse(text = paste0("attr(", activeDataSet(),
                                            ", 'independents')")))
  attrlvlVarBox <- variableListBox(
    attrlvlVarFrame,
    attrlvlVarVec,
    title = "Attribute/level variables \n(pick one or more)",
    selectmode = "multiple",
    listHeight = 10,
    initialSelection = varPosn(dialog.values$ini.attrlvlVar,
                               vars = attrlvlVarVec))
  
  # Covariates
  covariateVec <- eval(parse(text = paste0("attr(", activeDataSet(),
                                            ", 'covariates')")))
  covariateBox <- variableListBox(
    covariateFrame,
    covariateVec,
    title = "Covariates \n(pick one or more)",
    selectmode = "multiple",
    listHeight = 10,
    initialSelection = varPosn(dialog.values$ini.covariateVar,
                               vars = covariateVec))
  
  
  ##### onOK function #####
  onOK <- function() {
    attrlvlVar   <- getSelection(attrlvlVarBox)
    covariateVar <- getSelection(covariateBox)
    
    putDialog("dceInteractions", list(
      ini.attrlvlVar   = attrlvlVar,
      ini.covariateVar = covariateVar))
    
  closeDialog()
  
  interactionVars <- NULL
  for (i in attrlvlVar) {
    for (j in covariateVar) {
      doItAndPrint(paste0(activeDataSet(), "$", i, "_", j, " <- ", 
                          activeDataSet(), "$", i, " * ", 
                          activeDataSet(), "$", j))
      interactionVars <- c(interactionVars, paste0(i, j))
    }
  }
  
  activeDataSet(activeDataSet(),
                flushModel = FALSE,
                flushDialogMemory = FALSE)

  tkfocus(CommanderWindow())
  }

  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons
  OKCancelHelp(helpSubject = "dceInteractions",
               reset       = "dceInteractions",
               apply       = "dceInteractions")

  # Attribute/level variabels
  tkgrid(getFrame(attrlvlVarBox), sticky = "w")
  
  # Covariates
  tkgrid(getFrame(covariateBox), sticky = "w")

  tkgrid(attrlvlVarFrame, labelRcmdr(inputFrame, text = "   "),
         covariateFrame, sticky = "nw")
  
  tkgrid(inputFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  
  dialogSuffix()
}

###############################################################################

dceModel <- function() {
  initializeDialog(title = gettextRcmdr("Fit Model to DCE Data"))
  defaults <- list(
    ini.responseVarName    = "RES",
    ini.independentVarName = NULL,
    ini.strataVarName      = "STR")
  dialog.values <- getDialog("dceModel", defaults)

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
  modelName  <- tclVar(paste0("DCEmodel.", getRcmdr("modelNumber")))
  modelFrame <- tkframe(top)
  model      <- ttkentry(modelFrame, width = "14", textvariable = modelName)

  
  ##### Input Frame #####
  # Response variable
  responseVarFrame <- tkframe(top)
  responseVarName  <- tclVar(dialog.values$ini.responseVarName)
  responseVar      <- ttkentry(responseVarFrame, width = "5",
                               textvariable = responseVarName)

  # Independent variables
  indVarVec <- 
    eval(parse(text = paste0("attr(", activeDataSet(), ", 'independents')")))
  noncovVarVec <- 
    eval(parse(text = paste0("attr(", activeDataSet(), ", 'noncovariates')")))
  covVarVec <- 
    eval(parse(text = paste0("attr(", activeDataSet(), ", 'covariates')")))
  currentAllVarVec <- 
    eval(parse(text = paste0("colnames(", activeDataSet(), ")")))
  interactionVec <- 
    currentAllVarVec[!currentAllVarVec %in% c(noncovVarVec, covVarVec)]
  allIndVarVec <- c(indVarVec, interactionVec)

  independentVarFrame <- tkframe(top)
  independentVarBox <- variableListBox(
    independentVarFrame,
    allIndVarVec,
    title = "Independent variables \n(pick one or more)",
    selectmode = "multiple",
    listHeight = 10,
    initialSelection = varPosn(dialog.values$ini.independentVarName,
                               vars = allIndVarVec))

  # Stratification variable
  strataFrame    <- tkframe(top)
  strataVarFrame <- tkframe(strataFrame)
  strataVarName  <- tclVar(dialog.values$ini.strataVarName)
  strataVar      <- ttkentry(strataVarFrame, width = "5",
                             textvariable = strataVarName)

    
  ##### onOK function #####
  onOK <- function () {

    responseVar <- trim.blanks(tclvalue(responseVarName))
    strataVar   <- trim.blanks(tclvalue(strataVarName))
    indVar      <- getSelection(independentVarBox)  # Added 0.1-3
    if(length(indVar) == 0) covVar <- "1"           # Added 0.1-3
    
    putDialog("dceModel", list(
      ini.responseVarName    = responseVar,
      ini.independentVarName = indVar,
      ini.strataVarName      = strataVar))
    
    modelValue  <- trim.blanks(tclvalue(modelName))

    closeDialog()

    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == 
        gettextRcmdr("<all valid cases>") || trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste0(", subset = ", subset)
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
  OKCancelHelp(helpSubject = "dceModel",
               model       = TRUE,
               reset       = "resetDceModel",
               apply       = "dceModel")

  # Output
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")

  # Blank line
  tkgrid(labelRcmdr(top, text = ""))

  # Input
  ## Response variable
  tkgrid(labelRcmdr(responseVarFrame,
                    text = gettextRcmdr("Response variable ")),
         labelRcmdr(responseVarFrame,
                    text = tclvalue(responseVarName),
                    relief = "solid", foreground = "green"),
         sticky = "w")
  tkgrid(responseVarFrame, sticky = "w")

  ## Independent variables (Modified 0.1-3)
  tkgrid(getFrame(independentVarBox), sticky = "w")
  tkgrid(independentVarFrame, sticky = "w")

  ## Stratification variable
  tkgrid(labelRcmdr(strataVarFrame,
                    text = gettextRcmdr("Stratification variable ")),
         labelRcmdr(strataVarFrame,
                    text = tclvalue(strataVarName),
                    relief = "solid", foreground = "green"),
         sticky = "w")
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
  putDialog("dceModel", NULL)
  putDialog("dceModel", NULL, resettable = FALSE)
  dceModel()
}

###############################################################################

dceMwtp <- function() {
  initializeDialog(
    title = gettextRcmdr("Calculate Marginal Willingness to Pay"))
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
  output      <- ttkentry(outputFrame, width = "14",
                         textvariable = outputName)
  
  
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
  confLevel     <- ttkentry(clFrame, width = "7",
                            textvariable = confLevelName)

  NdrawsValue   <- tclVar(dialog.values$NdrawsValue)
  Ndraws        <- ttkentry(optionsKRFrame, width = "7",
                            textvariable = NdrawsValue)
  
  ## Random number generator seed
  RNGseedName <- tclVar(dialog.values$RNGseedName)
  RNGseed     <- ttkentry(optionsKRFrame, width = "7",
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
  OKCancelHelp(helpSubject = "dceMwtp",
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

dceLoad <- function() {
  file <- 
    tclvalue(
      tkgetOpenFile(
        filetype = 
          gettextRcmdr(' {"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}')))
  if (file == "") {
    return()
  }
  setBusyCursor()
  on.exit(setIdleCursor())

  cmd <- paste0('load("', file, '")')
  loadedObject <- justDoIt(cmd)
  logger(cmd)
  Message(paste0(gettextRcmdr("Name of loaded object: "),
          paste(loadedObject, collapse = ", ")),
          type = "note")

  tkfocus(CommanderWindow())
}

###############################################################################

dceClogitP <- function() {
  activeModelP() && 
  class(get(ActiveModel()))[1] == "clogit" && 
  class(get(ActiveDataSet()))[1] == "ce.dataset"
}

dceDataP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "ce.dataset"
}

