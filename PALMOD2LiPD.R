# function to read PalMOD format into LiPD
devtools::install_github("nickmckay/lipdR")
# this version does not split the metadata and data by retrieval
# and adds the updated age model to the paleo data

PALMOD2LiPD <- function(x, path = "dataLiPD"){   # x is list with data
  require(lipdR)
  require(tidyverse)

  #will use this later.
  createTSid <- function(){
    return(paste(c("R",sample(c(letters,LETTERS,seq(0,9)),size = 10,replace=TRUE)),collapse = ""))
  }

  # file
  e <- readRDS(x)

  #translate to LiPD.

  #intialize
  L <- list()

  #root
  L$createdBy <- "PALMOD2LiPD"
  L$dataSetName <- e$site$SiteName
  L$lipdVersion <- 1.3 # probably needs an update

  #NOTE THIS!!!!
  L$archiveType <- "marine sediment"

  #geo
  L$geo <- list()
  L$geo$latitude <- e$site$SiteLat
  L$geo$longitude <- e$site$SiteLon
  L$geo$elevation <- e$site$SiteDepth_m
  L$geo$notes <- e$site$SiteNotes
  L$geo$siteName <- e$site$SiteName
  # L$geo$ocean <- e$site$OceanBasin # include?

  #paleoData ####
  paleo <- c("ParameterOriginal",
             "Parameter",
             "ParameterType",
             "ParameterUnit",
             "ParameterAnalyticalError",
             "ParameterReproducibility",
             "Instrument",
             "Laboratory",
             "SampleThickness_cm",
             "Material",
             "Species",
             "Nshells",
             "SizeFraction_microm",
             "Notes",
             "RecordingSeason",
             "RecordingDepth",
             "EquilibriumOffset",
             "CalibrationEquation",
             "CalibrationUncertainty",
             "CalibrationDOI",
             "TransferFunctionTrainingSet",
             "TransferFunctionUncertainty",
             "TransferFunctionDOI",
             "PublicationDOI",
             "DataLink",
             "TSuID" # keeping this instead of randomly generated one
             )
  
  # PALMOD sometimes has the following addtional fields
  # DataDOI: not relevant as we have DataLink
  # RetrievalNumber: not relevant, only for internal use
  # Publication: for publications without link or doi  
  # PublicationLink: for publications without doi, but with alternative identifier  
  
  paleoLipd <- c("description", "variableName","variableType","units","analyticalError","reproducibility","measurementInstrument","measurementLaboratory","sampleThickness","measurementMaterial","sensorSpecies","measurementN","measurementSizeFraction","notes","habitatSeason","habitatDepth","equilibriumOffset","calibrationEquation","calibrationUncertainty","calibrationDOI","transferFunctionTrainingSet","transferFunctionUncertainty","transferFunctionDOI", 'hasPubDOI', 'hasDataLink', "TSid")
  
  # check names
  #cbind.data.frame(paleo, paleoLipd)

  makeMeasurementTable <- function(meta, data){
    mt <- vector(mode = "list", length = nrow(meta))
    orig.data.url <- c()
    # not all files have all meta data fields 
    Paleo <- paleo[paleo %in% names(meta)]
    PaleoLipd <- paleoLipd[paleo %in% names(meta)]
    
    for(i in 1:nrow(meta)){

      for(p in 1:length(Paleo)){

        if(!is.na(meta[[Paleo[p]]][i])){

          mt[[i]][[PaleoLipd[p]]] <- meta[[Paleo[p]]][i]
          if(Paleo[p] == "DataLink"){
            orig.data.url <- c(orig.data.url, meta[[Paleo[p]]][i])
          }
        }
      }
      mt[[i]]$values <- data[,i]
      mt[[i]]$number <- i

      # mt[[i]]$TSid <- createTSid()
      names(mt)[i] <- mt[[i]]$variableName
    }
    mt
  }
  
  paleoData <- list(makeMeasurementTable(e$meta, e$data)) # does this need to be a list?
  names(paleoData) <- NULL
  
  L$paleoData[[1]]$measurementTable <- paleoData
  if(exists("orig.data.url")){
    L$originalDataUrl <- orig.data.url
  }
  
  # this adds a primary age identifier to the updated age model data
  L$paleoData[[1]]$measurementTable[[1]]$medianAge_kaBP$primaryAgeColumn <- TRUE
  
  
  #chronData ####
  chronMeta <- c("ChronSource", "ChronDOI")
  chronMetaLipd <- c("chronURL", "chronDOI")

  cdc <- c("ChronType",
           "ChronDepthTop_cm",
           "ChronDepthBottom_cm",
           "ChronDepthMid_cm",
           "ChronSampleThickness_cm",
           "ChronAge_kaBP",
           "ChronAgeError_ka",
           "ChronDatedMaterial",
           "ChronDatedSpecies",
           "ChronNshellsDated",
           "ChronTuneTarget",
           "ChronTuneParameter",
           "Chron14CLabcode",
           "ChronAge14C_kaBP",
           "ChronAge14CError_ka",
           "ChronAge14CErrorUp_ka",
           "ChronAge14CErrorDown_ka",
           "ChronReservoirAge_ka",
           "ChronReservoirAgeError_ka",
           "ChronCalibCurve",
           "ChronCalendarAge_kaBP",
           "ChronCalibAge14C1sigLo_ka",
           "ChronCalibAge14C1sigUp_ka",
           "ChronCalendarAgeMin_kaBP",
           "ChronCalendarAgeMax_kaBP",
           "ChronAgeRejected",
           "ChronNotes")
  
  cdcLiPD <- c("dateType","depthTop","depthBottom","depth","thickness","age","ageUncertainty","measurementMaterial","sensorSpecies","measurementN","tuneTarget","tuneVariable","labID","age14C","age14CUncertainty","age14CHi","age14CLow","reservoirAge","reservoirAgeUncertainty", "calibrationCurve","age","ageUncertainty1sigLo", "ageUncertainty1sigHi", "ageUncertainty2sigLo", "ageUncertainty2sigHi",  "rejected","notes")
  
  # check names
  #cbind.data.frame(cdc, cdcLiPD)
  
  chronData <- e$chron[, names(e$chron) %in% cdc]
  
  if(!is.null(chronData)){ # catch cases without chronology data
    nGoodCol <- sum(colSums(!is.na(chronData))>0)
    mt <- vector(mode = "list", length = nGoodCol)
    i <- 1
    
    for(v in 1:length(cdc)){
      if(any(!is.na(chronData[[cdc[[v]]]]))){
        mt[[i]]$values <-  chronData[[cdc[[v]]]]
        mt[[i]]$TSid <- createTSid()
        mt[[i]]$variableName <- cdcLiPD[[v]]
        mt[[i]]$number <- v
        
        #metadata
        for(m in 1:length(chronMeta)){
          if(any(!is.na(e$chron[[chronMeta[[m]]]]))){
            mt[[i]][[chronMetaLipd[m]]] <- unique(e$chron[[chronMeta[m]]]) # note that link between dating point and meta is lost
          }
          
        }
        
        i <- i+1
        
      }
    }
    
    # add units
    depthChronUnits <- c("depthTop","depthBottom","depth","thickness")
    ageChronUnits <- c("age","ageUncertainty", "age14C","age14CUncertainty","age14CHi","age14CLow","reservoirAge","reservoirAgeUncertainty", "age","ageUncertainty1sigLo", "ageUncertainty1sigHi", "ageUncertainty2sigLo", "ageUncertainty2sigHi")
    
    mt <- lapply(mt, function(n){
      if(n$variableName %in% depthChronUnits){n$units <- 'cm'}
      else if(n$variableName %in% ageChronUnits){n$units <- 'kyr'}
      n
    })
    
    # faster to do this earlier
    if(nGoodCol > 0){L$chronData[[1]]$measurementTable[[1]] <- mt}
  }
  
  # no longer needed now included in data
  # create chron model
  # summary table
  
  # stc <- c("depth_m","meanAge_kaBP","loAge_kaBP","medianAge_kaBP","hiAge_kaBP")
  # 
  # stcLiPD <- c("depth","age","ageLow","ageMedian","ageHi")
  # stu <- c("m","ka","ka","ka","ka")
  # 
  # sumData <- e$AgeModel[, names(e$AgeModel) %in% stc]
  # 
  # nGoodCol <- sum(colSums(!is.na(sumData))>0)
  # st <- vector(mode = "list", length = nGoodCol)
  # i <- 1
  # 
  # for(v in 1:length(stc)){
  #   if(any(!is.na(sumData[[stc[[v]]]]))){
  #     st[[i]]$values <-  sumData[[stc[[v]]]]
  #     st[[i]]$TSid <- createTSid()
  #     st[[i]]$variableName <- stcLiPD[[v]]
  #     st[[i]]$number <- i
  #     st[[i]]$units <- stu[v]
  #     
  #     
  #     i <- i+1
  #     
  #   }
  # }
  # 
  # names(st) <- sapply(st,"[[","variableName")
  # 
  # sumTab <- vector(mode = "list",length = 1)
  # sumTab[[1]] <- st
  
  
  #chron method
  #get bacon parameters
  tgb <- which(sapply(e$BACON,length)==1)
  
  method <- list(model = "Bacon", parameters = e$BACON[tgb])
  
  #get age ensemble data
  
  ae <- vector(mode = "list",length = 2)
  
  #assign depth 
  ae[[1]]$values <-  e$AgeEnsemblesBACON[,1]
  ae[[1]]$TSid <- createTSid()
  ae[[1]]$variableName <- "depth"
  ae[[1]]$number <- 1
  ae[[1]]$units <- "m"
  
  #assign ageEnsemble
  ae[[2]]$values <-  e$AgeEnsemblesBACON[,-1]
  ae[[2]]$TSid <- createTSid()
  ae[[2]]$variableName <- "ageEnsemble"
  ae[[2]]$number <- seq(2, ncol(e$AgeEnsemblesBACON))
  ae[[2]]$units <- "ka"
  
  names(ae) <- c("depth","ageEnsemble")
  ageEns <- vector(mode = "list", length = 1)
  ageEns[[1]] <- ae

  
  #put the chron model together
  L$chronData[[1]]$model <- vector(mode = "list",length = 1)
  L$chronData[[1]]$model[[1]] <- list(method = method, ensembleTable = ageEns)
  
  # sst ensembles ####
  # @Nick: please check, should this be included in paleodata?
  L$sstEnsembles <- map(e$SurfaceTempEnsembles, function(x){
    list(
      method = list(
        model = ifelse(x$details$basis == "MgCa", "MgCaRB", "Bayspline"),
        parameters = as.list(x$details)
      ),
      ensembleTable = list(
        depth = list(
          values = x$ensembles[, 1],
          TSid = createTSid(),
          variableName = "depth",
          number = 1,
          units = "m"
        ),
        sstEnsemble = list(
          values =  x$ensembles[,-1],
          TSid <- createTSid(),
          variableName <- "sstEnsemble",
          number = seq(2, ncol(x$ensembles)),
          units = "ka"
        )
      )
    )
  })
  
  # sort out data links and publications ####

  # this simply takes all unique data links from the file
  # look in measurement table to find out which time series has which data link
  dataLinks <- unique(e$meta$DataLink[!is.na(e$meta$DataLink)])
  dataLinks <- unique(unlist(sapply(dataLinks, function(b) strsplit(b, '; '))))
  L$originalDataURL <- as.list(dataLinks)


  # for ease I am going to do this for DOI only, I'm pretty sure virtually everything has a DOI
  # also get DOIs from chron data
  # this simply takes all unique DOIs from the file
  # look in measurement table to find out which time series has which DOI


  pubDOIs <- unique(e$meta$PublicationDOI[!is.na(e$meta$PublicationDOI)])
  pubDOIs <- unlist(sapply(pubDOIs, function(b) strsplit(b, '; ')))

  chronDOIs <- unique(e$chron$ChronSource[!is.na(e$chron$ChronSource)])
  chronDOIs <- unlist(sapply(chronDOIs, function(b) strsplit(b, '; ')))

  allDOIs <- as.list(unique(c(pubDOIs, chronDOIs)))

  pub <- vector(mode = "list",length = length(allDOIs))
  for(p in 1:length(pub)){
    if(!is.na(allDOIs[p])){#only look at first entry
      pub[[p]]$doi <- allDOIs[p]
    }
  }
  L$pub <- pub

  #return(L)

  #L$pub <- as.list(unique(c(pubDOIs, chronDOIs)))

  # pub
  #pubMeta <- c("Publication.DOI","Authors","Publication title","Journal","Year","Volume","Issue","Pages","Report Number")
  #pubMetaLipd <- c("DOI","author","title","journal","year","volume","issue","pages","number")

  #pub <- vector(mode = "list",length = 1)
  #for(p in 1:length(pubMeta)){
  #  if(!is.na(e$meta[[pubMeta[p]]][1])){#only look at first entry
  #    pub[[1]][[pubMetaLipd[p]]] <- e$meta[[pubMeta[p]]][1]
  #  }
  #}
  #L$pub <- pub


  # for now, write to file
  #save(L, file = paste0(path, L$dataSetName, '.Rdata'))
  
  # # write to LiPD
  writeLipd(L, path = path)
  #L2 <- readLipd("MD02_2529.lpd")
}
