## ignore these. they are helpers i use during testing, to recreate what java does outside the data service
## ive just had to do this too many times to not have them in git right next to the thing im testing now

# to read in root-vocab endpoint results
# rootVocabHandle is a file handle to a two column data.table w no header
# first column is internal study id, second is a variable value from the vocab
# it is produced by the root-vocab endpoint in the subsetting service
getStudySpecificVocabularyByVariable <- function(rootVocabHandle, entityId, variableId, studyIdColName) {
  rootVocab <- readRootVocab(rootVocabHandle)
  tbl <- getStudyVocabTibble(rootVocab, entityId, variableId, studyIdColName)
  
  veupathUtils::StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(eval(parse(text=paste0('c(',paste(tbl$values, collapse=','),')')))))
}

readRootVocab <- function(rootVocabHandle) {
  rootVocab <- data.table::fread(rootVocabHandle, header=FALSE)
  names(rootVocab) <- c('studyId', 'value')

  if (nrow(rootVocab) == 0) {
    return(veupathUtils::StudySpecificVocabulariesByVariable())
  }

  return(rootVocab)
}

getStudyVocabTibble <- function(rootVocab, entityId, variableId, studyIdColName) {
  varSpecString <- getVarSpecAsString(entityId, variableId)

  tbl <- 
  dplyr::reframe(
    dplyr::group_by(
      rootVocab, 
      studyId
    ), 
    values=paste0(
      "veupathUtils::StudySpecificVocabulary(variableSpec=", 
      varSpecString, 
      ", vocabulary=c('",
      paste(value, collapse='\',\''),
      "'),study='",
      studyId, 
      "',studyIdColumnName='",
      studyIdColName,
      "')"
    )
  )

  return(unique(tbl))
}

getVarSpecAsString <- function(entityId, variableId) {
  paste0("veupathUtils::VariableSpec(entityId='", entityId, "', variableId='", variableId, "')")
}

veupathUtils::StudySpecificVocabulary(
  variableSpec=veupathUtils::VariableSpec(
    entityId='EUPATH_0000609', 
    variableId='PATO_0000047'
  ), 
  vocabulary=c('female,male'),
  study='1969-Iowa-surveillance',
  studyIdColumnName='EUPATH_0000605.Study_stable_id'
)


## some 'real life' test data, though minified for brevity
# some stuff for bobs analysis

sexVocabReal.mini <- 
  StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(
    studyIdColumnName='EUPATH_0000605.Study_stable_id', 
    study='2023-abundance-SLCMCD-2022', 
    variableSpec=VariableSpec(
      entityId='EUPATH_0000609',
      variableId='PATO_0000047'
    ), 
    vocabulary=c('male','mixed sex')
  )))

speciesVocabReal.mini <- 
  StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(
    studyIdColumnName='EUPATH_0000605.Study_stable_id', 
    study='2023-abundance-SLCMCD-2022', 
    variableSpec=VariableSpec(
      entityId='EUPATH_0000609',
      variableId='OBI_0001909'
    ), 
    vocabulary=c(
      'Aedes vexans nipponii',
      'Anopheles freeborni',
      'Coquillettidia perturbans',
      'Culex erythrothorax',
      'Culex pipiens',
      'Culex salinarius',
      'Culex tarsalis',
      'Culiseta incidens',
      'Culiseta inornata',
      'Ochlerotatus dorsalis',
      'Ochlerotatus increpitus',
      'Ochlerotatus nigromaculis',
      'Ochlerotatus sierrensis'
    )
  )))

  lifeStageVocabReal.mini <- 
  StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(
    studyIdColumnName='EUPATH_0000605.Study_stable_id', 
    study='2023-abundance-SLCMCD-2022', 
    variableSpec=VariableSpec(
      entityId='EUPATH_0000609',
      variableId='UBERON_0000105'
    ), 
    vocabulary=c('prime adult stage')
  )))

  feedingStatusVocabReal.mini <- 
  StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(
    studyIdColumnName='EUPATH_0000605.Study_stable_id', 
    study='2023-abundance-SLCMCD-2022', 
    variableSpec=VariableSpec(
      entityId='EUPATH_0000609',
      variableId='EUPATH_0043227'
    ), 
    vocabulary=c('')
  )))

  megastudyVariablesReal <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'POPBIO_8000017', entityId = 'EUPATH_0000609'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      hasStudyDependentVocabulary = FALSE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'PATO_0000047', entityId = 'EUPATH_0000609'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='POPBIO_8000017',entityId='EUPATH_0000609'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'OBI_0001909', entityId = 'EUPATH_0000609'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='POPBIO_8000017',entityId='EUPATH_0000609'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'UBERON_0000105', entityId = 'EUPATH_0000609'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='POPBIO_8000017',entityId='EUPATH_0000609'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'EUPATH_0043227', entityId = 'EUPATH_0000609'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='POPBIO_8000017',entityId='EUPATH_0000609'),
      hasStudyDependentVocabulary = TRUE)
  ))

  megastudyReal <- Megastudy(
    data=megastudyDataReal,
    ancestorIdColumns=c('EUPATH_0000605.Study_stable_id', 'GAZ_00000448.GeographicLocation_stable_id','OBI_0000659.ParentOfSample_stable_id','EUPATH_0000609.Sample_stable_id'),
    studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(sexVocabReal.mini, speciesVocabReal.mini, lifeStageVocabReal.mini, feedingStatusVocabReal.mini))
  )