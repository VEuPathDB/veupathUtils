megastudyDT <- data.table('study.id'=c('a','a','a','b','b','b'),
                          'study.author'=c('Cool Guy', 'Cool Guy', 'Cool Guy', 'Uncool Guy', 'Uncool Guy', 'Uncool Guy'),
                          'collection.id'=c(1,1,2,1,2,2), 
                          'collection.attractant'=c('A','A','B','C','D','D'),
                          'sample.id'=c(1,2,3,4,5,6),
                          'sample.species'=c('species1','species2','species1','species1','species1','species2'),
                          'sample.sex'=c('male','male','female','male','female','male'),
                          'sample.specimen_count'=c(10,20,15,15,10,20),
                          'assay.id'=c(11,12,13,14,15,16),
                          'assay.pathogen_prevalence'=c(.1,.2,.3,.4,.5,.6),
                          'assay.pathogen_presence'=c('Yes','Yes','No','No','Yes','No'),
                          'assay.pathogen2_presence'=c('Yes','No','Yes','No','Yes','No'),
                          'assay.pathogen3_presence'=c('No','Yes','No','Yes','No','Yes'),
                          'assay.weighting_variable'=c(5,10,15,20,25,30))

collectionsDT <- data.table(
  'study.id' = c('a', 'a', 'a','b', 'b', 'b', 'b'),
  'collection.id' = c(1, 2, 3, 1, 2, 3, 4))

sexVocabs.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'a', 'a', 'a', 'b', 'b'),
  'sample.sex' = c('female', 'male', 'non-binary', 'other', 'do not wish to specify', 'male', 'female'))

sexVocabs <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='sample',variableId='sex'),
  studyVocab=sexVocabs.dt
)

sexVocabsSMALL.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'b', 'b'),
  'sample.sex' = c('female', 'male', 'male', 'female'))

sexVocabsSMALL <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='sample',variableId='sex'),
  studyVocab=sexVocabsSMALL.dt
)

sexVocabsSingleStudy.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'a', 'a', 'a'),
  'sample.sex' = c('female', 'male', 'non-binary', 'other', 'do not wish to specify'))

sexVocabsSingleStudy <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='sample',variableId='sex'),
  studyVocab=sexVocabsSingleStudy.dt
)

speciesVocabs.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'a', 'b', 'b', 'b'),
  'sample.species' = c('species1', 'species2', 'species3', 'species1', 'species2', 'species5'))

speciesVocabs <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='sample',variableId='species'),
  studyVocab=speciesVocabs.dt
)

speciesVocabsSMALL.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'b', 'b'),
  'sample.species' = c('species1', 'species2', 'species1', 'species2'))

speciesVocabsSMALL <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='sample',variableId='species'),
  studyVocab=speciesVocabsSMALL.dt
)

pathogenVocabs.dt <- data.table::data.table(
  'study.id' = c('a', 'a', 'b', 'b'),
  'assay.pathogen_presence' = c('Yes', 'No', 'Yes', 'No'))

pathogenVocabs <- veupathUtils::StudySpecificVocabulariesByVariable(
  studyIdColumnName='study.id',
  variableSpec=veupathUtils::VariableSpec(entityId='assay',variableId='pathogen_presence'),
  studyVocab=pathogenVocabs.dt
)

test_that("Megastudy and associated validation works", {
  # works at all
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs)))

  expect_equal(slotNames(m), c('data','ancestorIdColumns','studySpecificVocabularies'))
  expect_equal(length(m@studySpecificVocabularies), 1)
  expect_equal(data.table::uniqueN(m@studySpecificVocabularies[[1]]@studyVocab[,1]), 2)
  expect_equal(slotNames(m@studySpecificVocabularies[[1]]), c("studyIdColumnName","variableSpec","studyVocab"))    

  # works w multiple vocab lists
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs)))

  expect_equal(slotNames(m), c('data','ancestorIdColumns','studySpecificVocabularies'))
  expect_equal(length(m@studySpecificVocabularies), 2)
  expect_equal(data.table::uniqueN(m@studySpecificVocabularies[[2]]@studyVocab[,1]), 2)
  expect_equal(slotNames(m@studySpecificVocabularies[[2]]), c("studyIdColumnName","variableSpec","studyVocab"))              

  # errs if no ancestors/ids?
  expect_error(Megastudy(data=megastudyDT,
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))

  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=NA,
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))

  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=NULL,
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))   

  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=c(),
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))    

  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=character(),
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))   

  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=NA_character_,
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))                                                                                  

  # errs if extra ancestor ids provided but not in dt
  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=c('study.id', 'collection.id', 'imNOTan.id'),
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs))))

  # errs if special vocabs on different entities
  expect_error(Megastudy(data=megastudyDT,
                         ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'),
                         studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, pathogenVocabs))))

  # errs if studyIdColumnName and varSpec col name not in dt
  expect_error(StudySpecificVocabulariesByVariable(
    studyIdColumnName='imNOTan.id', 
    variableSpec=VariableSpec(entityId='sample',variableId='species'), 
    vocabulary=speciesVocabs.dt)
  )

  # errs if no study id provided for study vocab
  expect_error(StudySpecificVocabulariesByVariable(variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName=NA, variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName=NULL, variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName=c(), variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName=character(), variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName=NA_character_, variableSpec=VariableSpec(entityId='sample',variableId='species'), studyVocab=speciesVocabs.dt))

  # errs if no var spec provided for study vocab
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName='study.id', studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName='study.id', variableSpec=NA, studyVocab=speciesVocabs.dt))
  expect_error(StudySpecificVocabulariesByVariable(studyIdColumnName='study.id', variableSpec=NULL, studyVocab=speciesVocabs.dt))
})

# TODO this could go in its own file maybe
test_that("imputeZeroes method is sane", {
  m <- Megastudy(data=megastudyDT[, c('study.id', 'collection.id', 'sample.id', 'sample.specimen_count', 'sample.sex', 'sample.species', 'collection.attractant', 'study.author'), with=FALSE],
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs)))

  # case where neither study nor collection vars in the plot
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  expect_equal(all(c("sample.species","sample.specimen_count") %in% names(imputedDT)), TRUE)
  # 5 sexes * 3 species in study A (15) + 2 sexes * 3 species in study B (6) * 2 collections per study = 42
  expect_equal(nrow(imputedDT), 42)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 36)

  # case where some collection ids are missing
  # in real life, we have some collections where all samples are 0 and so not loaded
  # in this case, the collection ids are missing from the data table R gets handed. 
  # we want to impute zeroes for their samples anyhow.
  m <- Megastudy(data=megastudyDT[, c('study.id', 'collection.id', 'sample.id', 'sample.specimen_count', 'sample.sex', 'sample.species', 'collection.attractant', 'study.author'), with=FALSE],
                  ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                  studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs)),
                  collectionIdColumn='collection.id',
                  collectionsDT=collectionsDT)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  expect_equal(all(c("sample.species","sample.specimen_count") %in% names(imputedDT)), TRUE)
  # 5 sexes * 3 species in study A (15) + 2 sexes * 3 species in study B (6) * 2 collections per study = 42
  expect_equal(nrow(imputedDT), 42)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 36)

  # case where one study vocab is missing a study
  mDTSexSingleStudy <- megastudyDT[, c('study.id', 'collection.id', 'sample.id', 'sample.specimen_count', 'sample.sex', 'sample.species', 'collection.attractant', 'study.author'), with=FALSE]
  mDTSexSingleStudy$sample.sex[mDTSexSingleStudy$study.id == 'b'] <- NA_character_
  mDTSexSingleStudy <- unique(mDTSexSingleStudy)

  mSexSingleStudy <- Megastudy(data=mDTSexSingleStudy,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabsSingleStudy)))

  imputedDT <- getDTWithImputedZeroes(mSexSingleStudy, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  expect_equal(all(c("sample.species","sample.specimen_count") %in% names(imputedDT)), TRUE)
  # 5 sexes * 3 species in study A (15) + 3 species in study B * 2 collections per study = 42
  expect_equal(nrow(imputedDT), 36)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 30)

  # collection entity var is present
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(c("sample.species","sample.specimen_count","collection.attractant") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 42)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 36)

  # both collection and study entity vars are present
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(c("sample.species","sample.specimen_count","collection.attractant","study.author") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 42)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 36)

  # all values in vocab already present
  megastudyDTSMALL <- rbind(megastudyDT, 
                            data.table::data.table(study.id=c('a','b'),
                                                   study.author=c('Cool Guy','Uncool Guy'),
                                                   collection.id=c(2,1),
                                                   collection.attractant=c('B','C'),
                                                   sample.id=c(7,8),
                                                   sample.species=c('species2','species2'),
                                                   sample.sex=c('female','female'),
                                                   sample.specimen_count=c(5,5),
                                                   assay.id=c(17,18),
                                                   assay.pathogen_prevalence=c(.7,.8),
                                                   assay.pathogen_presence=c('No','Yes'),
                                                   assay.pathogen2_presence=c('Yes','No'),
                                                   assay.pathogen3_presence=c('No','Yes'),
                                                   assay.weighting_variable=c(35,40)))
  
  mCOMPLETE <- Megastudy(data=megastudyDTSMALL[, c('study.id', 'collection.id', 'sample.id', 'sample.specimen_count', 'sample.species', 'sample.sex', 'collection.attractant', 'study.author'), with=FALSE],
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabsSMALL, sexVocabsSMALL)))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(mCOMPLETE, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(mCOMPLETE@data)), TRUE)
  # 2 species * 2 sexes * 2 collections * 2 studies = 16
  expect_equal(nrow(imputedDT), 16)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 8)

  # no weighting var in plot
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(mCOMPLETE, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(mCOMPLETE@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(mCOMPLETE@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # an assay var is present 
  m <- Megastudy(data=megastudyDT[, c('study.id', 'collection.id', 'sample.id','assay.id', 'assay.pathogen_prevalence', 'sample.species', 'sample.sex', 'collection.attractant', 'study.author')],
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs)))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'pathogen_prevalence', entityId = 'assay'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(m@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'pathogen_presence', entityId = 'assay'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      # empty plotReference means that it is not plotted
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE)
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(m@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # multiple special vocabs in same plot, w one shared weighting var
  m <- Megastudy(data=megastudyDT[, c('study.id', 'collection.id', 'sample.id', 'sample.specimen_count', 'sample.species', 'sample.sex', 'collection.attractant'), with=FALSE],
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs,sexVocabs)))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary=TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(c("sample.species","sample.specimen_count","sample.sex","collection.attractant") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 42)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 36)

  # special vocab on sample, regular weighting var on assay
  # phase this in
#  m <- Megastudy(data=megastudyDT,
#                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'),
#                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs)))
#
#  variables <- new("VariableMetadataList", SimpleList(
#    new("VariableMetadata",
#      variableClass = new("VariableClass", value = 'native'),
#      variableSpec = new("VariableSpec", variableId = 'weighting_variable', entityId = 'assay'),
#      plotReference = new("PlotReference", value = 'xAxis'),
#      dataType = new("DataType", value = 'NUMBER'),
#      dataShape = new("DataShape", value = 'CONTINUOUS')),
#    new("VariableMetadata",
#      variableClass = new("VariableClass", value = 'native'),
#      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
#      plotReference = new("PlotReference", value = 'yAxis'),
#      dataType = new("DataType", value = 'NUMBER'),
#      dataShape = new("DataShape", value = 'CONTINUOUS')),
#    new("VariableMetadata",
#      variableClass = new("VariableClass", value = 'native'),
#      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
#      plotReference = new("PlotReference", value = 'overlay'),
#      dataType = new("DataType", value = 'STRING'),
#      dataShape = new("DataShape", value = 'CATEGORICAL'),
#      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
#      hasStudyDependentVocabulary=TRUE),
#    new("VariableMetadata",
#      variableClass = new("VariableClass", value = 'native'),
#      variableSpec = new("VariableSpec", variableId = 'pathogen_prevalence', entityId = 'assay'),
#      plotReference = new("PlotReference", value = 'facet1'),
#      dataType = new("DataType", value = 'STRING'),
#      dataShape = new("DataShape", value = 'CATEGORICAL'),
#      weightingVariableSpec = VariableSpec(variableId='weighting_variable',entityId='assay'),
#      hasStudyDependentVocabulary = FALSE)
#  ))
#
#  imputedDT <- getDTWithImputedZeroes(m, variables)
#  expect_equal(names(imputedDT), c("sample.species","sample.specimen_count","collection.attractant","study.author","study.id","collection.id","sample.id"))
#  expect_equal(nrow(imputedDT), 12)
#  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 6)
#
  # no special vocab or weight present
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'pathogen_prevalence', entityId = 'assay'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(m@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # only regular weight var present
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'pathogen_prevalence', entityId = 'assay'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      weightingVariableSpec = VariableSpec(variableId='weighting_variable',entityId='assay')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'author', entityId = 'study'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(m@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # sample var without a study vocab present when another sample var has one
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs)))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'species', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'sex', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'attractant', entityId = 'collection'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  expect_error(getDTWithImputedZeroes(m, variables, FALSE))

  # variable collection exists in plot
  pathogenVariableCollectionVocabs.dt <- data.table::data.table(
    study.id = c('a','a','b','b'),
    assay.pathogen_presence_variable_collection = c('Yes','No','Yes','No')
  )
  pathogenVariableCollectionVocabs <- StudySpecificVocabulariesByVariable(
    studyIdColumnName='study.id',
    variableSpec=VariableSpec(entityId='assay',variableId='pathogen_presence_variable_collection'),
    studyVocab=pathogenVariableCollectionVocabs.dt
  )

  m <- Megastudy(data=megastudyDT[, c('study.id', 'collection.id', 'sample.id', 'assay.id', 'sample.specimen_count', 'collection.attractant', 'study.author', 'assay.pathogen_presence', 'assay.pathogen2_presence', 'assay.pathogen3_presence'), with=FALSE],
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id', 'assay.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(pathogenVariableCollectionVocabs)))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'pathogen_presence_variable_collection', entityId = 'assay'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'),
      isCollection = TRUE,
      weightingVariableSpec = VariableSpec(variableId='specimen_count',entityId='sample'),
      hasStudyDependentVocabulary = TRUE,
      members = VariableSpecList(S4Vectors::SimpleList(VariableSpec(variableId='pathogen_presence', entityId='assay'),
                                                       VariableSpec(variableId='pathogen2_presence', entityId='assay'),
                                                       VariableSpec(variableId='pathogen3_presence', entityId='assay')))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'specimen_count', entityId = 'sample'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  ## Im inclined to commenting this for now. i dont think we have a case like this yet,
  ## and im not even completely sure yet what the api would be for when we do...

  #imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  #expect_equal(all(c("assay.pathogen_presence","assay.pathogen2_presence","assay.pathogen3_presence","sample.specimen_count") %in% names(imputedDT)), TRUE)
  # 2 studies * 2 collections per study * 2 values for each of 3 pathogen variables = 32?
  # im not sure this test makes any sense, bc were imputing 0 on a sample for a collection on assay
  # im going to comment until we see a real use case
  #expect_equal(nrow(imputedDT), 32)
  #expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 26)
})

test_that("we have reasonable perf w a real-ish use case", {
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

  ## minimal use case, with real vocabularies. based on a heavily subsetted megastudy.
  megastudyReal <- Megastudy(
    data=megastudyDataReal,
    ancestorIdColumns=c(
      'EUPATH_0000605.Study_stable_id', 
      'GAZ_00000448.GeographicLocation_stable_id',
      'OBI_0000659.ParentOfSample_stable_id',
      'EUPATH_0000609.Sample_stable_id'
    ),
    studySpecificVocabularies=studyVocabsReal
  )

  benchmark <- microbenchmark::microbenchmark(getDTWithImputedZeroes(megastudyReal, megastudyVariablesReal, verbose = FALSE))
  expect_true(mean(benchmark$time)/1000000 < 50) ## this is in milliseconds
  expect_true(median(benchmark$time)/1000000 < 50)
})