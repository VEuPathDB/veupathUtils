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

studyAspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3'))
studyBspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species5'))
speciesVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspecies,studyBspecies))

studyAsex <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('female','male','non-binary','other','do not wish to specify'))
studyBsex <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('male','female'))
sexVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAsex,studyBsex))

studyAspeciesSMALL <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2'))
studyBspeciesSMALL <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2'))
speciesVocabsSMALL <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspeciesSMALL,studyBspeciesSMALL))

studyAsexSMALL <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('female','male'))
studyBsexSMALL <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('male','female'))
sexVocabsSMALL <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAsexSMALL,studyBsexSMALL))

pathogenVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='assay',variableId='pathogen_presence'), vocabulary=c('Yes','No')),
                                                                            StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='assay',variableId='pathogen_presence'), vocabulary=c('Yes','No'))))

test_that("Megastudy and associated validation works", {
  # works at all
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs)))

  expect_equal(slotNames(m), c('data','ancestorIdColumns','studySpecificVocabularies'))
  expect_equal(length(m@studySpecificVocabularies), 1)
  expect_equal(length(m@studySpecificVocabularies[[1]]), 2)
  expect_equal(slotNames(m@studySpecificVocabularies[[1]][[1]]), c("studyIdColumnName","study","variableSpec","vocabulary"))    

  # works w multiple vocab lists
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs, sexVocabs)))

  expect_equal(slotNames(m), c('data','ancestorIdColumns','studySpecificVocabularies'))
  expect_equal(length(m@studySpecificVocabularies), 2)
  expect_equal(length(m@studySpecificVocabularies[[2]]), 2)
  expect_equal(slotNames(m@studySpecificVocabularies[[2]][[1]]), c("studyIdColumnName","study","variableSpec","vocabulary"))              

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


  # errs if study id not consistent within a study vocab
  studyAspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3'))
  studyBspecies <- StudySpecificVocabulary(studyIdColumnName='imNOTa.studyId', study='b', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species5'))
  expect_error(speciesVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspecies,studyBspecies)))

  # errs if var spec not consistent
  studyAspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3'))
  studyBspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='imNOTaVariableId'), vocabulary=c('species1','species2','species5'))
  expect_error(speciesVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspecies,studyBspecies)))

  # errs if no study id provided for study vocab
  expect_error(StudySpecificVocabulary(study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName=NA, study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName=NULL, study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName=c(), study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName=character(), study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName=NA_character_, study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3')))

  # errs if no var spec provided for study vocab
  expect_error(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=NA, vocabulary=c('species1','species2','species3')))
  expect_error(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=NULL, vocabulary=c('species1','species2','species3')))
  # expect_error(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='', variableId=''), vocabulary=c('species1','species2','species3')))
})

# TODO this could go in its own file maybe
test_that("imputeZeroes method is sane", {
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabs)))

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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  # TODO lol its just possible this fxn shouldnt remove cols but my brain hurts enough already 
  expect_equal(all(c("sample.species","sample.specimen_count") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 12)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 6)

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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(c("sample.species","sample.specimen_count","collection.attractant") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 12)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 6)

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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(c("sample.species","sample.specimen_count","collection.attractant","study.author") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 12)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 6)

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
  
  mCOMPLETE <- Megastudy(data=megastudyDTSMALL,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id'),
                 studySpecificVocabularies=StudySpecificVocabulariesByVariableList(S4Vectors::SimpleList(speciesVocabsSMALL)))
  
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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(mCOMPLETE, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(mCOMPLETE@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(mCOMPLETE, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(mCOMPLETE@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(mCOMPLETE@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # an assay var is present 
  m <- Megastudy(data=megastudyDT,
                 ancestorIdColumns=c('study.id', 'collection.id', 'sample.id','assay.id'),
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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # are we ok that it leaves all cols if it decides nothing needs doing??
  # it wont hurt anything, its just inconsistent behavior
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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  expect_equal(all(names(imputedDT) %in% names(m@data)), TRUE)
  expect_equal(nrow(imputedDT), nrow(m@data))
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 0)

  # multiple special vocabs in same plot, w one shared weighting var
  m <- Megastudy(data=megastudyDT,
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
  expect_equal(nrow(imputedDT), 20)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 14)

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
  pathogenVariableCollectionVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='assay',variableId='pathogen_presence_variable_collection'), vocabulary=c('Yes','No')),
                                                                                                StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='assay',variableId='pathogen_presence_variable_collection'), vocabulary=c('Yes','No'))))


  m <- Megastudy(data=megastudyDT,
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

  imputedDT <- getDTWithImputedZeroes(m, variables, FALSE)
  # result has the columns needed to build a plot, based on variables AND the correct number of rows/ zeroes
  # TODO lol its just possible this fxn shouldnt remove cols but my brain hurts enough already 
  expect_equal(all(c("assay.pathogen_presence","assay.pathogen2_presence","assay.pathogen3_presence","sample.specimen_count") %in% names(imputedDT)), TRUE)
  expect_equal(nrow(imputedDT), 12)
  expect_equal(nrow(imputedDT[imputedDT$sample.specimen_count == 0]), 6)
})