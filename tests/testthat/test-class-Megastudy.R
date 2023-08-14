#these should probably be in
megastudyDT <- data.table('study.id'=c('a','a','a','b','b','b'),
                          'study.author'=c('Cool Guy', 'Cool Guy', 'Cool Guy', 'Uncool Guy', 'Uncool Guy', 'Uncool Guy'),
                          'collection.id'=c(1,1,2,1,2,2), 
                          'collection.attractant'=c('A','A','B','C','D','D'),
                          'sample.id'=c(1,2,3,4,5,6),
                          'sample.species'=c('species1','species2','species1','species1','species1','species2'),
                          'sample.sex'=c('male','male','female','male','female','male'),
                          'sample.specimen_count'=c(10,20,15,15,10,20))

studyAspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species3'))
studyBspecies <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='species'), vocabulary=c('species1','species2','species5'))
speciesVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspecies,studyBspecies))

studyAsex <- StudySpecificVocabulary(studyIdColumnName='study.id', study='a', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('female','male','non-binary','other','do not wish to specify'))
studyBsex <- StudySpecificVocabulary(studyIdColumnName='study.id', study='b', variableSpec=VariableSpec(entityId='sample',variableId='sex'), vocabulary=c('male','female'))
sexVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAsex,studyBsex))



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

  imputedDT <- imputeZeroes(m, variables)

  # collection var is present

  # both are present

  # a collection id and collection var are both present

  # all values in vocab already present

  # no weighting var in plot

  # an assay var is present 

  # multiple special vocabs in same plot, w one shared weighting var

  # special vocabs on different entities

  # special vocab on sample, regular weighting var on assay

  # no special vocab or weight present

  # only weight var present

  # sample var without a study vocab present when another sample var has one
})

# TODO should also look through the various methods ive added around the place and see if any of those 
# are complicated enough to warrant tests either here or in the VariableMetadata test file