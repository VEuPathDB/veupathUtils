megastudyDT <- data.table('study.id'=c('a','a','a','b','b','b'),
                          'study.author'=c('Cool Guy', 'Cool Guy', 'Cool Guy', 'Uncool Guy', 'Uncool Guy', 'Uncool Guy')
                      'collection.id'=c(1,1,2,1,2,2), 
                      'collection.attractant'=c('A','A','B','C','D','D')
                      'sample.species'=c('species1','species2','species1','species1','species1','species2'),
                      'sample.sex'=c('male','male','female','male','female','male')
                      'sample.specimen_count'=c(10,20,15,15,10,20))

studyAspecies <- StudySpecificVocabulary(study='a',variableSpec=VariableSpec(entityId='sample',variableId='species'),vocabulary=c('species1','species2','species3'))
studyBspecies <- StudySpecificVocabulary(study='b',variableSpec=VariableSpec(entityId='sample',variableId='species'),vocabulary=c('species1','species2','species5'))
speciesVocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAspecies,studyBspecies))

studyAsex <- StudySpecificVocabulary(study='a',variableSpec=VariableSpec(entityId='sample',variableId='sex'),vocabulary=c('female','male','non-binary','other','do not wish to specify'))
studyBsex <- StudySpecificVocabulary(study='b',variableSpec=VariableSpec(entityId='sample',variableId='sex'),vocabulary=c('male','female')) 

test_that("Megastudy and associated validation works", {
  # extra ancestor ids provided but not in dt

  # study id or var specs not consistent within a study vocab

  # no study id or var spec provided for study vocab
})

test_that("imputeZeroes method is sane", {
  # case where neither study nor collection vars in the plot

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
})