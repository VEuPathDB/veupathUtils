test.dt <- data.table('study'=c('a','a','a','b','b','b'), 
                      'collection'=c(1,2,1,1,2,2), 
                      'sample'=c('s1','s1','s2','s1','s1','s2'), 
                      'weight'=c(10,20,15,15,10,20))

studyAvocab <- StudySpecificVocabulary(study='a',variableSpec=VariableSpec(entityId='',variableId='sample'),vocabulary=c('s1','s2','s3'))
studyBvocab <- StudySpecificVocabulary(study='b',variableSpec=VariableSpec(entityId='',variableId='sample'),vocabulary=c('s1','s2','s5'))
vocabs <- StudySpecificVocabulariesByVariable(S4Vectors::SimpleList(studyAvocab,studyBvocab))

test_that("Megastudy validation works", {
 
})

test_that("StudySpecificVocabulariesByVariableList validation works", {
  
})

test_that("imputeZeroes method is sane", {



})