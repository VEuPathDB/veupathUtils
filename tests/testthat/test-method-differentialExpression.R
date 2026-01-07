# Tests for differential expression methods

test_that('differentialExpression returns a correctly formatted data.table', {

  testCollection <- testCountDataCollection
  testSampleMetadata <- getSampleMetadata(testCollection)


  # A Binary comparator variable
  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(length(result@droppedColumns), 0)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # When defined groups end up subsetting the incoming data
    comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_b"
                              ))
                            )
                          )
  )
  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(length(result@droppedColumns), 244)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  # expect_equal(sum(testSampleMetadata$entity.cat4 %in% c('cat4_a','cat4_b')), nrow(dt))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # With a continuous variable
  bin1 <- veupathUtils::Bin(binStart='2', binEnd='3', binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart='3', binEnd='4', binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart='4', binEnd='5', binLabel="[4, 5)")
  bin4 <- veupathUtils::Bin(binStart='5', binEnd='6', binLabel="[5, 6)")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  # expect_equal(nrow(dt), sum((testSampleMetadata[['entity.contA']] >= 2) * (testSampleMetadata[['entity.contA']] < 6)))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))

  ## With dates
  bin1 <- Bin(binStart=as.Date('1989-01-01'), binEnd=as.Date('1990-01-01'), binLabel='1989')
  bin2 <- Bin(binStart=as.Date('1990-01-01'), binEnd=as.Date('1991-01-01'), binLabel='1990')
  bin3 <- Bin(binStart=as.Date('1991-01-01'), binEnd=as.Date('1992-01-01'), binLabel='1991')
  bin4 <- Bin(binStart=as.Date('1992-01-01'), binEnd=as.Date('1993-01-01'), binLabel='1992')
  groupABins <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'dateA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  # expect_equal(nrow(dt), sum((testSampleMetadata[['entity.dateA']] >= as.Date('1989-01-01')) * (testSampleMetadata[['entity.dateA']] < as.Date('1993-01-01'))))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))

})

test_that("differentialExpression can handle messy inputs", {

  df <- testCountData
  nSamples <- dim(df)[1]
  testSampleMetadataMessy <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T),
    "entity.cat3" = rep(paste0("cat3_", letters[1:3]), nSamples/3, replace=T),
    "entity.cat4" = rep(paste0("cat4_", letters[1:4]), nSamples/4, replace=T),
    "entity.contA" = rnorm(nSamples, sd=5),
    "entity.dateA" = sample(seq(as.Date('1988/01/01'), as.Date('2000/01/01'), by="day"), nSamples)
    ))
  testSampleMetadataMessy$entity.contA[sample(1:nSamples, 50)] <- NA
  testSampleMetadataMessy$entity.cat4[sample(1:nSamples, 50)] <- NA


  testDataMessy <- veupathUtils::CountDataCollection(
              data = df,
              sampleMetadata = SampleMetadata(
                data = testSampleMetadataMessy,
                recordIdColumn = "entity.SampleID"
              ),
              name='test',
              recordIdColumn = 'entity.SampleID')


  # With only some comparisonVariable values found in the metadata
  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_a"
                              ), veupathUtils::Bin(
                                binLabel="cat4_c"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_b"
                              ), veupathUtils::Bin(
                                binLabel="test"
                              ))
                            )
                          )
  )

  result <- differentialExpression(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(sum(testSampleMetadataMessy$entity.cat4 %in% c('cat4_a','cat4_b','cat4_c')), nrow(dt))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # With a continuous variable that has NAs
  bin1 <- veupathUtils::Bin(binStart='2', binEnd='3', binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart='3', binEnd='4', binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart='4', binEnd='5', binLabel="[4, 5)")
  bin4 <- veupathUtils::Bin(binStart='5', binEnd='6', binLabel="[5, 6)")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialExpression(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum((testSampleMetadataMessy[['entity.contA']] >= 2) * (testSampleMetadataMessy[['entity.contA']] < 6), na.rm=T))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))


  # With a categorical variable that has NAs
  bin1 <- veupathUtils::Bin(binLabel="cat4_a")
  bin2 <- veupathUtils::Bin(binLabel="cat4_b")
  bin3 <- veupathUtils::Bin(binLabel="cat4_c")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3)))

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )
  result <- differentialExpression(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=T)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum(testSampleMetadataMessy$entity.cat4 %in% c('cat4_a','cat4_b','cat4_c')))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))


})


test_that("differentialExpression returns a ComputeResult with the correct slots" , {

  testCollection <- testCountDataCollection

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(result@parameters, 'recordIdColumn = entity.SampleID, comparatorColName = entity.binA, method = DESeq, groupA =binA_a, groupB = binA_b')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(class(result@droppedColumns), 'character')
})

test_that("differentialExpression fails with improper inputs", {

  testCollection <- testCountDataCollection
  
  # Fail when bins in Group A and Group B overlap
  bin1 <- veupathUtils::Bin(binStart=2, binEnd=3, binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart=3, binEnd=4, binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart=3, binEnd=5, binLabel="[3, 5)")
  bin4 <- veupathUtils::Bin(binStart=5, binEnd=6, binLabel="[5, 6)")
  groupABins <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- BinList(S4Vectors::SimpleList(c(bin3, bin4)))
  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  expect_error(differentialExpression(testCollection, comparator=comparisonVariable, method='DESeq', verbose=F))

})

test_that("differentialExpression catches deseq errors", {

  df <- testCountData
  nSamples <- dim(df)[1]
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
      )),
    recordIdColumn ="entity.SampleID"
  )

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  # Use only a few taxa
  testCollection <- veupathUtils::CountDataCollection(
              data = df[, c("entity.SampleID","entity.1174-901-12","entity.A2")],
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID',
              name='test'
              )

  expect_error(differentialExpression(testData, comparator=comparisonVariable, method='DESeq', verbose=T))


})


test_that("toJSON for differentialExpressionResult works",{
  
  df <- testCountData
  nSamples <- dim(df)[1]
  df$entity.wowtaxa <- rep(c(3, 1000), nSamples/2, replace=T) # will 'wow' us with its significance
  testSampleMetadata <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
    ))

  testCollection <- veupathUtils::CountDataCollection(
    data = df,
    sampleMetadata = SampleMetadata(
                      data = testSampleMetadata,
                      recordIdColumn = "entity.SampleID"
    ),
    recordIdColumn = 'entity.SampleID',
    name='test'
  )

  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialExpression(testCollection,
              comparator = comparatorVariable,
              method='DESeq',
              verbose=F)
  stats <- result@statistics
  jsonList <- jsonlite::fromJSON(toJSON(result@statistics))

  expect_true(all(c('effectSizeLabel', 'statistics', 'pValueFloor', 'adjustedPValueFloor') %in% names(jsonList)))
  expect_true(all(c('effectSize', 'pValue', 'adjustedPValue', 'pointID') %in% names(jsonList$statistics)))
  expect_true(is.character(jsonList$statistics$effectSize))
  expect_true(is.character(jsonList$statistics$pValue))
  expect_true(is.character(jsonList$statistics$adjustedPValue))
  expect_true(is.character(jsonList$pValueFloor))
  expect_true(is.character(jsonList$adjustedPValueFloor))
})

test_that("The smallest pvalue we can get is our p value floor", {

    df <- testCountData
  nSamples <- dim(df)[1]
  df$entity.wowtaxa <- rep(c(3, 10000), nSamples/2, replace=T) # will 'wow' us with its significance
  testSampleMetadata <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
    ))

  testCollection <- veupathUtils::CountDataCollection(
    data = df,
    sampleMetadata = SampleMetadata(
                      data = testSampleMetadata,
                      recordIdColumn = "entity.SampleID"
    ),
    recordIdColumn = 'entity.SampleID',
    name='test'
  )

  # A Binary comparator variable
  comparatorVariable <- veupathUtils::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  # Try with different p value floors
  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', pValueFloor = 0, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), 0)
  expect_equal(min(result@statistics@statistics$adjustedPValue, na.rm=T), 0) # Confirmed NAs are for pvalue=1

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', pValueFloor = P_VALUE_FLOOR, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), P_VALUE_FLOOR)
  expect_equal(min(result@statistics@statistics$adjustedPValue, na.rm=T), result@statistics@adjustedPValueFloor) # Confirmed NAs are for pvalue=1


})

test_that("differentialExpression fails if comparator has one value", {

  df <- testCountData
  
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = rep(c("binA"), nrow(df))
    )),
    recordIdColumn ="entity.SampleID"
  )

  testCollection <- veupathUtils::CountDataCollection(
    data = df,
    sampleMetadata = sampleMetadata,
    recordIdColumn = 'entity.SampleID',
    name='test'
  )

  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'binA',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA")))),
    groupB = veupathUtils::BinList(S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binB"))))
  )

  expect_error(differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F))
  expect_error(differentialExpression(testCollection, comparator=comparatorVariable, method='Maaslin', verbose=F))
})

# ===== Tests for limma method with ArrayDataCollection =====

test_that('differentialExpression with limma returns correctly formatted results', {

  testCollection <- testArrayDataCollection

  # Binary comparator
  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'treatment',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(
      S4Vectors::SimpleList(
        c(veupathUtils::Bin(binLabel="treatment_A"))
      )
    ),
    groupB = veupathUtils::BinList(
      S4Vectors::SimpleList(
        c(veupathUtils::Bin(binLabel="treatment_B"))
      )
    )
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='limma', verbose=F)

  # Check result structure
  expect_equal(length(result@droppedColumns), 0)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')

  # Check statistics
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))
})

test_that('differentialExpression with limma handles continuous comparator', {

  testCollection <- testArrayDataCollection

  # Continuous comparator (age)
  bin1 <- veupathUtils::Bin(binStart='35', binEnd='45', binLabel="[35, 45)")
  bin2 <- veupathUtils::Bin(binStart='45', binEnd='55', binLabel="[45, 55)")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin2)))

  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'age',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="CONTINUOUS")
    ),
    groupA = groupABins,
    groupB = groupBBins
  )

  result <- differentialExpression(testCollection, comparator=comparatorVariable, method='limma', verbose=F)

  dt <- result@data
  expect_s3_class(dt, 'data.table')
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
})

test_that('differentialExpression fails when using DESeq with ArrayDataCollection', {

  testCollection <- testArrayDataCollection

  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'treatment',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="treatment_A")))
    ),
    groupB = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="treatment_B")))
    )
  )

  # Should fail - ArrayDataCollection requires limma, not DESeq
  expect_error(
    differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F),
    "CountDataCollection"
  )
})

test_that('differentialExpression fails when using limma with CountDataCollection', {

  testCollection <- testCountDataCollection

  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'binA',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA_a")))
    ),
    groupB = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA_b")))
    )
  )

  # Should fail - CountDataCollection requires DESeq, not limma
  expect_error(
    differentialExpression(testCollection, comparator=comparatorVariable, method='limma', verbose=F),
    "ArrayDataCollection"
  )
})

test_that('differentialExpression backwards compatibility maintained', {

  # Existing tests should still pass
  testCollection <- testCountDataCollection

  comparatorVariable <- veupathUtils::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'binA',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA_a")))
    ),
    groupB = veupathUtils::BinList(
      S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA_b")))
    )
  )

  # Default method should still be DESeq
  result1 <- differentialExpression(testCollection, comparator=comparatorVariable, verbose=F)
  result2 <- differentialExpression(testCollection, comparator=comparatorVariable, method='DESeq', verbose=F)

  expect_equal(result1@statistics@statistics, result2@statistics@statistics)
})