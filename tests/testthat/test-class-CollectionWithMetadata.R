test_that('CollectionWithMetadata validation works', {

    nSamples <- 200
    sampleMetadataDT <- data.table::data.table(
        "entity.SampleID" = 1:nSamples,
        "entity.contA" = rnorm(nSamples),
        "entity.contB" = rnorm(nSamples),
        "entity.contC" = rnorm(nSamples)
    )

    df <- data.table::data.table(
        "entity.SampleID" = sampleMetadataDT$entity.SampleID,
        "entity.cont1" = rnorm(nSamples),
        "entity.cont2" = rnorm(nSamples),
        "entity.cont3" = rnorm(nSamples)
    )

    ## no record id provided
    expect_error(CollectonWithMetadata(
                data = df))

    ## too many record ids 
    expect_error(CollectionWithMetadata(
                data = df,
                recordIdColumn = c('entity.SampleID', 'test')))

    ## invalid record id
    expect_error(CollectionWithMetadata(
                data = df,
                recordIdColumn = c('test')))

    ## invalid ancestor id
    expect_error(CollectionWithMetadata(
                data = df,
                recordIdColumn = c('entity.SampleID'),
                ancestorIdColumns = c('test')))

    df$entity.strings <- 'a'

    ## contains a non-numeric data column
    expect_error(CollectionWithMetadata(
                data = df,
                recordIdColumn = c('entity.SampleID')))

    df$entity.strings <- .1
    names(df)[names(df) == 'entity.strings'] <- 'entity2.notstrings'

    ## collection contains multiple entities
    expect_error(CollectionWithMetadata(
                data = df,
                recordIdColumn = c('entity.SampleID')))

    sampleMetadata <- SampleMetadata(
        data = sampleMetadataDT[, 'entity.SampleID', with=F],
        recordIdColumn = 'entity.SampleID'
    )

    ## sampleMetadata exists but is empty
    expect_error(
        CollectionWithMetadata(
        data = df,
        sampleMetadata = sampleMetadata,
        recordIdColumn = 'entity.SampleID'
        )
    )

    df$entity2.notstrings <- NULL
    testing <- CollectionWithMetadata(
                name = 'testing',
                data = df,
                recordIdColumn = c('entity.SampleID'))

    ## works w empty sampleMetadata slot
    expect_true(inherits(testing, 'CollectionWithMetadata'))
    expect_equal(slotNames(testing), c('sampleMetadata', 'name', 'data', 'recordIdColumn', 'ancestorIdColumns', 'imputeZero', 'removeEmptyRecords'))
    expect_equal(nrow(testing@data), 200)
    expect_equal(ncol(testing@data), 4)
    expect_equal(testing@recordIdColumn, 'entity.SampleID')
    expect_equal(testing@ancestorIdColumns, character(0))
    expect_true(testing@imputeZero)

    sampleMetadata <- SampleMetadata(
        data = sampleMetadataDT,
        recordIdColumn = 'entity.SampleID'
    )

    testing <- CollectionWithMetadata(
                name = 'testing',
                data = df,
                sampleMetadata = sampleMetadata,
                recordIdColumn = 'entity.SampleID'
                )

    ## works w filled sampleMetadata slot
    expect_true(inherits(testing, 'CollectionWithMetadata'))
    expect_equal(slotNames(testing), c('sampleMetadata', 'name', 'data', 'recordIdColumn', 'ancestorIdColumns', 'imputeZero', 'removeEmptyRecords'))
    expect_equal(nrow(testing@data), 200)
    expect_equal(ncol(testing@data), 4)
    expect_equal(testing@recordIdColumn, 'entity.SampleID')
    expect_equal(testing@ancestorIdColumns, character(0))
    expect_true(testing@imputeZero)
})

test_that("getCollectionValues works", {
    nSamples <- 200
    df <- data.table::data.table(
        "entity.SampleID" = 1:nSamples,
        "entity.cont1" = rnorm(nSamples),
        "entity.cont2" = rnorm(nSamples),
        "entity.cont3" = rnorm(nSamples)
    )

    testing <- CollectionWithMetadata(
        name = 'testing',
        data = df,
        recordIdColumn = 'entity.SampleID'
    )

    values <- getCollectionValues(testing)
    expect_equal(nrow(values), nrow(df))
    expect_equal(ncol(values), ncol(df))

    ## remove ids
    values <- getCollectionValues(testing, includeIds = FALSE)
    expect_equal(nrow(values), nrow(df))
    expect_equal(ncol(values), ncol(df) - 1)

    ## remove empty samples
    df <- rbind(df,df[nrow(df)+1,])
    df$entity.SampleID[nrow(df)] <- 'im.a.sample'
    testing <- CollectionWithMetadata(
        name = 'testing',
        data = df,
        recordIdColumn = 'entity.SampleID',
        removeEmptyRecords = TRUE
    )

    values <- getCollectionValues(testing)
    expect_equal(nrow(values), nrow(df) -1)
    expect_equal(ncol(values), ncol(df))
})

test_that("pruneFeatures works", {
    nSamples <- 200
    df <- data.table::data.table(
        "entity.SampleID" = 1:nSamples,
        "entity.cont1" = 1:nSamples,
        "entity.cont2" = 201:(200+nSamples),
        "entity.cont3" = c(rep(0,(nSamples*.75)),rnorm(nSamples*.25))
    )
    
    testing <- CollectionWithMetadata(
        name = 'testing',
        data = df,
        recordIdColumn = 'entity.SampleID'
    )

    # pruneFeatures touched SampleMetadata, which this CollectionWithMetadata object has none. it shouldnt fail for that though.
    testing <- pruneFeatures(testing, veupathUtils::predicateFactory('proportionNonZero', 0.5))
    expect_equal(nrow(testing@data), 200)
    expect_equal(ncol(testing@data), 3)
})