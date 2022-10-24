#maybe break this into multiple tests? its getting long..
test_that("VariableMetadata validation works", {
    #support multiple variable classes IFF one of them is 'collection'
    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed", "collection")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c'),
                 members = new("VariableSpecList", S4Vectors::SimpleList(
                    new("VariableSpec", variableId = 'a', entityId = 'b'),
                    new("VariableSpec", variableId = 'c', entityId = 'd')
                 ))
            )
    expect_equal(vm@variableClass@value, c('computed', 'collection'))

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed", "derived")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c')
            )
    )
    
    #no strings w numeric display ranges, vice versa
    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1,
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 'a',
                 displayRangeMax = 'b',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = as.Date('2022-1-1'),
                 displayRangeMax = as.Date('2022-12-31'),
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c')
            )
    )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = '2022-1-1',
                 displayRangeMax = '2022-12-31',
                 dataType = new("DataType", value = 'DATE'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    expect_equal(vm@dataType@value, 'DATE')
    expect_equal(is.character(vm@displayRangeMin), TRUE)
    expect_equal(is.character(vm@displayRangeMax), TRUE)

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1.1,
                 displayRangeMax = 10.123,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    expect_equal(vm@dataType@value, 'NUMBER')
    expect_equal(is.numeric(vm@displayRangeMin), TRUE)
    expect_equal(is.numeric(vm@displayRangeMax), TRUE)

    #non native vars have display ranges etc
    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayRangeMin = 1,
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "derived"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a derived variable',
                 displayRangeMin = 1,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    expect_equal(vm@displayName, NA_character_)
    expect_equal(vm@displayRangeMin, NA_real_)
    expect_equal(vm@displayRangeMax, NA_real_)

    #only collections have members
    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "collection"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a collection variable',
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed", "collection")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a collection variable',
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("derived")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a collection variable',
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS'),
                 members = new("VariableSpecList", S4Vectors::SimpleList(
                    new("VariableSpec", variableId = 'a', entityId = 'b'),
                    new("VariableSpec", variableId = 'c', entityId = 'd')
                 ))
            )
    )

    #only strings have vocabs
    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "collection"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a collection variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    )

    expect_error(new("VariableMetadata",
                 variableClass = new("VariableClass", value = "collection"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a collection variable',
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS'),
                 vocabulary = c('a', 'b', 'c')
            )
    )
})

test_that("toJSON result is properly formatted", {
    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed", "collection")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1,
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS'),
                 members = new("VariableSpecList", S4Vectors::SimpleList(
                    new("VariableSpec", variableId = 'a', entityId = 'b'),
                    new("VariableSpec", variableId = 'c', entityId = 'd')
                 ))
            )
    vmjson <- veupathUtils::toJSON(vm)
    vmlist <- jsonlite::fromJSON(vmjson)

    expect_equal(names(vmlist), 'variableMetadata')
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'displayName', 'displayRangeMin', 'displayRangeMax', 'dataType', 'dataShape', 'members'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed', 'collection'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    #should display ranges always be strings as json??
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(length(vmlist$variableMetadata$members), 2)
    #this becomes data frame w 2 rows
    expect_equal(names(vmlist$variableMetadata$members), c('variableId', 'entityId'))

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'BINARY'),
                 vocabulary = c('a', 'b')
            )
    vmjson <- veupathUtils::toJSON(vm)
    vmlist <- jsonlite::fromJSON(vmjson)

    expect_equal(names(vmlist), 'variableMetadata')
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'displayName', 'dataType', 'dataShape', 'vocabulary'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    #should display ranges always be strings as json??
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(length(vmlist$variableMetadata$vocabulary), 2)
})