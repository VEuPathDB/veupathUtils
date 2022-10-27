#maybe break this into multiple tests? its getting long..
test_that("VariableMetadata validation works", {
    #only a single class is allowed
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

test_that("getColName returns column names or NULL", {
  variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def')
  expect_equal(getColName(variableSpec), 'def.abc')

  variableSpec = new("VariableSpec", variableId = '', entityId = 'def')
  expect_error(getColName(variableSpec))

  variableSpec = new("VariableSpec", variableId = 'abc', entityId = '')
  expect_equal(getColName(variableSpec), 'abc')

  variableSpec = new("VariableSpec", variableId = NA_character_, entityId = 'def')
  expect_error(getColName(variableSpec))

  variableSpec = new("VariableSpec", variableId = 'abc', entityId = NA_character_)
  expect_equal(getColName(variableSpec), 'abc')

  expect_equal(getColName(NULL), NULL)
})

test_that("toJSON result is properly formatted", {
    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1,
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS'),
                 isCollection = TRUE,
                 imputeZero = TRUE,
                 members = new("VariableSpecList", S4Vectors::SimpleList(
                    new("VariableSpec", variableId = 'a', entityId = 'b'),
                    new("VariableSpec", variableId = 'c', entityId = 'b')
                 ))
            )
    vmjson <- veupathUtils::toJSON(vm)
    vmlist <- jsonlite::fromJSON(vmjson)

    expect_equal(names(vmlist), 'variableMetadata')
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'plotReference', 'displayName', 'displayRangeMin', 'displayRangeMax', 'dataType', 'dataShape', 'isCollection', 'imputeZero', 'members'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    expect_equal(class(vmlist$variableMetadata$displayRangeMin), 'character')
    expect_equal(class(vmlist$variableMetadata$displayRangeMin), 'character')
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(vmlist$variableMetadata$imputeZero, TRUE)
    expect_equal(vmlist$variableMetadata$isCollection, TRUE)
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
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'plotReference', 'displayName', 'dataType', 'dataShape', 'vocabulary', 'isCollection', 'imputeZero'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(vmlist$variableMetadata$isCollection, FALSE)
    expect_equal(vmlist$variableMetadata$isCollection, FALSE)
    expect_equal(length(vmlist$variableMetadata$vocabulary), 2)
})