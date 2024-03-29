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