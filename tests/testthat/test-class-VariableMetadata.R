test_that("VariableMetadata validation works", {
    #support multiple variable classes
    #no strings w numeric display ranges, vice versa
    #non native vars have display ranges etc
    #collections have members
    #strings have vocabs
})

test_that("toJSON result is properly formatted", {
    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "computed"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'CATEGORICAL'),
                 vocabulary = c('a', 'b', 'c')
            )

    
})