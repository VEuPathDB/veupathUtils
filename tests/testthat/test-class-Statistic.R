test_that("Statistic validation works", {
    # only name and value slot is required, and takes a single value
    expect_error(new("Statistic",
                     "value" = NA_real_))
    expect_error(new("Statistic",
                     "name" = NA_character_,
                     "value" = 123))
    expect_error(new("Statistic",
                     "name" = c("foo", "bar"),
                     "value" = 123))
    expect_error(new("Statistic",
                     "name" = "foo",
                     "value" = c(123, 456)))

    stat <- new("Statistic", 
                "name" = "PI", 
                "value" = 3.141592)

    expect_equal(stat@pvalue, NA_character_)
    expect_equal(stat@confidenceInterval@minimum, NA_real_)
    expect_equal(stat@confidenceLevel, NA_real_)

    #pvalue must have a single value between 0 and 1
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "pvalue" = 1.2))
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "pvalue" = -0.05))
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "pvalue" = c(0.001, 0.0001)))

    #if CI provided, value is within it
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "confidenceInterval" = Range("minimum" = 0.001, "maximum" = 0.0001)))
    
    #CI has CI level
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "confidenceInterval" = Range("minimum" = 0.001, "maximum" = 10)))
    
    #CI level is between 0 and 1 and is accompanied by a CI
    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "confidenceInterval" = Range("minimum" = 0.001, "maximum" = 0.0001),
                     "confidenceLevel" = 1.2))

    expect_error(new("Statistic", 
                     "name" = "PI", 
                     "value" = 3.141592,
                     "confidenceLevel" = .95))
})

test_that("toJSON result is properly formatted for Statistic", {
    stat <- new("Statistic", 
                "name" = "PI", 
                "value" = 3.141592,
                "confidenceInterval" = Range("minimum" = 0.001, "maximum" = 10),
                "confidenceLevel" = .95,
                "pvalue" = '<0.0001')

    statjson <- veupathUtils::toJSON(stat)
    statlist <- jsonlite::fromJSON(statjson)

    expect_equal(names(statlist), 'PI')
    expect_equal(names(statlist$PI), c('value', 'confidenceInterval', 'confidenceLevel', 'pvalue'))
    expect_equal(statlist$PI$confidenceInterval, c('(0.001 - 10)'))

    stat2 <- new("Statistic", 
                "name" = "PI2", 
                "value" = 3.141592,
                "confidenceInterval" = Range("minimum" = 1, "maximum" = 5),
                "confidenceLevel" = .95,
                "pvalue" = ".47")
    stats <- StatisticList(S4Vectors::SimpleList(stat, stat2))

    statsjson <- veupathUtils::toJSON(stats)
    statslist <- jsonlite::fromJSON(statsjson)

    expect_equal(names(statslist), 'statistics')
    expect_equal(names(statslist$statistics), c('PI', 'PI2'))
    expect_equal(names(statslist$statistics$PI2), c('value', 'confidenceInterval', 'confidenceLevel', 'pvalue'))
    expect_equal(statslist$statistics$PI2$confidenceInterval, c(NA, '(1 - 5)'))
    expect_equal(class(statslist$statistics$PI2$pvalue[2]), 'character')
})
