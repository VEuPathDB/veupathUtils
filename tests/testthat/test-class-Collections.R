test_that("we can create a new Collection", {
    # an empty one
    collection <- Collection()
    expect_s4_class(collection, "Collection")

    # a manually populated one/ from a data frame
    collection <- Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y")
    expect_s4_class(collection, "Collection")

    # from a file
    fileName <- test_path('testdata','collection.tab') 
    collection <- Collection("my collection", fileName, "entity.id", "ancestor.y")
    expect_s4_class(collection, "Collection")
})

test_that("Collection validation works", {
    # empty name fails
    expect_error(Collection("", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y"))

    # empty data fails
    expect_error(Collection("my collection", data.frame(), "entity.id", "ancestor.y"))

    # empty id column fails
    expect_error(Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "", "ancestor.y"))
    expect_error(Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", ""))
    expect_error(Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id"))

    # id column not in data fails
    expect_error(Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2), "entity.z"))
    expect_error(Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2), "entity.id", "ancestor.y"))
})

test_that("we can make Collections", {
    # an empty one
    Collections <- Collections()
    expect_s4_class(Collections, "Collections")

    # from a list of Collections
    Collections <- Collections(list(
        Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y"), 
        Collection("my collection 2", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y")
    ))
    expect_s4_class(Collections, "Collections")

    # from a file
    file1 <- test_path('testdata','DiabImmune/DiabImmune_entity_16SRRNAV4Assay.txt')
    Collections <- Collections(file1)
    expect_s4_class(Collections, "Collections")
    
    # from a data frame
    df <- data.table::fread(file1)
    Collections <- Collections(df)
    expect_s4_class(Collections, "Collections")

    # from a list of files
    file2 <- test_path('testdata','DiabImmune/DiabImmune_MetagenomicSequencingAssay.txt')
    Collections <- Collections(list(file1, file2))
    expect_s4_class(Collections, "Collections")

    # from a list of data frames
    df2 <- data.table::fread(file2)
    Collections <- Collections(list(df, df2))
    expect_s4_class(Collections, "Collections")
})

test_that("Collection validation works", {
    # duplicate names fails
    expect_error(Collections(list(
        Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y"),
        Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y")
    )))

    # a collection w different ancestorIdColumns fails
    expect_error(Collections(list(
        Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.y = 1), "entity.id", "ancestor.y"),
        Collection("my collection", data.frame(entity.id = 1, entity.collection_x = 1, entity.collection_y = 2, ancestor.z = 1), "entity.id", "ancestor.z")
    )))
})