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
test_that("Collection accepts member ids that contain periods", {
    # as produced by pivoting tall gene data wide: the entity prefix is on the id columns only
    # but each data column is 'entity.memberId', and memberId itself may contain periods.
    # Even and odd numbers of members exercise both failure modes seen in production.
    for (nGenes in c(4, 5)) {
        for (suffix in c(".1", ".1.2", ".1.2.3")) {
            geneIds <- paste0("entity.PF3D7_", seq_len(nGenes), suffix)
            df <- data.frame(entity.id = 1:2, ancestor.y = 1:2)
            for (g in geneIds) df[[g]] <- c(1, 2)
            expect_no_error(
                Collection("genes", df, "entity.id", "ancestor.y"),
                message = paste("nGenes =", nGenes, "suffix =", suffix)
            )
        }
    }
})

test_that("Collection still rejects members from different entities, even with periods in ids", {
    df <- data.frame(entity.id = 1:2, ancestor.y = 1:2,
                     entityA.PF3D7_1.1 = c(1, 2), entityB.PF3D7_2.1 = c(1, 2),
                     check.names = FALSE)
    expect_error(Collection("genes", df, "entity.id", "ancestor.y"), "same entity")
    df2 <- data.frame(entity.id = 1:2, ancestor.y = 1:2,
                      entityA.gene1 = c(1, 2), entityB.gene2 = c(1, 2))
    expect_error(Collection("genes", df2, "entity.id", "ancestor.y"), "same entity")
})
