# Script to generate test array data for antibody array analysis
# This creates testArrayData and testArrayDataCollection objects

library(data.table)
library(veupathUtils)

set.seed(12345)
nSamples <- 50
nAntibodies <- 100

# Create sample IDs
sampleIds <- paste0("sample_", sprintf("%03d", 1:nSamples))

# Create continuous expression data (log2-scale, mean 0, sd 2)
# Simulating pre-normalized data from an antibody array
antibodyData <- matrix(
  rnorm(nSamples * nAntibodies, mean = 0, sd = 2),
  nrow = nSamples,
  ncol = nAntibodies
)
colnames(antibodyData) <- paste0("entity.antibody_", sprintf("%03d", 1:nAntibodies))

# Create data.table
testArrayData <- data.table(
  entity.SampleID = sampleIds,
  antibodyData
)

# Create sample metadata
testArraySampleMetadata <- data.frame(
  entity.SampleID = sampleIds,
  entity.treatment = rep(c("treatment_A", "treatment_B"), nSamples/2),
  entity.batch = rep(paste0("batch_", 1:5), each = nSamples/5),
  entity.age = rnorm(nSamples, mean = 45, sd = 10),
  entity.sex = sample(c("male", "female"), nSamples, replace = TRUE),
  stringsAsFactors = FALSE
)

# Create ArrayDataCollection
testArrayDataCollection <- ArrayDataCollection(
  data = testArrayData,
  sampleMetadata = SampleMetadata(
    data = testArraySampleMetadata,
    recordIdColumn = "entity.SampleID"
  ),
  recordIdColumn = "entity.SampleID",
  name = "testArrayData"
)

# Save data objects
save(testArrayData, file = "data/testArrayData.rda")
save(testArrayDataCollection, file = "data/testArrayDataCollection.rda")

cat("Test array data generated successfully!\n")
cat("  - testArrayData:", nrow(testArrayData), "samples x", ncol(testArrayData)-1, "antibodies\n")
cat("  - testArrayDataCollection created and saved\n")
