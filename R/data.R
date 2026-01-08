#' Some Real Life Popbio Megastudy Test Data
#'
#' It is the data used in the subset associated w this saved analysis:
#' https://vectorbase.org/vectorbase/app/workspace/maps/A4nuqcm/import
#' as of 9 Jan 2024.
#' As of the time of writing the filters in this analysis are like this:
#' "filters": [
#'    {
#'      "entityId": "EUPATH_0000605",
#'      "variableId": "POPBIO_8000215",
#'      "stringSet": [
#'        "VBP0000844"
#'      ],
#'      "type": "stringSet"
#'    },
#'    {
#'      "entityId": "GAZ_00000448",
#'      "variableId": "EUPATH_0000542",
#'      "stringSet": [
#'        "Canyon Rim Church"
#'      ],
#'      "type": "stringSet"
#'    },
#'    {
#'      "variableId": "EUPATH_0043256",
#'      "entityId": "OBI_0000659",
#'      "type": "dateRange",
#'      "min": "2022-05-01T00:00:00Z",
#'      "max": "2022-11-01T00:00:00Z"
#'    },
#'    {
#'      "type": "numberRange",
#'      "variableId": "OBI_0001620",
#'      "entityId": "GAZ_00000448",
#'      "min": 40.698307645373106,
#'      "max": 40.71193901146775
#'    },
#'    {
#'      "type": "longitudeRange",
#'      "variableId": "OBI_0001621",
#'      "entityId": "GAZ_00000448",
#'      "left": -111.8212938308716,
#'      "right": -111.79749727249147
#'    }
#'  ]
#'
#' @format ## `megastudyDataReal`
#' A data frame with 14 rows and 9 columns:
#' \describe{
#'   \item{EUPATH_0000609.Sample_stable_id}{Stable ID for the `Sample` entity}
#'   \item{OBI_0000659.ParentOfSample_stable_id}{Stable ID for the `Collections` entity}
#'   \item{GAZ_00000448.GeographicLocation_stable_id}{Stable ID for the `Collection sites` entity}
#'   \item{EUPATH_0000605.Study_stable_id}{Stable ID for the `Studies` entity}
#'   \item{EUPATH_0000609.PATO_0000047}{Sample -> Sex}
#'   \item{EUPATH_0000609.POPBIO_8000017}{Sample -> Unbiased Specimen Count}
#'   \item{EUPATH_0000609.OBI_0001909}{Sample -> Species}
#'   \item{EUPATH_0000609.EUPATH_0043227}{Sample -> Female Insect Feeding Status}
#'   \item{EUPATH_0000609.UBERON_0000105}{Sample -> Life Cycle Stage}
#' }
#' @source <https://vectorbase.org/vectorbase/app/workspace/maps/A4nuqcm/import>
"megastudyDataReal"

#' Test Array Data for Antibody Array Analysis
#'
#' A simulated antibody array dataset with continuous expression values.
#' Data includes both positive and negative values representing log-transformed
#' normalized expression levels. This data is used for testing limma-based
#' differential expression analysis.
#'
#' @format ## `testArrayData`
#' A data.table with 50 samples and 100 antibody targets plus sample ID column:
#' \describe{
#'   \item{entity.SampleID}{Sample identifier}
#'   \item{entity.antibody_*}{Continuous expression values for various antibody targets (log2-scale, mean 0, sd 2)}
#' }
"testArrayData"

#' Test Array Data Collection
#'
#' An ArrayDataCollection object created from testArrayData with associated sample metadata.
#' This object is used for testing the ArrayDataCollection class and limma-based differential
#' expression methods.
#'
#' @format ## `testArrayDataCollection`
#' An ArrayDataCollection object with:
#' \describe{
#'   \item{data}{50 samples x 100 antibody targets (continuous expression values)}
#'   \item{sampleMetadata}{Includes treatment, batch, age, and sex variables}
#'   \item{recordIdColumn}{"entity.SampleID"}
#' }
#'
#' Sample metadata variables:
#' \describe{
#'   \item{entity.treatment}{Binary treatment variable (treatment_A, treatment_B)}
#'   \item{entity.batch}{Batch variable (batch_1 through batch_5)}
#'   \item{entity.age}{Continuous age variable (normal distribution, mean 45, sd 10)}
#'   \item{entity.sex}{Binary sex variable (male, female)}
#' }
"testArrayDataCollection"
