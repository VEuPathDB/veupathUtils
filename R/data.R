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
