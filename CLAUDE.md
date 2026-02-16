# veupathUtils - Project Guide for Claude Code Sessions

## Project Overview

**veupathUtils** is an R package providing helper functions for VEuPathDB projects. It's used as a dependency in other R packages like plot.data and microbiomeComputations.

- **Version**: 2.9.0
- **License**: Apache 2.0
- **Language**: R (>= 2.10)
- **Repository**: https://github.com/VEuPathDB/veupathUtils

## Development Environment

### Docker Container Setup

Development is done in a **study-wrangler-dev** Docker container to ensure consistent R versioning.

The container setup (from README.md; note that the user will usually already have started this container):
```bash
cd /path/to/study-wrangler
screen -S RStudio docker run --rm -ti --name study-wrangler-dev \
  -v $PWD:/study.wrangler \
  -v ~/work/EDA/veupathUtils:/home/rstudio/veupathUtils \
  -e PASSWORD=password -p 8888:8787 veupathdb/study-wrangler
```

**Important paths**:
- Host: `~/work/EDA/veupathUtils`
- Container: `/home/rstudio/veupathUtils`
- Container is assumed to be running and accessible

### Running Code in Container

Execute R commands in the container:
```bash
docker exec study-wrangler-dev Rscript -e "setwd('/home/rstudio/veupathUtils'); library(devtools); test()"
```

**IMPORTANT - File Ownership:** When creating files in the container (e.g., test data, generated files), always use the `--user` flag to avoid root ownership issues:
```bash
# Correct - creates files owned by rstudio user (UID 1000)
docker exec --user rstudio study-wrangler-dev Rscript -e "..."

# Or use UID:GID directly
docker exec --user 1000:1000 study-wrangler-dev Rscript -e "..."

# Incorrect - creates files owned by root
docker exec study-wrangler-dev Rscript -e "..."
```

Files created without `--user` flag will be owned by `root:root` on the host, causing permission problems. The rstudio user in the container is UID 1000, GID 1000.

**Interactive development** (in RStudio console within container):
```r
library(devtools)
setwd("~/veupathUtils")

test()        # Run test suite
load_all()    # Load package for interactive work
document()    # Update documentation
```

## Testing

### Running Tests

**Option 1: Direct execution** (fast but uses ~9k tokens of context for output):
```bash
docker exec study-wrangler-dev Rscript -e "setwd('/home/rstudio/veupathUtils'); library(devtools); test()"
```

**Option 2: Using Task agent** (recommended for long test runs to save context):
Use a Task with subagent_type='general-purpose' to run tests and summarize results.

### Test Suite Status

- **Runtime**: 5+ minutes
- **Last known results**: [ FAIL 0 | WARN 35 | SKIP 0 | PASS 686 ] (occasional FAIL 1 is a flaky Megastudy performance benchmark, not a functional failure)
- **Test files**: Located in `tests/testthat/`
- **Largest test files**:
  - `test-class-Megastudy.R` (862 lines)
  - `test-method-differentialExpression.R` (704 lines - includes DESeq2 and limma tests)
  - `test-correlation.R` (509 lines)

## Project Structure

### Main Directories

```
veupathUtils/
├── R/                      # Source code
│   ├── class-*.R          # S4 class definitions
│   ├── method-*.R         # Analysis methods (DE, correlation, PCA)
│   ├── methods-*.R        # Methods for specific classes
│   └── utils-*.R          # Utility functions
├── tests/testthat/        # Test files
├── man/                   # Generated documentation
├── data/                  # Package data
├── .dev/                  # Development helpers
└── .github/workflows/     # CI/CD (R-CMD-check.yaml)
```

### Key Components

**Classes** (S4):
- `Bin`, `Range` - Data binning and ranges
- `Collections`, `CollectionWithMetadata` - Data collections
- `SampleMetadata`, `VariableMetadata` - Metadata handling
- `CountDataCollection` - Count data (RNA-seq, etc.) for DESeq2
- `ArrayDataCollection` - Continuous expression data (microarray, antibody array) for limma
- `Megastudy` - Large study data aggregation
- `Statistic`, `Comparator` - Statistical operations
- `ComputeResult`, `CorrelationResult` - Result containers

**Methods**:
- `method-differentialExpression.R` - Differential expression with DESeq2 (counts) or limma (arrays)
- `method-correlation.R` - Correlation analysis
- `method-pca.R` - Principal component analysis

**Utilities**:
- `utils.R` - General utilities
- `utils-string.R` - String manipulation
- `utils-numeric.R` - Numeric operations and binning
- `utils-cut.R` - Data cutting/discretization
- `utils-classes.R` - Class utilities

## Dependencies

### Key R Packages

From DESCRIPTION:
- **Bioconductor** (3.22): SummarizedExperiment, DESeq2, limma
- **Data handling**: data.table, S4Vectors, purrr
- **Statistics**: boot, Hmisc
- **Network**: SpiecEasi (v1.0.7)
- **Utilities**: digest, stringi, microbenchmark
- **JSON**: jsonlite (Depends)

### Installing the Package

```r
# From GitHub
remotes::install_github('VEuPathDB/veupathUtils')

# For development (if modifying veupathUtils itself)
devtools::load_all("/path/to/veupathUtils")
```

## Git Workflow

### Branching

- **Main branch**: `main`

### Active/Recent Branches

- `improve-integer-checks` (current - fix CountDataCollection integer type validation)
- `add-ab-array-limma` (limma implementation complete)
- `add-bioc-to-remotes`
- `add-percent-variance-pca`
- `noSpiecEasi`

### Making Commits

Standard git workflow. Package uses GitHub Actions for CI (R-CMD-check; **currently disabled** via GitHub web UI; has been failing for months needs fixing at some point).

## Development Guidelines

From README.md:

1. **Export functions**: As a general policy, export every function added (unless very good reason not to)
2. **Update tests**: Always update tests for changes
3. **Documentation**: Use roxygen2 comments (`#'`)
4. **Run `devtools::document()`** after documentation changes

### Adding veupathUtils as a Dependency

For other packages to depend on veupathUtils:

1. Add `veupathUtils` to `Imports` in DESCRIPTION
2. Add `Remotes` section to DESCRIPTION
3. Add `VEuPathDB/veupathUtils` to `Remotes` section (best to pin a specific version)
4. Add `#' @import veupathUtils` to package-level documentation file
5. Run `devtools::document()`

## Common Tasks

### Running Tests
```bash
docker exec study-wrangler-dev Rscript -e "setwd('/home/rstudio/veupathUtils'); library(devtools); test()"
```

### Building Documentation
```bash
docker exec study-wrangler-dev Rscript -e "setwd('/home/rstudio/veupathUtils'); library(devtools); document()"
```

### Checking Package
```bash
docker exec study-wrangler-dev Rscript -e "setwd('/home/rstudio/veupathUtils'); library(devtools); check()"
```

### Loading Package Interactively
```r
# In RStudio/R console within container
library(devtools)
load_all("~/veupathUtils")
```

## Debugging Live Computation Issues

When a bug involves data flowing from the Java EDA compute service through Rserve into veupathUtils, the most effective approach is to retrieve the actual input data from MinIO and reproduce the Java code's R operations locally.

### Step 1: Trigger a Compute Job and Get the Input Data from MinIO

The EDA compute service writes computation inputs to MinIO when a job is submitted. Trigger one via the web frontend:

```bash
# From ~/work/EDA/web-monorepo
EDA_SERVICE_URL=http://localhost:8081 yarn nx start @veupathdb/genomics-site
```

Navigate to the differential expression visualisation and configure the analysis you're debugging (same gene expression variable, same comparison group definitions each time). For easiest navigation, **clear the MinIO bucket before testing** so only one job's data is present:

```bash
podman exec -it eda-minio-local mc rm --recursive --force local/eda-compute-dev/
```

**Locating and downloading the file:**
- Web UI: http://localhost:9001
- Username: `eda-local-user`
- Password: value of the `eda_S3_SECRET_KEY_local` podman secret (default `local-dev-secret` for local dev — see `webservices-quadlets/README-eda-local-dev.md` Step 4)
- Bucket: `eda-compute-dev`
- With a near-empty bucket, browse to the one object and download it (a tab-separated file)

Copy it into the container:

```bash
docker exec study-wrangler-dev mkdir -p /home/rstudio/Downloads
docker cp ~/Downloads/differential_expression_input study-wrangler-dev:/home/rstudio/Downloads/
```

### Step 2: Write a Debug Script in study-wrangler/tmp/

Scripts placed in `~/work/EDA/study-wrangler/tmp/` are accessible inside the container at `/study.wrangler/tmp/`. Edit on the host, run in the container — no copying needed:

```bash
docker exec --user rstudio study-wrangler-dev Rscript /study.wrangler/tmp/my_debug_script.R
```

### Step 3: Reproduce the Java Plugin's R Operations

Find the plugin source (e.g., `../service-eda/.../DifferentialExpressionPlugin.java`) and look for `voidEval()` and `eval()` calls — these are the exact R commands issued via Rserve. Copy them into the debug script in order, using `fread()` to load the downloaded TSV as the starting data.

**Template:**

```r
library(data.table)
library(devtools)

# Load veupathUtils from the mounted repo — picks up any local edits immediately
setwd('/home/rstudio/veupathUtils')
load_all()

# Read the real input data downloaded from MinIO
dt <- fread('/home/rstudio/Downloads/differential_expression_input')

# Reproduce the Java plugin's operations (copy from DifferentialExpressionPlugin.java)
# e.g. the dcast pivot that converts tall → wide format:
countData <- data.table::dcast(dt, `SampleID_col` ~ `GeneID_col`,
                               value.var = 'CountCol', fill = NA_integer_)

# Now test the veupathUtils class/function with the result
cdc <- CountDataCollection(data = countData, recordIdColumn = 'SampleID_col', name = 'debug')
```

### Step 4: Iterate with load_all()

Edit source files in `R/` on the host, then re-run the script. The `load_all()` call at the top picks up all changes each run — no rebuild or reinstall needed.

## Known Issues / Warnings

The test suite produces 33 warnings (expected):
- DESeq2 warnings about character variables in design formula being converted to factors
- Warnings about `breaks` method `sd` not supporting `nbins`/`binwidth`
- Various expected warnings in edge case tests

These warnings are test-related and do not indicate problems with the package.

## Tips for Claude Code Sessions

1. **Long-running tests**: Use Task agent with general-purpose subagent to save context
2. **Container access**: Always use `/home/rstudio/veupathUtils` as working directory in container
3. **R documentation**: Use `?functionName` or check `man/` directory
4. **Bioconductor**: Remember this uses Bioc 3.22 packages - version compatibility matters
5. **S4 classes**: This package uses S4 OOP - use `showMethods()` and `getClass()` for inspection
6. **Data.table**: Many functions return data.table objects - be aware of reference semantics

## Recent Changes

- Version 2.9.0 (current)
- **New Feature**: Differential expression now supports both DESeq2 and limma methods
  - Added `ArrayDataCollection` class for continuous expression data
  - Added `limma` method for antibody arrays and microarrays
  - Full backwards compatibility maintained
  - See "Differential Expression Methods" section below
- Recent branches:
  - ✅ Array-based limma analysis (`add-ab-array-limma`) - **COMPLETE**
  - Bioconductor remotes configuration
  - PCA percent variance calculations

## Differential Expression Methods

The package supports two differential expression backends:

### DESeq2 (for count data)
- **Use with**: `CountDataCollection`
- **Data type**: Integer count data (RNA-seq, microbial abundance)
- **Method**: `differentialExpression(..., method = 'DESeq')`
- **Algorithm**: DESeq2 negative binomial GLM

### limma (for continuous data)
- **Use with**: `ArrayDataCollection`
- **Data type**: Continuous expression data (microarray, antibody array)
- **Expects**: Pre-normalized data (log-transformed, normalized)
- **Method**: `differentialExpression(..., method = 'limma')`
- **Algorithm**: limma empirical Bayes (lmFit + eBayes)

### Example Usage

```r
# For count data (RNA-seq)
countData <- CountDataCollection(...)
result <- differentialExpression(countData, comparator, method = 'DESeq')

# For array data (antibody arrays, microarrays)
arrayData <- ArrayDataCollection(...)  # Pre-normalized continuous data
result <- differentialExpression(arrayData, comparator, method = 'limma')
```

Both methods return the same `ComputeResult` structure with `DifferentialExpressionResult` statistics containing:
- `effectSize` - log2 fold change
- `pValue` - P-value
- `adjustedPValue` - Adjusted p-value (Benjamini-Hochberg)
- `pointID` - Feature identifier

### Implementation Notes

- All package calls use fully qualified syntax (`limma::lmFit()`, `DESeq2::DESeq()`)
- No namespace conflicts between DESeq2 and limma
- Test coverage: 82 differential expression tests (13 existing DESeq2, 5 new limma, plus integration tests)
- Test data: `testArrayDataCollection` (50 samples × 100 antibodies)

## Contact / Authors

- Danielle Callan (dcallan@upenn.edu)
- Ann Blevins (annsize@upenn.edu) - Maintainer
- Bob MacCallum (r.maccallum@imperial.ac.uk)

## Additional Resources

- **GitHub Issues**: https://github.com/VEuPathDB/veupathUtils/issues
- **CI Status**: Check R-CMD-check badge in README
- **License**: Apache 2.0 (see LICENSE.md)
