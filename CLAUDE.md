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
  -v ~/Desktop/EDA/veupathUtils:/home/rstudio/veupathUtils \
  -e PASSWORD=password -p 8888:8787 veupathdb/study-wrangler
```

**Important paths**:
- Host: `/home/maccallr/Desktop/EDA/veupathUtils`
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
- **Last known results**: [ FAIL 0 | WARN 33 | SKIP 0 | PASS 650 ]
- **Test files**: Located in `tests/testthat/`
- **Largest test files**:
  - `test-class-Megastudy.R` (862 lines)
  - `test-method-differentialExpression.R` (545 lines)
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
- `CountDataCollection` - Count data (RNA-seq, etc.)
- `Megastudy` - Large study data aggregation
- `Statistic`, `Comparator` - Statistical operations
- `ComputeResult`, `CorrelationResult` - Result containers

**Methods**:
- `method-differentialExpression.R` - DESeq2-based differential expression
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
- **Bioconductor** (3.22): SummarizedExperiment, DESeq2
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

- `add-ab-array-limma` (current)
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
- Recent branches focus on:
  - Array-based limma analysis (`add-ab-array-limma`)
  - Bioconductor remotes configuration
  - PCA percent variance calculations

## Contact / Authors

- Danielle Callan (dcallan@upenn.edu)
- Ann Blevins (annsize@upenn.edu) - Maintainer
- Bob MacCallum (r.maccallum@imperial.ac.uk)

## Additional Resources

- **GitHub Issues**: https://github.com/VEuPathDB/veupathUtils/issues
- **CI Status**: Check R-CMD-check badge in README
- **License**: Apache 2.0 (see LICENSE.md)
