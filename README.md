# veupathUtils

veupathUtils is an R package which provides helper functions for solving common problems in the VEuPathDB project.

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install veupathUtils. From the R command prompt:

```R
remotes::install_github('VEuPathDB/veupathUtils')
```

## Usage
This package is primarily intended for use as a dependency in other R packages. In order to establish that depedency the developer of the 
dependent package must follow these steps:
1. add ```veupathUtils``` to the ```Imports``` section of the dependent package's ```DESCRIPTION``` file.
2. add a ```Remotes``` section to the dependent package's ```DESCRIPTION``` file.
3. add ```VEuPathDB/veupathUtils``` to the ```Remotes``` section of the dependent package's ```DESCRIPTION``` file.
4. add ```#' @import veupathUtils``` to the dependent package's package-level documentation file (usually called ```{mypackage}-package.R```).
5. run ```devtools::document()```.

The developer of the dependent package can either install this package using ```remotes``` as descripted in the "Installation" section above,
or if they mean to also develop veupathUtils simultaneously, can use ```devtools::load_all("{path-to-veupathUtils}")``` to load this package in 
their R session.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

As a general policy, we're exporting every function that gets added here. So unless you have very good reason, export!

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

## Github Actions
<!-- badges: start -->
  [![R-CMD-check](https://github.com/VEuPathDB/veupathUtils/workflows/R-CMD-check/badge.svg)](https://github.com/VEuPathDB/veupathUtils/actions)
  <!-- badges: end -->
