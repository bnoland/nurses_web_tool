# Introduction

Web tool for viewing union membership and union contract coverage among
registered nurses in the United States using data from the Current Population
Survey (CPS). The tool is currently hosted at the following location:

<https://bnoland.shinyapps.io/nurses_web_tool/>

# General usage

For instructions on how to use the web tool read the included documentation,
located under `docs/docs.pdf`.

# Setting things up

To set up the web tool, first clone or download this repository. Included
alongside the code is an RStudio project. Using RStudio is perhaps the easiest
way to modify the code, update the underlying data, deploy the web tool, and so
forth.

## Directory structure

* `raw_data/` contains zip files of the raw CPS microdata files, obtainable from
the CPS microdata page:

    <https://thedataweb.rm.census.gov/ftp/cps_ftp.html?#>

* `data/` contains the processed data (including the data used directly by the
web tool).

* `tests/` contains regression tests which can be run using the `shinytest`
package.

* `docs/` contains the documentation for the web tool (including the LaTeX
source).

## Preparing the data

Several steps are required for preparing raw CPS data for use with the web tool:

* First, the data needs to be downloaded in the form of zip files from the CPS
microdata page and placed under `raw_data/`.

* Next, the raw data needs to be extracted from the zip files under `raw_data/`.
This process consists of extracting the pertinent variables from the raw CPS
files and merging the data into the single file `data/nurses_raw.csv`. This is
accomplished by running the script `get_raw_data.R`. Note that the raw CPS data
is in fixed-width format, and so this script needs to be updated accordingly if
the format specification changes.

* Finally, the data needs to be transformed into a format usable directly by the
web tool. This is accomplished by running the script `preprocess_raw_data.R`.
The resulting data is stored in the file `data/nurses_preprocessed.csv`.

See the scripts `get_raw_data.R` and `preprocess_raw_data.R` for more details.

## Running the regression tests

These are stored under `tests/`, and need to be run using the `shinytest`
package:

<https://cran.r-project.org/web/packages/shinytest/index.html>

Make sure the `shinytest` package is installed. Once you have it installed, you
can run all the tests at once using the commands
```r
library(shinytest)
testApp()
```
You should run the tests before you deploy the web tool every time you modify
the code or data, or if any of the packages used by the web tool are updated.

## Deploying to the web

To deploy the web tool to <https://www.shinyapps.io/>, first make sure that the
`rsconnect` package is installed and properly configured. Once you've done this,
deploying the web tool is as simple as running the command
```r
rsconnect::deployApp(appFileManifest = "manifest")
```
The named argument `appFileManifest = "manifest"` specifies the a manifest file
containing the names of the files to be deployed to the server. In this case,
the file is simply named `manifest`, and is contained in the top-level directory
of the repository.

# Licensing

The code is licensed under the GNU General Public License v3.0:

<https://www.gnu.org/licenses/gpl-3.0.en.html>

A copy of this license is available in the file `LICENSE`.
