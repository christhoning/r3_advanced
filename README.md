TODO: R3 Advanced

# r3_advanced:

TODO: Give a brief description of what your project is about

This project will be used for the R3 Advanced course. It will contain
various exercises and notes from the course.

# Brief description of folder and file contents

TODO: As project evolves, add brief description of what is inside the
data, doc and R folders.

The following folders contain: - `data-raw/`: The raw data, which has
not been wrangled or manipulated. - `data/`: Data outputs which have
undergone wrangling and manipulation. The data that is used for the
analysis. - `doc/`: Contain the .qmd files with the learning code, as
well as the finished code. - `R/`: Contains the R scripts, such as
functions and manual color schemes, which can be sourced into the .qmd
files.

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `r3_advanced.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.

