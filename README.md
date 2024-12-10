# AdvancedR3: DDEA Course on Reproducible Reseaarch in R.

This project is used in the Advanced DDEA Course on Reproducible
Research in R. A metabolomics dataset will be used for teaching
purposes. The focus of the course is collaboration, transparency, and
reproducibility. To aid these tasks, Rstudio, Git, and Github will be
employed.

# Brief description of folder and file contents

The following folders contain:

-   `data/`:
    -   Lipidomics (dataset)
-   `doc/`:
    -   Learning.qmd
        -   here we write some code, alter it, and prepare final
            functions
-   `R/`:
    -   functions.R
        -   here we store the final functions that we develop in
            learning.qmd)

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
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
