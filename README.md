# AdvancedR3: My little journey into the world of Quarto

This project will teach me the ways of reproducible research. Things
like:

-   Cool tidy pipelines
-   Using Git and linking with GitHub
-   Enjoyment

# Brief description of folder and file contents

The following folders contain:

-   `data/`: All data for this course
-   `doc/`: Includes documentation and fun stuff
-   `R/`: In here lies the powerful functions that will change the way
    you look at the world

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
