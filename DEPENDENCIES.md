# Dependency versions

The package declares supported minimum versions in `DESCRIPTION` and uses
ordinary CRAN packages. The following direct versions are the bounded Linux
acceptance environment for VoroGaze 0.2.0 on R 4.5.1:

```r
install.packages("pak")
pak::pkg_install(c(
  "bslib@0.9.0", "deldir@2.0-4", "digest@0.6.37", "dplyr@1.1.4",
  "DT@0.34.0", "htmltools@0.5.8.1", "magick@2.9.0", "png@0.1-8",
  "readr@2.1.5", "readxl@1.4.5", "rlang@1.1.6", "shiny@1.11.1",
  "stringr@1.5.2", "tibble@3.3.0", "writexl@1.5.4", "zip@3.0.2"
))
```

Pin the VoroGaze GitHub commit as described in the README for an exact source
snapshot. These direct versions do not pin transitive R dependencies, system
libraries or your operating system. Archive your analysis input, AOI choices,
exports and R `sessionInfo()` for reproducibility.

The acceptance run installs the standalone package into a new temporary
library and checks its public launcher and both tool areas. Windows/macOS
installation and every future package combination are not established by
that Linux check. No private container registry is required to install this
public R package.
