# VoroGaze

Run the VoroGaze facial area-of-interest (AOI) tools on your own computer.
This R package combines the **Worked Example** and the **Research Workbench**
in one local Shiny app. The worked example starts with a face image, prepared
AOI centres and synthetic fixations. The workbench imports your own fixation
report and face images, checks their geometry, and exports annotated results.

## Install and open

Install [R](https://cran.r-project.org/) 4.2.2 or newer. RStudio is optional.
Run these commands in the R console:

```r
install.packages("pak")
pak::pkg_install("VoroGaze=github::mjgreen/vorogaze")
VoroGaze::run_vorogaze()
```

The browser opens on your own computer. The first installation downloads the
dependencies listed in `DESCRIPTION`. On Windows and macOS, CRAN's binary
packages are the simplest installation route. Linux installations may need
development libraries for ImageMagick++, curl, SSL and XML; see the
[magick installation guidance](https://docs.ropensci.org/magick/articles/intro.html#installing-magick).

If you download this repository as a ZIP instead, extract it into a folder and
install that folder, including its dependencies:

```r
pak::local_install("/path/to/extracted/vorogaze-main")
VoroGaze::run_vorogaze()
```

For an exact source revision, add `@COMMIT_SHA` to the GitHub specification
above. `DEPENDENCIES.md` records the dependency versions used for the Linux
acceptance check and how to request those direct versions explicitly. This is
not a lockfile for every transitive package or operating-system library.

## First session

1. Open **Worked Example** to inspect the bundled face, synthetic fixations
   and AOI centres. Select a centre or table row to rename, move or delete it;
   click empty image space to add a centre. **Undo** restores the previous edit.
2. Choose **Assign fixations** after changing centres. **Reset examples**
   restores the prepared example.
3. For your own analysis, use **Fixations**, **Screen**, **Faces** and **Sanity**
   to check the report mappings and coordinate system.
4. In **AOI Workbench**, define and assign centres, then **Commit/replace face**
   for each face/condition you want to include.
5. In **Exports**, download the **Annotated fixation report – Recommended**.
   It preserves source rows and columns and appends assignment/geometry fields.
   The participant-summary route is Beta and requires explicit aggregation
   choices; its download includes the specification and an R reproduction script.

Only one small, deterministic 20-row example and one face image are bundled.
The gaze data describe two invented participants. Larger research datasets
and the former hosted workbench's full face collection are not part of this
distribution. The image is a real, attributed Face Research Lab London Set
image, licensed CC BY 4.0; the gaze observations are entirely synthetic.

## Local operation

The app listens on `127.0.0.1` by default. Uploaded files remain in a private
temporary directory for the Shiny session and are removed when that session
ends. Save wanted exports before closing. Use appropriately de-identified
data and your institution's approved storage arrangements.

Use the R console's Stop button or `Ctrl+C` to stop the app. To select a
different unused port or open the browser yourself:

```r
VoroGaze::run_vorogaze(port = 4848L, launch.browser = FALSE)
```

Keep the default host for desktop use. This package does not supply a hosted
service, authentication system or public-server deployment configuration.

## Versions and licensing

Version 0.2.0 contains the shared AOI editor used by both example and workbench.
The earlier [ECVP 2026 package](https://github.com/mjgreen/vorogaze_aoi_ecvp_2026) remains a separate frozen
conference snapshot; this repository is the maintained successor.

Package code is MIT licensed. The synthetic CSV is dedicated under CC0 1.0.
The face image remains CC BY 4.0. Preserve `THIRD_PARTY_NOTICES.md` and the
licence texts when redistributing. VoroGaze supports a documented analysis
plan; the mapping, geometry and aggregation choices still need your review.
