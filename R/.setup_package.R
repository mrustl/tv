# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install kwb.pkgbuild in R
install.packages('kwb.pkgbuild')


pkg <- list(
  name = "tv",
  title = "R Package for TV (Tarifvertrag)",
  desc = paste(
    "R package for TV (Tarifvertrag)."
  )
)

kwb.pkgbuild::use_pkg_skeleton("tv")

kwb.pkgbuild::use_pkg(
  pkg = pkg,
  copyright_holder = list(name = "Michael Rustler", start_year = NULL),
  user = "mrustl"
)

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("tv", org = "mrustl")

usethis::use_pipe()
usethis::use_vignette("workflow")
desc::desc_normalize()
