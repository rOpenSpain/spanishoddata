# before release
# usethis::use_version("patch")
# usethis::use_version("minor")
cffr::cff_write()
codemetar::write_codemeta(write_minimeta = T)
usethis::use_tidy_description()
# urlchecker::url_check()
# devtools::check(remote = TRUE, manual = TRUE)
# devtools::check(cran = TRUE)
# devtools::check_win_devel()
# revdepcheck::revdep_check(num_workers = 4)

# devtools::submit_cran()

# usethis::use_github_release()
# usethis::use_dev_version(push = TRUE)
